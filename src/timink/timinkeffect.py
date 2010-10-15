#!/usr/bin/env python

# Timink - an Inkscape extension for digital timing diagrams.
# Copyright (C) 2010 Daniel Lutz
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

"""
Effect:

  If nothing is selected:

    Inserts a Timink object into the current layer, approximately in the center of the current view.

  If one or more objects within a common Timink object is selected:

    Replaces the Timink object, conserving the previous line and fill styles and transformation.

  If others objects are selected:

    Error message.

XML representation of a Timink object (example excerpt):

  ...

  xmlns:ns0="http://timink.sourceforge.net/"

  ...

  <g                                                                        <-- signal cluster group
    ns0:version="..."
    ns0:signalclusterspec="Xxxx"
    ns0:usrparams="edgeTimeWidth:2.0px;..."
    id="...">

    <g                                                                      <-- signal group
       inkscape:label="signalgroup0"
       transform="..."
       id="...">

       <path inkscape:label="shadingpath" d="" style="..." id="..." />      <-- shading path
       <path inkscape:label="signalpath2" d="" style="..." id="..." />      <-- signal path
       <path inkscape:label="signalpath1" d="" style="..." id="..." />      <-- signal path
       <path inkscape:label="signalpath0" d="" style="..." id="..." />      <-- signal path
    <g/>

    ...

    <g                                                                      <-- signal group
       inkscape:label="signalgroup3"
       transform="..."
       id="...">

       <path inkscape:label="shadingpath" d="" style="..." id="..." />      <-- shading path
       <path inkscape:label="signalpath2" d="" style="..." id="..." />      <-- signal path
       <path inkscape:label="signalpath1" d="" style="..." id="..." />      <-- signal path
       <path inkscape:label="signalpath0" d="" style="..." id="..." />      <-- signal path
    <g/>
  </g>
"""


import sys
import re
import inkex

from ti_math import isfinite
from ti_info import EXTENSION_NAME, VERSIONJOINT
from ti_signalspec import SignalSpec, SignalClusterSpecParser
from ti_pointtransform import PointTransf
from ti_elem import Elem, PathElem, SignalGElem, SignalClusterGElem
from ti_usrparams import UsrParams
from ti_gui import escapeStringForUser, printInfo, UserError
from ti_gui import showErrorDlg, SignalClusterEditor


class TiminkEffect(inkex.Effect):

    def __init__(self):
        inkex.Effect.__init__(self)

    def analyzeExistingSignalGroups(self, sgDict):
        """
        Gathers information on the given signal groups.

        Identifies waste elements and collects (origin) positions.

        Returns: (sgInfoDict, signalOriginDict, wasteElemDict).
        """
        assert sgDict is not None

        # key: signalIndex, value: (oldSignalGroup, oldSignalPaths)
        sgInfoDict = dict()
        # key: signalIndex, value: list of waste elements
        wasteElemDict = dict()

        # signalOrigin: absolute position of origin of a signal group,
        # (0, 0), transformed by the transformation of the signal group
        signalOriginDict = dict() # key: signalIndex, value: signalOrigin (!= None)
        for signalIndex in sorted(sgDict.keys()):
            assert signalIndex >= 0

            sgList = sgDict[signalIndex]
            for wg in sgList[1:]:
                # all but the first element to be removed
                wasteElems = wasteElemDict.get(signalIndex, [])
                wasteElems.append(wg)
                wasteElemDict[signalIndex] = wasteElems
            oldSignalGroup = sgList[0]
            sgDict[signalIndex] = oldSignalGroup
            del sgList

            sgInfo = oldSignalGroup.getCategorizedPaths()
            if sgInfo is not None:
                # (re)use transform and style of existing signal group
                oldSignalPaths, oldShading, wastePaths = sgInfo
                for wp in wastePaths:
                    # all but the first element to be removed
                    wasteElems = wasteElemDict.get(signalIndex, [])
                    wasteElems.append(wp)
                    wasteElemDict[signalIndex] = wasteElems
                sgInfoDict[signalIndex] = (oldSignalGroup, oldSignalPaths, oldShading)

                # Determine path origin (only from signal group).
                # The 'transform' attribute of signal paths should be cleared later.
                oldSignalTransf = oldSignalGroup.getTransform()
                if oldSignalTransf is not None:
                    signalOriginDict[signalIndex] = oldSignalTransf.applyTo((0, 0))

            del oldSignalGroup
            del sgInfo

        return (sgInfoDict, signalOriginDict, wasteElemDict)


    def cleanupExistingSignalGroups(self, newSignalCount, sgDict, sgInfoDict, signalOriginDict, wasteElemDict):
        """
        Removes unused elements.
        """
        sgInfoDict = dict(sgInfoDict)
        signalOriginDict = dict(signalOriginDict)
        wasteElemDict = dict(wasteElemDict)
        for signalIndex in sorted(sgDict.keys()):
            assert signalIndex >= 0
            if signalIndex >= newSignalCount:
                # signal group _without_ a corresponding element in signal cluster specification
                # -> remove
                sgDict[signalIndex].remove()
                del sgDict[signalIndex]
                if signalIndex in sgInfoDict:
                    del sgInfoDict[signalIndex]
                if signalIndex in signalOriginDict:
                    del signalOriginDict[signalIndex]
                if signalIndex in wasteElemDict:
                    del wasteElemDict[signalIndex]
                printInfo('Signal {si}: Removed all SVG elements (no longer in signal cluster specification).'.format(si=signalIndex))

        for signalIndex in sorted(wasteElemDict.keys()):
            for we in wasteElemDict[signalIndex]:
                assert we is not None
                we.remove()
                escapedTag = escapeStringForUser(we.getNode().tag)
                printInfo('Signal {si}: Removed waste {et} element.'.format(si=signalIndex, et=escapedTag))

        return (sgInfoDict, signalOriginDict, wasteElemDict)


    @staticmethod
    def completeOriginsHomog(incomplSignalOriginDict, n, (originDiffX, originDiffY)):
        """
        Returns a copy of a dictionary of signal origins, extended to a given number of signals.

        The first signal with an known? origin is used as "fix point" (not changed).
        The origins of all other signals are recalculated such that the difference of
        the origin of each signal to tje origin of the preceding signal is
        (originDiffX, originDiffY).

        incomplSignalOriginDict:
          non-empty dictionary (key: signal index 0 .. n - 1, value: signal origin).
          signal origin: (x, y) where x and y are not None.
        n: number of signals.
        (originDiffX, originDiffY):
            used as signal origin difference of adjacent signals,
            if len(incomplSignalOriginDict) < 2.

        Returns:
            signalOriginDict, an extended copy of incomplSignalOriginDict.
            For each signal index i with 0 <= i < n:
              signalOriginDict[i] is the signal origin
        """

        assert incomplSignalOriginDict is not None
        assert len(incomplSignalOriginDict) > 0
        assert None not in incomplSignalOriginDict.values()
        assert len(incomplSignalOriginDict) == 0 \
            or (min(incomplSignalOriginDict) >= 0 and max(incomplSignalOriginDict) < n)
        assert isfinite(originDiffX) and isfinite(originDiffY)

        signalOriginDict = dict()

        refSignalIndex = min(incomplSignalOriginDict)
        xR, yR = incomplSignalOriginDict[refSignalIndex]
        for signalIndex in range(0, n):
            di = signalIndex - refSignalIndex
            signalOriginDict[signalIndex] = float(di * originDiffX) + xR, float(di * originDiffY) + yR

        return signalOriginDict


    @staticmethod
    def completeOriginsByInterp(incomplSignalOriginDict, n):
        """
        Returns a copy of a dictionary of signal origins, extended to a given number of signals.

        incomplSignalOriginDict:
          dictionary >= 2 elements (key: signal index 0 .. n - 1, value: signal origin).
          signal origin: (x, y) where x and y are not None.
        n: number of signals.

        Returns:
            signalOriginDict, an extended copy of incomplSignalOriginDict.
            For each signal index i with 0 <= i < n:
              signalOriginDict[i] is the signal origin
        """
        assert incomplSignalOriginDict is not None
        assert len(incomplSignalOriginDict) > 1
        assert None not in incomplSignalOriginDict.values()
        assert len(incomplSignalOriginDict) == 0 \
            or (min(incomplSignalOriginDict) >= 0 and max(incomplSignalOriginDict) < n)

        def vectorDiff((aX, aY), (bX, bY)):
            return (aX - bX, aY - bY)

        def vectorHomoMapping((vX, vY), scale, (oX, oY)):
            return (float(vX * scale + oX), float(vY * scale + oY))

        signalOriginDict = dict(incomplSignalOriginDict)

        for signalIndex, signalOrigin in incomplSignalOriginDict.iteritems():
            if signalOrigin is not None:
                signalOriginDict[signalIndex] = signalOrigin

        # interpolate signal origins of all signals between two signals at known origins
        siWithOrigin = sorted(signalOriginDict.keys())
        for i in range(0, len(siWithOrigin) - 1):
            l = siWithOrigin[i]
            r = siWithOrigin[i + 1]
            if l + 1 < r:
                signalOriginL = signalOriginDict[l]
                signalOriginR = signalOriginDict[r]
                d = vectorDiff(signalOriginR, signalOriginL)
                for j in range(l + 1, r):
                    signalOriginDict[j] = vectorHomoMapping(d, float(j - l) / (r - l), signalOriginL)
        siWithOrigin = sorted(signalOriginDict.keys())

        # extrapolate signal origins of all signals _before_ first signal at known origin
        iF = siWithOrigin[0]
        signalOriginF = signalOriginDict[iF]
        assert siWithOrigin[0] + 1 == siWithOrigin[1]
        d = vectorDiff(signalOriginDict[siWithOrigin[1]], signalOriginF)
        for j in range(0, iF):
            signalOriginDict[j] = vectorHomoMapping(d, j - iF, signalOriginF)

        # extrapolate signal origins of all signals _after_ last signal at known origin
        iL = siWithOrigin[-1]
        signalOriginL = signalOriginDict[iL]
        assert siWithOrigin[-2] + 1 == siWithOrigin[-1]
        signalOriginL = signalOriginDict[iL]
        d = vectorDiff(signalOriginL, signalOriginDict[siWithOrigin[-2]])
        for j in range(iL + 1, n):
            signalOriginDict[j] = vectorHomoMapping(d, j - iL, signalOriginL)

        assert None not in signalOriginDict.values()
        assert sorted(signalOriginDict.keys()) == range(0, n)
        return signalOriginDict


    def effect(self):
        """Perform the effect: create/modify Timink object (signal cluster group)."""

        selectedSignalClusterGroup, sg, so = SignalClusterGElem.getSelected(self.selected)
        try:
            if len(so) > 0:
                if len(so) == 1:
                    msg = 'Element selected which is not part of a signal cluster.'
                else:
                    msg = 'Elements selected which are not part of a signal cluster.'
                raise UserError(msg, 'Do select at most one signal cluster (and nothing else).')
            if len(sg) > 0 and selectedSignalClusterGroup is None:
                raise UserError('More than one signal cluster selected.',
                                'Do select at most one signal cluster (and nothing else).')
            del so, sg

            signalClusterSpecStr = ''
            versionJoint = None
            usrParams = UsrParams()
            if selectedSignalClusterGroup is not None:
                selectedSignalClusterGroup = SignalClusterGElem(selectedSignalClusterGroup)
                signalClusterSpecStr = selectedSignalClusterGroup.getSignalClusterSpec()
                if signalClusterSpecStr is None:
                    signalClusterSpecStr = ''
                versionJoint = selectedSignalClusterGroup.getVersionJoint()
                r = UsrParams.parseStr(selectedSignalClusterGroup.getUsrParams())
                if r is not None:
                    usrParams, invalidKeys, unsupportedParamKeys = r
                    if len(invalidKeys) > 0:
                        s = ', '.join(map(escapeStringForUser, list(invalidKeys)))
                        printInfo('Signal cluster (selected): Ignored unsupported attributes:\n' + s)
                    if len(unsupportedParamKeys) > 0:
                        s = ', '.join(map(escapeStringForUser, list(unsupportedParamKeys)))
                        printInfo('Signal cluster (selected): Ignored attributes with invalid values:\n' + s)

            if selectedSignalClusterGroup is None:
                sgDict = None
                sgInfoDict, signalOriginDict, wasteElemDict = (dict(), dict(), dict())
            else:
                sgDict = selectedSignalClusterGroup.getSignalGroups()
                sgInfoDict, signalOriginDict, wasteElemDict = self.analyzeExistingSignalGroups(sgDict)

            r = SignalClusterEditor(signalClusterSpecStr, usrParams, versionJoint,
                                    len(signalClusterSpecStr) == 0,
                                    len(signalOriginDict) > 1).run()

            if r is not None:

                signalClusterSpecStr, usrParams = r
                signalSpecStrs = SignalClusterSpecParser.split(signalClusterSpecStr)
                assert len(signalSpecStrs) > 0

                if selectedSignalClusterGroup is None:
                    # create new signal cluster group
                    scg = SignalClusterGElem.addEmpty(self.current_layer, signalClusterSpecStr, usrParams)
                else:
                    scg = selectedSignalClusterGroup
                    removedAttribs = sorted(list(scg.setAttribs(signalClusterSpecStr, usrParams)))
                    if len(removedAttribs) > 0:
                        s = ', '.join(map(lambda o: escapeStringForUser(o), removedAttribs))
                        printInfo('Signal cluster (selected): Removed unsupported attributes: ' + s)
                        del s
                    sgInfoDict, signalOriginDict, wasteElemDict = \
                        self.cleanupExistingSignalGroups(len(signalSpecStrs), sgDict, sgInfoDict,
                            signalOriginDict, wasteElemDict)
                del sgDict

                # set/complete signal origins
                centerOfView = (round(self.view_center[0]), round(self.view_center[1]))
                originDistY = UsrParams.getLengthValue(usrParams.originDistY)
                originDistX = UsrParams.getLengthValue(usrParams.originDistX)
                assert isfinite(originDistX) and isfinite(originDistY)
                if len(signalOriginDict) == 0:
                    signalOriginDict[0] = centerOfView
                if len(signalOriginDict) <= 1 or usrParams.placementMethod == u'homogeneous':
                    signalOriginDict = TiminkEffect.completeOriginsHomog(signalOriginDict, len(signalSpecStrs), (originDistX, originDistY))
                else:
                    signalOriginDict = TiminkEffect.completeOriginsByInterp(signalOriginDict, len(signalSpecStrs))
                del originDistX
                del originDistY

                unitTimeWidth = UsrParams.getLengthValue(usrParams.unitTimeWidth)
                assert isfinite(unitTimeWidth)
                signalHeight = UsrParams.getLengthValue(usrParams.signalHeight)
                assert isfinite(signalHeight)
                edgeTimeWidth = UsrParams.getLengthValue(usrParams.edgeTimeWidth)
                assert isfinite(edgeTimeWidth)

                # add / update signal groups according to sgInfoDict, signalOriginDict, signalSpecStrs
                for signalIndex in range(0, len(signalSpecStrs)):
                    signalGroup, signalPaths, shading = sgInfoDict.get(signalIndex, (None, [], None))
                    signalOrigin = signalOriginDict[signalIndex]

                    signalSpec = SignalSpec.createFromStr(signalSpecStrs[signalIndex], unitTimeWidth)
                    assert signalSpec is not None
                    pathVerticesList, shading01VerticesList = signalSpec.getAllPathVerticesAndShading(edgeTimeWidth)
                    pathNo = len(pathVerticesList)

                    # remove 'transform' of signal path and shading path
                    hadWasteTransf = False
                    for signalPath in signalPaths:
                        transf = signalPath.getTransform()
                        if transf is None or not transf.isIdentity():
                           signalPath.setTransform(None)
                           hadWasteTransf = True
                    if shading is not None:
                        transf = shading.getTransform()
                        if transf is None or not transf.isIdentity():
                           signalPath.setTransform(None)
                           hadWasteTransf = True
                    if hadWasteTransf:
                        printInfo('Signal {si}: Removed interfering \'transform\' attribute from \'path\' element.'.format(si=signalIndex))
                    del hadWasteTransf

                    signalPaths = signalPaths + [None] * (pathNo - len(signalPaths))
                    assert len(signalPaths) >= pathNo

                    if signalGroup is None:
                        signalGroup = SignalGElem.addEmpty(scg, signalIndex)
                        signalGroup.setTransform(PointTransf.createTransl(signalOrigin))
                    else:
                        transf = signalGroup.getTransform()
                        if transf is None:
                            signalGroup.setTransform(None)
                            printInfo('Signal {si}: Removed invalid \'transform\' attribute from \'g\' element.'.format(si=signalIndex))
                        else:
                            oldSignalOrigin = transf.applyTo((0, 0))
                            originDiff = (signalOrigin[0] - oldSignalOrigin[0], signalOrigin[1] - oldSignalOrigin[1])
                            if originDiff != (0.0, 0.0):
                                transf = PointTransf.createConcat(transf, PointTransf.createTransl(originDiff))
                                signalGroup.setTransform(transf)

                    # determine signal group, whose style is to be used for a new signal group
                    styleTmplSignalIndex = None
                    if signalIndex > 0:
                        styleTmplSignalIndex = signalIndex - 1
                    elif len(sgInfoDict) > 0:
                        # first existing signal group
                        styleTmplSignalIndex = min(sgInfoDict)
                    assert styleTmplSignalIndex is None or styleTmplSignalIndex in sgInfoDict

                    for pathIndex in range(0, pathNo):
                        pathVertices = pathVerticesList[pathIndex]
                        oldSignalPath = signalPaths[pathIndex]
                        if oldSignalPath is None:
                            # no old signal 'path' available -> create new one
                            newSignalPath = signalGroup.addSignalPath(pathIndex, [pathVertices], -signalHeight, (0, 0))
                            if styleTmplSignalIndex is not None:
                                tmplSignalGroup, signalPathsOfTmplSignalGroup, shadingOfTmplSignalGroup \
                                    = sgInfoDict[styleTmplSignalIndex]
                                if pathIndex < len(signalPathsOfTmplSignalGroup):
                                    # matching path in template signal group
                                    newSignalPath.copyStyleFrom(signalPathsOfTmplSignalGroup[pathIndex])
                                else:
                                    # new matching path in template signal group
                                    # -> copy style from preceding path instead
                                    newSignalPath.copyStyleFrom(signalPaths[pathIndex - 1])
                                if pathIndex == 0:
                                    # create empty 'path' element to preserve the style
                                    for pi in range(pathNo, len(signalPathsOfTmplSignalGroup)):
                                        emptySignalPath = signalGroup.addSignalPath(pi, [], -signalHeight, (0, 0))
                                        emptySignalPath.copyStyleFrom(signalPathsOfTmplSignalGroup[pi])
                                        del emptySignalPath
                        else:
                            newSignalPath = signalGroup.addSignalPath(pathIndex, [pathVertices], -signalHeight, (0, 0))
                            if pathIndex == 0:
                                newSignalPath.copyTransformFrom(oldSignalPath)

                            newSignalPath.copyIdFrom(oldSignalPath)
                            newSignalPath.copyStyleFrom(oldSignalPath)
                            # preserve position with respect to non-signal path elements in signal group:
                            oldSignalPath.makeSiblingPredecessorOf(newSignalPath)
                            oldSignalPath.remove()
                            if newSignalPath.removeFill():
                                printInfo('Signal {si}: Did reset interfering fill style from \'path\' element to \'none\'.'.format(si=signalIndex))
                            if newSignalPath.forceVisibleStroke():
                                printInfo('Signal {si}: Did reset invisible stroke style from \'path\' element to \'black\'.'.format(si=signalIndex))
                            if scg.removeStyle():
                                # possible, because assigning a style to a group in Inkscape
                                # sets the style of all contained elements (all levels below)
                                printInfo('Signal {si}: Did remove interfering style from \'g\' element.'.format(si=signalIndex))
                        # replace all transforms by transform of signal path 0
                        if pathIndex > 0:
                            newSignalPath.copyTransformFrom(signalPaths[0])
                        signalPaths[pathIndex] = newSignalPath

                    # remove unused signal paths
                    assert pathNo > 0
                    for pathIndex in range(len(signalPaths) - 1, pathNo - 1, -1):
                        assert pathIndex > 0
                        oldSignalPath = signalPaths[pathIndex]
                        if oldSignalPath is not None:
                            if signalPaths[pathIndex - 1] is not None and oldSignalPath.compareStyleWith(signalPaths[pathIndex - 1]):
                                # style can be derived of preceding signal path
                                oldSignalPath.remove()
                                del signalPaths[pathIndex]
                            else:
                                # perserve (otherwise style information is lost)
                                oldSignalPath.copyTransformFrom(signalPaths[0])
                                oldSignalPath.setToEmpty()

                    newShading = signalGroup.addShadingPath(shading01VerticesList, -signalHeight, (0, 0))
                    if shading is not None:
                        newShading.copyStyleFrom(shading)
                        shading.remove()
                    elif styleTmplSignalIndex is not None:
                        tmplSignalGroup, signalPathsOfTmplSignalGroup, shadingOfTmplSignalGroup \
                            = sgInfoDict[styleTmplSignalIndex]
                        if shadingOfTmplSignalGroup is not None:
                            newShading.copyStyleFrom(shadingOfTmplSignalGroup)
                    shading = newShading
                    shading.copyTransformFrom(signalPaths[0])
                    if shading.removeStroke():
                        printInfo('Signal {si}: Did reset stroke style from shading \'path\' element to \'none\'.'.format(si=signalIndex))

                    # sort signal path elements (drawing order)
                    existingSignalPaths = filter(lambda e: e is not None, signalPaths)
                    assert len(existingSignalPaths) > 0
                    for i in range(1, len(existingSignalPaths)):
                        existingSignalPaths[i - 1].makeSiblingPredecessorOf(existingSignalPaths[i])
                    # the order of all other non-signal path elements remains unchanged with
                    # respect to signal path 0 and to all other non-signal path elements

                    # draw shading first (appears as bottom-most)
                    existingSignalPaths[-1].makeSiblingPredecessorOf(shading)

                    assert signalGroup is not None
                    assert len(signalPaths) > 0
                    sgInfoDict[signalIndex] = (signalGroup, signalPaths, shading)

        except UserError, e:
            showErrorDlg(e.msg, e.hint)
            sys.exit(1)


    @staticmethod
    def testIt():
        assert TiminkEffect.completeOriginsHomog(\
            {0: (100, 100)},
            1, (100, 1000)) \
            == {0: (100.0, 100.0)}
        assert TiminkEffect.completeOriginsHomog(\
            {0: (100, 100)},
            2, (100, 1000)) \
            == {0: (100.0, 100.0), 1: (200.0, 1100.0)}
        assert TiminkEffect.completeOriginsHomog(\
            {1: (100, 100)},
            2, (100, 1000)) \
            == {0: (0.0, -900.0), 1: (100.0, 100.0)}
        assert TiminkEffect.completeOriginsByInterp(\
            {1: (200, 200), 2: (300, 300), 4: (0, 400)}, 7) \
            == {0: (100.0, 100.0), 1: (200, 200), 2: (300, 300), 3: (150.0, 350.0),
                4: (0, 400), 5: (-150.0, 450.0), 6: (-300.0, 500.0)}

