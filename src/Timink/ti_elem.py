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

import re
import simplestyle
import inkex
from ti_info import VERSIONJOINT
from ti_version import VersionJoint
from ti_math import isfinite
from ti_pointtransform import PointTransf
from ti_signalspec import SignalSpecParser
from ti_usrparams import UsrParams

NSS = inkex.NSS
assert 'timink' not in NSS
NSS['timink'] = u"http://timink.sourceforge.net/"

class Elem:
    """
    Helper for handling of 'path' elements.
    """

    def __init__(self, node):
        """
        node: etree node or None.
        """
        if isinstance(node, Elem):
            node = node.getNode()
        self._node = node

    def getNode(self):
        return self._node

    def getParent(self):
        assert self._node is not None
        return Elem(self._node.getparent())

    def getLabel(self):
        assert self._node is not None
        return self._node.attrib.get(inkex.addNS('label','inkscape'), None)

    def setLabel(self, label):
        assert self._node is not None
        attribName = inkex.addNS('label','inkscape')
        if label is not None:
            self._node.attrib[attribName] = label
        elif attribName in attribs:
            del self._node.attrib[attribName]

    @staticmethod
    def parseLength(s):
        m = re.match('^ *([+-]?[0-9.]+([eE][+-]?[0-9])?) *([^0-9+\\- ]*) *$', s)
        value, unit = (None, None)
        if m is not None:
            try:
                value = float(m.group(1))
                unit  = m.group(3)
            except ValueError:
                pass
        return (value, unit)

    @staticmethod
    def _parseTransformList(s):
        # http://www.w3.org/TR/2003/REC-SVG11-20030114/coords.html#TransformAttribute
        assert s is not None
        sR = re.sub('[\t\r\n]+', ' ', s)
        transfListRegexp = re.compile('^ *([^\\(\\), ]+) *\\( *([^\\(\\)]*) *\\) *,? *(.*)$')
        transfTokenList = []
        isComplete = False
        while not isComplete:
            m = transfListRegexp.match(sR)
            if m is None:
                isComplete = True
            else:
                transName, transfArgs, sR = m.groups()
                transfArgs = re.split(' *,? *', transfArgs)
                if transfArgs == [ '' ]:
                    transfArgs = []
                transfTokenList.append((transName, transfArgs))
                isComplete = len(sR) == 0
        return transfTokenList, sR

    def getTransform(self):
        assert self._node is not None
        return Elem.getTransformForStr(self._node.attrib.get('transform', ''))

    def setTransform(self, transform):
        assert self._node is not None
        transformAttrib = None
        if transform is not None and not transform.isIdentity():
            if transform.m == (1, 0, 0, 1):
                # translation only
                transformAttrib = 'translate(%s, %s)' % (str(transform.o[0]), str(transform.o[1]))
            else:
                m11, m12, m21, m22 = transform.m
                oX, oY = transform.o
                transformAttrib = 'matrix(%s)' % ', '.join(map(str, [m11, m21, m12, m22, oX, oY]))
        if transformAttrib is not None:
            self._node.attrib['transform'] = transformAttrib
        elif 'transform' in self._node.attrib:
            del self._node.attrib['transform']

    @staticmethod
    def getTransformForStr(transfromListAttrStr):
        assert transfromListAttrStr is not None
        transfTokenList, invalidEnd = Elem._parseTransformList(transfromListAttrStr)
        transf = PointTransf.createIdentity()
        if len(invalidEnd) == 0:
            try:
                for transfName, transfArgTokenList in transfTokenList:
                    transfArgList = map(lambda s: float(s), transfArgTokenList)
                    argNo = len(transfArgList)
                    t = None
                    if transfName == 'translate':
                        if argNo < 1 or argNo > 2:
                            raise ValueError()
                        if argNo < 2:
                            transfArgList.append(0)
                        tx, ty = transfArgList
                        t = PointTransf.createTransl((tx, ty))
                    elif transfName == 'scale':
                        if argNo < 1 or argNo > 2:
                            raise ValueError()
                        if argNo < 2:
                            transfArgList.append(transfArgList[0])
                        sx, sy = transfArgList
                        t = PointTransf.createScale(sx, sy)
                    elif transfName == 'rotate':
                        if argNo not in [1, 3]:
                            raise ValueError()
                        aDegree = transfArgList[0]
                        if argNo > 1:
                            cy, cy = transfArgList[1:]
                            t = PointTransf.createRot(aDegree, (cy, cy))
                        else:
                            t = PointTransf.createRot0(aDegree)
                    elif transfName == 'skewX':
                        if argNo != 1:
                            raise ValueError()
                        t = PointTransf.createSkewX(transfArgList[0])
                    elif transfName == 'skewY':
                        if argNo != 1:
                            raise ValueError()
                        t = PointTransf.createSkewY(transfArgList[0])
                    elif transfName == 'matrix':
                        if argNo != 6:
                            raise ValueError()
                        m11, m21, m12, m22 = transfArgList[:4]
                        oX, oY = transfArgList[4:]
                        t = PointTransf((m11, m12, m21, m22), (oX, oY))
                    else:
                        raise ValueError()
                    if not t.isIdentity():
                        transf = PointTransf.createConcat(transf, t)
            except ValueError:
                transf = None
        return transf

    def remove(self):
        assert self._node is not None
        assert self._node.getparent() is not None
        self._node.getparent().remove(self._node)

    def makeSiblingPredecessorOf(self, newPredecessor):
        """
        Makes a sibling node of this node a predecessor (on the left of this node).
        """
        newPredecessor = Elem(newPredecessor).getNode()
        assert newPredecessor is not None
        assert self._node is not None
        assert self._node != newPredecessor
        parent = self._node.getparent()
        assert parent is not None
        parent.remove(newPredecessor)
        parent.insert(parent.index(self._node), newPredecessor)

    def copyAttribFrom(self, srcNode, attribName):
        """
        Copys an optional attribute from another node.
        The node is left untouched, if srcNode is None or does not have an attribute
        of name attribName.
        """
        srcNode = Elem(srcNode).getNode()
        assert self._node is not None
        assert srcNode is None or srcNode != self._node
        if srcNode is not None:
            if attribName in self._node.attrib:
                del self._node.attrib[attribName]
            attribValue = srcNode.attrib.get(attribName, None)
            if attribValue is not None:
                self._node.attrib[attribName] = attribValue

    def copyIdFrom(self, srcNode):
        """
        Copys the 'id' attribute from another node.
        """
        self.copyAttribFrom(srcNode, 'id')

    def copyTransformFrom(self, srcNode):
        """
        Copys the 'transform' attribute from another node.
        """
        self.copyAttribFrom(srcNode, 'transform')

    def copyStyleFrom(self, srcNode):
        """
        Copys the 'style' attribute from another node.
        """
        self.copyAttribFrom(srcNode, 'style')

    def compareStyleWith(self, cmpNode):
        """
        Compares the styles of this nodes and another node.
        Returns True, if the 'style' attributes contain the same key/value pairs
        (regardless of their order, distinguishing upper/lower case).
        """
        cmpNode = Elem(cmpNode).getNode()
        assert self._node is not None
        assert cmpNode is not None
        key = 'style'
        style = simplestyle.parseStyle(self._node.attrib.get(key, None))
        cmpStyle = simplestyle.parseStyle(cmpNode.attrib.get(key, None))
        return style == cmpStyle

    def removeStyle(self):
        assert self._node is not None
        didChange = False
        if 'style' in self._node.attrib:
            del self._node.attrib['style']
            didChange = True
        return didChange

    def removeStroke(self):
        assert self._node is not None
        didChange = False
        styleDict = simplestyle.parseStyle(self._node.attrib.get('style', None))
        if styleDict.get('stroke', 'black') != 'none':
            styleDict['stroke'] = 'none'
            didChange = True
        strokePropertyNames = [
            'stroke-dasharray', 'stroke-dashoffset', 'stroke-linecap',
            'stroke-linejoin', 'stroke-miterlimit', 'stroke-opacity', 'stroke-width'
        ]
        for propertyName in strokePropertyNames:
            if propertyName in styleDict:
                del styleDict[propertyName]
                didChange = True
        self._node.attrib['style'] = simplestyle.formatStyle(styleDict)
        return didChange

    def removeFill(self):
        assert self._node is not None
        didChange = False
        styleDict = simplestyle.parseStyle(self._node.attrib.get('style', None))
        if styleDict.get('fill', 'black') != 'none':
            styleDict['fill'] = 'none'
            didChange = True
        for propertyName in [ 'fill-opacity', 'fill-rule' ]:
            if propertyName in styleDict:
                del styleDict[propertyName]
                didChange = True
        self._node.attrib['style'] = simplestyle.formatStyle(styleDict)
        return didChange

    def forceVisibleStroke(self):
        assert self._node is not None
        didChange = False
        styleDict = simplestyle.parseStyle(self._node.attrib.get('style', None))
        if styleDict.get('stroke', 'none') == 'none':
            styleDict['stroke'] = 'black'
            didChange = True

        width, unit = Elem.parseLength(styleDict.get('stroke-width', '1.0'))
        if width is None or width == 0.0:
            styleDict['stroke-width'] = '1.0'
            didChange = True
        self._node.attrib['style'] = simplestyle.formatStyle(styleDict)
        return didChange

    @staticmethod
    def testIt():
        assert Elem.parseLength('0') == (0.0, '')
        assert Elem.parseLength('') == (None, None)
        assert Elem.parseLength('1.0') == (1.0, '')
        assert Elem.parseLength('-1.0e+3mm') == (-1000.0, 'mm')
        assert Elem.parseLength('1.0e3 mm') == (1000.0, 'mm')

        transfTokenList, invalidEnd = Elem._parseTransformList('\nabc (1e,\tf2),  123 \n')
        assert transfTokenList == [ ('abc', ['1e', 'f2']) ]
        assert invalidEnd == '123  '
        transfTokenList, invalidEnd = Elem._parseTransformList('A() B (1, 2) ,C(1 3 4), ')
        assert transfTokenList == [ ('A', []), ('B', ['1', '2']), ('C', ['1', '3', '4']) ]
        assert invalidEnd == ''

        assert Elem.getTransformForStr('').isIdentity()

        assert not Elem.getTransformForStr('translate(0, 1)').isIdentity()
        assert not Elem.getTransformForStr('translate(1, 2), translate(-1, -2.1)').isIdentity()
        assert Elem.getTransformForStr('translate(1, 2), translate(-1, -2)').isIdentity()
        assert Elem.getTransformForStr('translate(1), translate(-1)').isIdentity()
        assert Elem.getTransformForStr('translate(1, 2, 3)') is None

        assert not Elem.getTransformForStr('scale(2, 4)').isIdentity()
        assert not Elem.getTransformForStr('scale(2, 4), scale(0.5, 0.24)').isIdentity()
        assert Elem.getTransformForStr('scale(2, 4), scale(0.5, 0.25)').isIdentity()
        assert Elem.getTransformForStr('scale(2), scale(0.5, 0.5)').isIdentity()
        assert Elem.getTransformForStr('scale(1, 2, 3)') is None

        assert not Elem.getTransformForStr('rotate(90)').isIdentity()
        assert Elem.getTransformForStr('rotate(90), rotate(270)').isIdentity()
        assert not Elem.getTransformForStr('rotate(90, 2, 3), rotate(-90)').isIdentity()
        assert Elem.getTransformForStr('rotate(90, 2, 3), rotate(-90, 2, 3)').isIdentity()
        assert Elem.getTransformForStr('rotate(1, 2, 3, 4)') is None

        assert not Elem.getTransformForStr('skewX(1)').isIdentity()
        assert Elem.getTransformForStr('skewX(0)').isIdentity()
        assert not Elem.getTransformForStr('skewX(90)').isIdentity()
        assert Elem.getTransformForStr('skewX(180)').isIdentity()
        assert Elem.getTransformForStr('skewX(1, 2)') is None

        assert not Elem.getTransformForStr('skewY(1)').isIdentity()
        assert Elem.getTransformForStr('skewY(0)').isIdentity()
        assert not Elem.getTransformForStr('skewY(90)').isIdentity()
        assert Elem.getTransformForStr('skewY(180)').isIdentity()
        assert Elem.getTransformForStr('skewY(1, 2)') is None

        assert Elem.getTransformForStr('matrix(1, 0, 0, 1, 0, 0)').isIdentity()
        assert Elem.getTransformForStr('matrix(2, 0, 0, 4, 10, -20), translate(-10, 20), scale(0.5, 0.25)').isIdentity()


class PathElem(Elem):
    """
    Helper for handling of 'path' elements.
    """

    def __init__(self, node):
        """
        node: etree node or None.
        """
        Elem.__init__(self, node)

    def getStartPoint(self):
        """
        Returns the start point of the path in absolute coordinates (before any transformation)
        or None, if this is not a 'path' node.
        """
        startPoint = None
        if self._node is not None and self._node.tag == inkex.addNS('path','svg'):
            m = re.match('^[mM] +([^, ]+) *, *([^, ]+) +', self._node.attrib.get('d', ''))
            if m is not None:
                try:
                    startPoint = (float(m.group(1)), float(m.group(2)))
                except ValueError:
                    pass
        return startPoint

    def setToEmpty(self):
        """
        Changes this to an empty path.
        """
        assert self._node is not None
        # 'd' attribute is required, bus can be empty.
        # http://www.w3.org/TR/2003/REC-SVG11-20030114/paths.html#PathData
        self._node.attrib['d'] = ''

    @staticmethod
    def addCombinedLines(parentNode, verticesList, yScale, startPoint, styleDict, doClose):
        """
        Inserts a 'path' element below parentNode, consisting of line segments
        according to the given vertices.

        verticesList is a (possibly empty) list of vertices.
        Each vertices is a list of at least 2 tuples (x, y) of vertex positions.
        The y-coordinates are inverted (increase: towards the top of the canvas).

        startPoint is None or (startPointX, startPointY).
        If startPoint is not, all vertex positions are calculated
        as (x + startPointX, yScale * y + startPointY).

        If startPoint is not None, the first -- probably invisible -- point of the resulting path
        is startPoint.

        If doClose is True, all subpaths (built by element of verticesList) are closed.

        styleDict is a dictionary of properties for the "style" attribute.
        """
        if startPoint is None:
            startPointX, startPointY = (0, 0)
        else:
            startPointX, startPointY = startPoint
        parentNode = Elem(parentNode).getNode()
        assert verticesList is not None
        assert isfinite(yScale)
        assert isfinite(startPointX) and isfinite(startPointY)

        pathSpecs = []
        for vertices in verticesList:
            partPathSpec = None
            for x, y in vertices:
                if partPathSpec is None:
                    partPathSpec = 'M'
                else:
                    partPathSpec = partPathSpec + ' L'
                assert isfinite(x) and isfinite(y)
                partPathSpec = partPathSpec + ' %s,%s' % (str(startPointX + x), str(startPointY + yScale * y))
            if startPoint is not None and vertices[0] != (startPointX, startPointY):
                partPathSpec = 'M %s,%s ' % (str(startPointX), str(startPointY)) + partPathSpec
            if doClose:
                partPathSpec = partPathSpec + ' Z'
            pathSpecs.append(partPathSpec)
        pathSpec = ' '.join(pathSpecs)
        del pathSpecs

        attribs = {
            'style': simplestyle.formatStyle(styleDict),
            'd':     pathSpec
        }
        return PathElem(inkex.etree.SubElement(parentNode, inkex.addNS('path','svg'), attribs))


class TiminkSignalGElem(Elem):
    """
    Helper for handling of signal groups ('g' elements representing a signal) created by Timink.

    A signal group can contain any number of elements, but the following elements are
    treated specially:
      - 'path' elements with label 'signalpath'<pathindex>
      - 'path' elements with label 'signalshading
    """

    _LABEL_SIGNALGROUP = u'signalgroup'
    _LABEL_SIGNALPATH = u'signalpath'
    _LABEL_SHADINGPATH = u'shadingpath'

    def __init__(self, node):
        """
        node: etree node or None.
        """
        Elem.__init__(self, node)

    def getSignalIndex(self):
        """
        Returns the signal index (>= 0) if this is a signal node
        and None otherwise.
        """
        index = None
        if self._node is not None and self._node.tag == inkex.addNS('g','svg'):
            if not TiminkTopLevelGElem(self._node).isValid():
                label = self.getLabel()
                if label is not None:
                    regexpStr = '^' + re.escape(TiminkSignalGElem._LABEL_SIGNALGROUP) + '(0|[1-9][0-9]*)$'
                    m = re.match(regexpStr, label)
                    if m is not None:
                        index = int(m.group(1))
        return index

    def _getChildPathNodesWithLabels(self):
        """
        Returns a dictonary of all 'path' element with a 'label' attribute, which are direct
        children of this node, or None is this node is None.

        Returned dictionary:
            key:   value 'label' attribute of node (!= None)
            value: non-empty list of PathElem objects (!= None) with path.getNode().getparent() != None
        """
        nodeDict = None
        if self._node is not None:
            nodeDict = dict()
            for child in self._node.iterchildren():
                if child.tag == inkex.addNS('path','svg'):
                    label = Elem(child).getLabel()
                    if label is not None:
                        nodeDict[label] = nodeDict.get(label, []) + [PathElem(child)]
        return nodeDict

    @staticmethod
    def _getSignalPathIndexOfLabel(label):
        index = None
        regexpStr = '^' + re.escape(TiminkSignalGElem._LABEL_SIGNALPATH) + '(0|[1-9][0-9]*)$'
        m = re.match(regexpStr, label)
        if m is not None:
            index = int(m.group(1))
        return index

    def getCategorizedPaths(self):
        """
        Returns several list of 'path' elements, grouped by meaning.

        Returns: None or (signalPaths, wasteSignalPaths)

        signalPaths is a non-empty list of PathElem objects.
        For each i with 0 <= i < len(signalPaths): signalPaths[i] is 'path' element
        for signal path node i.

        wasteSignalPaths is a list of supernumerous PathElem objects, which should be
        removed.
        """
        info = None
        pathNodeDict = self._getChildPathNodesWithLabels()
        if pathNodeDict is not None:
            signalPathNodeDict = dict() # key: path index, value: node ('path')
            shadingPath = None
            wasteSignalPaths = []
            if pathNodeDict is not None:
                for label, nodes in pathNodeDict.iteritems():
                    if label == TiminkSignalGElem._LABEL_SHADINGPATH:
                        shadingPath = nodes[0]
                        if len(nodes) > 1:
                            wasteSignalPaths.extend(nodes[1:])
                    else:
                        pathIndex = TiminkSignalGElem._getSignalPathIndexOfLabel(label)
                        if pathIndex is not None:
                            # all elements of nodes are signal paths
                            signalPathNodeDict[pathIndex] = nodes[0]
                            # all nodes but the first node of a given signal path index are treated as "waste"
                            if len(nodes) > 1:
                                wasteSignalPaths.extend(nodes[1:])
                        else:
                            wasteSignalPaths.extend(nodes)
            signalPaths = []
            if len(signalPathNodeDict) > 0:
                for pathIndex in range(0, max(signalPathNodeDict.keys()) + 1):
                   signalPaths.append(signalPathNodeDict.get(pathIndex, None))
            info = (signalPaths, shadingPath, wasteSignalPaths)
        return info

    @staticmethod
    def addEmpty(parentNode, signalIndex):
        """
        Adds an empty (without child nodes) signal group for the given signalIndex >= 0
        and returns the new node.
        """
        parentNode = Elem(parentNode).getNode()
        assert parentNode is not None
        assert signalIndex >= 0
        label = (TiminkSignalGElem._LABEL_SIGNALGROUP + '%d') %  signalIndex
        signalGroup = TiminkSignalGElem(inkex.etree.SubElement(parentNode, inkex.addNS('g','svg')))
        signalGroup.setLabel(label)
        return signalGroup

    def addSignalPath(self, pathIndex, verticesList, yScale, (startPointX, startPointY)):
        """
        Inserts a signal path element below this node, consisting of line segments
        according to the given vertices.

        verticesList is a (possibly) empty list of vertices.
        Each vertices is a list of at least 2 tuples (x, y) of vertex positions.
        The y-coordinates are inverted (increase: towards the top of the canvas).

        startPoint is None or (startPointX, startPointY).
        If startPoint is not, all vertex positions are calculated
        as (x + startPointX, yScale * y + startPointY).

        If startPoint is not None, the first -- probably invisible -- point of the resulting path
        is startPoint.
        """
        assert self._node is not None
        assert pathIndex >= 0
        assert verticesList is not None
        assert isfinite(startPointX) and isfinite(startPointY)

        styleDict = {'stroke': 'black', 'fill': 'none'}
        p = PathElem.addCombinedLines(self._node, verticesList, yScale, (startPointX, startPointY),
                                      styleDict, False)
        p.setLabel((TiminkSignalGElem._LABEL_SIGNALPATH + '%d') % pathIndex)
        return p

    def addShadingPath(self, verticesList, yScale, (startPointX, startPointY)):
        """
        Inserts a shading path element below this node, consisting of line segments
        according to the given point vertices.

        verticesList is a (possibly empty) list of vertices.
        Each vertices is a list of at least 2 tuples (x, y) of vertex positions.
        The y-coordinates are inverted (increase: towards the top of the canvas).

        startPoint is None or (startPointX, startPointY).
        If startPoint is not, all vertex positions are calculated
        as (x + startPointX, yScale * y + startPointY).

        If startPoint is not None, the first -- probably invisible -- point of the resulting path
        is startPoint.
        """
        assert self._node is not None
        assert verticesList is not None
        assert isfinite(startPointX) and isfinite(startPointY)

        styleDict = {'stroke': 'none', 'fill': 'black', 'fill-opacity': '0.25'}
        p = PathElem.addCombinedLines(self._node, verticesList, yScale, (startPointX, startPointY),
                                      styleDict, True)
        p.setLabel(TiminkSignalGElem._LABEL_SHADINGPATH)
        return p


class TiminkTopLevelGElem(Elem):
    """
    Helper for handling of top-level group ('g' element created by Timink).
    """

    # element names of top-level group
    _ATTRIBNAME_VERSION    = u'version'
    _ATTRIBNAME_SIGNALSPEC = u'signalspec'
    _ATTRIBNAME_USRPARAMS  = u'usrparams'
    _ATTRIBNAMESET = set([
        _ATTRIBNAME_VERSION,
        _ATTRIBNAME_SIGNALSPEC,
        _ATTRIBNAME_USRPARAMS
    ])

    def __init__(self, node):
        """
        node: etree node or None.
        """
        Elem.__init__(self, node)

    def _getAttribs(self):
        assert self._node is not None
        assert '{' not in NSS['timink'] and '}' not in NSS['timink']
        attribDict = dict()
        try:
            for a, v in self._node.attrib.iteritems():
                prefix = '{%s}' % NSS['timink']
                if a.startswith(prefix):
                    an = a[len(prefix):]
                    attribDict[an] = v.decode('string-escape').decode('utf-8')
        except UnicodeDecodeError:
            attribDict = None
        return attribDict

    def isValid(self, beStrict=False):
        """
        Check if this is a Timink top level 'g' element.
        """
        ok = False
        attribDict = self.getAttribs()
        if attribDict is not None:
            # only valid version is required
            versionStr = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_VERSION, '')
            try:
                version = VersionJoint(versionStr)
                ok = True
            except ValueError:
                pass
            if beStrict:
                signalSpec = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_SIGNALSPEC, '')
                ok = ok and SignalSpecParser.isValid(signalSpec)
                usrParams = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_USRPARAMS, '')
                usrParams = UsrParams.fromStr(usrParams)
                ok = ok and usrParams is not None and usrParams.isValid()
        return ok

    def getAttribs(self):
        """
        Returns the Timink specific attributes, assuming that this node is the top
        'g' element generated by Timink.
        Returns None, if there is an error in the encoding of an attribute.
        """
        attribDict = None
        if self._node is not None and self._node.tag == inkex.addNS('g','svg'):
            # is 'g' element
            attribDict = self._getAttribs()
        return attribDict

    def setAttribs(self, signalSpec, usrParams):
        """
        Sets the value of all Timink specific attributes for this node.

        signalSpec: valid signal specification
        usrParams:  valid user params
        """
        assert self._node is not None
        assert SignalSpecParser.isValid(signalSpec)
        assert usrParams is not None and usrParams.isValid()
        attribDict = self._getAttribs()
        removedAttribSet = set(attribDict.keys()) - TiminkTopLevelGElem._ATTRIBNAMESET
        for a in removedAttribSet:
            del self._node.attrib['{%s}%s' % (NSS['timink'], a)]
            del attribDict[a]
        attribDict[TiminkTopLevelGElem._ATTRIBNAME_VERSION]    = str(VERSIONJOINT)
        attribDict[TiminkTopLevelGElem._ATTRIBNAME_SIGNALSPEC] = signalSpec
        attribDict[TiminkTopLevelGElem._ATTRIBNAME_USRPARAMS]  = usrParams.toStr()
        for a, v in attribDict.iteritems():
            attribDict[a] = v.encode('utf-8').encode('string-escape')
        for a in attribDict.keys():
            self._node.attrib['{%s}%s' % (NSS['timink'], a)] = attribDict[a]
        return removedAttribSet

    def getVersionJoint(self):
        versionJoint = None
        attribDict = self.getAttribs()
        if attribDict is not None:
            try:
                versionStr = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_VERSION, '')
                versionJoint = VersionJoint(versionStr)
            except ValueError:
                pass
        return versionJoint

    def getSignalSpec(self):
        signalSpec = None
        attribDict = self.getAttribs()
        if attribDict is not None:
            signalSpec = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_SIGNALSPEC, signalSpec)
        return signalSpec

    def getUsrParams(self):
        usrParams = None
        attribDict = self.getAttribs()
        if attribDict is not None:
            usrParams = attribDict.get(TiminkTopLevelGElem._ATTRIBNAME_USRPARAMS, usrParams)
        return usrParams

    @staticmethod
    def getSelected(selectedNodeDict):
        """
        Returns the selected top-level groups
        (topTiminkGroup, selectedTiminkGroups, selectedOtherElems).

        selectedTiminkGroups:
            list of all top-level groups with selected elements (lowest level).
        topTiminkGroup:
            None or lowest level top-level group containing all elements
            of selectedTiminkGroups.
        selectedOtherElems:
            list of all other selected elements.
        """

        def findLowestContaining(node):
            gnode = node
            while gnode is not None and not TiminkTopLevelGElem(gnode).isValid():
                gnode = gnode.getparent()
            return gnode # None if not found

        def getNodePath(node):
            p = []
            while node is not None:
                p.append(node)
                node = node.getparent()
            p.reverse()
            return p

        def findLowestCommonParent(nodeA, nodeB):
            pathA = getNodePath(nodeA)
            pathB = getNodePath(nodeB)
            i = 0
            n = min(len(pathA), len(pathB))
            while i < n and pathA[i] == pathB[i]:
                i = i + 1
            if i > 0:
                return pathA[i - 1]
            else:
                return None

        selectedTiminkGroups = set()
        selectedOtherElems = []
        for key, node in selectedNodeDict.iteritems():
            gnode = findLowestContaining(node)
            if gnode is not None:
                # found containing 'g' element generated by Timink
                selectedTiminkGroups.add(gnode)
            else:
                # node is not contained in a 'g' element generated by Timink
                selectedOtherElems.append(node)
        selectedTiminkGroups = list(selectedTiminkGroups)
        topTiminkGroup = list(selectedTiminkGroups)
        while len(topTiminkGroup) > 1:
            # find common containing 'g' element generated by Timink
            gnode = findLowestCommonParent(topTiminkGroup[0], topTiminkGroup[1])
            gnode = findLowestContaining(gnode)
            del topTiminkGroup[0:2]
            if gnode is not None:
                # replace by common parent 'g' element
                topTiminkGroup.append(gnode)
        if len(topTiminkGroup) > 0:
            topTiminkGroup = topTiminkGroup[0]
        else:
            topTiminkGroup = None
        return (topTiminkGroup, selectedTiminkGroups, selectedOtherElems)

    def getSignalGroups(self):
        """
        Returns a dictionary of all signal groups, which are direct children of this node,
        of None is this node is None.

        key:   signal index (>= 0)
        value: TiminkSignalGElem object (!= None) with getNode().getparent() != None
        """
        signalGroupsDict = None
        if self._node is not None:
            signalGroupsDict = dict()
            for child in self._node.iterchildren():
                i = TiminkSignalGElem(child).getSignalIndex()
                if i is not None:
                    signalGroupsDict[i] = signalGroupsDict.get(i, []) + [TiminkSignalGElem(child)]
        return signalGroupsDict

    @staticmethod
    def addEmpty(parentNode, signalSpec, usrParams):
        """
        Adds an empty (without child nodes) Timink top-level group element and returns the new node.
        """
        parentNode = Elem(parentNode).getNode()
        assert SignalSpecParser.isValid(signalSpec)
        assert usrParams is not None and usrParams.isValid()
        signalGroup = TiminkTopLevelGElem(inkex.etree.SubElement(parentNode, inkex.addNS('g','svg')))
        removedAttribSet = signalGroup.setAttribs(signalSpec, usrParams)
        assert len(removedAttribSet) == 0
        assert signalGroup.isValid(True)
        return signalGroup

Elem.testIt()
