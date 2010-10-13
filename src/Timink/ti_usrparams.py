# Timink - an Inkscape plugin for digital timing diagrams.
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

import math
import re
from ti_math import isfinite

class UsrParams(object):

    # from inkex.uuconv:
    LENGTH_UNIT_DICT = {
        'px': 1.0,
        'pt': 1.25,
        'mm': 3.5433070866,
        'in': 90.0
    }

    def __init__(self):
        self.resetToDefault()

    @staticmethod
    def buildLength(x, unitStr):
        assert isfinite(x)
        assert unitStr is None or unitStr in UsrParams.LENGTH_UNIT_DICT
        lengthStr = unicode(x)
        if unitStr is not None:
            lengthStr = lengthStr + unitStr
        return lengthStr

    @staticmethod
    def parseLength(lengthStr):
        r = None
        if lengthStr is not None and (isinstance(lengthStr, str) or isinstance(lengthStr, unicode)):
            # http://www.w3.org/TR/2003/REC-SVG11-20030114/types.html
            lengthRegexp = re.compile(r'^(([-+]?[0-9]+(\.[0-9]*)?|[-+]?\.[0-9]+)([eE][-+]?[0-9]+)?)( *([a-zA-Z]+))?$')
            m = lengthRegexp.match(lengthStr)
            if m is not None:
                try:
                    x = float(m.group(1))
                    if m.group(6) is None:
                        unitStr = u'px'
                    else:
                        unitStr = unicode(m.group(6))
                    pxPerUnit = UsrParams.LENGTH_UNIT_DICT[unitStr]
                    valueInPx = x * pxPerUnit
                    if isfinite(valueInPx):
                        r = (x, unitStr, valueInPx)
                except (ValueError, KeyError):
                    pass
        return r

    @staticmethod
    def getLengthValue(lengthStr):
        value = float('nan')
        r = UsrParams.parseLength(lengthStr)
        if r is not None:
            x, unitStr, value = r
            assert isfinite(value)
        return value

    def resetToDefault(self):
        self.unitTimeWidth = u'10px'
        self.signalHeight = u'10px'
        self.edgeTimeWidth = u'2px'
        self.placementMethod = u'homogeneous'
        self.originDistX = u'0px'
        self.originDistY = u'20px'

    def isValid(self):
        self.unitTimeWidth
        ok =        UsrParams.getLengthValue(self.unitTimeWidth) > 0.0
        ok = ok and UsrParams.getLengthValue(self.signalHeight) > 0.0
        ok = ok and UsrParams.getLengthValue(self.edgeTimeWidth) >= 0.0
        ok = ok and self.placementMethod in (u'homogeneous', u'individual')
        ok = ok and isfinite(UsrParams.getLengthValue(self.originDistY))
        ok = ok and UsrParams.getLengthValue(self.originDistY) > 0.0
        return ok

    def toStr(self):
        """
        Returns a unique representation of this object as a string, consisting only of
        printable characters.
        """

        if not self.isValid():
            raise ValueError
        d = {
            u'unitTimeWidth':   self.unitTimeWidth,
            u'signalHeight':    self.signalHeight,
            u'edgeTimeWidth':   self.edgeTimeWidth,
            u'placementMethod': self.placementMethod,
            u'originDistX':     self.originDistX,
            u'originDistY':     self.originDistY
        }
        def encode(s):
            s = s.encode('utf-8').encode('string-escape')
            s = s.replace(u':', u'\\x3a').replace(u';', u'\\x3b')
            return s
        pairs = []
        for k in sorted(d.keys()):
            pairs.append(encode(k) + ':' + encode(d[k]))
        return ';'.join(pairs)

    @staticmethod
    def fromStr(paramStr):
        p = UsrParams.parseStr(paramStr)
        if p is not None:
            p, invalidKeys, unsupportedParamKeys = p
        return p

    @staticmethod
    def parseStr(paramStr):
        r = None

        def decode(s):
            return s.decode('string-escape').decode('utf-8')

        paramDict = None
        if paramStr is not None:
            try:
                paramDict = dict()
                if paramStr != '':
                    for pair in paramStr.split(';'):
                        k, v = pair.split(':') # ValueError, if wrong number of elements
                        if len(k) == 0:
                            raise ValueError
                        paramDict[decode(k)] = decode(v)
            except (UnicodeDecodeError, ValueError):
                paramDict = None

        if paramDict is not None:
            params = UsrParams()
            invalidKeys = set()
            unsupportedParamKeys = set()
            for k, v in paramDict.iteritems():
                valueOk = False
                if k == u'unitTimeWidth':
                    if UsrParams.getLengthValue(v) > 0.0:
                        params.unitTimeWidth = v
                        valueOk = True
                elif k == u'signalHeight':
                    if UsrParams.getLengthValue(v) > 0.0:
                        params.signalHeight = v
                        valueOk = True
                elif k == u'edgeTimeWidth':
                    if UsrParams.getLengthValue(v) >= 0.0:
                        params.edgeTimeWidth = v
                        valueOk = True
                elif k == u'placementMethod':
                    if v in (u'homogeneous', u'individual'):
                        params.placementMethod = v
                        valueOk = True
                elif k == u'originDistX':
                    if isfinite(UsrParams.getLengthValue(v)):
                        params.originDistX = v
                        valueOk = True
                elif k == u'originDistY':
                    if UsrParams.getLengthValue(v) > 0.0:
                        params.originDistY = v
                        valueOk = True
                else:
                    invalidKeys.add(k)
                    valueOk = True
                if not valueOk:
                    unsupportedParamKeys.add(k)

            r = (params, invalidKeys, unsupportedParamKeys)

        return r

    def __str__(self):
        return self.toStr()

    def __cmp__(self, other):
        """
        Defines a strict total order on the set of UsrParams object with isValid() = True.
        """
        return cmp(self.toStr(), other.toStr())

    @staticmethod
    def testIt():
        assert UsrParams.buildLength(0.0, None) == u'0.0'
        assert UsrParams.buildLength(0.0, 'mm') == u'0.0mm'
        assert UsrParams.buildLength(-1.23E45, 'mm') == u'-1.23e+45mm'

        assert UsrParams.parseLength(None) == None
        assert UsrParams.parseLength(1.0) == None
        assert UsrParams.parseLength('') == None
        assert UsrParams.parseLength('0mm') == (0.0, u'mm', 0.0)
        assert UsrParams.parseLength('0') == (0.0, u'px', 0.0)
        assert UsrParams.parseLength('0  mm') == (0.0, u'mm', 0.0)
        assert UsrParams.parseLength('0 mm ') == None
        assert UsrParams.parseLength(' 0mm') == None
        assert UsrParams.parseLength(' 0 mm') == None
        assert UsrParams.parseLength('mm') == None
        assert UsrParams.parseLength('1xy') == None
        assert UsrParams.parseLength('-1.23E45 px') == (-1.23E45, u'px', -1.23E45)
        assert UsrParams.parseLength('inf px') == None
        assert UsrParams.parseLength('inf') == None
        assert UsrParams.parseLength('1 mm') == (1.0, u'mm', 3.5433070866)
        assert UsrParams.parseLength('1 ft') == None

        assert math.isnan(UsrParams.getLengthValue(None))
        assert math.isnan(UsrParams.getLengthValue(''))
        assert math.isnan(UsrParams.getLengthValue('x'))
        assert UsrParams.getLengthValue('0 mm') == 0.0
        assert UsrParams.getLengthValue('-1.23E45 px') == -1.23E45
        assert UsrParams.getLengthValue('1 mm') == 3.5433070866

        assert UsrParams.fromStr(None) is None
        assert UsrParams.fromStr('') is not None
        assert UsrParams.fromStr('x::y') is None
        assert UsrParams.fromStr('x;;y') is None
        p = UsrParams.fromStr('unitTimeWidth:12 mm')
        assert p is not None
        assert p.unitTimeWidth == u'12 mm'
        p = UsrParams.fromStr('unitTimeWidth: 12 mm')
        assert p is not None
        p = UsrParams.fromStr('unitTimeWidth:12 mm;unitTimeWidth:13 mm')
        assert p is not None
        assert p.unitTimeWidth == u'13 mm'
        p = UsrParams.fromStr('\x75nitTimeWidth:\x312 mm')
        assert p is not None
        assert p.unitTimeWidth == u'12 mm'
        p = UsrParams.fromStr('unitTimeWidth:12 mm\\x')
        assert p is None
        p = UsrParams.fromStr('\\xFFunitTimeWidth:12 mm')
        assert p is None
        r = UsrParams.parseStr('itTimeWidth:1;nitTimeWidth:12 mm;unitTimeWidth:x')
        assert r is not None
        p, invalidKeys, unsupportedParamKeys = r
        assert invalidKeys == set([ u'itTimeWidth', u'nitTimeWidth' ])
        assert unsupportedParamKeys == set([ u'unitTimeWidth' ])
        r = UsrParams.parseStr('unitTimeWidth:')
        assert r is not None
        p, invalidKeys, unsupportedParamKeys = r
        assert invalidKeys == set()
        assert unsupportedParamKeys == set([u'unitTimeWidth'])

        assert UsrParams.fromStr(UsrParams().toStr()) is not None

        assert UsrParams() == UsrParams()
        assert not (UsrParams() != UsrParams())
        p = UsrParams()
        p.unitTimeWidth = '12 mm'
        assert not (p == UsrParams())
        assert p != UsrParams()
        assert p > UsrParams()
        assert not (p < UsrParams())
        assert UsrParams.fromStr('') == UsrParams()

        assert UsrParams.fromStr(p.toStr()) == p

        p = UsrParams()
        assert p.isValid()
        p.toStr()
        p.unitTimeWidth = 0.0
        assert not p.isValid()
        try:
            p.toStr()
            assert False
        except ValueError:
            pass

UsrParams.testIt()
