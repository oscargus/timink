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

import math
import re
from ti_math import sign, quotientOrInf, isfinite, gcd, lcm

class SignalSpecParser(object):
    """
    Signal specification.

    See http://sourceforge.net/apps/trac/timink/wiki/UserManual for the complete grammar.
    """

    @staticmethod
    def parse(signalSpecStr, unitTime, breakTime):
        """
        Creates the fragment state sequence for SignalSpec from a signal specification (for one signal).

        signalSpecStr: signal specification for one signal (exactly one non-empty line).
        unitTime: time for each state (> 0)
        breakTime: time for each break (> 0)
        Returns: list of state sequences for SignalSpec(), or None if signalSpecStr is not valid
        """

        assert isfinite(unitTime) and unitTime > 0.0
        assert isfinite(breakTime) and breakTime > 0.0

        STATESTRDICT = {
            '0': '0',
            '1': '1',
            '-': '-',
            'X': '10',
            'x': '01',
            'Y': '1001',
            'y': '0110',
        }

        fragmentStates = []
        incomplStates = []
        t = 0
        doShade = False
        multiStateStr = None
        ok = True

        i = 0

        try:
            while ok and i < len(signalSpecStr):
                c = signalSpecStr[i]
                if c == '[':
                    ok = not doShade
                    doShade = True
                elif c == ']':
                    ok = doShade
                    doShade = False
                elif c == '(':
                    ok = multiStateStr is None
                    multiStateStr = ''
                elif c == ')':
                    ok = multiStateStr is not None and len(multiStateStr) > 0
                    incomplStates.append((t, multiStateStr, doShade))
                    t = t + unitTime
                    multiStateStr = None
                elif c in ' \t':
                    pass
                elif c == '_':
                    if len(incomplStates) > 0:
                        incomplStates.append((t,) + incomplStates[-1][1:])
                        fragmentStates.append(incomplStates)
                        incomplStates = []
                    ok = multiStateStr is None and len(fragmentStates) > 0
                    t = t + breakTime
                elif multiStateStr is None:
                    incomplStates.append((t, STATESTRDICT[c], doShade))
                    t = t + unitTime
                elif c in '01-':
                    multiStateStr = multiStateStr + c
                else:
                    ok = False

                i = i + 1
        except KeyError:
            ok = False

        if len(incomplStates) > 0:
            incomplStates.append((t,) + incomplStates[-1][1:])
            fragmentStates.append(incomplStates)
        ok = ok and not doShade and multiStateStr is None and len(incomplStates) > 0 and len(fragmentStates) > 0

        if not ok:
            fragmentStates = None

        return fragmentStates

    @staticmethod
    def testIt():
        assert SignalSpecParser.parse('0011', 10.0, 5.0) == [
            [(0.0, '0', False), (10.0, '0', False), (20.0, '1', False), (30.0, '1', False), (40.0, '1', False)]
        ]
        assert SignalSpecParser.parse('01-Xx', 10.0, 5.0) == [
            [
                (0.0, '0', False), (10.0, '1', False), (20.0, '-', False), (30.0, '10', False),
                (40.0, '01', False), (50.0, '01', False)
            ]
        ]
        assert SignalSpecParser.parse('-[X(01-)]0', 10.0, 5.0) == [
            [
                (0.0, '-', False),
                (10.0, '10', True),
                (20.0, '01-', True),
                (30.0, '0', False),
                (40.0, '0', False)
            ]
        ]
        assert SignalSpecParser.parse('-[X()]0', 10.0, 5.0) is None
        assert SignalSpecParser.parse('-[X]0(', 10.0, 5.0) is None
        assert SignalSpecParser.parse('(10X)', 10.0, 5.0) is None
        assert SignalSpecParser.parse('(X10)', 10.0, 5.0) is None
        assert SignalSpecParser.parse(' ( 0 1- ) ', 10.0, 5.0) is not None
        assert SignalSpecParser.parse('', 10.0, 5.0) is None
        assert SignalSpecParser.parse('01\n0', 10.0, 5.0) is None

        assert SignalSpecParser.parse('01-_01', 10.0, 5.0) == [
            [(0.0, '0', False), (10.0, '1', False), (20.0, '-', False), (30.0, '-', False)],
            [(35.0, '0', False), (45.0, '1', False), (55.0, '1', False)]
        ]
        assert SignalSpecParser.parse('01- __ _ 01', 10.0, 5.0) == [
            [(0.0, '0', False), (10.0, '1', False), (20.0, '-', False), (30.0, '-', False)],
            [(45.0, '0', False), (55.0, '1', False), (65.0, '1', False)]
        ]
        assert SignalSpecParser.parse('_ _0', 10.0, 5.0) is None
        assert SignalSpecParser.parse('0_', 10.0, 5.0) is None

class SignalClusterSpecValidator(object):
    """
    Signal cluster specification.

    See http://sourceforge.net/apps/trac/timink/wiki/UserManual for the complete grammar.
    """

    @staticmethod
    def cleanUp(signalClusterSpec):
        assert signalClusterSpec is not None
        s = re.sub('(\r\n)|\r|\n', '\n', signalClusterSpec)
        s = re.sub('\t', ' ', s)
        s = re.sub(' *\n', '\n', s) # remove trailing white space
        s = re.sub('\n\n+', '\n\n', s)
        s = re.sub('^( *\n)*', '', s) # remove leading empty lines
        s = re.sub('( *\n *)*$', '', s) # remove trailing empty lines
        return s

    @staticmethod
    def normalize(signalClusterSpec):
        assert signalClusterSpec is not None
        s = SignalClusterSpecValidator.cleanUp(signalClusterSpec)
        s = re.sub(' *', '', s)  # remove white space
        s = re.sub('\n+', '\n', s) # remove empty lines
        return s

    @staticmethod
    def getInvalidCharPos(signalClusterSpec):
        assert signalClusterSpec is not None
        invCharPosList = []
        for i in range(0, len(signalClusterSpec)):
            if signalClusterSpec[i] not in '01-XxYy[]()_ \t\n\r':
                invCharPosList.append(i)
        return invCharPosList

    @staticmethod
    def getFirstNonmatchingParenthesis(signalClusterSpec):
        openElems = []
        openElemIndices = [] # openElemIndices[i] = signalClusterSpec[openElems[i]]
        nonmatchingIndex = None
        OPENINGELEMDICT = { '[': ']', '(': ')' }
        CLOSINGELEMS = OPENINGELEMDICT.values()
        i = 0
        while nonmatchingIndex is None and i < len(signalClusterSpec):
            c = signalClusterSpec[i]
            if c in '\r\n':
                if nonmatchingIndex is None and len(openElemIndices) > 0:
                    nonmatchingIndex = openElemIndices[0]
            elif c in OPENINGELEMDICT:
                if c in openElems: # nesting not allowed
                    nonmatchingIndex = i
                openElems.append(c)
                openElemIndices.append(i)
            elif c in CLOSINGELEMS:
                if len(openElemIndices) > 0 and OPENINGELEMDICT[signalClusterSpec[openElemIndices[-1]]] == c:
                    del openElems[-1]
                    del openElemIndices[-1]
                else:
                    nonmatchingIndex = i
            i = i + 1
        if nonmatchingIndex is None and len(openElemIndices) > 0:
            nonmatchingIndex = openElemIndices[0]
        return nonmatchingIndex

    @staticmethod
    def getFirstInvalidMultiPathState(signalClusterSpec):
        invalidCharRange = None
        start = signalClusterSpec.find('(')
        while invalidCharRange is None and start >= 0:
            end = signalClusterSpec.find(')', start + 1)
            if end > start:
                multiStateStr = signalClusterSpec[start + 1: end].replace('\t', '').replace(' ', '')
                if len(multiStateStr) == 0 or len(multiStateStr.strip('01-')) > 0:
                    invalidCharRange = (start, end + 1)
                start = signalClusterSpec.find('(', end + 1)
            else:
                invalidCharRange = (start, len(signalClusterSpec))
        return invalidCharRange

    @staticmethod
    def getFirstInvalidShading(signalClusterSpec):
        invalidCharRange = None
        m = re.search(r'(\[[ \t]*\])', signalClusterSpec)
        if m is not None:
            invalidCharRange = (m.start(1), m.end(1))
        return invalidCharRange

    @staticmethod
    def getFirstInvalidBreak(signalClusterSpec):
        invalidCharRange = None
        breakStartRegexp = re.compile(r'^[ \t\[]*(_([ \t]*_)*)', re.MULTILINE)
        breakEndRegexp = re.compile(r'(_([ \t]*_)*)[ \t\]]*$', re.MULTILINE)
        m = breakStartRegexp.search(signalClusterSpec)
        if m:
            invalidCharRange = (m.start(1), m.end(1))
        m = breakEndRegexp.search(signalClusterSpec)
        if m and (invalidCharRange is None or invalidCharRange[0] > m.start(1)):
            invalidCharRange = (m.start(1), m.end(1))
        return invalidCharRange

    @staticmethod
    def isValid(signalClusterSpec):
        assert signalClusterSpec is not None
        signalSpecs = SignalClusterSpecValidator.normalize(signalClusterSpec).split('\n')
        ok = len(signalSpecs) > 0
        i = 0
        while ok and i < len(signalSpecs):
            ok = SignalSpecParser.parse(signalSpecs[i], 1.0, 1.0) is not None
            i = i + 1
        return ok

    @staticmethod
    def testIt():
        assert SignalClusterSpecValidator.cleanUp('') == ''
        assert SignalClusterSpecValidator.cleanUp(' 0\t 1x AB  ') == ' 0  1x AB  '
        assert SignalClusterSpecValidator.cleanUp(' \t01xAB\nC\t \n  ') == '  01xAB\nC'
        assert SignalClusterSpecValidator.cleanUp('  \t\n\n 01xAB\nC\t \n  \t') == ' 01xAB\nC'
        assert SignalClusterSpecValidator.cleanUp('A\n\rB\r\nC\n\n\nC\r\r\rD') == 'A\n\nB\nC\n\nC\n\nD'

        assert SignalClusterSpecValidator.normalize('') == ''
        assert SignalClusterSpecValidator.normalize(' \t ') == ''
        assert SignalClusterSpecValidator.normalize(' 0\t 1x AB  ') == '01xAB'
        assert SignalClusterSpecValidator.normalize(' \t01xAB\nC\t \n  ') == '01xAB\nC'
        assert SignalClusterSpecValidator.normalize('  \t\n\n 01xAB\nC\t \n  \t') == '01xAB\nC'
        assert SignalClusterSpecValidator.normalize('A\n\rB\r\nC\n\n\nC\r\r\rD') == 'A\nB\nC\nC\nD'

        assert SignalClusterSpecValidator.getInvalidCharPos('') == []
        assert SignalClusterSpecValidator.getInvalidCharPos('01-xX') == []
        assert SignalClusterSpecValidator.getInvalidCharPos('0XB1xC') == [2, 5]

        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('') == None
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0123xx?634') == None
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[123x][x?634]') == None
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1]2[3') == 5
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1]23]') == 6
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1[2[3]') == 3
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1]\n2[3]') == None
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1\n]2[3]') == 1
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1()]2[3]') == None
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1()]2[3])') == 10
        assert SignalClusterSpecValidator.getFirstNonmatchingParenthesis('0[1(]2[3])') == 4

        assert not SignalClusterSpecValidator.isValid('')
        assert not SignalClusterSpecValidator.isValid('  ')
        assert not SignalClusterSpecValidator.isValid(' \t ')
        assert SignalClusterSpecValidator.isValid(' x 10 X- ')
        assert not SignalClusterSpecValidator.isValid(' \n  ')
        assert SignalClusterSpecValidator.isValid(' \n 1 ')

        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('') is None
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X') is None
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X()Y') == (3, 5)
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X(01-00-)Y') is None
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X( 0\t1 -0 0- )Y') is None
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X(01-0)Y(-)') is None
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X(01-0)Y(X-)') == (10, 14)
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X()') == (3, 5)
        assert SignalClusterSpecValidator.getFirstInvalidMultiPathState('01X(\n') == (3, 5)

        assert SignalClusterSpecValidator.getFirstInvalidShading('[0]') is None
        assert SignalClusterSpecValidator.getFirstInvalidShading('  [ \t]  ') == (2, 6)

        assert SignalClusterSpecValidator.getFirstInvalidBreak('') is None
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0_1\n10-\r\n0__0_ _1') is None
        assert SignalClusterSpecValidator.getFirstInvalidBreak(' __ _0') == (1, 5)
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0 _  __ ') == (2, 7)
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0_1\n10-\r\n _\t_ 0__0_ _1') == (10, 13)
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0_1_\n10-\r\n _\t_ 0__0_ _1') == (3, 4)
        assert SignalClusterSpecValidator.getFirstInvalidBreak('[_1]0') == (1, 2)
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0[_1]0') is None
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0[1_]0') is None
        assert SignalClusterSpecValidator.getFirstInvalidBreak('0[1_]') == (3, 4)

        assert SignalClusterSpecValidator.isValid('-[X(01-)]0')
        assert not SignalClusterSpecValidator.isValid('-[X()]0')

class SignalSpec(object):
    """
    Signal specification.

    The signal consists of _fragments_ separated by _breaks_.
    Each fragment is described by the sequence of states.
    """

    def __init__(self, fragmentStates=[]):
        self.fragmentStates = []
        for states in fragmentStates:
            self.appendFragment(states)

    @staticmethod
    def _getShadingRanges(states):
        """
        Returns a list of pair-wise disjunct intervals of all indices i of states
        with states[i][3] = True.

        Returns:
          [(start ,  end ), ..., start   , end    )]
                 0      0             n-1     n-1
          For each interval j: states[i][3] = True for each i with start_j <= i < end_j.
        """
        shadedRanges = []
        incomplStartIndex = None
        for i in range(0, len(states)):
            t, y, doShade = states[i]
            if doShade:
                if incomplStartIndex is None:
                    incomplStartIndex = i
            else:
                if incomplStartIndex is not None:
                    shadedRanges.append((incomplStartIndex, i))
                    incomplStartIndex = None
        if incomplStartIndex is not None:
            shadedRanges.append((incomplStartIndex, len(states)))
        return shadedRanges

    @staticmethod
    def _reduceToPeriod(stateStr):
        """
        Returns the shortest substring s of stateStr, such that
        stateStr[i] = s[i % len(s)] for all i with 0 <= i < len(stateStr).
        """
        s = stateStr
        i = stateStr.find(stateStr[0], 1)
        found = False
        while not found and i > 0:
            j = i
            found = len(stateStr) - i >= i
            while found and j < len(stateStr):
                found = (stateStr[j] == stateStr[j % i])
                j = j + 1
            if found: # periodic with period i?
                s = stateStr[:i]
            else:
                i = stateStr.find(stateStr[0], i + 1) # try longer
        return s

    @staticmethod
    def _isCollinear(p1, p0, p2):
        x1, y1 = p1
        x0, y0 = p0
        x2, y2 = p2
        v1 = (x1 - x0, y1 - y0)
        v2 = (x2 - x0, y2 - y0)
        # scalar product of difference vectors
        return (v1[0] * v2[0] + v1[1] * v2[1])**2 == (v1[0]**2 + v1[1]**2) * (v2[0]**2 + v2[1]**2)

    @staticmethod
    def _removeCollinearVertices(vertices):
        """
        Remove collinear vertices.
        """
        v = []
        for t0, y0 in vertices:
            if len(v) >= 2 and SignalSpec._isCollinear(v[-2], v[-1], (t0, y0)):
                del v[-1]
            v.append((t0, y0))
        return v

    def appendFragment(self, states):
        """
        Appends the states of a fragment.

        states: List of tuples (t, y) or (t, y, doShade), sorted by increasing t.
                t of the first element must be greater than last element of the last fragment of
                the current state sequence of this object.
        """
        assert len(states) > 0
        assert len(self.fragmentStates) == 0 or states[0][0] > self.fragmentStates[-1][-1][0]
        fragmentStates = []
        for s in states:
            assert len(s) in (2, 3)
            (t, stateStr, doShade) = (s + (False,))[:3]
            assert isfinite(2 * t * t) # guarantees finite scalar products
            assert len(stateStr) > 0
            assert len(fragmentStates) == 0 or t > fragmentStates[-1][0]
            reducedStateStr = SignalSpec._reduceToPeriod(stateStr)
            doShade = doShade and len(reducedStateStr) >= 2 and reducedStateStr[0] != reducedStateStr[1]
            if len(fragmentStates) >= 2 and fragmentStates[-2][1:] == fragmentStates[-1][1:]:
                del fragmentStates[-1]
            fragmentStates.append((float(t), reducedStateStr, doShade))
        self.fragmentStates.append(fragmentStates)

    def getFragmentCount(self):
        return len(self.fragmentStates)

    def getPeriod(self):
        """
        Returns the smalles integer n, such that getFragmPathVertices(j, i, ..) = getFragmPathVertices(j, i + n)
        for all i and j.
        """
        if len(self.fragmentStates) > 0:
            period = 1
            for states in self.fragmentStates:
                assert len(states) > 0
                for t, stateStr, doShade in states:
                    period = lcm(period, len(stateStr))
        else:
            period = 0
        return period

    @staticmethod
    def _interpolateAt(vertices, i, t):
        """
        Returns the interpolated value on the line vertices[i] to vertices[i + 1]
        at a given time in this interval.
        """
        assert i >= 0 and i + 1 < len(vertices)
        t0, y0 = vertices[i]
        t1, y1 = vertices[i + 1]
        assert t >= t0 and t <= t1
        dt = t1 - t0
        if dt == 0.0:
            y = (y0 + y1) / 2
        else:
            y = (t - t0) / dt * (y1 - y0) + y0
        y = max(y, min(y0, y1))
        y = min(y, max(y0, y1))
        return y

    @staticmethod
    def _getVerticesSlice(vertices, startTime, endTime):
        """
        Returns a "temporal slice" of the given vertices.
        """
        assert isfinite(startTime) and isfinite(endTime)

        v = []

        if startTime < endTime:
            i = 0
            while i + 1 < len(vertices) and vertices[i + 1][0] <= startTime:
                i = i + 1
            j = len(vertices) - 1
            while j - 1 >= i and vertices[j - 1][0] >= endTime:
                j = j - 1

            assert j >= -1
            v = vertices[i:j+1]

            if len(v) >= 2:
                if v[0][0] < startTime:
                    v[0]  = (startTime, SignalSpec._interpolateAt(v, 0, startTime))
                if v[-1][0] > endTime:
                    v[-1] = (endTime, SignalSpec._interpolateAt(v, len(v) - 2, endTime))
            else:
                v = []

        return v

    @staticmethod
    def _getVertexIntersPoint(vA, vB, iA, iB):
        """
        Returns the intersection point of the lines between vA[iA] to vA[iA + 1] and
        vB[iB] to vB[iB + 1], respectively, or None, if no intersection.

        If the two lines intersect at more than one point, the one with the smallest time
        is returned and the median of the vertical intersection.
        """
        assert iA >= 0 and iA + 1 < len(vA)
        assert iB >= 0 and iB + 1 < len(vB)

        p = None

        if vA[iA][0] <= vB[iB + 1][0] and vA[iA + 1][0] >= vB[iB][0]:

            tA = vA[iA][0]
            tAE = vA[iA + 1][0]
            dtA = tAE - tA
            tB = vB[iB][0]
            tBE = vB[iB + 1][0]
            dtB = tBE - tB
            yA = vA[iA][1]
            yAE = vA[iA + 1][1]
            dyA = yAE - yA
            yB = vB[iB][1]
            yBE = vB[iB + 1][1]
            dyB = yBE - yB

            assert dtA >= 0.0
            assert dtB >= 0.0

            sA = quotientOrInf(dyA, dtA)
            sB = quotientOrInf(dyB, dtB)

            assert not math.isnan(sA)
            assert not math.isnan(sB)

            if math.isinf(sA) and math.isinf(sB):
                t = (tA + tB) / 2  # tA != tB possible, if round to zero during division
                if t >= tA and t <= tAE and t >= tB and t <= tBE:
                    dy0 = yA - yB
                    dy1 = yAE - yBE
                    if dy0 == 0.0:
                        p = (t, (yA + yB) / 2)
                    elif dy1 == 0.0:
                        p = (t, (yAE + yBE) / 2)
                    elif sign(dy0) * sign(dy1) < 0.0:
                        # intersects at tA = tB -> choose median of all common points
                        if sign(dy0) > 0.0:
                            y = (min(yA, yBE) + max(yB, yAE)) / 2
                        else:
                            y = (max(yA, yBE) + min(yB, yAE)) / 2
                        p = (t, y)
            elif math.isinf(sA) and not math.isinf(sB):
                p = (tA, SignalSpec._interpolateAt(vB, iB, tA))
            elif not math.isinf(sA) and math.isinf(sB):
                p = (tB, SignalSpec._interpolateAt(vA, iA, tB))
            else:
                # (t - tA) * sA + yA = (t - tB) * sB + yB
                ds = sA - sB
                if ds != 0.0:
                    t = (yB - yA + tA * sA - tB * sB) / ds
                    if t >= tA and t <= tAE and t >= tB and t <= tBE:
                        p = (t, SignalSpec._interpolateAt(vA, iA, t))
                elif SignalSpec._isCollinear(vA[iA], vB[iB], vA[iA + 1]) or SignalSpec._isCollinear(vB[iB], vA[iA], vB[iB + 1]):
                    if tA >= tB:
                        p = (tA, yA)
                    else:
                        p = (tB, yB)

        return p

    @staticmethod
    def _getFillPathsBetween(verticesA, verticesB):
        """
        Returns a list of the paths around all non-empty areas between two paths.

        verticesA, verticesB:
          paths of vertices (t, y) with non-decreasing t along the indices
        Returns:
          (fillPaths, isFirstShadingOpen, isLastShadingOpen).
          fillPaths is a unique list of non-crossing paths (vertices) around all non-empty areas
          enclosed by the two paths described by verticesA, verticesB.
          Each pair of paths has at most one point in common.
          Path direction: counter-clockwise, starting at leftmost (and topmost, if ambiguous)
          common point.
          isFirstShadingOpen is True, if the first shading area in fillPaths is not bounded
          to the left by an intersection of the paths, but by the beginning of the paths.
          isLastShadingOpen is True, if the last shading area in fillPaths is not bounded
          to the right by an intersection of the paths, but by the end of the paths.
        """

        fillPaths = []
        # shading "open" to the left or the right, respectively?
        isFirstShadingOpen = False
        isLastShadingOpen = False

        if len(verticesA) >= 2 and len(verticesB) >= 2:

            vA = list(verticesA)
            vB = list(verticesB)

            iA = 0
            iB = 0

            shadingIndexRanges = [] # list of tuples ((iAStart, iBStart), (iAStop, iBStop))
            incomplStartIndices = None  # None or (iAStart, iBStart)
            if vA[iA] != vB[iB]:
                # shading is not "closed" to the left
                isFirstShadingOpen = True
                incomplStartIndices = (iA, iB)

            while iA + 1 < len(vA) and iB + 1 < len(vB):

                if incomplStartIndices is not None:

                    # find next intersection (not before max(vA[iA][0], vB[iB][0]))
                    found = False
                    p0 = vA[iA]
                    while not found and iA + 1 < len(vA) and iB + 1 < len(vB):
                        p = SignalSpec._getVertexIntersPoint(vA, vB, iA, iB)
                        if p is not None and p != p0:
                            # intersection
                            if vA[iA] != p:
                                iA = iA + 1
                                if vA[iA] != p:
                                    vA.insert(iA, p)
                            if vB[iB] != p:
                                iB = iB + 1
                                if vB[iB] != p:
                                    vB.insert(iB, p)
                            shadingIndexRanges.append((incomplStartIndices, (iA + 1, iB + 1)))
                            incomplStartIndices = None
                            found = True
                        else:
                            # no intersection
                            jB = iB + 1
                            if vB[jB][0] > vA[iA + 1][0]:
                                # no common point between intervals [vA[iA][0], vA[iA+1][0]] and
                                # [vA[jB][0], vA[jB+1][0]].
                                iA = iA + 1
                            else:
                                iB = jB

                    assert found or iA + 1 >= len(vA) or iB + 1 >= len(vB)
                    assert not found or vA[iA] == vB[iB]

                else:

                    # find last common point before next "split" and determine indices
                    # for search for next intersection.
                    found = False
                    while not found and iA + 1 < len(vA) and iB + 1 < len(vB):
                        if vA[iA + 1] == vB[iB + 1]:
                            iA = iA + 1
                            iB = iB + 1
                        else:
                            if vA[iA + 1][0] == vB[iB + 1][0]:
                                # vA[iA], vB[iB] is the last pair of common points
                                incomplStartIndices = (iA, iB)
                                found = True
                            elif vA[iA + 1][0] > vB[iB + 1][0]:
                                t = vB[iB + 1][0]
                                vA.insert(iA + 1, (t, SignalSpec._interpolateAt(vA, iA, t)))
                            else:
                                t = vA[iA + 1][0]
                                vB.insert(iB + 1, (t, SignalSpec._interpolateAt(vB, iB, t)))

            if incomplStartIndices is not None:
                # shading is not "closed" to the right
                isLastShadingOpen = vA[-1] != vB[-1]
                shadingIndexRanges.append((incomplStartIndices, (len(vA), len(vB))))
                incomplStartIndices = None

            for startIndices, stopIndices in shadingIndexRanges:
                fillPathUpper = vA[startIndices[0]:stopIndices[0]]
                fillPathLower = vB[startIndices[1]:stopIndices[1]]
                assert len(fillPathUpper) >= 2
                assert len(fillPathLower) >= 2

                # make sure that fillPathLower is "below" fillPathUpper
                doFlip = False
                if fillPathUpper[0] == fillPathLower[0]:
                    sUpper = quotientOrInf(fillPathUpper[1][1] - fillPathUpper[0][1],
                                           fillPathUpper[1][0] - fillPathUpper[0][0])
                    sLower = quotientOrInf(fillPathLower[1][1] - fillPathLower[0][1],
                                           fillPathLower[1][0] - fillPathLower[0][0])
                    doFlip = sLower > sUpper
                elif fillPathUpper[-1] == fillPathLower[-1]:
                    sUpper = quotientOrInf(fillPathUpper[-1][1] - fillPathUpper[-2][1],
                                           fillPathUpper[-1][0] - fillPathUpper[-2][0])
                    sLower = quotientOrInf(fillPathLower[-1][1] - fillPathLower[-2][1],
                                           fillPathLower[-1][0] - fillPathLower[-2][0])
                    doFlip = sLower < sUpper
                else:
                    doFlip = fillPathLower[0][1] > fillPathUpper[0][1]
                if doFlip:
                    fillPathUpper, fillPathLower = fillPathLower, fillPathUpper

                if fillPathUpper[0] != fillPathLower[0]:
                    # to make removal of collinear vertices work:
                    fillPathLower = [fillPathUpper[0]] + fillPathLower
                fillPathUpper.reverse()
                fillPath = SignalSpec._removeCollinearVertices(fillPathLower + fillPathUpper)
                if fillPath[-1] == fillPath[0]:
                    del fillPath[-1]

                fillPaths.append(fillPath)

        assert len(fillPaths) > 0 or not isFirstShadingOpen
        assert len(fillPaths) > 0 or not isLastShadingOpen

        return (fillPaths, isFirstShadingOpen, isLastShadingOpen)

    def getFragmPathVertices(self, fragmentIndex, pathIndex, edgeTimeWidth):
        """
        Returns a vertex list representing signal path pathindex for the specified fragment
        of this object.

        fragmentIndex: 0-based fragment index (0 .. getFragmentCount() - 1)
        pathIndex:     signal path index (non-negative integer)
        edgeTimeWidth: width of a complete rising or falling edge (finite and non-negative)
        Returns:
          [(t_1, y_1), ...,  (t_n, y_n)], where t_i <= t_{i+1} and 0 <= y_i <= i
          for all i with 0 <= i < n - 1.
          Empty or at least 2 elements.
        """
        assert fragmentIndex >= 0 and fragmentIndex < len(self.fragmentStates)
        assert isfinite(edgeTimeWidth) and edgeTimeWidth >= 0.0

        def bound(x, a, b):
            return max(min(x, max(a, b)), min(a, b))

        states = self.fragmentStates[fragmentIndex]
        vertices = []

        YDICT = { '0': 0.0, '1': 1.0, '-': 0.5 }

        edges = []
        tPrev = None
        sPrev = None
        for i in range(0, len(states)):
            t, sVec, doShade = states[i]
            assert isfinite(2 * t * t)
            assert tPrev is None or tPrev < t
            assert len(sVec) > 0
            s = sVec[pathIndex % len(sVec)]
            assert s in YDICT
            if (i == 0) or (s != sPrev) or (i + 1 == len(states) and tPrev < t):
                edges.append((t, YDICT[s]))
            tPrev = t
            sPrev = s

        if len(edges) > 1:
            vertices.append(edges[0])
            for i in range(0, len(edges)):
                # "draw" edge starting at (edges[i][0], vertices[-1][1])
                tPrev, yPrev = vertices[-1]
                t, y = edges[i]
                vertices.append((t, yPrev))
                if i + 1 < len(edges):
                    # not last edge
                    tNext, yNext = edges[i + 1]
                    dy = y - yPrev
                    tEdgeEnd, yEdgeEnd = (t, y)
                    if edgeTimeWidth > 0.0:
                        dt = abs(dy) * edgeTimeWidth
                        assert not math.isnan(dt)
                        tEdgeEnd = t + dt  # (tEdgeEnd, y) is (or would be) end of complete edge
                        if tEdgeEnd > tNext:
                            # incomplete edge
                            tEdgeEnd = tNext
                            dt = tEdgeEnd - t
                            dyEdge = dt / edgeTimeWidth
                            assert not math.isnan(dyEdge)
                            if dy < 0.0:
                                dyEdge = -dyEdge
                            yEdgeEnd = bound(yPrev + dyEdge, yPrev, y)
                    assert isfinite(tEdgeEnd) and tEdgeEnd <= tNext
                    vertices.append((tEdgeEnd, yEdgeEnd))
            vertices = SignalSpec._removeCollinearVertices(vertices)

        assert len(vertices) == 0 or len(vertices) > 1

        return vertices

    def getAllPathVerticesAndShading(self, edgeTimeWidth):
        """
        Returns all signal paths and the paths around all shaded areas between
        signal paths 0 and 1 of this object.

        edgeTimeWidth: width of a complete rising or falling edge (finite and non-negative)

        Returns:
          (pathFragmentVerticesList, shading01VerticesList).

          pathFragmentVerticesList[pathIndex][fragmentIndex] is a vertex list
          for each pathIndex, fragmentIndex with 0 <= pathIndex < getPeriod()
          and 0 <= fragmentIndex < getFragmentCount().
          Each pathFragmentVerticesList[pathIndex][fragmentIndex] contains at least 2 elements.

          pathFragmentVerticesList[areaIndex] is a vertex list for each areaIndex.
          Each pathFragmentVerticesList[areaIndex] contains at least 3 elements
          and describes a non-empty area between pathFragmentVerticesList[0][fragmentIndex] and
          pathFragmentVerticesList[1][fragmentIndex] (if getPeriod() >= 2).
        """

        pathCount = self.getPeriod()

        pathFragmentVerticesList = [] # pathFragmentVerticesList[pathIndex][fragmentIndex] is a vertex list
        shading01VerticesList = [] # pathFragmentVerticesList[areaIndex] is a vertex list

        for fragmentIndex in range(0, self.getFragmentCount()):

            pathVerticesList = [] # pathVerticesList[pathIndex] is a vertex list
            for pathIndex in range(0, pathCount):
                pathVerticesList.append(self.getFragmPathVertices(fragmentIndex, pathIndex, edgeTimeWidth))

            if pathCount >= 2:
                states = self.fragmentStates[fragmentIndex]
                for startIndex, endIndex in self._getShadingRanges(states):
                    startTimeBound = states[startIndex][0]
                    endTime = states[min(endIndex, len(states) - 1)][0]
                    startTime = min(startTimeBound + edgeTimeWidth,
                                    states[min(startIndex + 1, len(states) - 1)][0])
                    endTimeBound = min(endTime + edgeTimeWidth,
                                       states[min(endIndex + 1, len(states) - 1)][0])
                    path0Sub = SignalSpec._getVerticesSlice(pathVerticesList[0], startTimeBound, endTimeBound)
                    path1Sub = SignalSpec._getVerticesSlice(pathVerticesList[1], startTimeBound, endTimeBound)
                    shading01Vertices, isFirstShadingOpen, isLastShadingOpen = SignalSpec._getFillPathsBetween(path0Sub, path1Sub)
                    if endTimeBound != endTime and len(shading01Vertices) > 0 and shading01Vertices[-1][0][0] < endTime and isLastShadingOpen:
                        # last shading area starts before endTime and ends with path0Sub, path1Sub
                        # -> no intersection after endTime
                        # -> shorten to endTime
                        path0Sub = SignalSpec._getVerticesSlice(pathVerticesList[0], startTimeBound, endTime)
                        path1Sub = SignalSpec._getVerticesSlice(pathVerticesList[1], startTimeBound, endTime)
                        shading01Vertices, isFirstShadingOpen, isLastShadingOpen = SignalSpec._getFillPathsBetween(path0Sub, path1Sub)
                    else:
                        while len(shading01Vertices) > 1 and shading01Vertices[-1][0][0] > endTime:
                            del shading01Vertices[-1]
                    if len(shading01Vertices) > 1 and shading01Vertices[1][0][0] < startTime:
                        del shading01Vertices[0]
                    shading01VerticesList.extend(shading01Vertices)

            if len(pathFragmentVerticesList) > 0:
                for pathIndex in range(0, pathCount):
                    pathFragmentVerticesList[pathIndex].append(pathVerticesList[pathIndex])
            else:
                for pathIndex in range(0, pathCount):
                    pathFragmentVerticesList.append([pathVerticesList[pathIndex]])

        return (pathFragmentVerticesList, shading01VerticesList)

    @staticmethod
    def createFromStr(signalSpecStr, unitTime, breakTime):
        """
        Creates a SignalSpec object from a signal specification (for one signal).

        signalSpecStr: signal specification for one signal (exactly one non-empty line).
        unitTime: time for each state (> 0)
        breakTime: time for each break (> 0)
        Returns: SignalSpec object corresponding to signalSpecStr, or None if signalSpecStr is not valid
        """

        assert isfinite(unitTime) and unitTime > 0.0
        assert isfinite(breakTime) and breakTime > 0.0

        fragmentStates = SignalSpecParser.parse(signalSpecStr, unitTime, breakTime)
        if fragmentStates is None:
            s = None
        else:
            s = SignalSpec(fragmentStates)
        return s

    @staticmethod
    def testIt():
        assert SignalSpec._reduceToPeriod('0') == '0'
        assert SignalSpec._reduceToPeriod('00') == '0'
        assert SignalSpec._reduceToPeriod('000') == '0'
        assert SignalSpec._reduceToPeriod('010') == '010'
        assert SignalSpec._reduceToPeriod('0101') == '01'
        assert SignalSpec._reduceToPeriod('010101') == '01'
        assert SignalSpec._reduceToPeriod('010110101') == '010110101'
        assert SignalSpec._reduceToPeriod('0101101011') == '01011'

        assert SignalSpec._removeCollinearVertices([]) == []
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0)]) == [(0.0, 0.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 0.0)]) == [(0.0, 0.0), (1.0, 0.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 0.2), (1.0, 0.2), (2.0, 0.3)]) == [(0.0, 0.0), (1.0, 0.2), (2.0, 0.3)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 0.0), (1.0, 0.0), (1.0, 0.0)]) == [(0.0, 0.0), (1.0, 0.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 0.0), (2.0, 0.0)]) == [(0.0, 0.0), (2.0, 0.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 1.0), (2.0, 0.0)]) == [(0.0, 0.0), (1.0, 1.0), (2.0, 0.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.0), (1.0, 0.0), (2.0, 1.0)]) == [(0.0, 0.0), (1.0, 0.0), (2.0, 1.0)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.25), (1.0, 0.5), (2.0, 0.75)]) == [(0.0, 0.25), (2.0, 0.75)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.25), (0.0, 0.25), (2.0, 0.75)]) == [(0.0, 0.25), (2.0, 0.75)]
        assert SignalSpec._removeCollinearVertices([(0.0, 0.25), (0.0, 0.25), (2.0, 0.75), (4.0, 0.75), (4.0, 0.75)]) == [(0.0, 0.25), (2.0, 0.75), (4.0, 0.75)]

        assert SignalSpec([[(0, '01')]]).getFragmPathVertices(0, 3, 0.0) == []
        s = SignalSpec([[(0, '0'), (2, '1'), (3, '1')]])
        assert s.getFragmPathVertices(0, 0, 0) == [(0.0, 0.0), (2.0, 0.0), (2.0, 1.0), (3.0, 1.0)]
        assert s.getFragmPathVertices(0, 0, 0.5) == [(0.0, 0.0), (2.0, 0.0), (2.5, 1.0), (3.0, 1.0)]
        assert s.getFragmPathVertices(0, 0, 0.0) == [(0.0, 0.0), (2.0, 0.0), (2.0, 1.0), (3.0, 1.0)]
        s = SignalSpec([[(0, '-'), (2, '1'), (3, '0'), (5, '1')]])
        assert s.getFragmPathVertices(0, 0, 0) == [(0.0, 0.5), (2.0, 0.5), (2.0, 1.0), (3.0, 1.0), (3.0, 0.0), (5.0, 0.0)]
        assert s.getFragmPathVertices(0, 0, 0.5) == [(0.0, 0.5), (2.0, 0.5), (2.25, 1.0), (3.0, 1.0), (3.5, 0.0), (5.0, 0.0)]
        assert s.getFragmPathVertices(0, 0, 2.0) == [(0.0, 0.5), (2.0, 0.5), (3.0, 1.0), (5.0, 0.0)]
        assert s.getFragmPathVertices(0, 0, 4.0) == [(0.0, 0.5), (2.0, 0.5), (3.0, 0.75), (5.0, 0.25)]

        assert SignalSpec([[(0, '0101'), (1, '01'), (2, '010101')]]).fragmentStates == [[(0.0, '01', False), (2.0, '01', False)]]
        assert SignalSpec([[(0, '0101'), (1, '01', True), (2, '010101')]]).fragmentStates == [[(0.0, '01', False), (1.0, '01', True), (2.0, '01', False)]]
        assert SignalSpec.createFromStr('0011', 10.0, 5.0).fragmentStates == [[
            (0.0, '0', False), (20.0, '1', False), (40.0, '1', False)
        ]]
        assert SignalSpec.createFromStr('01-Xx', 10.0, 5.0).fragmentStates == [[
            (0.0, '0', False), (10.0, '1', False), (20.0, '-', False), (30.0, '10', False),
            (40.0, '01', False), (50.0, '01', False)
        ]]
        l = SignalSpec.createFromStr('-[X(01-)]0', 10.0, 5.0).fragmentStates == [[
            (0.0, '-', False),
            (10.0, '10', True),
            (20.0, '01-', True),
            (30.0, '0', False),
            (40.0, '0', False)
        ]]
        l = SignalSpec.createFromStr('0_1__Xx', 10.0, 5.0).fragmentStates
        assert l == [
            [(0.0, '0', False), (10.0, '0', False)],
            [(15.0, '1', False), (25.0, '1', False)],
            [(35.0, '10', False), (45.0, '01', False), (55.0, '01', False)]
        ]
        l = SignalSpec.createFromStr('0[_X__X]x', 10.0, 5.0).fragmentStates
        assert l == [
            [(0.0, '0', False), (10.0, '0', False)],
            [(15.0, '10', True), (25.0, '10', True)],
            [(35.0, '10', True), (45.0, '01', False), (55.0, '01', False)]
        ]
        assert SignalSpec.createFromStr('-[X()]0', 10.0, 5.0) is None
        assert SignalSpec.createFromStr('-[X]0(', 10.0, 5.0) is None
        assert SignalSpec.createFromStr('(10X)', 10.0, 5.0) is None
        assert SignalSpec.createFromStr('(X10)', 10.0, 5.0) is None
        assert SignalSpec.createFromStr(' ( 0 1- ) ', 10.0, 5.0) is not None
        assert SignalSpec.createFromStr('', 10.0, 5.0) is None
        assert SignalSpec.createFromStr('01\n0', 10.0, 5.0) is None

        s = SignalSpec.createFromStr('Xx', 1.0, 5.0)
        assert s.fragmentStates == [[(0.0, '10', False), (1.0, '01', False), (2.0, '01', False)]]
        assert s.getFragmPathVertices(0, 0, 0) == [(0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (2.0, 0.0)]
        assert s.getFragmPathVertices(0, 1, 0) == [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (2.0, 1.0)]
        assert s.getFragmPathVertices(0, 2, 0) == [(0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (2.0, 0.0)]

        assert SignalSpec().getPeriod() == 0
        assert SignalSpec([[(0.0, '0')]]).getPeriod() == 1
        assert SignalSpec([[(0.0, '0'), (1.0, '10'), (2.0, '101')]]).getPeriod() == 6
        l = [
            [(0.0, '0'), (1.0, '10'), (2.0, '101')],
            [(3.0, '0001')]
        ]
        assert SignalSpec(l).getPeriod() == 12

        assert SignalSpec._interpolateAt([(0.0, 0.0), (1.0, 0.5), (2.0, 1.0)], 1, 1.0) == 0.5
        assert SignalSpec._interpolateAt([(0.0, 0.0), (1.0, 0.5), (2.0, 1.0)], 1, 2.0) == 1.0
        assert SignalSpec._interpolateAt([(0.0, 0.0), (1.0, 0.5), (2.0, 1.0)], 1, 1.5) == 0.75
        assert SignalSpec._interpolateAt([(0.0, 0.0), (1.0, 0.5), (1.0, 1.0)], 1, 1.0) == 0.75

        assert SignalSpec._getVerticesSlice([], 0.0, 1.0) == []
        assert SignalSpec._getVerticesSlice([(1.0, 0.0)], 0.0, 2.0) == []
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0)], 0.0, -2.0) == []
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0)], 1.5, 1.5) == []
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0)], 1.0, 2.0) == [(1.0, 0.0), (2.0, 1.0)]
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0), (3.0, 0.5)], 0.0, 2.5) == [(1.0, 0.0), (2.0, 1.0), (2.5, 0.75)]
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0), (3.0, 0.5)], 1.5, 2.5) == [(1.5, 0.5), (2.0, 1.0), (2.5, 0.75)]
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0), (3.0, 0.5)], 0.5, 1.5) == [(1.0, 0.0), (1.5, 0.5)]
        assert SignalSpec._getVerticesSlice([(1.0, 0.0), (2.0, 1.0), (3.0, 0.5)], 1.5, 1.5) == []

        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 1.0)], [(1.0, 1.0), (2.0, 0.0)], 0, 0) == (1.5, 0.5)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.5)], [(1.0, 1.0), (2.0, 0.5)], 0, 0) == (2.0, 0.5)
        assert SignalSpec._getVertexIntersPoint([(0.0, 0.0), (1.0, 0.0)], [(0.0, 0.0), (1.0, 0.0)], 0, 0) == (0.0, 0.0)
        assert SignalSpec._getVertexIntersPoint([(0.1, 0.0), (1.0, 0.0)], [(0.0, 0.0), (1.0, 0.0)], 0, 0) == (0.1, 0.0)
        assert SignalSpec._getVertexIntersPoint([(0.0, 0.0), (1.0, 0.0)], [(0.2, 0.0), (1.0, 0.0)], 0, 0) == (0.2, 0.0)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.5), (2.0, 1.0)], [(0.0, 1.0), (1.0, 0.0)], 0, 0) == None
        assert SignalSpec._getVertexIntersPoint([(0.0, 1.0), (1.0, 0.0)], [(1.0, 0.5), (2.0, 1.0)], 0, 0) == None
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 1.0)], [(1.0, 1.0), (2.0, 0.0)], 0, 0) == (2.0, 0.0)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 1.0)], [(1.0, 1.0), (3.0, 0.0)], 1, 0) == (2.0, 0.5)
        assert SignalSpec._getVertexIntersPoint([(1.0, 1.0), (3.0, 0.0)], [(1.0, 0.0), (2.0, 0.0), (2.0, 1.0)], 0, 1) == (2.0, 0.5)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 1.0)], [(1.0, 1.0), (2.0, 1.0), (2.0, 0.0)], 1, 1) == (2.0, 0.5)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 0.5)], [(1.0, 1.0), (2.0, 1.0), (2.0, 0.5)], 1, 1) == (2.0, 0.5)
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 0.4)], [(1.0, 1.0), (2.0, 1.0), (2.0, 0.5)], 1, 1) == None
        assert SignalSpec._getVertexIntersPoint([(1.0, 0.0), (2.0, 0.0), (2.0, 0.6)], [(1.0, 1.0), (2.0, 1.0), (2.0, 0.5)], 1, 1) == (2.0, 0.55)
        assert SignalSpec._getVertexIntersPoint([(1.0, 1.0), (2.0, 1.0), (2.0, 0.5)], [(1.0, 0.0), (2.0, 0.0), (2.0, 0.6)], 1, 1) == (2.0, 0.55)

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (1.0, 1.0)],
                                                    [(0.0, 1.0), (1.0, 1.0)])
        assert p == []
        assert not oL and not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0)],
                                                    [(0.0, 1.0), (1.0, 1.0)])
        assert p == []
        assert not oL and not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (1.0, 1.0)],
                                                    [])
        assert p == []
        assert not oL and not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (2.0, 1.0)],
                                                    [(0.0, 0.0), (2.0, 0.0)])
        assert p == [
            [(0.0, 1.0), (0.0, 0.0), (2.0, 0.0), (2.0, 1.0)]
        ]
        assert oL
        assert oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (2.0, 1.0), (2.0, 0.0)],
                                                    [(0.0, 0.0), (2.0, 0.0)])
        assert p == [
            [(0.0, 1.0), (0.0, 0.0), (2.0, 0.0), (2.0, 1.0)]
        ]
        assert oL
        assert not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (0.0, 0.0), (2.0, 0.0)],
                                                    [(0.0, 1.0), (2.0, 1.0)])
        assert p == [
            [(0.0, 1.0), (0.0, 0.0), (2.0, 0.0), (2.0, 1.0)]
        ]
        assert not oL
        assert oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(0.0, 1.0), (0.0, 0.0), (2.0, 0.0), (2.0, 1.0)],
                                                    [(0.0, 1.0), (2.0, 1.0)])
        assert p == [
            [(0.0, 1.0), (0.0, 0.0), (2.0, 0.0), (2.0, 1.0)]
        ]
        assert not oL
        assert not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(1.0, 1.0), (2.0, 1.0), (2.5, 0.5), (3.5, 0.5), (4.0, 0.0), (4.5, 0.25), (6.0, 1.0)],
                                                    [(1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (3.5, 0.5), (4.0, 1.0), (6.0, 0.0)])
        assert p == [
            [(1.0, 1.0), (1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (2.0, 1.0)],
            [(3.5, 0.5), (4.0, 0.0), (5.0, 0.5), (4.0, 1.0)],
            [(5.0, 0.5), (6.0, 0.0), (6.0, 1.0)]
        ]
        assert oL
        assert oR

        p, oL, oR = SignalSpec._getFillPathsBetween(
            [(1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (3.5, 0.5), (4.0, 1.0), (6.0, 0.0)],
            [(1.0, 1.0), (2.0, 1.0), (2.5, 0.5), (3.5, 0.5), (4.0, 0.0), (4.5, 0.25), (6.0, 1.0)])
        assert p == [
            [(1.0, 1.0), (1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (2.0, 1.0)],
            [(3.5, 0.5), (4.0, 0.0), (5.0, 0.5), (4.0, 1.0)],
            [(5.0, 0.5), (6.0, 0.0), (6.0, 1.0)]
        ]
        assert oL
        assert oR

        p, oL, oR = SignalSpec._getFillPathsBetween(
            [(0.0, 1.0), (1.0, 0.5), (5.0, 0.5)],
            [(1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (3.5, 0.5), (4.0, 1.0), (6.0, 0.0)])
        assert p == [
            [(0.0, 1.0), (1.0, 0.0), (2.0, 0.0), (2.5, 0.5), (1.0, 0.5)],
            [(3.5, 0.5), (5.0, 0.5), (4.0, 1.0)]
        ]
        assert oL
        assert not oR

        p, oL, oR = SignalSpec._getFillPathsBetween(
            [(0.0, 1.0), (3.0, 1.0), (4.5, 0.25), (6.0, 1.0)],
            [(0.0, 0.5), (1.0, 0.5), (2.0, 1.0), (4.0, 0.0), (6.0, 1.0), (7.0, 1.0)])
        assert p == [
            [(0.0, 1.0), (0.0, 0.5), (1.0, 0.5), (2.0, 1.0)],
            [(2.0, 1.0), (4.0, 0.0), (4.5, 0.25), (3.0, 1.0)]
        ]
        assert oL
        assert not oR

        p, oL, oR = SignalSpec._getFillPathsBetween([(1.0, 1.0), (3.0, 0.0), (3.0, 1.0), (5.0, 0.0)],
                                                    [(0.0, 0.0), (4.0, 1.0)])
        assert len(p) == 4
        assert p[0] == [(1.0, 1.0), (0.0, 0.0), (2.0, 0.5)]
        assert p[1] == [(2.0, 0.5), (3.0, 0.0), (3.0, 0.75)]
        assert p[2][0] == (3.0, 0.75)
        assert round(p[2][1][0] - 10.0 / 3.0, 9) == 0.0
        assert round(p[2][1][1] - 5.0 / 6.0, 9) == 0.0
        assert p[2][2:] == [(3.0, 1.0)]
        assert p[3][0] == p[2][1]
        assert p[3][1:] == [(4.0, 0.5), (5.0, 0.0), (4.0, 1.0)]
        assert oL
        assert oR

        assert SignalSpec._getShadingRanges([]) == []
        l = SignalSpec([[(0.0, '01'), (1.0, '10', True)]]).fragmentStates[0]
        assert SignalSpec._getShadingRanges(l) == [(1, 2)]
        l = SignalSpec([[(0.0, '01'), (1.0, '10', True), (2.0, '01', True)]]).fragmentStates[0]
        assert SignalSpec._getShadingRanges(l) == [(1, 3)]
        l = SignalSpec([[(0.0, '01', True), (1.0, '10'), (2.0, '01', True), (3.0, '10', True), (4.0, '01'), (5.0, '10')]]).fragmentStates[0]
        assert SignalSpec._getShadingRanges(l) == [(0, 1), (2, 4)]
        l = SignalSpec([[(0.0, '0', True), (1.0, '1', True), (2.0, '110', True)]]).fragmentStates[0]
        assert SignalSpec._getShadingRanges(l) == []

        s = [[
            (0.0, '0'), (1.0, '01', True), (3.0, '10'), (4.0, '01'),
            (5.0, '10', True), (7.0, '10', True)
        ]]
        l = SignalSpec(s).getAllPathVerticesAndShading(1.5)[1]
        assert l == [
            [(1.0, 0.0), (3.0, 0.0), (3.75, 0.5), (3.0, 1.0), (2.5, 1.0)],
            [(5.75, 0.5), (6.5, 0.0), (7.0, 0.0), (7.0, 1.0), (6.5, 1.0)]
        ]

        l = SignalSpec.createFromStr('[x]', 1.0, 5.0).getAllPathVerticesAndShading(1.0)[1]
        assert l == [[(0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0)]]
        l = SignalSpec.createFromStr('[X]', 1.0, 5.0).getAllPathVerticesAndShading(1.0)[1]
        assert l == [[(0.0, 1.0), (0.0, 0.0), (1.0, 0.0), (1.0, 1.0)]]

        l = SignalSpec.createFromStr('x[XxX]x', 10.0, 5.0).getAllPathVerticesAndShading(0.0)[1]
        assert l == [
            [(10.0, 1.0), (10.0, 0.0), (20.0, 0.0), (20.0, 1.0)],
            [(20.0, 1.0), (20.0, 0.0), (30.0, 0.0), (30.0, 1.0)],
            [(30.0, 0.0), (40.0, 0.0), (40.0, 1.0), (30.0, 1.0)]
        ]
        l = SignalSpec.createFromStr('x[XxX]x', 10.0, 5.0).getAllPathVerticesAndShading(5.0)[1]
        assert l == [
            [(12.5, 0.5), (15.0, 0.0), (20.0, 0.0), (22.5, 0.5), (20.0, 1.0), (15.0, 1.0)],
            [(22.5, 0.5), (25.0, 0.0), (30.0, 0.0), (32.5, 0.5), (30.0, 1.0), (25.0, 1.0)],
            [(32.5, 0.5), (35.0, 0.0), (40.0, 0.0), (42.5, 0.5), (40.0, 1.0), (35.0, 1.0)]
        ]

        def verticesListEqual(vLA, vLB, absTTol, absYTol):
            same = False
            if len(vLA) == len(vLB):
                i = 0
                same = True
                while same and i < len(vLA):
                    vA = vLA[i]
                    vB = vLB[i]
                    if len(vA) == len(vB):
                        j = 0
                        while same and j < len(vA):
                            same = abs(vA[j][0] - vB[j][0]) <= absTTol
                            same = same and abs(vA[j][1] - vB[j][1]) <= absYTol
                            j = j + 1
                    else:
                        same = False
                    i = i + 1
            return same

        l = SignalSpec.createFromStr('x[XxX]x', 10.0, 5.0).getAllPathVerticesAndShading(15.0)[1]
        lExpected = [
            [(17.5, 0.5), (20.0, 1.0 / 3.0), (22.5, 0.5), (20.0, 2.0 / 3.0)],
            [(22.5, 0.5), (30.0, 0.0), (37.5, 0.5), (30.0, 1.0)], [(37.5, 0.5), (40.0, 1.0 / 3.0), (42.5, 0.5), (40.0, 2.0 / 3.0)]
        ]
        assert verticesListEqual(l, lExpected, 1e-6, 1e-6)

        l = SignalSpec.createFromStr('x[XxX]x', 10.0, 5.0).getAllPathVerticesAndShading(25.0)[1]
        lExpected = [
            [(10.0, 1.0), (10.0, 0.0), (20.0, 0.4), (30.0, 0.0), (40.0, 0.4), (40.0, 0.6), (30.0, 1.0), (20.0, 0.6)]
        ]
        assert verticesListEqual(l, lExpected, 1e-6, 1e-6)
        l = SignalSpec.createFromStr('x[X]x[X]xx', 10.0, 5.0).getAllPathVerticesAndShading(25.0)[1]
        lExpected = [
            [(10.0, 1.0), (10.0, 0.0), (20.0, 0.4), (20.0, 0.6)], [(30.0, 1.0), (30.0, 0.0), (40.0, 0.4), (40.0, 0.6)]
        ]
        assert verticesListEqual(l, lExpected, 1e-6, 1e-6)

        l = SignalSpec.createFromStr('0[0]X[1]1', 10.0, 5.0).getAllPathVerticesAndShading(25.0)[1]
        assert l == []

        l = SignalSpec.createFromStr('0(01)(10)', 10.0, 5.0).getAllPathVerticesAndShading(0.0)
        assert l[0] == [
            [[(0.0, 0.0), (20.0, 0.0), (20.0, 1.0), (30.0, 1.0)]],
            [[(0.0, 0.0), (10.0, 0.0), (10.0, 1.0), (20.0, 1.0), (20.0, 0.0), (30.0, 0.0)]]
        ]
        l = SignalSpec.createFromStr('0[(01)_(10)]', 10.0, 5.0).getAllPathVerticesAndShading(0.0)
        assert l[0] == [
            [[(0.0, 0.0), (20.0, 0.0)], [(25.0, 1.0), (35.0, 1.0)]],
            [[(0.0, 0.0), (10.0, 0.0), (10.0, 1.0), (20.0, 1.0)], [(25.0, 0.0), (35.0, 0.0)]]
        ]
        assert l[1] == [
            [(10.0, 1.0), (10.0, 0.0), (20.0, 0.0), (20.0, 1.0)],
            [(25.0, 1.0), (25.0, 0.0), (35.0, 0.0), (35.0, 1.0)]
        ]

SignalSpecParser.testIt()
SignalClusterSpecValidator.testIt()
SignalSpec.testIt()

