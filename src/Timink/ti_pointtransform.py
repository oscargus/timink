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
from ti_math import isfinite

class PointTransf:
    """
    SVG transformation.
    http://www.w3.org/TR/2003/REC-SVG11-20030114/coords.html#TransformMatrixDefined

    / a  c  e \   / x \
    | b  d  f | . | y |
    \ 0  0  1 /   \ 1 /

         / m11 m12 \        / oX \
    m := |         |,  o := |    |
         \ m21 m22 /        \ oY /
    """

    def __init__(self, (m11, m12, m21, m22), (oX, oY)):
        self.m = (m11, m12, m21, m22)
        self.o = (oX, oY)
        assert self.isFinite()

    def isFinite(self):
        return all(map(isfinite, self.m)) and all(map(isfinite, self.o))

    def isIdentity(self):
        assert self.isFinite()
        return self.o == (0, 0) and self.m == (1, 0, 0, 1)

    @staticmethod
    def _mod360Degree(degrees):
        d = math.fmod(degrees, 360.0)
        if d < 0.0:
            d = d + 360.0
        if d > 180.0:
            d = d - 360.0
        if d <= -180:
            d = d + 360.0
        assert d > -180.0 and d <= 180.0
        return d

    @staticmethod
    def createIdentity():
        return PointTransf((1, 0, 0, 1), (0, 0))

    @staticmethod
    def createTransl((tx, ty)):
        t = None
        if isfinite(tx) and isfinite(ty):
            t = PointTransf((1, 0, 0, 1), (tx, ty))
        return t

    @staticmethod
    def createScale(sx, sy):
        t = None
        if isfinite(sx) and isfinite(sy):
            m = (sx, 0,
                 0,  sy)
            t = PointTransf(m, (0, 0))
        return t

    @staticmethod
    def createRot0(aDegree):
        t = None
        try:
            aDegreeMod = PointTransf._mod360Degree(aDegree)
            aDegreeModAbs = abs(aDegreeMod)
            if aDegreeModAbs == 0:
                cosA = 1
                sinA = 0
            elif aDegreeModAbs == 90:
                cosA = 0
                sinA = 1
            elif aDegreeModAbs == 180:
                cosA = -1
                sinA = 0
            elif aDegreeModAbs == 280:
                cosA = 0
                sinA = -1
            else:
                a = math.radians(aDegreeModAbs)
                cosA = math.cos(a)
                sinA = math.sin(a)
            if aDegreeMod < 0:
                sinA = -sinA
            m = (cosA, -sinA,
                 sinA, cosA)
            t = PointTransf(m, (0, 0))
        except ValueError:
            pass
        return t

    @staticmethod
    def createRot(aDegree, (cX, cY)):
        t = None
        if isfinite(cX) and isfinite(cY):
            t = PointTransf.createRot0(aDegree)
            t = PointTransf.createConcat(PointTransf.createTransl((cX, cY)), t)
            t = PointTransf.createConcat(t, PointTransf.createTransl((-cX, -cY)))
        return t

    @staticmethod
    def createSkewX(aDegree):
        t = None
        try:
            aDegreeMod = PointTransf._mod360Degree(2 * aDegree) / 2
            tanA = math.tan(math.radians(aDegreeMod))
            if isfinite(tanA):
                m = (1, tanA,
                     0, 1)
                t = PointTransf(m, (0, 0))
        except ValueError:
            pass
        return t

    @staticmethod
    def createSkewY(aDegree):
        t = None
        try:
            aDegreeMod = PointTransf._mod360Degree(2 * aDegree) / 2
            tanA = math.tan(math.radians(aDegreeMod))
            if isfinite(tanA):
                m = (1,    0,
                     tanA, 1)
                t = PointTransf(m, (0, 0))
        except ValueError:
            pass
        return t

    def createConcat(tA, tB):
        # tB after tA
        if tA.isIdentity():
            t = PointTransf(tB.m, tB.o)
        elif tB.isIdentity():
            t = PointTransf(tA.m, tA.o)
        else:
            a11, a12, a21, a22 = tA.m
            oAX, oAY = tA.o
            b11, b12, b21, b22 = tB.m
            oBX, oBY = tB.o

            m11 = b11 * a11 + b12 * a21
            m12 = b11 * a12 + b12 * a22
            m21 = b21 * a11 + b22 * a21
            m22 = b21 * a12 + b22 * a22

            oX  = b11 * oAX + b12 * oAY + oBX
            oY  = b21 * oAX + b22 * oAY + oBY

            t = PointTransf((m11, m12, m21, m22), (oX, oY))
        return t

    def applyTo(self, (x, y)):
        if self.isIdentity():
            xt, yt = (x, y)
        else:
            m11, m12, m21, m22 = self.m
            xt = m11 * x + m12 * y + self.o[0]
            yt = m21 * x + m22 * y + self.o[1]
        return (xt, yt)

    @staticmethod
    def testIt():
        assert PointTransf._mod360Degree(0) == 0
        assert PointTransf._mod360Degree(360) == 0
        assert PointTransf._mod360Degree(-180) == 180
        assert PointTransf._mod360Degree(180) == 180
        assert PointTransf._mod360Degree(-180 + 720) == 180
        assert PointTransf._mod360Degree(180 + 720) == 180

        assert PointTransf.createIdentity().isIdentity()
        assert PointTransf.createTransl((0, 0)).isIdentity()
        assert not PointTransf.createTransl((0, 1)).isIdentity()
        assert not PointTransf.createTransl((1, 0)).isIdentity()
        assert PointTransf.createTransl((11, 222)).applyTo((10, 20)) == (21, 242)
        assert PointTransf.createTransl((float('inf'), 222)) is None
        assert PointTransf.createScale(2, 3).applyTo((10, 20)) == (20, 60)
        assert PointTransf.createScale(2, float('inf')) is None
        assert PointTransf.createRot0(0).applyTo((10, 20)) == (10, 20)
        assert PointTransf.createRot0(90).applyTo((10, 20)) == (-20, 10)
        assert PointTransf.createRot0(float('inf')) is None

        assert PointTransf.createSkewX(float('inf')) is None
        assert PointTransf.createSkewY(float('inf')) is None

        assert PointTransf.createConcat(PointTransf.createTransl((11, 222)),
                                        PointTransf.createTransl((-11, -222))).applyTo((0, 0)) == (0, 0)
        assert PointTransf.createConcat(PointTransf.createRot0(90),
                                        PointTransf.createRot0(-90)).applyTo((1, 1)) == (1, 1)

        t = PointTransf((1, 2, 3, 4), (5, 6))
        assert t.o == (5, 6)
        t2 = PointTransf.createConcat(t, PointTransf.createTransl((11, 222)))
        assert t.m == t2.m
        assert t2.o == (5 + 11, 6 + 222)

PointTransf.testIt()

