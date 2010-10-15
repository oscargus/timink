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

def sign(x):
    if x == 0:
        return 0.0
    else:
        return float(x) / abs(x)

def quotientOrInf(x, y):
    try:
        z = x / y
    except ZeroDivisionError:
        if x == 0.0:
            z = 0.0 # note: not NaN (like IEEE-754)
        elif x > 0.0:
            z = float('inf')
        else:
            z = -float('inf')
    return z

def isfinite(x):
  return not math.isnan(x) and not math.isinf(x)

# http://www.python-forum.org/pythonforum/viewtopic.php?f=2&t=7262
def gcd(a, b):
    """
    Greatest common denominator of positive integers a and b.
    """
    assert a >= 1
    assert b >= 1
    while b != 0:
        a, b = b, a % b
    return a

def lcm(a, b):
    return (a * b) // gcd(a, b)

assert lcm(1, 1) == 1
assert lcm(6, 4) == 12
assert quotientOrInf(0.0, 0.0) == 0.0
assert quotientOrInf(2.0, 0.0) == float('inf')
assert quotientOrInf(-3.0, 0.0) == -float('inf')
assert quotientOrInf(-2.0, 4.0) == -0.5

