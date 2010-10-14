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

class Version(object):
    VERSIONSTR_REGEXP = re.compile(r'^([0-9]*(\.[0-9]*)*)([a-z]+[0-9]*)?$')
    APPENDIX_REGEXP = re.compile(r'([a-z]*)([0-9]*)')

    def __init__(self, versionStr):
        m = Version.VERSIONSTR_REGEXP.match(versionStr)
        if m is None:
            raise ValueError('invalid version number')
        componentsStr = m.group(1)
        appendix = m.group(3)
        self.components = tuple(map(lambda s: int(s), componentsStr.split('.')))
        self.appendix = appendix
        if str(self) != versionStr:
            raise ValueError('invalid version number (not normalised)')

    def __str__(self):
        s = '.'.join(map(lambda s: str(s), self.components))
        if self.appendix is not None:
            s = s + self.appendix
        return s

    def __repr__(self):
        return str(self)

    def __cmp__(self, other):
        d = cmp(self.components, other.components)
        if d == 0:
            if self.appendix is None and other.appendix is not None:
                # other < self
                d = 1
            elif self.appendix is not None and other.appendix is None:
                # other > self
                d = -1
            elif self.appendix is not None or other.appendix is not None:
                d = cmp(Version._parseAppenix(self.appendix), Version._parseAppenix(other.appendix))
        return d

    @staticmethod
    def _parseAppenix(s):
        m = Version.APPENDIX_REGEXP.match(s)
        n = m.group(2)
        if not n:
            n = '0'
        return (m.group(1), int(n))

    @staticmethod
    def testIt():
        v = Version('1.2.3a3')
        assert v.components == (1, 2, 3)
        assert v.appendix == 'a3'
        v = Version('1.2')
        assert v.components == (1, 2)
        assert Version('1.2') < Version('1.3')
        assert Version('2.1') > Version('1.3')
        assert Version('1.0') < Version('1.0.0') # to make __cmp__ a perfect order on the set of valid version strings
        assert Version('1.2a') < Version('1.2')
        assert Version('1.2a') > Version('1.1')
        assert Version('1.2a') < Version('1.2b')
        assert Version('1.2a10') > Version('1.2a2')
        assert Version('1.2a10') > Version('1.2a')

class VersionExtension(Version):
    def __init__(self, versionStr):
        Version.__init__(self, versionStr)
        if len(self.components) != 3:
            raise ValueError('invalid version number')

class VersionModel(Version):
    def __init__(self, versionStr):
        Version.__init__(self, versionStr)
        if len(self.components) != 2 or self.appendix is not None:
            raise ValueError('invalid version number')

class VersionJoint(object):
    def __init__(self, versionStr):
        versionTokens = versionStr.split('/')
        if len(versionTokens) != 2:
            raise ValueError('invalid joint version number')
        self.extension = VersionExtension(versionTokens[0])
        self.model = VersionModel(versionTokens[1])

    def __cmp__(self, other):
        return cmp((self.extension, self.model), (other.extension, other.model))

    def __str__(self):
        return str(self.extension) + '/' + str(self.model)

    def __repr__(self):
        return str(self)

    @staticmethod
    def testIt():
        assert VersionJoint('0.0.1/0.0') == VersionJoint('0.0.1/0.0')
        assert VersionJoint('0.0.1/0.0') != VersionJoint('0.0.1/0.1')
        assert VersionJoint('0.0.2/0.0') > VersionJoint('0.0.1/0.0')
        assert VersionJoint('0.0.2/10.0') > VersionJoint('0.0.2/1.2')

        # the following test must not be changed to ensure compatibility with existing versions:
        assert str(VersionJoint('0.0.1/0.0')) == '0.0.1/0.0'

Version.testIt()
VersionJoint.testIt()
