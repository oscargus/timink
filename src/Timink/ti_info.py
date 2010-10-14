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

from ti_version import VersionJoint

EXTENSION_NAME = 'Timink'

# <joint-version-number> ::= <extension-version-number> "/" <model-version-number>.
#
# <extension-version-number>:
#     version number <major> "." <minor> "." <mini> [ <appendix> ] of the extension
#
# <model-version-number>:
#     version number <major> "." <minor> of the model (the SVG representation of a Timink object).
#     If only new elements or attributes are added without changing any of the elements or attributes:
#         Increment only <minor>.
#     If one of the elements or attributes is removed are has a different meaning:
#         Increase <major> and set <minor> to 0.

VERSIONJOINT = VersionJoint('0.0.1/0.0')
