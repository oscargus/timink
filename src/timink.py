# Inkscape 0.47 does not support path separators in .inx files, so this file has to reside
# directly in one of the extension directories.

import sys
if sys.version_info >= (2, 6) and sys.version_info < (3, 0):
    from timink.timinkeffect import TiminkEffect
    TiminkEffect.testIt()
    TiminkEffect().affect()
else:
    print >> sys.stderr, 'Error: Requires Python 2.x with x >= 6.'
    sys.exit(1)
