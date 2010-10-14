# run the Timink/__init__.py
# (this indirection is necessary when using subdirectories,
# because Inkscape 0.47 does not support path separators in .inx files)

import sys
if sys.version_info >= (2, 6) and sys.version_info < (3, 0):
    import Timink
else:
    print >> sys.stderr, 'Error: Requires Python 2.x with x >= 6.'
    sys.exit(1)
