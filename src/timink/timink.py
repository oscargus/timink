import sys
if sys.version_info >= (2, 6) and sys.version_info < (3, 0):
    from timinkeffect import TiminkEffect
    TiminkEffect.testIt()
    TiminkEffect().affect()
else:
    print >> sys.stderr, 'Error: Requires Python 2.x with x >= 6.'
    sys.exit(1)
