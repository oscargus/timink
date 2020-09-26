import sys
if sys.version_info >= (3, 5):
    from timinkeffect import TiminkEffect
    TiminkEffect.testIt()
    TiminkEffect().run()
else:
    print('Error: Requires Python 3.5 or higher.', file=sys.stderr)
    sys.exit(1)
