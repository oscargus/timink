import sys
import os.path
import tarfile
import mercurial.hg
import mercurial.ui
sys.path.append('src/')
from timink.ti_info import EXTENSION_NAME, VERSIONJOINT
from timink.ti_version import Version

repo = mercurial.hg.repository(mercurial.ui.ui(), '.')
tags = []
for tag, revId in repo.tags().iteritems():
    try:
        tags.append((repo[revId].rev(), Version(tag)))
    except ValueError:
        pass
tags.sort()
assert len(tags) > 0 # at least one version tag
latestVersion = tags[-1][1]
assert VERSIONJOINT.extension >= latestVersion

srcDirname = 'src/'
distDirname = 'dist/'

archiveFilename = '{title}-{version}.tar.gz'.format(title=EXTENSION_NAME, version=VERSIONJOINT.extension)
archiveFilename = os.path.join(distDirname, archiveFilename)
tar = tarfile.open(archiveFilename, 'w:gz')

srcFilenames = []
for visitedDirPath, dirnames, filenames in os.walk(srcDirname):
    for filename in filenames:
        if os.path.splitext(filename)[1] in ('.py', '.inx'):
            srcFilename = os.path.join(visitedDirPath, filename)
            dstFilename = os.path.relpath(srcFilename, srcDirname)
            tar.add(srcFilename, arcname=dstFilename, recursive=False)
for srcFilename in ['README', 'COPYING']:
    dstFilename = os.path.join('timink', os.path.basename(srcFilename))
    tar.add(srcFilename, arcname=dstFilename, recursive=False)

tar.close()

