#!/usr/bin/env bash

set -e

TmpDir=/tmp/bluetoothlaz
rm -rf $TmpDir
mkdir -p $TmpDir

rsync -av --exclude=".svn" ../bluetooth $TmpDir/

for Ext in ppu o a compiled exe rst zip tgz bak lps;do
  find $TmpDir -name "*.$Ext" -exec rm {} \;
done
find $TmpDir -name "*~" -exec rm {} \;
# remove all programs without extension
find . -type f -perm -100 -iregex '.*\/[a-z]+$' -exec rm {} \;

TargetFile=$(pwd)/bluetoothlaz.zip
rm -f $TargetFile
cd $TmpDir
zip -r $TargetFile bluetooth
cd -

echo "Created: $TargetFile"

# end.

