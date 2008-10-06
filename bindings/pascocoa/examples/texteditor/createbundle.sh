#!/bin/sh
# Force Bourne shell in case tcsh is default.
#
appname=texteditor
appfolder=$appname.app
macosfolder=$appfolder/Contents/MacOS
plistfile=$appfolder/Contents/Info.plist
appfile=texteditor
#
if ! [ -e $appfile ]
then
  echo "$appfile does not exist"
elif [ -e $appfolder ]
then
  echo "$appfolder already exists"
else
  echo "Creating $appfolder..."
  mkdir $appfolder
  mkdir $appfolder/Contents
  mkdir $appfolder/Contents/MacOS
  mkdir $appfolder/Contents/Resources
  mkdir $appfolder/Contents/Resources/English.lproj
#
# Instead of copying executable into .app folder after each compile,
# simply create a symbolic link to executable.
  ln -s ../../../$appname $macosfolder/$appname
# Copy resource files
  cp imgOpen.png $appfolder/Contents/Resources/
  cp imgSave.png $appfolder/Contents/Resources/
  cp imgClose.png $appfolder/Contents/Resources/
  cp -r applemenu.nib $appfolder/Contents/Resources/English.lproj/
#
# Create PkgInfo file.
  echo "TXTEDIT#" >$appfolder/Contents/PkgInfo
#
# Create information property list file (Info.plist).
  echo '<?xml version="1.0" encoding="UTF-8"?>' >$plistfile
  echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>$plistfile
  echo '<plist version="1.0">' >>$plistfile
  echo '<dict>' >>$plistfile
  echo '  <key>CFBundleDevelopmentRegion</key>' >>$plistfile
  echo '  <string>English</string>' >>$plistfile
  echo '  <key>CFBundleExecutable</key>' >>$plistfile
  echo '  <string>'$appname'</string>' >>$plistfile
  echo '  <key>CFBundleIconFile</key>' >>$plistfile
  echo '  <string></string>' >>$plistfile
  echo '  <key>CFBundleIdentifier</key>' >>$plistfile
  echo '  <string>org.magnifier.magnifier</string>' >>$plistfile
  echo '  <key>CFBundleInfoDictionaryVersion</key>' >>$plistfile
  echo '  <string>6.0</string>' >>$plistfile
  echo '  <key>CFBundlePackageType</key>' >>$plistfile
  echo '  <string>EDIT</string>' >>$plistfile
  echo '  <key>CFBundleSignature</key>' >>$plistfile
  echo '  <string>TXT#</string>' >>$plistfile
  echo '  <key>CFBundleVersion</key>' >>$plistfile
  echo '  <string>1.0</string>' >>$plistfile
  echo '  <key>NSMainNibFile</key>' >>$plistfile
  echo '  <string>applemenu</string>' >>$plistfile
  echo '</dict>' >>$plistfile
  echo '</plist>' >>$plistfile
fi
