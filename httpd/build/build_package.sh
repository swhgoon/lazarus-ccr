#!/bin/sh
#
# This script generates packages for the Apache headers for Pascal
#

##################################
# Constants
##################################

PRODUCT="Apache Headers"
VERSION="0.3"

TARGET_DIR="./httpd-$VERSION/"
TARGET_TAR="httpd-$VERSION.tar"
TARGET_ZIP="httpd-$VERSION.zip"


##################################
# Cleans a directory
##################################
CleanDirectory ()
{
  rm -rf ${1}*.o
  rm -rf ${1}*.ppu
  rm -rf ${1}*.bak
  rm -rf ${1}*.sh~
}

##################################
# Creates a source package
##################################
SourcePackage ()
{
  # Goes to the root directory of the magnifier

  cd ..

  # Clean up

  CleanDirectory ./
  CleanDirectory ./build/
  CleanDirectory ./httpd_1_3/
  CleanDirectory ./httpd_2_0
  CleanDirectory ./httpd_2_0/apr/
  CleanDirectory ./httpd_2_0/aprutil/
  CleanDirectory ./httpd_2_0/apriconv/
  CleanDirectory ./httpd_2_2
  CleanDirectory ./httpd_2_2/apr/
  CleanDirectory ./httpd_2_2/aprutil/
  CleanDirectory ./httpd_2_2/apriconv/

  # The Subversion directories will be copied

  # copies all files to a new temporary directory

  cp -r ./ ../httpd-$VERSION/

  # Creates the package

  cd ..

  zip -r $TARGET_ZIP httpd-$VERSION/

  # Clean up

  rm -rf httpd-$VERSION/

  return
}

##################################
# Main section
##################################

echo "========================================================"
echo "    Apache headers for Pascal build script"
echo "========================================================"
echo ""
echo " Please select which package you would like to build:"
echo ""
echo " 1 > Source .zip package"
echo " 0 > Exit"

read command

case $command in

  1) SourcePackage;;

  0) exit 0;;

  *) echo "Invalid command"
     exit 0;;

esac
