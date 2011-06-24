#
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
# 

DESTDIR=""
EXENAME="lazimageeditor"

for arg; do

  case $arg in

    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;

  esac;

done

#
# Does the install
#

mkdir -p $DESTDIR/usr/share/$EXENAME
mkdir -p $DESTDIR/usr/share/$EXENAME/Images

cp ./Images/*.png $DESTDIR/usr/share/$EXENAME/Images/

mkdir -p $DESTDIR/usr/bin

cp ./$EXENAME $DESTDIR/usr/bin/$EXENAME
