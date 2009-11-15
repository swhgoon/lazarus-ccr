#!/bin/sh
lazdir=~/lazarus
if ! [ -e $lazdir ] 
then
  lazdir=/usr/local/share/lazarus
fi
if [ `arch` = "ppc" ]
then
  proc=powerpc
else
  proc=i386
fi
lclunits=$lazdir/lcl/units/$proc-darwin
/usr/local/bin/fpc -dLCL -dLCLCarbon -WG -Sda -gl -O1 -Cirot -k'-framework' -k'carbon' -k'-framework' -k'opengl' -Fu$lclunits -Fu$lclunits/carbon -Fu../ -Fu../Components GBrowser.dpr
