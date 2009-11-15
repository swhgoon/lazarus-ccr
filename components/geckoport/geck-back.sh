#!/bin/sh
zipfile=GeckoPort

zip -D $zipfile *.*
zip -d $zipfile *.o
zip -d $zipfile *.ppu
zip -d $zipfile *.bak

zip -D $zipfile each-version/*.*
zip -d $zipfile each-version/*.o
zip -d $zipfile each-version/*.ppu
zip -d $zipfile each-version/*.bak

zip -D $zipfile Components/*.*
zip -d $zipfile Components/*.o
zip -d $zipfile Components/*.ppu
zip -d $zipfile Components/*.bak

zip -D $zipfile SampleApps/*.*
zip -d $zipfile SampleApps/*.o
zip -d $zipfile SampleApps/*.ppu
zip -d $zipfile SampleApps/*.bak
zip -d $zipfile SampleApps/GBrowser
zip -d $zipfile SampleApps/BrowseWin
