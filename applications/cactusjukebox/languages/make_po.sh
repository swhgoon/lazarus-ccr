#!/bin/bash

cat ../source/obj/mainform.rst ../source/obj/cdrip.rst ../source/obj/settings.rst > ./cactus.rst

rstconv -i cactus.rst -o cactus_tmp1.po

cat ./header.po cactus_tmp1.po > cactus_tmp.po


#msgmerge cactus.po cactus_tmp.po -o cactus.po

cp cactus_tmp.po cactus.po

rm cactus_tmp.po
rm cactus_tmp1.po

echo " cactus.po created. now translate this file to any language you want..."
