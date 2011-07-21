#!/bin/bash

if [ "$1" == "" ]; then
	echo Wrong parameters!! 
	echo "Please give language code(en, de, gr, sv, ...)"
	echo
	exit 
fi

cat ../source/obj/mainform.rst ../source/obj/cdrip.rst ../source/obj/settings.rst > ./cactus.rst

rstconv -i cactus.rst -o cactus_tmp1.po

cat ./header.po cactus_tmp1.po > cactus_tmp.po


msgmerge cactus.$1.po cactus_tmp.po -o cactus.$1.po

rm cactus_tmp.po
rm cactus_tmp1.po

echo cactus.$1.po updated. now update/modify/correct the translations inside...
