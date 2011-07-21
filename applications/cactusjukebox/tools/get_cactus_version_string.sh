#!/bin/bash
# argument must be path to cactus_const.inc
cat $1 | grep ' *CACTUS_VERSION *=.*;' | sed -e 's/const....[A-Z_ =;]*.//g' -e 's/.;//g' -e's/ //g'
