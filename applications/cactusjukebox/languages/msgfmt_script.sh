#!/bin/bash

if [ "$1" == "" ]; then
        echo Wrong parameters!!
        echo "Please give language code(en, de, gr, sv, ...)"
        echo
        exit
fi

echo formatting "'$1'" translations...
msgfmt cactus.$1.po -o cactus.$1.mo
echo "done"

