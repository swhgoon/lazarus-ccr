set path=%path%;H:\other\graphviz\bin

fpclasschart --input=../src/nvwidgets/nvWidgets.pas --input=../src/nvwidgets/nvglwidgets.pas --input=../src/nvwidgets/nvglutwidgets.pas --input=../src/nvglutils/nvshaderutils.pas --format=graphviz  --output=nvwidgets.dot

dot -Tpng nvwidgets.dot > ./images/nvwidgets.png

del nvwidgets.dot

::create chm documentation
fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets/nvWidgets.pas --descr=nvglwidgets.xml --input=../src/nvwidgets/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/nvwidgets/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/nvglutils/nvshaderutils.pas --css-file=fpdoc.css --format=chm --output=nvwidgets.chm

::delete old html documentation
del *.html
del /Q /S nvwidgets\*.html
del /Q /S nvglutwidgets\*.html
del /Q /S nvglwidgets\*.html
del /Q /S nvshaderutils\*.html

::create html documentation
fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets/nvWidgets.pas --descr=nvglwidgets.xml --input=../src/nvwidgets/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/nvwidgets/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/nvglutils/nvshaderutils.pas --css-file=fpdoc.css --format=html
