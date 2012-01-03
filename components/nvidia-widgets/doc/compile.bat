set path=%path%;H:\other\graphviz\bin

fpclasschart --input=../src/nvwidgets/nvWidgets.pas --input=../src/nvwidgets/nvglwidgets.pas --input=../src/nvwidgets/nvglutwidgets.pas --input=../src/nvglutils/nvshaderutils.pas --input=../src/gl/glfreetype.pas --format=graphviz  --output=nvwidgets.dot

dot -Tpng nvwidgets.dot > ./images/nvwidgets.png

del nvwidgets.dot

::delete old chm documentation
del *.chm

::create chm documentation
fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets/nvWidgets.pas --descr=nvglwidgets.xml --input=../src/nvwidgets/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/nvwidgets/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/nvglutils/nvshaderutils.pas --descr=glfreetype.xml --input=../src/gl/glfreetype.pas --css-file=fpdoc.css --image-url=images/ --format=chm --output=nvwidgets.chm

::decompile the chm file (for debugging)
::del /Q /S html\*
::hh -decompile html nvwidgets.chm

::delete old html documentation
del *.html
del /Q /S nvwidgets\*.html
del /Q /S nvglutwidgets\*.html
del /Q /S nvglwidgets\*.html
del /Q /S nvshaderutils\*.html

::create html documentation
fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets/nvWidgets.pas --descr=nvglwidgets.xml --input=../src/nvwidgets/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/nvwidgets/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/nvglutils/nvshaderutils.pas --descr=glfreetype.xml --input=../src/gl/glfreetype.pas --image-url=images/ --format=html
