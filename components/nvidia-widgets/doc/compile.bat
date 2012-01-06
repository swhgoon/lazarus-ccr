set path=%path%;H:\other\graphviz\bin

fpclasschart --input=../src/nvwidgets.pas --input=../src/gl/nvglwidgets.pas --input=../src/glut/nvglutwidgets.pas --input=../src/gl/nvshaderutils.pas --input=../src/gl/glfreetype.pas --input=../src/nvbasefont.pas --format=graphviz  --output=nvwidgets.dot

dot -Tpng nvwidgets.dot > ./images/nvwidgets.png

del nvwidgets.dot

::delete old chm documentation
del *.chm

::create chm documentation
fpdoc --package=nvidia-widgets --auto-index --auto-toc --make-searchable --footer-date="mmm dd yyyy" --descr=nvwidgets.xml --input=../src/nvwidgets.pas --descr=nvglwidgets.xml --input=../src/gl/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/glut/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/gl/nvshaderutils.pas --descr=glfreetype.xml --input=../src/gl/glfreetype.pas --descr=nvbasefont.xml --input=../src/nvbasefont.pas --css-file=fpdoc.css --image-url=images/ --format=chm --output=nvwidgets.chm

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
::once documentation settles add --footer-date="mmm dd yyyy" 
fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets.pas --descr=nvglwidgets.xml --input=../src/gl/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/glut/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/gl/nvshaderutils.pas --descr=glfreetype.xml --input=../src/gl/glfreetype.pas --descr=nvbasefont.xml --input=../src/nvbasefont.pas --image-url=images/ --format=html
