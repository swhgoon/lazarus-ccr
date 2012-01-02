set path=%path%;H:\other\graphviz\bin

fpclasschart --input=../src/nvwidgets/nvWidgets.pas --input=../src/nvwidgets/nvglwidgets.pas --input=../src/nvwidgets/nvglutwidgets.pas --input=../src/nvglutils/nvshaderutils.pas --format=graphviz  --output=nvwidgets.dot

dot -Tpng nvwidgets.dot > nvwidgets.png

del nvwidgets.dot

fpdoc --package=nvidia-widgets --descr=nvwidgets.xml --input=../src/nvwidgets/nvWidgets.pas --descr=nvglwidgets.xml --input=../src/nvwidgets/nvglwidgets.pas --descr=nvglutwidgets.xml --input=../src/nvwidgets/nvglutwidgets.pas --descr=nvshaderutils.xml --input=../src/nvglutils/nvshaderutils.pas --format=chm --output=nvwidgets.chm