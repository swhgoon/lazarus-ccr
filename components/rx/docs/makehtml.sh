#!/bin/bash
#надо скопировать rx.inc в текущий каталог, иначе не соберём (глюк fpdoc)
cp ../rx.inc rx.inc 
fpdoc --package=rxfpc --format=html  \
  --input=../rxversinfo.pas --descr=rxfpc.xml \
  --input=../rxdbgrid.pas --descr=rxfpc.xml \
  --input=../curredit.pas --descr=rxfpc.xml \
  --input=../rxlookup.pas --descr=rxfpc.xml \
  --input=../rxfileutils.pas --descr=rxfpc.xml \
  --input=../pickdate.pas --descr=rxfpc.xml
  
