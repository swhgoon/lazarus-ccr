#!/bin/bash
#надо скопировать rx.inc в текущий каталог, иначе не соберём (глюк fpdoc)
cp ../rx.inc rx.inc 
fpdoc --package=rxfpc --format=html  \
  --input=../rxdbgrid.pas --descr=rxfpc.xml \
  --input=../rxctrls.pas --descr=rxfpc.xml \
  --input=../curredit.pas --descr=rxfpc.xml \
  --input=../rxlookup.pas --descr=rxfpc.xml \
  --input=../dbdateedit.pas --descr=rxfpc.xml \
  --input=../folderlister.pas --descr=rxfpc.xml \
  --input=../rxmemds.pas --descr=rxfpc.xml \
  --input=../tooledit.pas --descr=rxfpc.xml \
  --input=../pickdate.pas --descr=rxfpc.xml \
  --input=../rxversinfo.pas --descr=rxfpc.xml \
  --input=../rxtoolbar.pas --descr=rxfpc.xml \
  --input=../rxfileutils.pas --descr=rxfpc.xml 
  
