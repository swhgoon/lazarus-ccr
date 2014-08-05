#!/bin/bash
#надо скопировать rx.inc в текущий каталог, иначе не соберём (глюк fpdoc)
cp ../rx.inc rx.inc 
fpdoc --package=rxfpc --format=html --index-colcount=4 --hide-protected \
  --input=../curredit.pas --descr=rxfpc.xml \
  --input=../dbdateedit.pas --descr=rxfpc.xml \
  --input=../duallist.pas --descr=rxfpc.xml \
  --input=../folderlister.pas --descr=rxfpc.xml \
  --input=../rxctrls.pas --descr=rxfpc.xml \
  --input=../rxdice.pas --descr=rxfpc.xml \
  --input=../rxlookup.pas --descr=rxfpc.xml \
  --input=../rxlogin.pas --descr=rxfpc.xml \
  --input=../rxtoolbar.pas --descr=rxfpc.xml \
  --input=../rxspin.pas --descr=rxfpc.xml \
  --input=../rxclock.pas --descr=rxfpc.xml \
  --input=../rxmemds.pas --descr=rxfpc.xml \
  --input=../rxswitch.pas --descr=rxfpc.xml \
  --input=../tooledit.pas --descr=rxfpc.xml \
  --input=../pickdate.pas --descr=rxfpc.xml \
  --input=../rxversinfo.pas --descr=rxfpc.xml \
  --input=../rxtimeedit.pas --descr=rxfpc.xml \
  --input=../rxdbcomb.pas --descr=rxfpc.xml \
  --input=../rxdbtimeedit.pas --descr=rxfpc.xml \
  --input=../rxdbgrid.pas --descr=rxfpc.xml \
  --input=../rxdbgrid_columsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgrid_findunit.pas --descr=rxfpc.xml \
  --input=../rxdbctrls.pas --descr=rxfpc.xml \
  --input=../rxdbspinedit.pas --descr=rxfpc.xml \
  --input=../rxaboutformunit.pas --descr=rxfpc.xml \
  --input=../rxaboutdialog.pas --descr=rxfpc.xml \
  --input=../dateutil.pas --descr=rxfpc.xml \
  --input=../rxfileutils.pas --descr=rxfpc.xml \
  --input=../rxdbgridexportspreadsheet_paramsunit.pas --descr=rxfpc.xml \
  --input=../rxdbgridexportspreadsheet.pas --descr=rxfpc.xml 
