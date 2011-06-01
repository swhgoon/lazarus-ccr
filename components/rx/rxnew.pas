{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxnew; 

interface

uses
  rxlookup, vclutils, dateutil, dbutils, rxapputils, rxdconst, rxstrutils, 
  dbdateedit, registerrx, curredit, folderlister, rxdbgrid, rxmemds, duallist, 
  boxprocs, tooledit, rxswitch, rxdice, rxdbcomb, rxtoolbar, rxtbrsetup, 
  fduallst, rxxpman, pagemngr, rxappicon, seldsfrm, rxctrls, rxlogin, 
  rxdbgrid_findunit, rxdbgrid_columsunit, rxpopupunit, rxcustomchartpanel, 
  rxsortmemds, AutoPanel, pickdate, rxiconv, rxceEditLookupFields, rxclock, 
  rxspin, RxDBSpinEdit, RegisterRxDB, RxTimeEdit, RxDBTimeEdit, RxDBCtrls, 
  rxfilterby, rxconst, rxFileUtils, RxVersInfo, RxAboutDialog, 
  rxAboutFormUnit, dbcurredit, RxViewsPanel, RxSystemServices, 
  RegisterRxTools, RxDBColorBox, rxConfigValues, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('registerrx', @registerrx.Register); 
  RegisterUnit('RegisterRxDB', @RegisterRxDB.Register); 
  RegisterUnit('RegisterRxTools', @RegisterRxTools.Register); 
end; 

initialization
  RegisterPackage('rxnew', @Register); 
end.
