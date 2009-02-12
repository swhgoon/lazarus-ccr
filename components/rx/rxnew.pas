{ Этот файл был автоматически создан Lazarus. Н�
  � редактировать!
  Исходный код используется только для комп�
    �ляции и установки пакета.
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
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('registerrx', @registerrx.Register); 
  RegisterUnit('RegisterRxDB', @RegisterRxDB.Register); 
end; 

initialization
  RegisterPackage('rxnew', @Register); 
end.
