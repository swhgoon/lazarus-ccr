unit RegisterRxDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;
implementation
uses RxDBSpinEdit, RxDBTimeEdit, RxDBCtrls;

procedure RegisterRxDBSpinEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBSpinEdit]);
end;

procedure RegisterRxDBTimeEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBTimeEdit]);
end;

procedure RegisterRxDBCtrls;
begin
  RegisterComponents('RX DBAware',[TRxDBProgressBar, TRxDBTrackBar]);
end;

procedure Register;
begin
  RegisterUnit('RxDBTimeEdit', @RegisterRxDBTimeEdit);
  RegisterUnit('RxDBSpinEdit', @RegisterRxDBSpinEdit);
  RegisterUnit('RxDBCtrls', @RegisterRxDBCtrls);
end;

end.

