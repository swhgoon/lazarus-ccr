unit RegisterRxDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;
implementation
uses RxDBSpinEdit, RxDBTimeEdit;

procedure RegisterRxDBSpinEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBSpinEdit]);
end;

procedure RegisterRxDBTimeEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBTimeEdit]);
end;

procedure Register;
begin
  RegisterUnit('RxDBTimeEdit', @RegisterRxDBTimeEdit);
  RegisterUnit('RxDBSpinEdit', @RegisterRxDBSpinEdit);
end;

end.

