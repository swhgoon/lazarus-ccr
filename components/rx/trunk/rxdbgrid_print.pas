{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxdbgrid_print;

interface

uses
  RxDBGridPrintGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxDBGridPrintGrid', @RxDBGridPrintGrid.Register);
end;

initialization
  RegisterPackage('rxdbgrid_print', @Register);
end.
