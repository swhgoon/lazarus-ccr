{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_sort_sqldb;

interface

uses
  RxSortSqlDB, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxSortSqlDB', @RxSortSqlDB.Register);
end;

initialization
  RegisterPackage('rx_sort_sqldb', @Register);
end.
