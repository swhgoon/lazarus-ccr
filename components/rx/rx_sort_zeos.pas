{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_sort_zeos;

interface

uses
  RxSortZeos, exsortzeos, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxSortZeos', @RxSortZeos.Register);
end;

initialization
  RegisterPackage('rx_sort_zeos', @Register);
end.
