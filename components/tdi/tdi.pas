{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TDI ;

interface

uses
  TDIClass, TDIConst, TDIReg, LazarusPackageIntf;

implementation

procedure Register ;
begin
  RegisterUnit('TDIReg', @TDIReg.Register) ;
end ;

initialization
  RegisterPackage('TDI', @Register) ;
end.
