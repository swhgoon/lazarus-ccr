{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iOSDesigner;

interface

uses
  iOSNIBDesigner, iOS_Views, iOSIdeIntf, iOSXIBResource, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('iOSNIBDesigner', @iOSNIBDesigner.Register);
  RegisterUnit('iOSIdeIntf', @iOSIdeIntf.Register);
end;

initialization
  RegisterPackage('iOSDesigner', @Register);
end.
