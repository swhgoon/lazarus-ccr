{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit JvXPBarLaz; 

interface

uses
  JvXPCoreUtils, JvXPBar, JvXPCore, JvXPBarReg, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('JvXPBarReg', @JvXPBarReg.Register); 
end; 

initialization
  RegisterPackage('JvXPBarLaz', @Register); 
end.
