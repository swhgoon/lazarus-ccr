{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit bluetoothlaz; 

interface

uses
  Bluetooth, WiiMoteTools, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('bluetoothlaz', @Register); 
end.
