{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gradcontrols; 

interface

uses
  ugradbtn, ugradtabcontrol, uRotateBitmap, gradcustomcontrol, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradbtn', @ugradbtn.Register); 
  RegisterUnit('ugradtabcontrol', @ugradtabcontrol.Register); 
end; 

initialization
  RegisterPackage('gradcontrols', @Register); 
end.
