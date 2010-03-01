{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit gradcontrols; 

interface

uses
  ugradtabcontrol, ugradbtn, uRotateBitmap, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradtabcontrol', @ugradtabcontrol.Register); 
  RegisterUnit('ugradbtn', @ugradbtn.Register); 
end; 

initialization
  RegisterPackage('gradcontrols', @Register); 
end.
