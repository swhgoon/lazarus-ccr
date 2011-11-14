{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpsound_pkg; 

interface

uses
  fpsound_openal, fpsound, fpsound_wav, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('fpsound_pkg', @Register); 
end.
