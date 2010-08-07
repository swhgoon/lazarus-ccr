{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit chelper;

interface

uses
    toSourceEditor, ctopasconvert, extconvdialog, cconvconfig, 
  converteridesettings, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('toSourceEditor',@toSourceEditor.Register);
end;

initialization
  RegisterPackage('chelper',@Register);
end.
