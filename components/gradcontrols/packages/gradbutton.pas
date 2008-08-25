{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit gradbutton; 

interface

uses
ugradbtn, uRotateBitmap, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ugradbtn', @ugradbtn.Register); 
end; 

initialization
  RegisterPackage('gradbutton', @Register); 
end.
