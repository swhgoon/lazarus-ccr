{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit rtfviewpkg; 

interface

uses
  RTFView, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RTFView', @RTFView.Register); 
end; 

initialization
  RegisterPackage('rtfviewpkg', @Register); 
end.
