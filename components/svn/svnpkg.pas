{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit svnpkg; 

interface

uses
  svnclasses, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('svnpkg', @Register); 
end.
