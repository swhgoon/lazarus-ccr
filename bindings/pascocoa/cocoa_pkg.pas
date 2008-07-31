{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit cocoa_pkg; 

interface

uses
appkit, foundation, objc, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('cocoa_pkg', @Register); 
end.
