{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit iphonelazext; 

interface

uses
    ideext, iPhoneExtStr, iPhoneBundle, XCodeProject, 
  environment_iphone_options, project_iphone_options, iPhoneExtOptions, 
  xcodetemplate, lazfilesutils, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ideext', @ideext.Register); 
end; 

initialization
  RegisterPackage('iphonelazext', @Register); 
end.
