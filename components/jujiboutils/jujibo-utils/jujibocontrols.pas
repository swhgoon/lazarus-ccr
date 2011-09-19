{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jujibocontrols; 

interface

uses
  jdbcurrencyedit, jdbdateedit, jdbintegeredit, jcontrolutils, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('jdbcurrencyedit', @jdbcurrencyedit.Register); 
  RegisterUnit('jdbdateedit', @jdbdateedit.Register); 
  RegisterUnit('jdbintegeredit', @jdbintegeredit.Register); 
end; 

initialization
  RegisterPackage('jujibocontrols', @Register); 
end.
