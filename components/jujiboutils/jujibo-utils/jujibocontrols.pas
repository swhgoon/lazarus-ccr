{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jujibocontrols; 

interface

uses
  jdbcurrencyedit, jdbdateedit, jdbintegeredit, jcontrolutils, 
  jdblabeledcurrencyedit, jdblabeleddateedit, jdblabeledintegeredit, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('jdbcurrencyedit', @jdbcurrencyedit.Register); 
  RegisterUnit('jdbdateedit', @jdbdateedit.Register); 
  RegisterUnit('jdbintegeredit', @jdbintegeredit.Register); 
  RegisterUnit('jdblabeledcurrencyedit', @jdblabeledcurrencyedit.Register); 
  RegisterUnit('jdblabeleddateedit', @jdblabeleddateedit.Register); 
  RegisterUnit('jdblabeledintegeredit', @jdblabeledintegeredit.Register); 
end; 

initialization
  RegisterPackage('jujibocontrols', @Register); 
end.
