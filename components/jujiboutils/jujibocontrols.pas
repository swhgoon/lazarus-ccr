{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jujibocontrols; 

interface

uses
  jdblabeledintegeredit, jdblabeledcurrencyedit, jdblabeleddateedit, 
  jcontrolutils, JLabeledIntegerEdit, JLabeledCurrencyEdit, JLabeledDateEdit, 
  JDBGridControl, jdbgridutils, JDBLabeledEdit, JLabeledTimeEdit, 
  JDBLabeledTimeEdit, JLabeledDateTimeEdit, JDBLabeledDateTimeEdit, 
  JLabeledFloatEdit, JDBLabeledFloatEdit, jinputconsts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('jdblabeledintegeredit', @jdblabeledintegeredit.Register); 
  RegisterUnit('jdblabeledcurrencyedit', @jdblabeledcurrencyedit.Register); 
  RegisterUnit('jdblabeleddateedit', @jdblabeleddateedit.Register); 
  RegisterUnit('JLabeledIntegerEdit', @JLabeledIntegerEdit.Register); 
  RegisterUnit('JLabeledCurrencyEdit', @JLabeledCurrencyEdit.Register); 
  RegisterUnit('JLabeledDateEdit', @JLabeledDateEdit.Register); 
  RegisterUnit('JDBGridControl', @JDBGridControl.Register); 
  RegisterUnit('JDBLabeledEdit', @JDBLabeledEdit.Register); 
  RegisterUnit('JLabeledTimeEdit', @JLabeledTimeEdit.Register); 
  RegisterUnit('JDBLabeledTimeEdit', @JDBLabeledTimeEdit.Register); 
  RegisterUnit('JLabeledDateTimeEdit', @JLabeledDateTimeEdit.Register); 
  RegisterUnit('JDBLabeledDateTimeEdit', @JDBLabeledDateTimeEdit.Register); 
  RegisterUnit('JLabeledFloatEdit', @JLabeledFloatEdit.Register); 
  RegisterUnit('JDBLabeledFloatEdit', @JDBLabeledFloatEdit.Register); 
end; 

initialization
  RegisterPackage('jujibocontrols', @Register); 
end.
