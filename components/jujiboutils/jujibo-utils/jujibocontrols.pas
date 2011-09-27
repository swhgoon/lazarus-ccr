{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jujibocontrols; 

interface

uses
  jdbintegeredit, jdblabeledintegeredit, jdbcurrencyedit, 
  jdblabeledcurrencyedit, jdbdateedit, jdblabeleddateedit, jcontrolutils, 
  JIntegerEdit, JLabeledIntegerEdit, JCurrencyEdit, JLabeledCurrencyEdit, 
  JDateEdit, JLabeledDateEdit, JDBGridControl, jdbgridutils, JDBLabeledEdit, 
  JTimeEdit, JLabeledTimeEdit, JDBTimeEdit, JDBLabeledTimeEdit, JDateTimeEdit, 
  JLabeledDateTimeEdit, JDBDateTimeEdit, JDBLabeledDateTimeEdit, JFloatEdit, 
  JLabeledFloatEdit, JDBFloatEdit, JDBLabeledFloatEdit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('jdbintegeredit', @jdbintegeredit.Register); 
  RegisterUnit('jdblabeledintegeredit', @jdblabeledintegeredit.Register); 
  RegisterUnit('jdbcurrencyedit', @jdbcurrencyedit.Register); 
  RegisterUnit('jdblabeledcurrencyedit', @jdblabeledcurrencyedit.Register); 
  RegisterUnit('jdbdateedit', @jdbdateedit.Register); 
  RegisterUnit('jdblabeleddateedit', @jdblabeleddateedit.Register); 
  RegisterUnit('JIntegerEdit', @JIntegerEdit.Register); 
  RegisterUnit('JLabeledIntegerEdit', @JLabeledIntegerEdit.Register); 
  RegisterUnit('JCurrencyEdit', @JCurrencyEdit.Register); 
  RegisterUnit('JLabeledCurrencyEdit', @JLabeledCurrencyEdit.Register); 
  RegisterUnit('JDateEdit', @JDateEdit.Register); 
  RegisterUnit('JLabeledDateEdit', @JLabeledDateEdit.Register); 
  RegisterUnit('JDBGridControl', @JDBGridControl.Register); 
  RegisterUnit('JDBLabeledEdit', @JDBLabeledEdit.Register); 
  RegisterUnit('JTimeEdit', @JTimeEdit.Register); 
  RegisterUnit('JLabeledTimeEdit', @JLabeledTimeEdit.Register); 
  RegisterUnit('JDBTimeEdit', @JDBTimeEdit.Register); 
  RegisterUnit('JDBLabeledTimeEdit', @JDBLabeledTimeEdit.Register); 
  RegisterUnit('JDateTimeEdit', @JDateTimeEdit.Register); 
  RegisterUnit('JLabeledDateTimeEdit', @JLabeledDateTimeEdit.Register); 
  RegisterUnit('JDBDateTimeEdit', @JDBDateTimeEdit.Register); 
  RegisterUnit('JDBLabeledDateTimeEdit', @JDBLabeledDateTimeEdit.Register); 
  RegisterUnit('JFloatEdit', @JFloatEdit.Register); 
  RegisterUnit('JLabeledFloatEdit', @JLabeledFloatEdit.Register); 
  RegisterUnit('JDBFloatEdit', @JDBFloatEdit.Register); 
  RegisterUnit('JDBLabeledFloatEdit', @JDBLabeledFloatEdit.Register); 
end; 

initialization
  RegisterPackage('jujibocontrols', @Register); 
end.
