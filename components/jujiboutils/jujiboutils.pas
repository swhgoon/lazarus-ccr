{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jujiboutils;

interface

uses
  JDBGridControl, JDBLabeledEdit, jdblabeledintegeredit, JDBLabeledFloatEdit, 
  jdblabeledcurrencyedit, jdblabeleddateedit, jcontrolutils, 
  JLabeledIntegerEdit, JLabeledFloatEdit, JLabeledCurrencyEdit, 
  JLabeledDateEdit, jdbgridutils, JLabeledTimeEdit, JDBLabeledTimeEdit, 
  JLabeledDateTimeEdit, JDBLabeledDateTimeEdit, jinputconsts, JDbEnumCombo, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JDBGridControl', @JDBGridControl.Register);
  RegisterUnit('JDBLabeledEdit', @JDBLabeledEdit.Register);
  RegisterUnit('jdblabeledintegeredit', @jdblabeledintegeredit.Register);
  RegisterUnit('JDBLabeledFloatEdit', @JDBLabeledFloatEdit.Register);
  RegisterUnit('jdblabeledcurrencyedit', @jdblabeledcurrencyedit.Register);
  RegisterUnit('jdblabeleddateedit', @jdblabeleddateedit.Register);
  RegisterUnit('JLabeledIntegerEdit', @JLabeledIntegerEdit.Register);
  RegisterUnit('JLabeledFloatEdit', @JLabeledFloatEdit.Register);
  RegisterUnit('JLabeledCurrencyEdit', @JLabeledCurrencyEdit.Register);
  RegisterUnit('JLabeledDateEdit', @JLabeledDateEdit.Register);
  RegisterUnit('JLabeledTimeEdit', @JLabeledTimeEdit.Register);
  RegisterUnit('JDBLabeledTimeEdit', @JDBLabeledTimeEdit.Register);
  RegisterUnit('JLabeledDateTimeEdit', @JLabeledDateTimeEdit.Register);
  RegisterUnit('JDBLabeledDateTimeEdit', @JDBLabeledDateTimeEdit.Register);
  RegisterUnit('JDbEnumCombo', @JDbEnumCombo.Register);
end;

initialization
  RegisterPackage('jujiboutils', @Register);
end.
