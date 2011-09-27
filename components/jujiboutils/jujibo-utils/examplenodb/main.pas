unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, JIntegerEdit, JLabeledIntegerEdit, JCurrencyEdit,
  JLabeledCurrencyEdit, JDateEdit, JLabeledDateEdit, JFloatEdit,
  JLabeledFloatEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    JCurrencyEdit1: TJCurrencyEdit;
    JDateEdit1: TJDateEdit;
    JFloatEdit1: TJFloatEdit;
    JIntegerEdit1: TJIntegerEdit;
    JLabeledCurrencyEdit1: TJLabeledCurrencyEdit;
    JLabeledDateEdit1: TJLabeledDateEdit;
    JLabeledFloatEdit1: TJLabeledFloatEdit;
    JLabeledIntegerEdit1: TJLabeledIntegerEdit;
    JLabeledIntegerEdit2: TJLabeledIntegerEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

end.

