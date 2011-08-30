unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbCtrls, ExtCtrls, InterfaceBase;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2; 

implementation

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var
  lChecked, lEdit1Text: String;
  lComboBox: String;
begin
  if CheckBox1.Checked then lChecked := 'True'
  else lChecked := 'False';

  WidgetSet.DebugLogLn('3');
  lEdit1Text := Edit1.Text;

  WidgetSet.DebugLogLn('4');
  lComboBox := IntToStr(ComboBox1.ItemIndex);

  WidgetSet.DebugLogLn('5');

  Edit2.Text :=
    'Edit1.Text='+lEdit1Text+LineEnding+
    'Caption='+Caption+LineEnding+
    'Checked?='+lChecked+LineEnding+
    'ComboBox='+lComboBox;

  Caption := lEdit1Text;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  WidgetSet.DebugLogLn('TForm2.Button2Click');
  InputBox('Caption', 'Prompt', 'Default');
  WidgetSet.DebugLogLn('TForm2.Button2Click 2');
end;

initialization
  {$I mainform.lrs}

end.

