unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

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

{$ifdef LCLAndroid}
uses androidpipescomm;
{$endif}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var
  lChecked, lEdit1Text: String;
  lComboBox: String;
begin
  if CheckBox1.Checked then lChecked := 'True'
  else lChecked := 'False';

  {$ifdef LCLAndroid}
  vAndroidPipesComm.Log('3');
  {$endif}
  lEdit1Text := Edit1.Text;

  {$ifdef LCLAndroid}
  vAndroidPipesComm.Log('4');
  {$endif}
  lComboBox := IntToStr(ComboBox1.ItemIndex);

  {$ifdef LCLAndroid}
  vAndroidPipesComm.Log('5');
  {$endif}

  Edit2.Text :=
    'Edit1.Text='+lEdit1Text+LineEnding+
    'Caption='+Caption+LineEnding+
    'Checked?='+lChecked+LineEnding+
    'ComboBox='+lComboBox;

  Caption := lEdit1Text;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  {$ifdef LCLAndroid}
  vAndroidPipesComm.Log('TForm2.Button2Click');
  {$endif}
  InputBox('Caption', 'Prompt', 'Default');
  {$ifdef LCLAndroid}
  vAndroidPipesComm.Log('TForm2.Button2Click');
  {$endif}
end;

initialization
  {$I mainform.lrs}

end.

