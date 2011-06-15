unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, android_sdk_bindings_gen;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    editInputPath: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  AndroidSDKBindingsGen.GenerateAllBindings(editInputPath.Text, editInputPath.Text, editInputPath.Text);
end;

end.

