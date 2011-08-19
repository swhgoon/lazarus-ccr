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
    Button2: TButton;
    editInputPath: TDirectoryEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
var
  lSourcePath, lDestPath: String;
begin
  lSourcePath := IncludeTrailingPathDelimiter(editInputPath.Text) + 'android_all.pas';
  lDestPath := '/home/felipe/Programas/lazarus/lcl/interfaces/android/android_all.pas';
  FileUtil.CopyFile(lSourcePath, lDestPath);
  lSourcePath := IncludeTrailingPathDelimiter(editInputPath.Text) + 'AndroidAll.java';
  lDestPath := '/home/felipe/Programas/lazarus-ccr/examples/androidlcl/android/src/com/pascal/androidlcl/AndroidAll.java';
  FileUtil.CopyFile(lSourcePath, lDestPath);
end;

end.

