unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons, EditBtn, ACS_File, StdCtrls
  ,ACS_AllFormats,ACS_Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    btStart: TBitBtn;
    cbOutput: TComboBox;
    DirectoryEdit: TDirectoryEdit;
    FileIn: TACSFileIn;
    FileOut: TACSFileOut;
    Label1: TLabel;
    pbProgress: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FileOutDone(Sender: TComponent);
    procedure FileOutProgress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  FileIn.Open;
  DirectoryEdit.Text := ExtractFileDir(FileIn.FileName);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
var
  newfilename : string;
begin
  newfilename := ExtractFileName(FileIn.FileName);
  if newfilename = '' then
    begin
      ShowMessage('You must open an file first !');
      exit;
    end;
  newfilename := copy(newfilename,0,length(newfilename)-length(ExtractFileExt(newfilename)));
  if DirectoryEdit.Directory = '' then
    begin
      ShowMessage('You must select an directory first !');
      exit;
    end;
  FileOut.FileName := DirectoryEdit.Directory+DirectorySeparator+newfilename+'.'+cbOutput.Text;
  btStart.Enabled := False;
  FileOut.Run;
end;

procedure TForm1.FileOutDone(Sender: TComponent);
begin
  btStart.Enabled := True;
  pbProgress.Position := 0;
end;

procedure TForm1.FileOutProgress(Sender: TComponent);
begin
  pbProgress.Position := round(FileOut.Progress);
  Update;
  Application.Processmessages;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to Fileformats.Count-1 do
    if TACSFileFormat(Fileformats[i]).FileClass.InheritsFrom(TACSCustomFileOut) then
      cbOutput.Items.Add(TACSFileFormat(Fileformats[i]).Extension);
end;

initialization
  {$I umain.lrs}

end.

