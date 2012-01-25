unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnOpenPlayAndClose: TButton;
    pathEdit: TFileNameEdit;
    procedure btnOpenPlayAndCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses fpsound, fpsound_wav, fpsound_openal;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnOpenPlayAndCloseClick(Sender: TObject);
begin
  SoundPlayer.LoadFromFile(pathEdit.FileName);
  SoundPlayer.SetSoundPlayer(spOpenAL);
  SoundPlayer.Play;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef Windows}
  pathEdit.FileName := ExtractFilePath(Application.ExeName) + '..\testsounds\test.wav';
  pathEdit.FileName := SysUtils.ExpandFileName(pathEdit.FileName);
  {$endif}
  {$ifdef Darwin}
  pathEdit.FileName := ExtractFilePath(Application.ExeName) + '..\..\..\..\testsounds\test.wav';
  pathEdit.FileName := SysUtils.ExpandFileName(pathEdit.FileName);
  {$endif}
end;

end.

