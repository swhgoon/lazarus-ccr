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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses fpsound, fpsound_wav;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnOpenPlayAndCloseClick(Sender: TObject);
var
  lSoundDoc: TSoundDocument;
begin
  lSoundDoc := TSoundDocument.Create;
  lSoundDoc.LoadFromFile(pathEdit.FileName);
  lSoundDoc.Free;
end;

end.

