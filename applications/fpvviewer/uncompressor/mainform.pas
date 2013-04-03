unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    editFileName: TFileNameEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure InflateGZ(AGZFilename: string; ADest: TStream);
  end;

var
  Form1: TForm1;

implementation

uses zstream;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  DestStream: TFileStream;
  lDestFilename: string;
begin
  lDestFilename := Copy(editFileName.Text, 1, Length(editFileName.Text)-1);
  DestStream := TFileStream.Create(lDestFilename, fmCreate);
  try
    InflateGZ(editFileName.Text, DestStream);
  finally
    DestStream.Free;
  end;
end;

procedure TForm1.InflateGZ(AGZFilename: string; ADest: TStream);
var
  GZStream: TGZFileStream;
  chunk:string;
  cnt:integer;
const
  CHUNKSIZE=4096;
begin
  GZStream := TGZFileStream.Create(AGZFilename, gzopenread);
  try
    setlength(chunk,CHUNKSIZE);
    repeat
      cnt := GZStream.read(chunk[1],CHUNKSIZE);
      if cnt<CHUNKSIZE then
        setlength(chunk,cnt);
      ADest.Write(chunk[1], Length(chunk));
    until cnt<CHUNKSIZE;
  finally
    GZStream.Free;
  end;
end;

end.

