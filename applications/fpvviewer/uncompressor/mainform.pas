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
    buttonUncompressAndShow: TButton;
    editFileName: TFileNameEdit;
    Label1: TLabel;
    memoOutput: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure buttonUncompressAndShowClick(Sender: TObject);
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

procedure TForm1.buttonUncompressAndShowClick(Sender: TObject);
var
  DestStream: TMemoryStream;
  i: Integer;
  lStr: string;
  lByte: Byte;
begin
  DestStream := TMemoryStream.Create();
  try
    InflateGZ(editFileName.Text, DestStream);
    DestStream.Position := 0;
    for i := 0 to DestStream.Size-1 do
    begin
      lByte := DestStream.ReadByte();
      lStr := lStr + Format('%x=%s ', [lByte, Char(lByte)])
    end;
    memoOutput.Text := lStr;
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

