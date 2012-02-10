unit FileCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FastBitmap;

type
  TFileCacheItem = record
    start: longint;
    length: longint;
    number: integer;
  end;

  { TFileCache }

  TFileCache = class
  private
    cache_stream: TFileStream;
    FCount: integer;
    FCacheList: array of TFileCacheItem;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;

    property Count: integer read FCount;
    function GetData(Number: integer; var Bitmap: TFastBitmap): boolean;
    procedure Add(Number: integer; Bitmap: TFastBitmap);
    procedure Clear;
  end;

implementation

{ TFileCache }

constructor TFileCache.Create(AFileName: string);
begin
  FCount := 0;
  cache_stream := TFileStream.Create(AFileName, fmCreate);
end;

destructor TFileCache.Destroy;
begin
  Clear;
  cache_stream.Free;

  inherited Destroy;
end;

function TFileCache.GetData(Number: integer; var Bitmap: TFastBitmap): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FCount - 1 do
    if FCacheList[i].number = Number then
    begin
      cache_stream.Position := FCacheList[i].start;
      cache_stream.Read(Bitmap.PixelsData^, FCacheList[i].length);
      Result := True;
      exit;
    end;
end;

procedure TFileCache.Add(Number: integer; Bitmap: TFastBitmap);
begin
  if Bitmap = nil then
    exit;

  Inc(FCount);
  SetLength(FCacheList, FCount);

  FCacheList[FCount - 1].number := Number;

  //move to the end of the stream
  cache_stream.Position := cache_stream.Size;

  FCacheList[FCount - 1].start := cache_stream.Position;
  cache_stream.Write(Bitmap.PixelsData^, Bitmap.Size.x * Bitmap.Size.y * 4);
  FCacheList[FCount - 1].length := cache_stream.Position - FCacheList[FCount - 1].start;
end;

procedure TFileCache.Clear;
begin
  FCount := 0;
  SetLength(FCacheList, FCount);
end;

end.

