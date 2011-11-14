{
}
unit fpsound_wav;

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

// WAVE UTILS

type
  TRiffHeader = packed record
    ID      : array [0..3] of char;
    Size    : LongWord;
    Format  : array [0..3] of char;
  end;

  TWaveFormat = packed record
    ID            : array [0..3] of char;
    Size          : LongWord;
    Format        : Word;
    Channels      : Word;
    SampleRate    : LongWord;
    ByteRate      : LongWord;
    BlockAlign    : Word;
    BitsPerSample : Word;
  end;

  TDataChunk = packed record
    Id      : array [0..3] of char;
    Size    : LongWord;
  end;

  { TWaveReader }

  TWaveReader = class(TObject)
  private
    loaded    : Boolean;
    chunkdata : TDataChunk;
    chunkpos  : Int64;
    pos       : Int64;
    eof       : Boolean;
  public
    fmt   : TWaveFormat;
    fStream   : TStream;
    function LoadFromStream(AStream: TStream): Boolean;
    function ReadBuf(var Buffer; BufferSize: Integer): Integer;
  end;

implementation

const
  ID_RIFF = 'RIFF';
  ID_WAVE = 'WAVE';
  ID_fmt  = 'fmt ';
  ID_data = 'data';

{ TWaveReader }

function TWaveReader.LoadFromStream(AStream:TStream):Boolean;
var
  riff  : TRiffHeader;
begin
  fStream:=AStream;
  loaded:=True;
  try
    Result:=fStream.Read(riff, sizeof(riff))=sizeof(riff);
    riff.Size:=LEtoN(riff.Size);
    Result:=Result and (riff.ID=ID_RIFF) and (riff.Format=ID_WAVE);
    if not Result then Exit;

    Result:=fStream.Read(fmt, sizeof(fmt))=sizeof(fmt);
    fmt.Size:=LEtoN(fmt.Size);
    fmt.Format:=LEtoN(fmt.Format);
    fmt.Channels:=LEtoN(fmt.Channels);
    fmt.SampleRate:=LEtoN(fmt.SampleRate);
    fmt.ByteRate:=LEtoN(fmt.ByteRate);
    fmt.BlockAlign:=LEtoN(fmt.BlockAlign);
    fmt.BitsPerSample:=LEtoN(fmt.BitsPerSample);

    Result:=fmt.ID=ID_fmt;
    pos:=-1;
  except
    Result:=False;
    Exit;
  end;
end;

function Min(a,b: Integer): Integer;
begin
  if a<b then Result:=a
  else Result:=b;
end;

function TWaveReader.ReadBuf(var Buffer;BufferSize:Integer):Integer;
var
  sz  : Integer;
  p   : PByteArray;
  i   : Integer;
begin
  FillChar(Buffer, BufferSize, 0);
  Result:=0;
  // all data read
  if eof then Exit;

  p:=@Buffer;
  i:=0;
  while (not eof) and (i<bufferSize) do begin
    if chunkpos>=chunkdata.Size then begin
      if pos<0 then
        fstream.Position:=sizeof(TRiffHeader)+Int64(fmt.Size)+sizeof(TDataChunk)
     else
        fstream.Position:=pos+chunkdata.size+SizeOf(chunkdata);

      eof:=pos>=fStream.Size;
      if not eof then begin
        pos:=fStream.Position;
        sz:=fstream.Read(chunkdata, sizeof(chunkdata));
        chunkdata.Size:=LEtoN(chunkdata.Size);
        if (sz<>sizeof(chunkdata)) or (chunkdata.Id<>ID_data) then
          chunkpos:=chunkdata.Size
        else
          chunkpos:=0;
      end;
    end else begin
      sz:=Min(BufferSize, chunkdata.Size-chunkpos);
      fStream.Position:=pos+sizeof(chunkdata)+chunkpos;
      sz:=fStream.Read(p[i], sz);
      if sz<0 then Exit;
      inc(chunkpos, sz);
      inc(i, sz);
    end;
  end;
  Result:=i;
end;

end.

