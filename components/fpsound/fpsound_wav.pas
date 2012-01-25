{
WAV format reader for the fpSound library

License: The same modified LGPL as the LCL

Authors:

JiXian Yang
Felipe Monteiro de Carvalho

Canonical WAV file description here:

https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
}
unit fpsound_wav;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Math,
  fpsound;

// WAVE UTILS

type
  // WAV is formed by the following structures in this order
  // All items are in little endian order, except the char arrays
  // Items might be in big endian order if the RIFF identifier is RIFX

  TRiffHeader = packed record
    ID      : array [0..3] of char; // should be RIFF
    Size    : LongWord; // 4 + (8 + SubChunk1Size) + (8 + SubChunk2Size). The entire file size excluding TRiffHeader.ID and .Size
    Format  : array [0..3] of char; // should be WAVE
  end;

  TWaveFormat = packed record
    ID            : array [0..3] of char; // Should be "fmt "
    Size          : LongWord; // SubChunk1Size
    Format        : Word; // PCM = 1 (Linear quantization), values > 1 indicate a compressed format
    Channels      : Word; // Mono = 1, Stereo = 2, etc
    SampleRate    : LongWord; // 8000, 44100, etc
    ByteRate      : LongWord; // = SampleRate * NumChannels * BitsPerSample/8
    BlockAlign    : Word; // = NumChannels * BitsPerSample/8
    BitsPerSample : Word; // examples: 8 bits, 16 bits, etc
  end;
  // If the format is not PCM then there will also be:
//  TWaveFormatExtension = packed record
//    ExtraParamSize: Word;
//    ExtraParams...
//  end;

  TDataChunk = packed record
    Id      : array [0..3] of char; // should be "data"
    Size    : LongWord; // == NumSamples * NumChannels * BitsPerSample/8
  end;
  // And after this header the actual data comes, which is an array of samples

  { TWaveReader }

  TWaveReader = class(TSoundReader)
  public
    fmt: TWaveFormat;
    datachunk: TDataChunk;
    NumSamples: Integer;
    procedure ReadFromStream(AStream: TStream; ADest: TSoundDocument); override;
    procedure ReadHeaders(AStream: TStream; ADest: TSoundDocument);
    procedure ReadAllSamples(AStream: TStream; ADest: TSoundDocument);
    procedure ReadSample(AStream: TStream; ADest: TSoundDocument);
    //function ReadBuf(var Buffer; BufferSize: Integer): Integer;
  end;

implementation

const
  ID_RIFF = 'RIFF';
  ID_WAVE ='WAVE';
  fD_fmt  = 'fmt ';
  ID_data = 'data';

{ TWaveReader }

procedure TWaveReader.ReadFromStream(AStream:TStream; ADest: TSoundDocument);
begin
  ReadHeaders(AStream, ADest);
  ReadAllSamples(AStream, ADest);
end;

procedure TWaveReader.ReadHeaders(AStream: TStream; ADest: TSoundDocument);
var
  riff  : TRiffHeader;
  lKeyElement: TSoundKeyElement;
begin
  AStream.Read(riff, sizeof(riff));//=sizeof(riff);
  riff.Size:=LEtoN(riff.Size);
  //Result:=Result and (riff.ID=ID_RIFF) and (riff.Format=ID_WAVE);
  //if not Result then Exit;

  AStream.Read(fmt, sizeof(fmt));//=sizeof(fmt);
  fmt.Size:=LEtoN(fmt.Size);
  fmt.Format:=LEtoN(fmt.Format);
  fmt.Channels:=LEtoN(fmt.Channels);
  fmt.SampleRate:=LEtoN(fmt.SampleRate);
  fmt.ByteRate:=LEtoN(fmt.ByteRate);
  fmt.BlockAlign:=LEtoN(fmt.BlockAlign);
  fmt.BitsPerSample:=LEtoN(fmt.BitsPerSample);

  AStream.Read(datachunk, sizeof(datachunk));
  datachunk.Size := LEtoN(datachunk.Size);

  NumSamples := fmt.BlockAlign div datachunk.size;

  //Result:=fmt.ID=ID_fmt;
//  pos:=-1;

  // Store the data in the document
  lKeyElement := TSoundKeyElement.Create;
  lKeyElement.SampleRate := fmt.SampleRate;
  lKeyElement.BitsPerSample := fmt.BitsPerSample;
  lKeyElement.Channels := fmt.Channels;
  ADest.AddSoundElement(lKeyElement);
end;

procedure TWaveReader.ReadAllSamples(AStream: TStream; ADest: TSoundDocument);
var
  i: Integer;
begin
  for i := 0 to NumSamples - 1 do
    ReadSample(AStream, ADest);
end;

procedure TWaveReader.ReadSample(AStream: TStream; ADest: TSoundDocument);
var
  lSoundSample8: TSoundSample8;
  lSoundSample16: TSoundSample16;
  i: Integer;
  lByteData: Byte;
  lWordData: SmallInt;
begin
  if fmt.BitsPerSample = 8 then
  begin
    lSoundSample8 := TSoundSample8.Create;
    SetLength(lSoundSample8.ChannelValues, fmt.Channels);
    for i := 0 to fmt.Channels - 1 do
    begin
      lByteData := AStream.ReadByte();
      lSoundSample8.ChannelValues[i] := lByteData;
    end;

    ADest.AddSoundElement(lSoundSample8);
  end
  else if fmt.BitsPerSample = 16 then
  begin
    lSoundSample16 := TSoundSample16.Create;
    SetLength(lSoundSample16.ChannelValues, fmt.Channels);
    for i := 0 to fmt.Channels - 1 do
    begin
      AStream.Read(lWordData, 2);
      lSoundSample16.ChannelValues[i] := lWordData;
    end;

    ADest.AddSoundElement(lSoundSample16);
  end
  else
    raise Exception.Create(Format('[TWaveReader.ReadSample] Invalid number of bits per sample: %d', [fmt.BitsPerSample]));
end;

{function TWaveReader.ReadBuf(var Buffer;BufferSize:Integer):Integer;
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
end;}

initialization
  RegisterSoundReader(TWaveReader.Create, sfWav);
end.

