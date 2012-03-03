{
A generic library for playing sound with modular backends

In the future it might be extended to be able to modify and save sound files in
multiple formats too.

Copyright: Felipe Monteiro de Carvalho 2010-2011
}
unit fpsound;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  TSoundDocument = class;

  TSoundFormat = (sfWav, sfMP3, sfOGG, sfMID, sfAMR, sf3GP, sfMP4);

  { TSoundReader }

  TSoundReader = class
  public
    constructor Create; virtual;
    procedure ReadFromStream(AStream: TStream; ADest: TSoundDocument); virtual; abstract;
  end;

  TSoundPlayerKind = (spNone, spOpenAL, spMPlayer, spFMod, spExtra1, spExtra2);

  { TSoundPlayer }

  TSoundPlayer = class
  protected
    FInitialized: Boolean;
  public
    constructor Create; virtual;
    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;
    procedure Play(ASound: TSoundDocument); virtual; abstract;
    procedure Pause(ASound: TSoundDocument); virtual; abstract;
    procedure Stop(ASound: TSoundDocument); virtual; abstract;
  end;

  // Sound data representation

  TSoundElement = class
  end;

  // A Key element sets the basic information of the music for the following samples,
  // such as sample rate. It has no sample data in itself
  TSoundKeyElement = class(TSoundElement)
  public
    SampleRate: Cardinal; // example values: 8000, 44100, etc.
    BitsPerSample: Byte; // Tipical values: 8 and 16
    Channels: Byte; // Number of channels
  end;

  TSoundSample8 = class(TSoundElement)
  public
    ChannelValues: array of Byte;
  end;

  TSoundSample16 = class(TSoundElement)
  public
    ChannelValues: array of SmallInt;
  end;

  { TSoundThread }

  TSoundThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  { TSoundDocument }

  TSoundDocument = class
  private
    FPlayer: TSoundPlayer;
    FPlayerKind: TSoundPlayerKind;
    FCurElementIndex: Integer;
    FSoundData: TFPList; // of TSoundElement
  public
    SoundDocStream: TMemoryStream;
    constructor Create; virtual;
    destructor Destroy; override;
    // Document read/save methods
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromFile(AFileName: string; AFormat: TSoundFormat);
    class function GuessFormatFromSoundFile(AFileName: string): TSoundFormat;
    // Document edition methods
    procedure Clear;
    procedure AddSoundElement(const AElement: TSoundElement);
    function GetSoundElement(const AIndex: Integer): TSoundElement;
    function GetSoundElementCount: Integer;
    function GetFirstSoundElement: TSoundKeyElement;
    function GetNextSoundElement: TSoundElement;
    // Document playing methods
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Seek(ANewPos: Double);
    procedure SetSoundPlayer(AKind: TSoundPlayerKind);
  end;

var
  SoundPlayer: TSoundDocument;

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TSoundPlayerKind);
procedure RegisterSoundReader(AReader: TSoundReader; AFormat: TSoundFormat);
function GetSoundPlayer(AKind: TSoundPlayerKind): TSoundPlayer;
function GetSoundReader(AFormat: TSoundFormat): TSoundReader;

implementation

var
  GSoundPlayers: array[TSoundPlayerKind] of TSoundPlayer = (nil, nil, nil, nil, nil, nil);
  GSoundReaders: array[TSoundFormat] of TSoundReader = (nil, nil, nil, nil, nil, nil, nil);
//  GSoundWriter: array[TSoundFormat] of TSoundWriter = (nil, nil, nil, nil, nil, nil, nil);

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TSoundPlayerKind);
begin
  GSoundPlayers[AKind] := APlayer;
end;

procedure RegisterSoundReader(AReader: TSoundReader; AFormat: TSoundFormat);
begin
  GSoundReaders[AFormat] := AReader;
end;

function GetSoundPlayer(AKind: TSoundPlayerKind): TSoundPlayer;
begin
  Result := GSoundPlayers[AKind];
end;

function GetSoundReader(AFormat: TSoundFormat): TSoundReader;
begin
  Result := GSoundReaders[AFormat];
end;

{ TSoundThread }

procedure TSoundThread.Execute;
begin

end;

{ TSoundPlayer }

constructor TSoundPlayer.Create;
begin
  inherited Create;
end;

{ TSoundReader }

constructor TSoundReader.Create;
begin
  inherited Create;
end;

{ TSoundDocument }

constructor TSoundDocument.Create;
begin
  inherited Create;

  FSoundData := TFPList.Create;
  SoundDocStream := TMemoryStream.Create;
end;

destructor TSoundDocument.Destroy;
begin
  FSoundData.Free;
  SoundDocStream.Free;
  if FPlayer <> nil then FPlayer.Finalize;
  inherited Destroy;
end;

procedure TSoundDocument.LoadFromFile(AFileName: string);
var
  lFormat: TSoundFormat;
begin
  lFormat := GuessFormatFromSoundFile(AFileName);
  LoadFromFile(AFileName, lFormat);
end;

procedure TSoundDocument.LoadFromFile(AFileName: string; AFormat: TSoundFormat);
var
  lReader: TSoundReader;
  lStream: TFileStream;
begin
  lReader := GetSoundReader(AFormat);
  lStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Clear();
    lReader.ReadFromStream(lStream, Self);
    lStream.Position := 0;
    SoundDocStream.Clear;
    SoundDocStream.LoadFromStream(lStream);
  finally
    lStream.Free;
  end;
end;

class function TSoundDocument.GuessFormatFromSoundFile(AFileName: string): TSoundFormat;
var
  lExt: String;
begin
  Result := sfWav;
  lExt := ExtractFileExt(AFileName);
  if CompareText(lExt, 'wav') = 0 then Result := sfWav;
  //raise Exception.Create(Format('[TSoundDocument.LoadFromFile] Unknown extension: %s', [lExt]));
end;

procedure TSoundDocument.Clear;
var
  i: Integer;
begin
  for i := 0 to FSoundData.Count - 1 do
    TSoundElement(FSoundData.Items[i]).Free;

  FSoundData.Clear;
end;

procedure TSoundDocument.AddSoundElement(const AElement: TSoundElement);
begin
  FSoundData.Add(AElement);
end;

function TSoundDocument.GetSoundElement(const AIndex: Integer): TSoundElement;
begin
  Result := TSoundElement(FSoundData.Items[AIndex]);
end;

function TSoundDocument.GetSoundElementCount: Integer;
begin
  Result := FSoundData.Count;
end;

function TSoundDocument.GetFirstSoundElement: TSoundKeyElement;
begin
  if GetSoundElementCount() = 0 then
    Result := nil
  else
    Result := GetSoundElement(0) as TSoundKeyElement;
  FCurElementIndex := 1;
end;

function TSoundDocument.GetNextSoundElement: TSoundElement;
begin
  if GetSoundElementCount() >= FCurElementIndex then Exit(nil);
  Result := GetSoundElement(FCurElementIndex);
  Inc(FCurElementIndex);
end;

procedure TSoundDocument.Play;
begin
  if FPlayer = nil then Exit;
  FPlayer.Play(Self);
end;

procedure TSoundDocument.Pause;
begin
  if FPlayer = nil then Exit;
  FPlayer.Pause(Self);
end;

procedure TSoundDocument.Stop;
begin
  if FPlayer = nil then Exit;
  FPlayer.Stop(Self);
  FPlayer.Finalize;
end;

procedure TSoundDocument.Seek(ANewPos: Double);
begin
  if FPlayer = nil then Exit;

end;

procedure TSoundDocument.SetSoundPlayer(AKind: TSoundPlayerKind);
begin
  if AKind = FPlayerKind then Exit;
  FPlayerKind := AKind;
  Stop;
  FPlayer := GSoundPlayers[AKind];
end;

var
  lReaderIndex: TSoundFormat;
  lPlayerIndex: TSoundPlayerKind;

initialization
  SoundPlayer := TSoundDocument.Create;

finalization
  SoundPlayer.Free;

  for lReaderIndex := Low(TSoundFormat) to High(TSoundFormat) do
    if GSoundReaders[lReaderIndex] <> nil then GSoundReaders[lReaderIndex].Free;

  for lPlayerIndex := Low(TSoundPlayerKind) to High(TSoundPlayerKind) do
    if GSoundPlayers[lPlayerIndex] <> nil then GSoundPlayers[lPlayerIndex].Free;
end.

