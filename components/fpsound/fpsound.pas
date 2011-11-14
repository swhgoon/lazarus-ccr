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

  TSoundFormat = (sfWav, sfMP3);

  TSoundReader = class
  public
    procedure ReadFromStream(AStream: TStream; ADest: TSoundDocument); virtual; abstract;
  end;

  TSoundPlayerKind = (spOpenAL, spMPlayer, spFMod, spExtra1);

  TSoundPlayer = class
  public
    procedure Play(ASound: TSoundDocument); virtual; abstract;
  end;

  { TSoundDocument }

  TSoundDocument = class
  private
    AStream: TStream;
    FPlayer: TSoundPlayer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromFile(AFileName: string; AFormat: TSoundFormat);
    procedure Play;
    procedure Pause;
    procedure Seek(ANewPos: Double);
    procedure SetSoundPlayer(AKind: TSoundPlayerKind);
  end;

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TSoundPlayerKind);
procedure RegisterSoundReader(AReader: TSoundReader; AFormat: TSoundFormat);
function GetSoundPlayer(AKind: TSoundPlayerKind): TSoundPlayer;
function GetSoundReader(AFormat: TSoundFormat): TSoundReader;

implementation

var
  GSoundPlayers: array[TSoundPlayerKind] of TSoundPlayer = (nil, nil, nil, nil);
  GSoundReader: array[TSoundFormat] of TSoundReader = (nil, nil);
//  GSoundWriter: array[TSoundFormat] of TSoundWriter = (nil, nil);

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TSoundPlayerKind);
begin
  GSoundPlayers[AKind] := APlayer;
end;

procedure RegisterSoundReader(AReader: TSoundReader; AFormat: TSoundFormat);
begin
  GSoundReader[AFormat] := AReader;
end;

function GetSoundPlayer(AKind: TSoundPlayerKind): TSoundPlayer;
begin

end;

function GetSoundReader(AFormat: TSoundFormat): TSoundReader;
begin

end;

{ TSoundDocument }

constructor TSoundDocument.Create;
begin

end;

destructor TSoundDocument.Destroy;
begin
  inherited Destroy;
end;

procedure TSoundDocument.LoadFromFile(AFileName: string);
var
  lExt: String;
begin
  lExt := ExtractFileExt(AFileName);
  if CompareText(lExt, 'wav') = 0 then LoadFromFile(AFileName, sfWav)
  else
    raise Exception.Create(Format('[TSoundDocument.LoadFromFile] Unknown extension: %s', [lExt]));
end;

procedure TSoundDocument.LoadFromFile(AFileName: string; AFormat: TSoundFormat);
begin

end;

procedure TSoundDocument.Play;
begin

end;

procedure TSoundDocument.Pause;
begin

end;

procedure TSoundDocument.Seek(ANewPos: Double);
begin

end;

procedure TSoundDocument.SetSoundPlayer(AKind: TSoundPlayerKind);
begin

end;

end.

