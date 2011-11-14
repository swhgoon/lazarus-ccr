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
  TSoundPlayerKind = (spOpenAL, spMPlayer, spFMod, spExtra1);

  TSoundPlayer = class
  public
    procedure Play;
  end;

  { TSoundDocument }

  TSoundDocument = class
  private
    AStream: TStream;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure Play;
    procedure SetSoundPlayer(AKind: TSoundPlayerKind);
  end;

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TFPSoundPlayerKind);

implementation

var
  GSoundPlayers: array[TSoundPlayerKind] of TSoundPlayer = (nil, nil, nil, nil);

procedure RegisterSoundPlayer(APlayer: TSoundPlayer; AKind: TFPSoundPlayerKind);
begin
  GSoundPlayers[AKind] := APlayer;
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
begin

end;

procedure TSoundDocument.Play;
begin

end;

procedure TSoundDocument.SetSoundPlayer(AKind: TFPSoundPlayerKind);
begin

end;

end.

