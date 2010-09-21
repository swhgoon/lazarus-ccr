unit chessconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TChessConfig }

  TChessConfig = class
  public
    function GetResourcesDir: string;
    function GetCurrentSkinDir: string;
  end;

var
  vChessConfig: TChessConfig;

implementation

{ TChessConfig }

function TChessConfig.GetResourcesDir: string;
begin

end;

function TChessConfig.GetCurrentSkinDir: string;
begin
  Result := GetResourcesDir() + 'skins' + PathDelim + 'classic' + PathDelim;
end;

initialization

vChessConfig := TChessConfig.Create;

finalization

vChessConfig.Free;

end.

