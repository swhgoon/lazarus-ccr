unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  { TConfig }

  TConfig = class
    private
      ExePath:string; //слэш на конце
    public
      constructor Create;
      destructor Destroy;override;

      //полный абсолютный путь к базе данных
      function BaseFile:string;
      function DebugLogFile:string;
      function DebugLogDirectory:string;
  end;

var GlobalConfig:TConfig;
implementation
uses Forms;

{ TConfig }

constructor TConfig.Create;
begin
  ExePath:=ExtractFilePath(Application.ExeName);
end;

destructor TConfig.Destroy;
begin
  inherited Destroy;
end;

function TConfig.BaseFile: string;
begin
  Result:=ExePath + 'base/germesorders.db3';
end;

function TConfig.DebugLogFile: string;
begin
  Result:=ExePath + 'debug.log';
end;

function TConfig.DebugLogDirectory: string;
begin
  Result:=ExePath + 'logs';
end;

initialization
  GlobalConfig:=TConfig.Create;
finalization
  FreeAndNil(GlobalConfig);
end.

