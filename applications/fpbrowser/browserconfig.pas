unit browserconfig;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TFPBrowserConfig }

  TFPBrowserConfig = class
  public
    UserAgent: string;
    constructor Create; virtual;
  end;

var
  FPBrowserConfig: TFPBrowserConfig;

implementation

{ TFPBrowserConfig }

constructor TFPBrowserConfig.Create;
begin
  inherited Create;
  UserAgent := 'FPBrowser/1.0 (Mobile; U; en-GB)';
end;

initialization
  FPBrowserConfig := TFPBrowserConfig.Create;

finalization
  FPBrowserConfig.Free;
end.

