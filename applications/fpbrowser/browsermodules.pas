unit browsermodules;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  TBrowserModuleUIElement = (bmueEnabledDisableMenu, bmueCommandsSubmenu);
  TBrowserModuleUIElements = set of TBrowserModuleUIElement;

  { TBrowserModule }

  TBrowserModule = class
  public
    ShortDescription: string;
    Activated: Boolean;
    constructor Create; virtual;
    //
    function GetModuleUIElements(): TBrowserModuleUIElements; virtual;
    // For active/disabled modules
    function HandleOnPageLoad(AInput: string; out AOutput: string): Boolean; virtual;
    // For expansions
    function GetCommandCount: Integer; virtual;
    function GetCommandName(AID: Integer): string; virtual;
    procedure ExecuteCommand(AID: Integer); virtual;
  end;

procedure RegisterBrowserModule(AModule: TBrowserModule);
function GetBrowserModule(AIndex: Integer): TBrowserModule;
function GetBrowserModuleCount(): Integer;

implementation

var
  gBrowserModules: TList;

procedure RegisterBrowserModule(AModule: TBrowserModule);
begin
  if AModule = nil then raise Exception.Create('[RegisterBrowserModule] Attempted to register a nil Module');
  gBrowserModules.Add(AModule);
end;

function GetBrowserModule(AIndex: Integer): TBrowserModule;
begin
  if AIndex < 0 then Exit(nil);
  Result := TBrowserModule(gBrowserModules.Items[AIndex]);
end;

function GetBrowserModuleCount: Integer;
begin
  Result := gBrowserModules.Count;
end;

{ TBrowserModule }

constructor TBrowserModule.Create;
begin

end;

function TBrowserModule.GetModuleUIElements: TBrowserModuleUIElements;
begin
  Result := [bmueEnabledDisableMenu];
end;

function TBrowserModule.HandleOnPageLoad(AInput: string; out AOutput: string): Boolean;
begin
  AOutput := '';
  Result := False;
end;

function TBrowserModule.GetCommandCount: Integer;
begin
  Result := 0;
end;

function TBrowserModule.GetCommandName(AID: Integer): string;
begin
  Result := '';
end;

procedure TBrowserModule.ExecuteCommand(AID: Integer);
begin

end;

initialization
  gBrowserModules := TList.Create;
finalization
  gBrowserModules.Free;
end.

