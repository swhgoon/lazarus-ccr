unit browsermodules;

{$mode delphi}

interface

uses
  Classes, SysUtils; 

type

  { TBrowserModule }

  TBrowserModule = class
  public
    ShortDescription: string;
    Activated: Boolean;
    constructor Create; virtual;
    function HandleOnPageLoad(AInput: string; out AOutput: string): Boolean; virtual;
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

function TBrowserModule.HandleOnPageLoad(AInput: string; out AOutput: string): Boolean;
begin
  AOutput := '';
  Result := False;
end;

initialization
  gBrowserModules := TList.Create;
finalization
  gBrowserModules.Free;
end.

