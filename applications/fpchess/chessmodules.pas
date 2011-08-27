unit chessmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Controls;

type
  TChessModuleKind = (cmkSinglePlayer, cmkInternet, cmkAI);

  TChessModule = class
  public
    Kind: TChessModuleKind;
    Description: string;
    procedure CreateUserInterface(); virtual; abstract;
    procedure ShowUserInterface(AParent: TWinControl); virtual; abstract;
    procedure HideUserInterface(); virtual; abstract;
    procedure FreeUserInterface(); virtual; abstract;
  end;

var
  gSelectedModuleIndex: Integer;

procedure RegisterChessModule(AModule: TChessModule);
procedure PopulateChessModulesList(AList: TStrings);
function GetChessModule(AIndex: Integer): TChessModule;
function GetChessModuleCount(): Integer;

implementation

var
  gChessModules: TList;

procedure RegisterChessModule(AModule: TChessModule);
begin
  if AModule = nil then raise Exception.Create('[RegisterChessModule] Attempted to register a nil module');
  gChessModules.Add(AModule);
  AModule.CreateUserInterface();
end;

procedure PopulateChessModulesList(AList: TStrings);
var
  i: Integer;
  lModule: TChessModule;
begin
  AList.Clear;
  for i := 0 to gChessModules.Count - 1 do
  begin
    lModule := TChessModule(gChessModules.Items[i]);
    if lModule <> nil then
      AList.Add(lModule.Description);
  end;
end;

function GetChessModule(AIndex: Integer): TChessModule;
begin
  if AIndex < 0 then Exit(nil);
  Result := TChessModule(gChessModules.Items[AIndex]);
end;

function GetChessModuleCount: Integer;
begin
  Result := gChessModules.Count;
end;

initialization
  gChessModules := TList.Create;
  gSelectedModuleIndex := -1;
finalization
  gChessModules.Free;
end.

