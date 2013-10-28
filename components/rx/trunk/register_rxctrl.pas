unit register_rxctrl;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;
implementation
uses RxLogin, ComponentEditors, RxAppIcon, Dialogs, rxconst;

resourcestring
  sTestTRxLoginDialog = 'Test TRxLoginDialog';
  sLoadIcon        = 'Load icon';

type

  { TRxLoginDialogEditor }

  TRxLoginDialogEditor = class(TComponentEditor)
  public
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;


  { TRxAppIcon }

  TRxAppIconEditor = class(TComponentEditor)
  public
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;

{ TRxLoginDialogEditor }

constructor TRxLoginDialogEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TComponent;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TRxLoginDialogEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TRxLoginDialogEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

function TRxLoginDialogEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sTestTRxLoginDialog;
    end;
  end;
end;

procedure TRxLoginDialogEditor.ExecuteVerb(Index: integer);
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:(Component as TRxLoginDialog).Login;
    end;
  end;
end;

{ TRxAppIcon }

type
  PClass = ^TClass;

constructor TRxAppIconEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TComponent;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TRxAppIconEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TRxAppIconEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

function TRxAppIconEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sLoadIcon;
    end;
  end;
end;

procedure TRxAppIconEditor.ExecuteVerb(Index: integer);
var
  OpenDialog1: TOpenDialog;
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:begin
          OpenDialog1:=TOpenDialog.Create(nil);
          OpenDialog1.Filter:=sWindowsIcoFiles;
          try
            if OpenDialog1.Execute then
              (Component as TRxAppIcon).LoadFromFile(OpenDialog1.FileName);
          finally
            OpenDialog1.Free;
          end;
          Modified;
        end;
    end;
  end;
end;


procedure Register;
begin
  //
  RegisterComponentEditor(TRxLoginDialog, TRxLoginDialogEditor);
  RegisterComponentEditor(TRxAppIcon, TRxAppIconEditor);
end;

end.

