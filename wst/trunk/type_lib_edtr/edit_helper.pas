unit edit_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pastree, pascal_parser_intf;

type

  TEditType = ( etCreate, etUpdate, etDelete );
  
  { TObjectUpdater }

  TObjectUpdater = class
  public
    class function CanHandle(AObject : TObject):Boolean;virtual;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;virtual;abstract;
  end;
  TObjectUpdaterClass = class of TObjectUpdater;
  
  function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
  function CreateCompoundObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  function CreateInterface(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  
  function HasEditor(AObject : TPasElement):Boolean;
  function UpdateObject(
    AObject : TPasElement;
    ASymbolTable : TwstPasTreeContainer
  ):Boolean;
  
  procedure FillList(ALs : TStrings;AContainer : TwstPasTreeContainer);
  
implementation
uses Contnrs, Forms, ufEnumedit, ufclassedit, uinterfaceedit;

type

  { TUpdaterRegistry }

  TUpdaterRegistry = class
  private
    FList : TClassList;
  private
    function FindHanlderIndex(AObj : TObject):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterHandler(AHandlerClass : TObjectUpdaterClass);
    function FindHandler(AObj : TObject; out AHandler : TObjectUpdaterClass) : Boolean;
  end;

var UpdaterRegistryInst : TUpdaterRegistry;

function CreateInterface(ASymbolTable: TwstPasTreeContainer): TPasClassType;
var
  f : TfInterfaceEdit;
begin
  Result := nil;
  f := TfInterfaceEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function HasEditor(AObject: TPasElement): Boolean;
var
  h : TObjectUpdaterClass;
begin
  Result := UpdaterRegistryInst.FindHandler(AObject,h);
end;

function UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
):Boolean;
var
  h : TObjectUpdaterClass;
begin
  if not UpdaterRegistryInst.FindHandler(AObject,h) then begin
    raise Exception.Create('No handler found.');
  end;
  Result := h.UpdateObject(AObject,ASymbolTable);
end;

type
  { TEnumUpdater }

  TEnumUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TClassUpdater }

  TClassUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TInterfaceUpdater }

  TInterfaceUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

{ TInterfaceUpdater }

class function TInterfaceUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okInterface ) );
end;

class function TInterfaceUpdater.UpdateObject(AObject: TPasElement; ASymbolTable: TwstPasTreeContainer): Boolean;
var
  f : TfInterfaceEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfInterfaceEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TClassUpdater }

class function TClassUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okClass ) );
end;

class function TClassUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfClassEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfClassEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TUpdaterRegistry }

function TUpdaterRegistry.FindHanlderIndex(AObj : TObject): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(FList.Count) do begin
    if TObjectUpdaterClass(FList[i]).CanHandle(AObj) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TUpdaterRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TUpdaterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TUpdaterRegistry.RegisterHandler(AHandlerClass : TObjectUpdaterClass);
begin
  if ( FList.IndexOf(AHandlerClass) < 0 ) then begin
    FList.Add(AHandlerClass);
  end;
end;

function TUpdaterRegistry.FindHandler(
      AObj      : TObject;
  out AHandler  : TObjectUpdaterClass
): Boolean;
var
  i : Integer;
begin
  AHandler := nil;
  i := FindHanlderIndex(AObj);
  Result := ( i >= 0 );
  if Result then begin
    AHandler := TObjectUpdaterClass(FList[i]);
  end;
end;

{ TEnumUpdater }

class function TEnumUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and AObject.InheritsFrom(TPasEnumType);
end;

class function TEnumUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfEnumEdit;
  e : TPasEnumType;
begin
  e := AObject as TPasEnumType;
  f := TfEnumEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
var
  f : TfEnumEdit;
begin
  Result := nil;
  f := TfEnumEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,AContainer);
  finally
    f.Release();
  end;
end;

function CreateCompoundObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
var
  f : TfClassEdit;
begin
  Result := nil;
  f := TfClassEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;


{ TObjectUpdater }

class function TObjectUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := Assigned(AObject);
end;

procedure InternalFillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  i : Integer;
  sym : TPasElement;
  decList : TList;
begin
  decList := AContainer.CurrentModule.InterfaceSection.Declarations;
  for i := 0 to Pred(decList.Count) do begin
    sym := TPasElement(decList[i]);
    if sym.InheritsFrom(TPasType) and
       ( sym.InheritsFrom(TPasClassType) or
         sym.InheritsFrom(TPasNativeSimpleType) or
         ( sym.InheritsFrom(TPasAliasType) and
           Assigned(TPasAliasType(sym).DestType) and
           ( TPasAliasType(sym).DestType.InheritsFrom(TPasClassType) or
             TPasAliasType(sym).DestType.InheritsFrom(TPasNativeSimpleType)
           )
         )
       )
    then begin
      if ( ALs.IndexOfObject(sym) = -1 ) then begin
        ALs.AddObject(AContainer.GetExternalName(sym),sym);
      end;
    end;
  end;
end;

procedure FillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillList(locLST,AContainer);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

initialization
  UpdaterRegistryInst := TUpdaterRegistry.Create();
  UpdaterRegistryInst.RegisterHandler(TEnumUpdater);
  UpdaterRegistryInst.RegisterHandler(TClassUpdater);
  UpdaterRegistryInst.RegisterHandler(TInterfaceUpdater);
  
finalization
  FreeAndNil(UpdaterRegistryInst);
  
end.
