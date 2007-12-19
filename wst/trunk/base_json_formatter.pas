{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit base_json_formatter;

interface
uses
  Classes, SysUtils, TypInfo, Contnrs,
  base_service_intf,
  fpjson;

type

  TJsonInteger = Int64;

  EJsonRpcException = class(EBaseRemoteException)
  end;

  { TStackItem }

  TStackItem = class
  private
    FScopeObject: TJSONData;
    FScopeType: TScopeType;
  protected
    function GetItemsCount() : Integer;virtual;
  public
    constructor Create(AScopeObject : TJSONData;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TJSONData;virtual;abstract;
    function CreateStringBuffer(
      Const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;virtual;abstract;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;virtual;abstract;
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;virtual;abstract;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;virtual;abstract;
    function CreateObjectBuffer(const AName : string) : TJSONObject;virtual;abstract;
    function CreateArrayBuffer(const AName : string) : TJSONArray;virtual;abstract;
    property ScopeObject : TJSONData Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property ItemsCount : Integer read GetItemsCount;
  end;
  
  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  protected
    function GetDataObject() : TJSONObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(AScopeObject : TJSONObject);
    function FindNode(var ANodeName : string):TJSONData;override;
    function CreateStringBuffer(
      const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;override;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;override;
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;override;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;override;
    function CreateObjectBuffer(const AName : string) : TJSONObject;override;
    function CreateArrayBuffer(const AName : string) : TJSONArray;override;
  end;
  
  { TJsonRpcBaseFormatter }

  TJsonRpcBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FSerializationStyle : TSerializationStyle;
    FStack : TObjectStack;
  private
    function GetCurrentScope : String;
    function HasScope():Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckScope();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ClearStack();
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    procedure PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    function GetFormatName() : string;
    procedure Clear();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : PTypeInfo
    );
    procedure BeginArray(
      const AName         : string;
      const ATypeInfo     : PTypeInfo;
      const AItemTypeInfo : PTypeInfo;
      const ABounds       : Array Of Integer;
      const AStyle        : TArrayStyle
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    procedure AddScopeAttribute(Const AName,AValue : string);
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AStyle     : TArrayStyle;
      const AItemName  : string
    ):Integer;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;
    procedure EndScopeRead();
    property CurrentScope : String Read GetCurrentScope;

    procedure BeginHeader();
    procedure EndHeader();

    procedure Put(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData
    );
    procedure PutScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      const AData
    );
    procedure Get(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData
    );
    procedure GetScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      var   AData
    );
    function ReadBuffer(const AName : string) : string;
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  public
    constructor Create();override;
    destructor Destroy();override;
  end;
  
  
implementation
uses jsonparser;


{ TJsonRpcBaseFormatter }

function TJsonRpcBaseFormatter.HasScope() : Boolean;
begin
  Result := FStack.AtLeast(1);
end;

procedure TJsonRpcBaseFormatter.CheckScope();
begin
  if not HasScope() then
    Error('There is no scope.');
end;

procedure TJsonRpcBaseFormatter.ClearStack();
var
  i, c : Integer;
begin
  c := FStack.Count;
  for I := 1 to c do
    FStack.Pop().Free();
end;

function TJsonRpcBaseFormatter.StackTop() : TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TJsonRpcBaseFormatter.PopStack() : TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

procedure TJsonRpcBaseFormatter.PutInt64(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : Int64
);
begin
  StackTop().CreateIntBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TJsonRpcBaseFormatter.GetSerializationStyle() : TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TJsonRpcBaseFormatter.GetFormatName(): string;
begin
  Result := 'json';
end;

function TJsonRpcBaseFormatter.GetCurrentScope : string;
begin
  CheckScope();
  Result := '';
end;

procedure TJsonRpcBaseFormatter.Clear();
begin
  ClearStack();
end;

procedure TJsonRpcBaseFormatter.BeginObject(
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  elt : TJSONObject;
begin
  if HasScope() then
    elt := StackTop().CreateObjectBuffer(AName)
  else
    elt := TJSONObject.Create();
  FStack.Push(TObjectStackItem.Create(elt));
end;

procedure TJsonRpcBaseFormatter.BeginArray(const AName : string;
  const ATypeInfo : PTypeInfo; const AItemTypeInfo : PTypeInfo;
  const ABounds : array of Integer; const AStyle : TArrayStyle);
begin

end;

procedure TJsonRpcBaseFormatter.NilCurrentScope();
begin

end;

function TJsonRpcBaseFormatter.IsCurrentScopeNil() : Boolean;
begin

end;

procedure TJsonRpcBaseFormatter.EndScope();
begin

end;

procedure TJsonRpcBaseFormatter.AddScopeAttribute(const AName, AValue : string);
begin
  Put(AName,TypeInfo(string),AValue);
end;

function TJsonRpcBaseFormatter.BeginObjectRead(var AScopeName : string;
  const ATypeInfo : PTypeInfo) : Integer;
begin

end;

function TJsonRpcBaseFormatter.BeginArrayRead(var AScopeName : string;
  const ATypeInfo : PTypeInfo; const AStyle : TArrayStyle;
  const AItemName : string) : Integer;
begin

end;

function TJsonRpcBaseFormatter.GetScopeItemNames(const AReturnList : TStrings
  ) : Integer;
begin

end;

procedure TJsonRpcBaseFormatter.EndScopeRead();
begin

end;

procedure TJsonRpcBaseFormatter.BeginHeader();
begin

end;

procedure TJsonRpcBaseFormatter.EndHeader();
begin

end;

procedure TJsonRpcBaseFormatter.Put(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData
);
begin

end;

procedure TJsonRpcBaseFormatter.PutScopeInnerValue(const ATypeInfo : PTypeInfo;
  const AData);
begin

end;

procedure TJsonRpcBaseFormatter.Get(const ATypeInfo : PTypeInfo;
  var AName : String; var AData);
begin

end;

procedure TJsonRpcBaseFormatter.GetScopeInnerValue(const ATypeInfo : PTypeInfo;
  var AData);
begin

end;

function TJsonRpcBaseFormatter.ReadBuffer(const AName : string) : string;
begin

end;

procedure TJsonRpcBaseFormatter.WriteBuffer(const AValue: string);
begin

end;

procedure TJsonRpcBaseFormatter.SaveToStream(AStream : TStream);
var
  locBuffer : string;
begin
  CheckScope();
  locBuffer := StackTop().ScopeObject.AsJSON;
  AStream.WriteBuffer(locBuffer[1],Length(locBuffer));
end;

procedure TJsonRpcBaseFormatter.LoadFromStream(AStream : TStream);
var
  locParser : TJSONParser;
begin
  ClearStack();
  FSerializationStyle := Low(TSerializationStyle);
  locParser := TJSONParser.Create(AStream);
  try
    //f locParser.Parse;
  finally
    locParser.Free();
  end;
end;

procedure TJsonRpcBaseFormatter.Error(const AMsg : string);
begin
  raise EJsonRpcException.Create(AMsg);
end;

procedure TJsonRpcBaseFormatter.Error(const AMsg : string; const AArgs : array of const);
begin
  raise EJsonRpcException.CreateFmt(AMsg,AArgs);
end;

constructor TJsonRpcBaseFormatter.Create();
begin
  inherited Create();
  FStack := TObjectStack.Create();
end;

destructor TJsonRpcBaseFormatter.Destroy();
begin
  FStack.Free();
  inherited Destroy();
end;

{ TStackItem }

function TStackItem.GetItemsCount() : Integer;
begin
  Result := FScopeObject.Count;
end;

constructor TStackItem.Create(AScopeObject : TJSONData; AScopeType : TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

{ TObjectStackItem }

function TObjectStackItem.GetDataObject() : TJSONObject;
begin
  Result := TJSONObject(ScopeObject);
end;

constructor TObjectStackItem.Create(AScopeObject : TJSONObject);
begin
  inherited Create(AScopeObject,stObject);
end;

function TObjectStackItem.FindNode(var ANodeName : string) : TJSONData;
begin
  Result := GetDataObject().Elements[ANodeName];
end;

function TObjectStackItem.CreateStringBuffer(
  const AName  : string;
  const AValue : TJSONStringType
) : TJSONData;
var
  locObj : TJSONObject;
  i : PtrInt;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName];
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsString := AValue;
  end;
end;

function TObjectStackItem.CreateIntBuffer(
  const AName : string;
  const AValue : TJsonInteger
) : TJSONData;
var
  locObj : TJSONObject;
  i : PtrInt;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName];
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsInteger := AValue;
  end;
end;

function TObjectStackItem.CreateFloatBuffer(
  const AName : string;
  const AValue : TJSONFloat
) : TJSONData;
var
  locObj : TJSONObject;
  i : PtrInt;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName];
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsFloat := AValue;
  end;
end;

function TObjectStackItem.CreateBoolBuffer(
  const AName : string;
  const AValue : Boolean
) : TJSONData;
var
  locObj : TJSONObject;
  i : PtrInt;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName];
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsBoolean := AValue;
  end;
end;

function TObjectStackItem.CreateObjectBuffer(const AName : string) : TJSONObject;
var
  locObj : TJSONObject;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName] as TJSONObject;
  if ( Result = nil ) then begin
    Result := TJSONObject.Create();
    locObj.Add(AName,Result);
  end;
end;

function TObjectStackItem.CreateArrayBuffer(const AName : string) : TJSONArray;
var
  locObj : TJSONObject;
begin
  locObj := GetDataObject();
  Result := locObj.Elements[AName] as TJSONArray;
  if ( Result = nil ) then begin
    Result := TJSONArray.Create();
    locObj.Add(AName,Result);
  end;
end;

end.

