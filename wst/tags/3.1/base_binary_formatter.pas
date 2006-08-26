{   This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit base_binary_formatter;

{$mode objfpc}{$H+}
{$DEFINE wst_binary_header}

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  base_service_intf, binary_streamer;

const
  sROOT   = 'ROOT';
{$IFDEF wst_binary_header}
  sHEADER = 'HEADER';
{$ENDIF}

type
  EBinaryFormatterException = class(EServiceException)
  End;

  TDataName = AnsiString;
  TDataType = (
    dtInt8U,    dtInt8S,
    dtInt16U,   dtInt16S,
    dtInt32U,   dtInt32S,
    dtInt64U,   dtInt64S,
    dtBool, dtEnum,
    dtSingle, dtDouble, dtExtended, dtCurrency,
    dtString,
    dtObject, dtArray
  );
  
  PStringBuffer = ^TStringBuffer;
  PObjectBuffer = ^TObjectBuffer;
  PArrayBuffer = ^TArrayBuffer;
  PDataBuffer = ^TDataBuffer;

  TDataBuffer = Record
    Name : TDataName;
    Case DataType : TDataType of
      dtInt8S    : ( Int8S : TInt8S );
      dtInt8U    : ( Int8U : TInt8U );
      dtInt16U   : ( Int16U : TInt16U );
      dtInt16S   : ( Int16S : TInt16S );
      dtInt32U   : ( Int32U : TInt32U );
      dtInt32S   : ( Int32S : TInt32S );
      dtInt64U   : ( Int64U : TInt64U );
      dtInt64S   : ( Int64S : TInt64S );
      dtBool     : ( BoolData : TBoolData );
      dtEnum     : ( EnumData : TEnumData );
      dtSingle   : ( SingleData : TFloat_Single_4 );
      dtDouble   : ( DoubleData : TFloat_Double_8 );
      dtExtended   : ( ExtendedData : TFloat_Extended_10 );
      dtCurrency   : ( CurrencyData : TFloat_Currency_8 );
      
      dtString   : ( StrData : PStringBuffer );
      dtObject   : ( ObjectData : PObjectBuffer );
      dtArray    : ( ArrayData : PArrayBuffer );
  End;

  TStringBuffer = Record
    Data : String;
  End;

  PObjectBufferItem = ^TObjectBufferItem;
  TObjectBufferItem = Record
    Data : PDataBuffer;
    Next : PObjectBufferItem;
  End;

  TObjectBuffer = Record
    NilObject   : TBoolData;
    Count       : Integer;
    Head        : PObjectBufferItem;
    Last        : PObjectBufferItem;
    Attributes  : PObjectBuffer;
  End;

  PDataBufferList = ^TDataBufferList;
  TDataBufferList = array[0..MAX_ARRAY_LENGTH] of PDataBuffer;
  TArrayBuffer = Record
    Count : Integer;
    Items : PDataBufferList;
    Attributes  : PObjectBuffer;
  End;

  { TStackItem }

  TStackItem = class
  private
    FScopeObject: PDataBuffer;
    FScopeType: TScopeType;
  Public
    constructor Create(const AScopeObject : PDataBuffer;AScopeType : TScopeType);
    function GetItemCount():Integer;virtual;abstract;
    function Find(var AName : TDataName):PDataBuffer;virtual;abstract;
    function GetByIndex(const AIndex : Integer):PDataBuffer;virtual;abstract;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;virtual;abstract;
    procedure NilCurrentScope();virtual;abstract;
    function IsCurrentScopeNil():Boolean;virtual;abstract;
    property ScopeObject : PDataBuffer Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
  End;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    constructor Create(const AScopeObject : PDataBuffer);
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
  End;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  Private
    FIndex : Integer;
  Public
    constructor Create(const AScopeObject : PDataBuffer);
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
  End;

  { TBaseBinaryFormatter }

  TBaseBinaryFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FRootData : PDataBuffer;
    FStack : TObjectStack;
    FSerializationStyle : TSerializationStyle;
    {$IFDEF wst_binary_header}
    FHeaderEnterCount : Integer;
    {$ENDIF}
  private
    function GetCurrentScopeObject():PDataBuffer;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
  protected
    function HasScope():Boolean;
    procedure CheckScope();
    procedure ClearStack();
    procedure PushStack(AScopeObject : PDataBuffer;Const AScopeType : TScopeType = stObject);
    function StackTop():TStackItem;
    function PopStack():TStackItem;
    function GetRootData() : PDataBuffer;
  protected
    procedure PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TFloat_Extended_10
    );
    procedure PutInt(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TInt64S
    );
    procedure PutStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    );
    procedure PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumData
    );
    procedure PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    );
    procedure PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    );
    procedure PutObj(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TObject
    );

    function GetDataBuffer(var AName : String):PDataBuffer;
    procedure GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumData
    );
    procedure GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    );
    procedure GetFloat(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TFloat_Extended_10
    );
    procedure GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TInt64S
    );
    procedure GetInt64(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Int64
    );
    procedure GetStr(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : String
    );
    procedure GetObj(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TObject
    );
  public
    constructor Create();override;
    destructor Destroy();override;

    function GetCurrentScope():string;
    procedure Clear();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : PTypeInfo
    );
    procedure BeginArray(
      Const AName         : string;
      Const AItemTypeInfo : PTypeInfo;
      Const ABounds       : Array Of Integer
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    procedure AddScopeAttribute(Const AName,AValue : string);
    //If the scope is an array the return value must be the array' length;
    function BeginScopeRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AScopeType : TScopeType = stObject
    ):Integer;
    procedure EndScopeRead();
    
    procedure BeginHeader();
    procedure EndHeader();

    property CurrentScope : String Read GetCurrentScope;

    procedure Put(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData
    );
    procedure Get(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData
    );

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    procedure Error(Const AMsg:string);
    procedure Error(Const AMsg:string; Const AArgs : array of const);
  End;

  TDBGPinterProc = procedure(const AMsg:string);
  
  procedure ClearObj(const AOwner: PDataBuffer);
  function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
  procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
  function CreateArrayBuffer(
    const ALength   : Integer;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;
  function CreateObjBuffer(
    const ADataType : TDataType;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;
  
  procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);

implementation

procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);
Var
  p : PObjectBufferItem;
  s : string;
  i ,j: Integer;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  s := StringOfChar(' ',ALevel);
  Case ARoot^.DataType Of
    dtInt8S   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8S) );
    dtInt8U   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8U) );
    dtInt32U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32U) );
    dtInt32S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32S) );
    dtInt64U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64U) );
    dtInt64S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64S) );
    
    dtSingle  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.SingleData) );
    dtDouble  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.DoubleData) );
    dtExtended  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.ExtendedData) );
    dtCurrency  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.CurrencyData) );
    
    dtString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.StrData^.Data );
    dtObject  :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ObjectData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          APrinterProc('( ' + IntToStr(ARoot^.ObjectData^.Count) + ' Objects )');
          p := ARoot^.ObjectData^.Head;
          i := ALevel + 1;
          While Assigned(p) Do Begin
            PrintObj(p^.Data,i,APrinterProc);
            p := p^.Next;
          End;
        End;
      End;
    dtArray :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ArrayData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          j := ARoot^.ArrayData^.Count;
          APrinterProc('( Objects[ '+ IntToStr(j)+ '] )');
          i := ALevel + 1;
          For j := 0 To Pred(j) Do Begin
            PrintObj(ARoot^.ArrayData^.Items^[j],i,APrinterProc);
          End;
        End;
      End;
  End;
End;

function FindObj(const AOwner: PDataBuffer; const AName : TDataName) : PDataBuffer;
Var
  p : PObjectBufferItem;
Begin
  Assert(AOwner^.DataType >= dtObject);
  Result := Nil;
   p:= AOwner^.ObjectData^.Head;
  While Assigned(p) Do Begin
    If AnsiSameText(AName,p^.Data^.Name) Then Begin
      Result := p^.Data;
      Exit;
    End;
    p := p^.Next;
  End;
End;

procedure AddObj(
  const AOwner, AChildData: PDataBuffer;
  const AIndex : Integer = -1
);
Var
  p : PObjectBufferItem;
Begin
  If ( AOwner^.DataType = dtObject ) Then Begin
    p := GetMem(SizeOf(PObjectBufferItem^));
    p^.Data := AChildData;
    p^.Next := Nil;
    If Assigned(AOwner^.ObjectData^.Head) Then Begin
      AOwner^.ObjectData^.Last^.Next := p;
    End Else Begin
      AOwner^.ObjectData^.Head := p;
    End;
    AOwner^.ObjectData^.Last := p;
    Inc(AOwner^.ObjectData^.Count);
  End Else If ( AOwner^.DataType = dtArray ) Then Begin
    If ( AIndex >= 0 ) And ( AIndex < AOwner^.ArrayData^.Count ) Then
      AOwner^.ArrayData^.Items^[AIndex] := AChildData
    Else
      Raise EBinaryFormatterException.CreateFmt('Invalid array acces : %d',[AIndex])
  End Else Begin
    Raise EBinaryFormatterException.CreateFmt('Invalid data type in this context : %d',[Ord(AOwner^.DataType)])
  End;
End;

function CreateObjBuffer(
  const ADataType : TDataType;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
var
  resLen, i : Integer;
begin
  resLen := SizeOf(PDataBuffer^);
  Result := GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := ADataType;
    Case Result^.DataType Of
      dtString :
        Begin
          i := SizeOf(PStringBuffer^);
          Result^.StrData := GetMem(i);
          FillChar(Result^.StrData^,i,#0);
          Result^.StrData^.Data := '';
        End;
      dtObject :
        Begin
          Result^.ObjectData := GetMem(SizeOf(PObjectBuffer^));
          FillChar(Result^.ObjectData^,SizeOf(PObjectBuffer^),#0);
        End;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Result := nil;
    Raise;
  End;
end;

function CreateArrayBuffer(
  const ALength   : Integer;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
Var
  i, resLen : Integer;
begin
  Assert(ALength>=0);
  resLen := SizeOf(PDataBuffer^);
  Result := GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := dtArray;
    Result^.ArrayData := GetMem(SizeOf(PArrayBuffer^));
    FillChar(Result^.ArrayData^,SizeOf(PArrayBuffer^),#0);
    Result^.ArrayData^.Count := ALength;
    If ( ALength > 0 ) Then Begin
      i := ALength*SizeOf(PDataBuffer);
      Result^.ArrayData^.Items := GetMem(i);
      FillChar(Result^.ArrayData^.Items^[0],i,#0);
    End Else Begin
      Result^.ArrayData^.Items := Nil;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Result := nil;
    Raise;
  End;
end;

procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
Var
  p : PObjectBufferItem;
  i : TInt32S;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  i := Ord(ARoot^.DataType);
  ADest.WriteInt32S(i);
  ADest.WriteStr(ARoot^.Name);
  Case ARoot^.DataType Of
    dtInt8S  : ADest.WriteInt8S(ARoot^.Int8S);
      dtInt8U  : ADest.WriteInt8U(ARoot^.Int8U);
    dtInt16U  : ADest.WriteInt16U(ARoot^.Int16U);
      dtInt16S  : ADest.WriteInt16S(ARoot^.Int16S);
    dtInt32U  : ADest.WriteInt32U(ARoot^.Int32U);
      dtInt32S  : ADest.WriteInt32S(ARoot^.Int32S);
    dtInt64U  : ADest.WriteInt64U(ARoot^.Int64U);
      dtInt64S  : ADest.WriteInt64S(ARoot^.Int64S);
      
    dtSingle  : ADest.WriteSingle(ARoot^.SingleData);
    dtDouble  : ADest.WriteDouble(ARoot^.DoubleData);
    dtExtended  : ADest.WriteExtended(ARoot^.ExtendedData);
    dtCurrency  : ADest.WriteCurrency(ARoot^.CurrencyData);
    
    dtString  : ADest.WriteStr(ARoot^.StrData^.Data);
    dtBool    : ADest.WriteBool(ARoot^.BoolData);
    dtEnum    : ADest.WriteEnum(ARoot^.EnumData);
    dtObject :
      Begin
        i := ARoot^.ObjectData^.Count;
        ADest.WriteInt32S(i);

        If ( i > 0 ) Then Begin
          p := ARoot^.ObjectData^.Head;
          For i := 1 To i Do Begin
            SaveObjectToStream(p^.Data,ADest);
            p := p^.Next;
          End;
        End;
      End;
    dtArray :
      Begin
        i := ARoot^.ArrayData^.Count;
        ADest.WriteInt32S(i);

        If ( i > 0 ) Then Begin
          For i := 0 To Pred(i) Do Begin
            SaveObjectToStream(ARoot^.ArrayData^.Items^[i],ADest);
          End;
        End;
      End;
  End;
End;

function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
Var
  i : TInt32S;
  s : string;
Begin
  Result := Nil;
  If AStoreRdr.IsAtEof() Then
    Exit;
  i := AStoreRdr.ReadInt32S();
  s := AStoreRdr.ReadStr();
  If ( TDataType(i) < dtArray ) Then
    Result := CreateObjBuffer(TDataType(i),s);
  Case TDataType(i) Of
    dtInt8S   : Result^.Int8S := AStoreRdr.ReadInt8S();
    dtInt8U   : Result^.Int8S := AStoreRdr.ReadInt8U();
    dtInt16U  : Result^.Int16U := AStoreRdr.ReadInt16U();
    dtInt16S  : Result^.Int16S := AStoreRdr.ReadInt16S();
    dtInt32U  : Result^.Int32U := AStoreRdr.ReadInt32U();
    dtInt32S  : Result^.Int32S := AStoreRdr.ReadInt32S();
    dtInt64U  : Result^.Int64U := AStoreRdr.ReadInt64U();
    dtInt64S  : Result^.Int64S := AStoreRdr.ReadInt64S();
    
    dtSingle  : Result^.SingleData := AStoreRdr.ReadSingle();
    dtDouble  : Result^.DoubleData := AStoreRdr.ReadDouble();
    dtExtended  : Result^.ExtendedData := AStoreRdr.ReadExtended();
    dtCurrency  : Result^.CurrencyData := AStoreRdr.ReadCurrency();
    
    dtString  : Result^.StrData^.Data := AStoreRdr.ReadStr();
    dtBool    : Result^.BoolData := AStoreRdr.ReadBool();
    dtEnum    : Result^.EnumData := AStoreRdr.ReadEnum();
    dtObject  :
      Begin
        i := AStoreRdr.ReadInt32S();
        For i := 1 To i Do Begin
          AddObj(Result,LoadObjectFromStream(AStoreRdr));
        End;
      End;
    dtArray  :
      Begin
        i := AStoreRdr.ReadInt32S();
        Result := CreateArrayBuffer(i,s);
        For i := 0 To Pred(i) Do Begin
          AddObj(Result,LoadObjectFromStream(AStoreRdr),i);
        End;
      End;
  End;
End;

procedure ClearObjectBuffer(var ABuffer : PObjectBuffer);
var
  p,q : PObjectBufferItem;
begin
  if Assigned(ABuffer) then begin
    if Assigned(ABuffer^.Attributes) then
      ClearObjectBuffer(ABuffer^.Attributes);
    p := ABuffer^.Head;
    while Assigned(p) do begin
      q := p;
      p := p^.Next;
      ClearObj(q^.Data);
      Freemem(q^.Data);
      q^.Data := Nil;
      Freemem(q);
    end;
    //ABuffer^.Head := nil;
    //ABuffer^.Last := nil;
    Freemem(ABuffer);
    ABuffer := nil;
  end;
end;

procedure ClearObj(const AOwner: PDataBuffer);
Var
  i , j: Integer;
  eltLen : Integer;
Begin
  AOwner^.Name := '';
  Case AOwner^.DataType Of
    dtString :
      Begin
        AOwner^.StrData^.Data := '';
        Freemem(AOwner^.StrData);
        AOwner^.StrData := Nil;
      End;
    dtObject :
      Begin
        ClearObjectBuffer(AOwner^.ObjectData);
      End;
    dtArray :
      Begin
        eltLen := SizeOf(PDataBuffer^);
        For j := 0 to Pred(AOwner^.ArrayData^.Count) Do Begin
          ClearObj(AOwner^.ArrayData^.Items^[j]);
          Freemem(AOwner^.ArrayData^.Items^[j],eltLen);
          AOwner^.ArrayData^.Items^[j] := Nil;
        End;
        i := AOwner^.ArrayData^.Count * SizeOf(PDataBuffer);
        Freemem(AOwner^.ArrayData^.Items,i);
        AOwner^.ArrayData^.Items := Nil;
        ClearObjectBuffer(AOwner^.ArrayData^.Attributes);
        i := SizeOf(PArrayBuffer^);
        Freemem(AOwner^.ArrayData,i);
        AOwner^.ArrayData := Nil;
      End;
  End;
End;


{ TStackItem }

constructor TStackItem.Create(const AScopeObject: PDataBuffer; AScopeType: TScopeType);
begin
  Assert(Assigned(AScopeObject));
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

{ TObjectStackItem }

constructor TObjectStackItem.Create(const AScopeObject: PDataBuffer);
begin
  Inherited Create(AScopeObject,stObject);
end;

function TObjectStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ObjectData^.Count;
end;

function TObjectStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  Result := FindObj(ScopeObject,AName);
end;

function TObjectStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
Var
  p : PObjectBufferItem;
  i : Integer;
begin
  If ( AIndex >=0 ) And ( AIndex < ScopeObject^.ObjectData^.Count) Then Begin
    p := ScopeObject^.ObjectData^.Head;
    For i := 1 To AIndex Do
      p := p^.Next;
    Result := p^.Data;
  End Else
    Raise EBinaryFormatterException.CreateFmt('Invalid index access : %d',[AIndex]);
end;

function TObjectStackItem.CreateBuffer(
  Const AName     : String;
  const ADataType : TDataType
):PDataBuffer;
begin
  Result := CreateObjBuffer(ADataType,AName,ScopeObject);
end;

procedure TObjectStackItem.NilCurrentScope();
begin
  Assert(ScopeObject^.ObjectData^.Count = 0);
  ScopeObject^.ObjectData^.NilObject := True;
end;

function TObjectStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result:= ScopeObject^.ObjectData^.NilObject;
end;

//----------------------------------------------------------------
{ TBaseBinaryFormatter }

procedure TBaseBinaryFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

procedure TBaseBinaryFormatter.PushStack(AScopeObject: PDataBuffer;const AScopeType: TScopeType);
begin
  If ( AScopeType = stObject ) Then
    FStack.Push(TObjectStackItem.Create(AScopeObject))
  Else If ( AScopeType = stArray ) Then
    FStack.Push(TArrayStackItem.Create(AScopeObject))
  Else
    Assert(False);
end;

function TBaseBinaryFormatter.StackTop(): TStackItem;
begin
  Result := FStack.Peek() as TStackItem;
end;

function TBaseBinaryFormatter.PopStack(): TStackItem;
begin
  Result := FStack.Pop() as TStackItem;
end;

function TBaseBinaryFormatter.GetRootData(): PDataBuffer;
begin
  Result := FRootData;
end;

procedure TBaseBinaryFormatter.PutFloat(
  const AName       : String;
  const ATypeInfo   : PTypeInfo;
  const AData       : TFloat_Extended_10
);
begin
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle   : StackTop().CreateBuffer(AName,dtSingle)^.SingleData  := AData;
    ftDouble   : StackTop().CreateBuffer(AName,dtDouble)^.DoubleData   := AData;
    ftExtended : StackTop().CreateBuffer(AName,dtExtended)^.ExtendedData := AData;
    ftCurr     : StackTop().CreateBuffer(AName,dtCurrency)^.CurrencyData := AData;
    Else
      StackTop().CreateBuffer(AName,dtExtended)^.ExtendedData := AData;
  End;
end;

function TBaseBinaryFormatter.GetCurrentScopeObject(): PDataBuffer;
begin
  Result := StackTop().ScopeObject;
end;

procedure TBaseBinaryFormatter.SetSerializationStyle(
  const ASerializationStyle: TSerializationStyle
);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TBaseBinaryFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TBaseBinaryFormatter.HasScope(): Boolean;
begin
  Result := Assigned(FStack.Peek);
end;

procedure TBaseBinaryFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error('There is no scope.');
end;

function TBaseBinaryFormatter.GetCurrentScope: String;
begin
  Result := GetCurrentScopeObject()^.Name;
end;

procedure TBaseBinaryFormatter.PutInt(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TInt64S
);
begin
  Case GetTypeData(ATypeInfo)^.OrdType Of
    otSByte : StackTop().CreateBuffer(AName,dtInt8S)^.Int8S   := AData;
    otUByte : StackTop().CreateBuffer(AName,dtInt8U)^.Int8U   := AData;
    otSWord : StackTop().CreateBuffer(AName,dtInt16S)^.Int16S := AData;
    otUWord : StackTop().CreateBuffer(AName,dtInt16U)^.Int16U := AData;
    otULong : StackTop().CreateBuffer(AName,dtInt32U)^.Int32U := AData;
    otSLong : StackTop().CreateBuffer(AName,dtInt32S)^.Int32S := AData;
  End;
end;

procedure TBaseBinaryFormatter.PutStr(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : String
);
begin
  StackTop().CreateBuffer(AName,dtString)^.StrData^.Data := AData;
end;

procedure TBaseBinaryFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumData
);
begin
  StackTop().CreateBuffer(AName,dtEnum)^.EnumData := AData;
end;

procedure TBaseBinaryFormatter.PutBool(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: Boolean
);
begin
  StackTop().CreateBuffer(AName,dtBool)^.BoolData := AData;
end;

procedure TBaseBinaryFormatter.PutInt64(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: Int64
);
begin
  StackTop().CreateBuffer(AName,dtInt64S)^.Int64S := AData;
end;

procedure TBaseBinaryFormatter.PutObj(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

function TBaseBinaryFormatter.GetDataBuffer(var AName: String): PDataBuffer;
begin
  Result := StackTop().Find(AName);
  If Not Assigned(Result) Then
    Error('Param not found : "%s"',[AName]);
end;

procedure TBaseBinaryFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TEnumData
);
begin
  AData := GetDataBuffer(AName)^.EnumData;
end;

procedure TBaseBinaryFormatter.GetBool(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: Boolean
);
begin
  AData := GetDataBuffer(AName)^.BoolData;
end;

procedure TBaseBinaryFormatter.GetFloat(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TFloat_Extended_10
);
Var
  t : PDataBuffer;
begin
  t := GetDataBuffer(AName);
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle     : AData := t^.SingleData;
    ftDouble     : AData := t^.DoubleData;
    ftExtended   : AData := t^.ExtendedData;
    ftCurr       : AData := t^.CurrencyData;
    Else
      AData := t^.ExtendedData;
  End;
end;

procedure TBaseBinaryFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TInt64S
);
Var
  t : PDataBuffer;
begin
  t := GetDataBuffer(AName);
  Case GetTypeData(ATypeInfo)^.OrdType Of
    otSByte : AData := t^.Int8S;
    otUByte : AData := t^.Int8U;
    otSWord : AData := t^.Int16S;
    otUWord : AData := t^.Int16U;
    otSLong : AData := t^.Int32S;
    otULong : AData := t^.Int32U;
    Else
      Assert(False);
  End;
end;

procedure TBaseBinaryFormatter.GetInt64(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: Int64
);
begin
  AData := GetDataBuffer(AName)^.Int64S;
end;

procedure TBaseBinaryFormatter.GetStr(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: String
);
begin
  AData := GetDataBuffer(AName)^.StrData^.Data;
end;

procedure TBaseBinaryFormatter.GetObj(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
end;

procedure TBaseBinaryFormatter.Clear();
begin
  ClearStack();
  ClearObj(FRootData);
  Freemem(FRootData);
  FRootData := CreateObjBuffer(dtObject,sROOT);
  PushStack(FRootData,stObject);
end;

procedure TBaseBinaryFormatter.BeginArray(const AName: string;const AItemTypeInfo: PTypeInfo; const ABounds: array of Integer);
Var
  i, j, k : Integer;
begin
  If ( Length(ABounds) < 2 ) Then
    Raise EBinaryFormatterException.Create('Invalid array bounds.');
  i := ABounds[0];
  j := ABounds[1];
  k := ( j - i + 1 );
  If ( k < 0 ) Then
    Raise EBinaryFormatterException.Create('Invalid array bounds.');
  PushStack(CreateArrayBuffer(k,AName,StackTop().ScopeObject),stArray);
end;

procedure TBaseBinaryFormatter.NilCurrentScope();
begin
  CheckScope();
  StackTop().NilCurrentScope();
end;

function TBaseBinaryFormatter.IsCurrentScopeNil(): Boolean;
begin
  Result := StackTop().IsCurrentScopeNil();
end;

procedure TBaseBinaryFormatter.BeginObject(const AName: TDataName;const ATypeInfo: PTypeInfo);
begin
  PushStack(StackTop().CreateBuffer(AName,dtObject));
end;

procedure TBaseBinaryFormatter.EndScope();
begin
  FStack.Pop().Free();
end;

procedure TBaseBinaryFormatter.AddScopeAttribute(const AName, AValue: string);
begin
end;

function TBaseBinaryFormatter.BeginScopeRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AScopeType : TScopeType = stObject
): Integer;
Var
  locNode : PDataBuffer;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.Find(AScopeName);
  If Not Assigned(locNode) Then
    Error('Scope not found : "%s"',[AScopeName]);
  PushStack(locNode,AScopeType);
  Result := StackTop().GetItemCount();
end;

procedure TBaseBinaryFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TBaseBinaryFormatter.BeginHeader();
begin
{$IFDEF wst_binary_header}
  if ( FHeaderEnterCount <= 0 ) then begin
    Inc(FHeaderEnterCount);
    BeginObject(sHEADER,nil);
  end;
{$ENDIF}
end;

procedure TBaseBinaryFormatter.EndHeader();
begin
{$IFDEF wst_binary_header}
  if ( FHeaderEnterCount > 0 ) then begin
    Dec(FHeaderEnterCount);
    EndScope();
  end;
{$ENDIF}
end;

procedure TBaseBinaryFormatter.Put(const AName: String; const ATypeInfo: PTypeInfo;const AData);
Var
  intData : Integer;
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumData;
  floatDt : TFloat_Extended_10;
begin
  Case ATypeInfo^.Kind Of
    tkLString, tkAString :
      Begin
        strData := String(AData);
        PutStr(AName,ATypeInfo,strData);
      End;
    tkInt64,tkQWord :
      Begin
        int64Data := Int64(AData);
        PutInt64(AName,ATypeInfo,int64Data);
      End;
    tkClass :
      Begin
        objData := TObject(AData);
        PutObj(AName,ATypeInfo,objData);
      End;
    tkBool :
      Begin
        boolData := Boolean(AData);
        PutBool(AName,ATypeInfo,boolData);
      End;
    tkInteger, tkEnumeration :
      Begin
        enumData := 0;
        Case GetTypeData(ATypeInfo)^.OrdType Of
          otSByte : enumData := ShortInt(AData);
          otUByte : enumData := Byte(AData);
          otSWord : enumData := SmallInt(AData);
          otUWord : enumData := Word(AData);
          otSLong : enumData := LongInt(AData);
          otULong : enumData := LongWord(AData);
        End;
        If ( ATypeInfo^.Kind = tkInteger ) Then
          PutInt(AName,ATypeInfo,enumData)
        Else
          PutEnum(AName,ATypeInfo,enumData);
      End;
    tkFloat :
      Begin
        floatDt := 0;
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : floatDt := Single(AData);
          ftDouble : floatDt := Double(AData);
          ftExtended : floatDt := Extended(AData);
          ftCurr : floatDt := Currency(AData);
          ftComp : floatDt := Comp(AData);
        End;
        PutFloat(AName,ATypeInfo,floatDt);
      End;
  End;
end;

procedure TBaseBinaryFormatter.Get(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumData;
  floatDt : TFloat_Extended_10;
begin
  Case ATypeInfo^.Kind Of
    tkInt64, tkQWord :
      Begin
        int64Data := 0;
        GetInt64(ATypeInfo,AName,int64Data);
        Int64(AData) := int64Data;
      End;
    tkLString, tkAString :
      Begin
        strData := '';
        GetStr(ATypeInfo,AName,strData);
        String(AData) := strData;
      End;
    tkClass :
      Begin
        objData := TObject(AData);
        GetObj(ATypeInfo,AName,objData);
        TObject(AData) := objData;
      End;
    tkBool :
      Begin
        boolData := False;
        GetBool(ATypeInfo,AName,boolData);
        Boolean(AData) := boolData;
      End;
    tkInteger, tkEnumeration :
      Begin
        enumData := 0;
        If ( ATypeInfo^.Kind = tkInteger ) Then
          GetInt(ATypeInfo,AName,enumData)
        Else
          GetEnum(ATypeInfo,AName,enumData);
        Case GetTypeData(ATypeInfo)^.OrdType Of
          otSByte : ShortInt(AData)    := enumData;
          otUByte : Byte(AData)        := enumData;
          otSWord : SmallInt(AData)    := enumData;
          otUWord : Word(AData)        := enumData;
          otSLong : LongInt(AData)     := enumData;
          otULong : LongWord(AData)    := enumData;
        End;
      End;
    tkFloat :
      Begin
        floatDt := 0;
        GetFloat(ATypeInfo,AName,floatDt);
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : Single(AData)    := floatDt;
          ftDouble : Double(AData)    := floatDt;
          ftExtended : Extended(AData)    := floatDt;
          ftCurr : Currency(AData)    := floatDt;
          ftComp : Comp(AData)    := floatDt;
        End;
      End;
  End;
end;

procedure TBaseBinaryFormatter.SaveToStream(AStream: TStream);
Var
  locStore : IDataStore;
begin
  locStore := CreateBinaryWriter(AStream);
  SaveObjectToStream(FRootData,locStore);
end;

procedure TBaseBinaryFormatter.LoadFromStream(AStream: TStream);
Var
  locRdr : IDataStoreReader;
  tmpRoot : PDataBuffer;
begin
  locRdr := CreateBinaryReader(AStream);
  tmpRoot := LoadObjectFromStream(locRdr);

  ClearStack();
  ClearObj(FRootData);
  Freemem(FRootData);
  FRootData := tmpRoot;
  PushStack(FRootData,stObject);
end;

procedure TBaseBinaryFormatter.Error(const AMsg: string);
begin
  Raise EBinaryFormatterException.Create(AMsg);
end;

procedure TBaseBinaryFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EBinaryFormatterException.CreateFmt(AMsg,AArgs);
end;

constructor TBaseBinaryFormatter.Create();
begin
  FRootData := CreateObjBuffer(dtObject,sROOT);
  FStack := TObjectStack.Create();
  PushStack(FRootData,stObject);
end;

destructor TBaseBinaryFormatter.Destroy();
begin
  ClearStack();
  FreeAndNil(FStack);
  ClearObj(FRootData);
  Freemem(FRootData);
  inherited Destroy();
end;

{ TArrayStackItem }

constructor TArrayStackItem.Create(const AScopeObject: PDataBuffer);
begin
  Inherited Create(AScopeObject,stArray);
  FIndex := 0;
end;

function TArrayStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ArrayData^.Count;
end;

function TArrayStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[FIndex]
  Else
    Raise EBinaryFormatterException.CreateFmt('Invalid array index : %d',[FIndex]);
  Inc(FIndex);
end;

function TArrayStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
begin
  If ( AIndex >= 0 ) And ( AIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[AIndex]
  Else
    Raise EBinaryFormatterException.CreateFmt('Invalid array index : %d',[AIndex]);
end;

function TArrayStackItem.CreateBuffer(
  const AName     : String;
  const ADataType : TDataType
): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := CreateObjBuffer(ADataType,AName,Nil)
  Else
    Raise EBinaryFormatterException.CreateFmt('Invalid array index : %d',[FIndex]);
  ScopeObject^.ArrayData^.Items^[FIndex] := Result;
  Inc(FIndex);
end;

procedure TArrayStackItem.NilCurrentScope();
begin
end;

function TArrayStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result := False;
end;

end.
