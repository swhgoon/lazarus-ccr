{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit base_soap_formatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  DOM,
  base_service_intf;
  
Const
  sPROTOCOL_NAME = 'SOAP';

  sXML_NS = 'xmlns';
  sXSI_NS = 'http://www.w3.org/1999/XMLSchema-instance';
  sXSI_TYPE = 'xsi:type';
  sXSI_NIL = 'xsi:nil';

  sSOAP_ENC = 'http://schemas.xmlsoap.org/soap/encoding/';
  sSOAP_ENC_ABR = 'SOAP-ENC';

  sARRAY_TYPE = 'arrayType';

  sCONTENT_TYPE = 'contenttype';
  sSOAP_CONTENT_TYPE = 'text/xml';

  sHEADER   = 'Header';
  sENVELOPE = 'Envelope';

Type

  TEnumIntType = Int64;

  { ESOAPException }

  ESOAPException = class(EBaseRemoteException)
  End;
  
  { TStackItem }

  TStackItem = class
  private
    FNameSpace: string;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
    function GetItemsCount: Integer;
  Public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    procedure SetNameSpace(const ANameSpace : string);
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property NameSpace : string Read FNameSpace;
    property ItemsCount : Integer read GetItemsCount;
  End;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    function FindNode(var ANodeName : string):TDOMNode;override;
  End;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  Private
    FIndex : Integer;
  Public
    function FindNode(var ANodeName : string):TDOMNode;override;
  End;

  TSOAPEncodingStyle = ( Encoded, Litteral );
  TSOAPDocumentStyle = ( RPC, Document );
  
{$M+}

  { TSOAPBaseFormatter }

  TSOAPBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FContentType: string;
    FEncodingStyle: TSOAPEncodingStyle;
    FStyle: TSOAPDocumentStyle;
    FHeaderEnterCount : Integer;
    
    FNameSpaceCounter : Integer;
    FDoc : TXMLDocument;
    FStack : TObjectStack;

    FKeepedStyle : TSOAPDocumentStyle;
    FKeepedEncoding : TSOAPEncodingStyle;
    FSerializationStyle : TSerializationStyle;

    procedure InternalClear(const ACreateDoc : Boolean);
    
    function NextNameSpaceCounter():Integer;//inline;
    function HasScope():Boolean;//inline;

    procedure CheckScope();//inline;
    function InternalPutData(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : string
    ):TDOMNode;
    function PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    ):TDOMNode;
    function PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    ):TDOMNode;
    function PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    ):TDOMNode;
    function PutStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    ):TDOMNode;
    function PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Extended
    ):TDOMNode;
    procedure PutObj(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TObject
    );

    function GetNodeValue(var AName : String):DOMString;
    procedure GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumIntType
    );
    procedure GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    );
    procedure GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Integer
    );
    procedure GetInt64(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Int64
    );
    procedure GetFloat(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Extended
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
  protected
    function GetXmlDoc():TXMLDocument;
    function PushStack(AScopeObject : TDOMNode;Const AScopeType : TScopeType = stObject):TStackItem;
    function FindAttributeByValueInNode(
      Const AAttValue : String;
      Const ANode     : TDOMNode;
      Out   AResAtt   : string
    ):boolean;
    function FindAttributeByNameInNode(
      Const AAttName     : String;
      Const ANode        : TDOMNode;
      Out   AResAttValue : string
    ):boolean;
    function FindAttributeByValueInScope(Const AAttValue : String):String;
    function FindAttributeByNameInScope(Const AAttName : String):String;
  protected
    function GetCurrentScope():String;
    function GetCurrentScopeObject():TDOMElement;
    function StackTop():TStackItem;
    function PopStack():TStackItem;
    procedure ClearStack();
    procedure BeginScope(
      Const AScopeName,ANameSpace : string;
      Const ANameSpaceShortName   : string = '';
      Const AScopeType            : TScopeType = stObject
    );

    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetStyleAndEncoding(
      const AStyle : TSOAPDocumentStyle;
      const AEncoding : TSOAPEncodingStyle
    );
    procedure RestoreStyleAndEncoding();
    procedure Prepare();
    function ReadHeaders(ACallContext : ICallContext):Integer;
    function WriteHeaders(ACallContext : ICallContext):Integer;
  public
    constructor Create();override;
    destructor Destroy();override;
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
    function BeginScopeRead(
      Var   AScopeName : string;
      Const ATypeInfo  : PTypeInfo;
      Const AScopeType : TScopeType = stObject
    ):Integer;
    procedure EndScopeRead();

    procedure BeginHeader();
    procedure EndHeader();

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
  Published
    property EncodingStyle : TSOAPEncodingStyle Read FEncodingStyle Write FEncodingStyle;
    property ContentType : string Read FContentType Write FContentType;
    property Style : TSOAPDocumentStyle Read FStyle Write FStyle;
  End;
{$M-}

implementation
Uses XMLWrite, XMLRead, StrUtils,
     imp_utils;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
var
  chdLst : TDOMNodeList;
begin
  if ANode.HasChildNodes then begin
    chdLst := ANode.ChildNodes;
    try
      Result := chdLst.Count
    finally
      chdLst.Release();
    end;
  end else begin
    Result := 0;
  end;
end;

{ TStackItem }

function TStackItem.GetItemsCount: Integer;
begin
  Result := GetNodeItemsCount(ScopeObject);
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

procedure TStackItem.SetNameSpace(const ANameSpace: string);
begin
  FNameSpace := ANameSpace;
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  Result:= ScopeObject.FindNode(ANodeName);
end;

{ TArrayStackItem }

function TArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
var
  chdLst : TDOMNodeList;
begin
  if not ScopeObject.HasChildNodes then
    raise ESOAPException.Create('This node has no children.');
  chdLst := ScopeObject.ChildNodes;
  try
    if ( FIndex >= chdLst.Count ) then
      raise ESOAPException.CreateFmt('Index out of bound : %d',[FIndex]);
    Result:= chdLst.Item[FIndex];
    Inc(FIndex);
    ANodeName := Result.NodeName;
  finally
    chdLst.Release();
  end;
end;

{ TSOAPBaseFormatter }

procedure TSOAPBaseFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

function TSOAPBaseFormatter.PushStack(
        AScopeObject : TDOMNode;
  Const AScopeType   : TScopeType
) : TStackItem;
begin
  if ( AScopeType = stArray ) then
    Result := FStack.Push(TArrayStackItem.Create(AScopeObject,AScopeType)) as TStackItem
  else
    Result := FStack.Push(TObjectStackItem.Create(AScopeObject,AScopeType)) as TStackItem;
end;

function TSOAPBaseFormatter.BeginScopeRead(
  Var   AScopeName : string;
  Const ATypeInfo  : PTypeInfo;
  Const AScopeType : TScopeType = stObject
):Integer;
Var
  locNode : TDOMNode;
  stk : TStackItem;
  
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  strNodeName : string;
begin
  if ( Style = Document ) then begin
    typData := GetTypeRegistry().Find(ATypeInfo,False);
    if not Assigned(typData) then
      Error('Object type not registered : %s',[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
    nmspc := typData.NameSpace;
    if IsStrEmpty(nmspc) then
      nmspcSH := ''
    else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if not IsStrEmpty(nmspcSH) then begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    End;
    if IsStrEmpty(nmspcSH) then begin
      strNodeName := AScopeName
    end else begin
      if ( Pos(':',AScopeName) < 1 ) then
        strNodeName := nmspcSH + ':' + AScopeName
      else
        strNodeName := AScopeName;
    end;
  end else begin
    nmspcSH := '';
    strNodeName := AScopeName;
  end;

  stk := StackTop();
  locNode := stk.FindNode(strNodeName);//(AScopeName);
  If Not Assigned(locNode) Then
    Error('Scope not found : "%s"',[strNodeName]);//[AScopeName]);
  PushStack(locNode,AScopeType);
  if ( Style = Document ) then begin
    StackTop().SetNameSpace(nmspc);
  end;
  if locNode.HasChildNodes then
    Result := GetNodeItemsCount(locNode)
  else
    Result := 0;
end;

procedure TSOAPBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TSOAPBaseFormatter.BeginHeader();
begin
  if ( FHeaderEnterCount <= 0 ) then begin
    Inc(FHeaderEnterCount);
    Prepare();
    BeginScope(sHEADER,sSOAP_ENV,sSOAP_ENV_ABR);
    SetStyleAndEncoding(Document,Litteral);
  end;
end;

procedure TSOAPBaseFormatter.EndHeader();
begin
  if ( FHeaderEnterCount > 0 ) then begin
    Dec(FHeaderEnterCount);
    RestoreStyleAndEncoding();
    EndScope();
  end;
end;

procedure TSOAPBaseFormatter.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  FreeAndNil(FDoc);
  if ACreateDoc then
    FDoc := TXMLDocument.Create();
end;

function TSOAPBaseFormatter.NextNameSpaceCounter(): Integer;
begin
  Inc(FNameSpaceCounter);
  Result := FNameSpaceCounter;
end;

function TSOAPBaseFormatter.HasScope(): Boolean;
begin
  Result := Assigned(FStack.Peek);
end;

function TSOAPBaseFormatter.FindAttributeByValueInNode(
  Const AAttValue : String;
  Const ANode     : TDOMNode;
  Out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByNameInNode(
  const AAttName: String;
  const ANode: TDOMNode;
  Out AResAttValue: string
): boolean;
var
  i,c : Integer;
begin
  AResAttValue := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttName,ANode.Attributes.Item[i].NodeName) Then Begin
        AResAttValue := ANode.Attributes.Item[i].NodeValue;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByValueInScope(const AAttValue: String): String;
Var
  tmpNode : TDOMNode;
begin
  If HasScope() Then Begin
    tmpNode := GetCurrentScopeObject();
    While Assigned(tmpNode) Do Begin
      If FindAttributeByValueInNode(AAttValue,tmpNode,Result) Then
        Exit;
      tmpNode := tmpNode.ParentNode;
    End;
  End;
  Result := '';
end;

function TSOAPBaseFormatter.FindAttributeByNameInScope(const AAttName: String): String;
var
  tmpNode : TDOMNode;
begin
  if HasScope() then begin
    tmpNode := GetCurrentScopeObject();
    while Assigned(tmpNode) do begin
      if FindAttributeByNameInNode(AAttName,tmpNode,Result) then
        Exit;
      tmpNode := tmpNode.ParentNode;
    end;
  end;
  Result := '';
end;

procedure TSOAPBaseFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error('There is no scope.');
end;

function ExtractNameSpaceShortName(const ANameSpaceDeclaration : string):string;
var
  i : integer;
begin
  i := AnsiPos(sXML_NS,ANameSpaceDeclaration);
  if ( i > 0 ) then begin
    Result := Copy(ANameSpaceDeclaration, (i + Length(sXML_NS) + 1 ), MaxInt );
  end else begin
    Result := '';
  end;
end;

function TSOAPBaseFormatter.InternalPutData(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : string
): TDOMNode;
Var
  namespaceLongName, namespaceShortName, strName, strNodeName, s : string;
  regItem : TTypeRegistryItem;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := Copy(FindAttributeByValueInScope(StackTop().NameSpace),AnsiPos(':',namespaceShortName) + 1,MaxInt);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    Result := FDoc.CreateElement(strNodeName);
    Result.AppendChild(FDoc.CreateTextNode(AData));
    GetCurrentScopeObject().AppendChild(Result);
  end else begin
    Result := GetCurrentScopeObject();
    (Result as TDOMElement).SetAttribute(strNodeName,AData);
  end;
  If ( EncodingStyle = Encoded ) Then Begin
    regItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
    strName := regItem.DeclaredName;
    namespaceLongName := regItem.NameSpace;
    If Not IsStrEmpty(namespaceLongName) Then Begin
      namespaceShortName := FindAttributeByValueInScope(namespaceLongName);
      If IsStrEmpty(namespaceShortName) Then Begin
        namespaceShortName := Format('ns%d',[NextNameSpaceCounter()]);
        AddScopeAttribute(sXML_NS + ':'+namespaceShortName,namespaceLongName);
      End Else Begin
        namespaceShortName := ExtractNameSpaceShortName(namespaceShortName);//Copy(namespaceShortName,AnsiPos(':',namespaceShortName) + 1,MaxInt);
      End;
      strName := Format('%s:%s',[namespaceShortName,strName])
    End;
    (Result As TDOMElement).SetAttribute(sXSI_TYPE,strName);
  End;
end;

function TSOAPBaseFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumIntType
): TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,GetEnumName(ATypeInfo,AData));
end;

function TSOAPBaseFormatter.PutBool(
  const AName     : String;
  const ATypeInfo : PTypeInfo;
  const AData     : Boolean
): TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,LowerCase(BoolToStr(AData)));
end;

function TSOAPBaseFormatter.PutInt64(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Int64
): TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,IntToStr(AData));
end;

function TSOAPBaseFormatter.PutStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: String
):TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,AData);
end;

procedure TSOAPBaseFormatter.PutObj(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

function TSOAPBaseFormatter.PutFloat(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
):TDOMNode;
Var
  s, frmt : string;
  prcsn,i : Integer;
begin
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle,
    ftCurr,
    ftComp      : prcsn := 7;
    ftDouble,
    ftExtended  : prcsn := 15;
  End;
  frmt := '#.' + StringOfChar('#',prcsn) + 'E-0';
  s := FormatFloat(frmt,AData);
  i := Pos(',',s);
  If ( i > 0 ) Then
    s[i] := '.';
  Result := InternalPutData(AName,ATypeInfo,s);
end;

function TSOAPBaseFormatter.GetNodeValue(var AName: String): DOMString;
Var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s : string;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := Copy(FindAttributeByValueInScope(StackTop().NameSpace),AnsiPos(':',namespaceShortName) + 1,MaxInt);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName) As TDOMElement;
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;

  if Assigned(locElt) then begin
    if locElt.HasChildNodes then
      Result := locElt.FirstChild.NodeValue
    else
      Result := locElt.NodeValue;
  end else begin
    Error('Param not found : "%s"',[AName]);
  end;
  //WriteLn(StringOfChar(' ',FStack.Count), AName,' = ',Result);
end;

procedure TSOAPBaseFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TEnumIntType
);
Var
  locBuffer : String;
begin
  locBuffer := Trim(GetNodeValue(AName));
  If IsStrEmpty(locBuffer) Then
    AData := 0
  Else
    AData := GetEnumValue(ATypeInfo,locBuffer)
End;

procedure TSOAPBaseFormatter.GetBool(
  const ATypeInfo  : PTypeInfo;
  var   AName      : String;
  var   AData      : Boolean
);
Var
  locBuffer : String;
begin
  locBuffer := LowerCase(Trim(GetNodeValue(AName)));
  If IsStrEmpty(locBuffer) Then
    AData := False
  Else
    AData := StrToBool(locBuffer);
end;

procedure TSOAPBaseFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: Integer
);
begin
  AData := StrToIntDef(Trim(GetNodeValue(AName)),0);
end;

procedure TSOAPBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Int64
);
begin
  AData := StrToInt64Def(Trim(GetNodeValue(AName)),0);
end;

procedure TSOAPBaseFormatter.GetFloat(
  const ATypeInfo  : PTypeInfo;
  var AName        : String;
  var AData        : Extended
);
begin
  AData := StrToFloatDef(Trim(GetNodeValue(AName)),0);
end;

procedure TSOAPBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : String
);
begin
  AData := GetNodeValue(AName);
end;

procedure TSOAPBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
end;

function TSOAPBaseFormatter.GetXmlDoc(): TXMLDocument;
begin
  Result := FDoc;
end;

function TSOAPBaseFormatter.GetCurrentScope(): String;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TSOAPBaseFormatter.GetCurrentScopeObject(): TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TSOAPBaseFormatter.StackTop(): TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TSOAPBaseFormatter.PopStack(): TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

constructor TSOAPBaseFormatter.Create();
begin
  Inherited Create();
  FContentType := sSOAP_CONTENT_TYPE;
  FStack := TObjectStack.Create();
  FDoc := TXMLDocument.Create();
  FDoc.Encoding := 'UTF-8';
end;

destructor TSOAPBaseFormatter.Destroy();
begin
  FDoc.Free();
  ClearStack();
  FStack.Free();
  inherited Destroy();
end;

procedure TSOAPBaseFormatter.Clear();
begin
  InternalClear(True);
end;

procedure TSOAPBaseFormatter.BeginObject(
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  mustAddAtt : Boolean;
  strNodeName : string;
begin
  typData := GetTypeRegistry().Find(ATypeInfo,False);
  If Not Assigned(typData) Then
    Error('Object type not registered : %s',[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
  mustAddAtt := False;
  nmspc := typData.NameSpace;
  If IsStrEmpty(nmspc) Then
    nmspcSH := 'tns'
  Else Begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    If IsStrEmpty(nmspcSH) Then Begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      If HasScope() Then
        AddScopeAttribute('xmlns:'+nmspcSH, nmspc)
      Else Begin
        mustAddAtt := True;
      End;
    End Else Begin
      nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
    End;
  End;

  if ( Style = Document ) then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  BeginScope(strNodeName,'');
  If mustAddAtt Then
    AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
  if ( EncodingStyle = Encoded ) then
    AddScopeAttribute(sXSI_TYPE,Format('%s:%s',[nmspcSH,typData.DeclaredName]));
  StackTop().SetNameSpace(nmspc);
end;

procedure TSOAPBaseFormatter.BeginArray(
  const AName          : string;
  const AItemTypeInfo  : PTypeInfo;
  const ABounds        : array of Integer
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  i,j, k : Integer;
  strNodeName : string;
begin
  If ( Length(ABounds) < 2 ) Then
    Error('Invalid array bounds.');
  i := ABounds[0];
  j := ABounds[1];
  k := j - i + 1;
  If ( k < 0 ) Then
    Error('Invalid array bounds.');
  k := j - i + 1;
  typData := GetTypeRegistry().Find(AItemTypeInfo,False);
  If Not Assigned(typData) Then
    Error('Array item''type not registered.');
  nmspc := typData.NameSpace;
  If IsStrEmpty(nmspc) Then
    nmspcSH := 'tns'
  Else Begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    If IsStrEmpty(nmspcSH) Then Begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
    End;
  End;

  if ( Style = Document ) then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  BeginScope(strNodeName,'');

  if ( EncodingStyle = Encoded ) then begin
    //AddScopeAttribute(sXSI_TYPE,nmspc);
    //SOAP-ENC:arrayType="xsd:int[2]"
    AddScopeAttribute(
      Format('%s:%s',[sSOAP_ENC_ABR,sARRAY_TYPE]) ,
      Format('%s:%s[%d]',[nmspcSH,typData.DeclaredName,k])
    );
  end;
  StackTop().SetNameSpace(nmspc);
end;

procedure TSOAPBaseFormatter.NilCurrentScope();
begin
  CheckScope();
  GetCurrentScopeObject().SetAttribute(sXSI_NIL,'true');
end;

function TSOAPBaseFormatter.IsCurrentScopeNil(): Boolean;
Var
  s,nsShortName,nilName : string;
begin
  CheckScope();
  nsShortName := FindAttributeByValueInScope(sXSI_NS);
  Result := False;
  if not IsStrEmpty(nsShortName) then begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    if not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
    nilName := nsShortName + 'nil';
    s := Trim(GetCurrentScopeObject().GetAttribute(nilName));
    if ( Length(s) > 0 ) and ( AnsiSameText(s,'true') or AnsiSameText(s,'"true"') ) then
      Result := True;
  end
end;

procedure TSOAPBaseFormatter.BeginScope(
  Const AScopeName,ANameSpace : string;
  Const ANameSpaceShortName   : string;
  Const AScopeType            : TScopeType
);
Var
  nsStr, scpStr : String;
  e : TDOMElement;
  hasNmspc, addAtt : Boolean;
begin
  scpStr := AScopeName;
  hasNmspc := Not IsStrEmpty(ANameSpace);
  If hasNmspc Then Begin
    nsStr := FindAttributeByValueInScope(ANameSpace);
    addAtt := IsStrEmpty(nsStr);
    If addAtt Then Begin
      If IsStrEmpty(ANameSpaceShortName) Then
        nsStr := 'ns' + IntToStr(NextNameSpaceCounter())
      Else
        nsStr := Trim(ANameSpaceShortName);
    End Else Begin
      nsStr := Copy(nsStr,Succ(AnsiPos(':',nsStr)),MaxInt);
    End;
    scpStr := nsStr + ':' + scpStr;
  End;

  e := FDoc.CreateElement(scpStr);
  If HasScope() Then
    GetCurrentScopeObject().AppendChild(e)
  Else
    FDoc.AppendChild(e);
  PushStack(e,AScopeType);
  if hasNmspc and addAtt then begin
    e.SetAttribute('xmlns:'+nsStr,ANameSpace);
    StackTop().SetNameSpace(ANameSpace);
  end;
end;

procedure TSOAPBaseFormatter.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TSOAPBaseFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

procedure TSOAPBaseFormatter.SetStyleAndEncoding(
  const AStyle: TSOAPDocumentStyle;
  const AEncoding: TSOAPEncodingStyle
);
begin
  FKeepedStyle := Style;
  FKeepedEncoding := EncodingStyle;
  Style := AStyle;
  EncodingStyle := AEncoding;
end;

procedure TSOAPBaseFormatter.RestoreStyleAndEncoding();
begin
  EncodingStyle := FKeepedEncoding;
  Style := FKeepedStyle;
end;

procedure TSOAPBaseFormatter.Prepare();
var
  locDoc : TDOMDocument;
begin
  locDoc := GetXmlDoc();
  if Assigned(locDoc.DocumentElement) and
     AnsiSameText(locDoc.DocumentElement.NodeName,( sSOAP_ENV_ABR + ':' + sENVELOPE ))
  then begin
    ClearStack();
    PushStack(locDoc.DocumentElement,stObject);
  end else begin
    BeginScope(sENVELOPE,sSOAP_ENV,sSOAP_ENV_ABR);
      AddScopeAttribute('xmlns:xsi',sXSI_NS);
      AddScopeAttribute('xmlns:'+sXSD, sXSD_NS);
      AddScopeAttribute('xmlns:'+sSOAP_ENC_ABR, sSOAP_ENC);
  end;
end;

function TSOAPBaseFormatter.ReadHeaders(ACallContext: ICallContext): Integer;

  function ExtractTypeInfo(ANode : TDOMElement) : TTypeRegistryItem;
  var
    j : Integer;
    ndName, nsSN, nsLN, s : string;
  begin
    ndName := ANode.NodeName;
    j := Pos(':',ndName);
    if ( j > 0 ) then
      nsSN := Copy(ndName,1,Pred(j))
    else
      nsSN := '';
    if IsStrEmpty(nsSN) then
      s := sXML_NS
    else
      s := sXML_NS + ':' + nsSN;
    nsLN := FindAttributeByNameInScope(s);
    Result := GetTypeRegistry().FindByDeclaredName(Copy(ndName,Succ(j),MaxInt),nsLN);
  end;

var
  i : Integer;
  nd : TDOMElement;
  typItm : TTypeRegistryItem;
  tmpObj : THeaderBlock;
  locName : string;
  chdLst : TDOMNodeList;
begin
  SetStyleAndEncoding(Document,Litteral);
  try
    Result := StackTop().ItemsCount;
    if ( Result > 0 ) then begin
      chdLst := StackTop().ScopeObject.ChildNodes;
      try
        for i := 0 to Pred(Result) do begin
          nd := chdLst.Item[i] as TDOMElement;
          typItm := ExtractTypeInfo(nd);
          if Assigned(typItm) then begin
            if ( typItm.DataType^.Kind = tkClass ) then begin
              tmpObj := nil;
              locName := nd.NodeName;
              Get(typItm.DataType,locName,tmpObj);
              if Assigned(tmpObj) then begin
                tmpObj.Direction := hdIn;
                ACallContext.AddHeader(tmpObj,True);
              end;
            end;
          end;
        end;
      finally
        chdLst.Release();
      end;
    end;
  finally
    RestoreStyleAndEncoding();
  end;
end;

function TSOAPBaseFormatter.WriteHeaders(ACallContext : ICallContext): Integer;
var
  ptyp : PTypeInfo;
  h : THeaderBlock;
  i, c : Integer;
begin
  Result := ACallContext.GetHeaderCount([hdOut]);
  if ( Result > 0 ) then begin
    BeginHeader();
    try
      c := ACallContext.GetHeaderCount(AllHeaderDirection);
      for i := 0 to Pred(c) do begin
        h := ACallContext.GetHeader(i);
        if ( h.Direction = hdOut ) then begin
          ptyp := PTypeInfo(h.ClassInfo);
          Put(GetTypeRegistry().ItemByTypeInfo[ptyp].DeclaredName,ptyp,h);
        end;
      end;
    finally
      EndHeader();
    end;
  end;
end;

procedure TSOAPBaseFormatter.EndScope();
begin
  CheckScope();
  FStack.Pop().Free();
end;

procedure TSOAPBaseFormatter.AddScopeAttribute(const AName, AValue: string);
begin
  CheckScope();
  GetCurrentScopeObject().SetAttribute(AName,AValue);
end;

procedure TSOAPBaseFormatter.Put(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
begin
  Case ATypeInfo^.Kind Of
    tkInt64, tkQWord :
      Begin
        int64Data := Int64(AData);
        PutInt64(AName,ATypeInfo,int64Data);
      End;
    tkLString, tkAString :
      Begin
        strData := String(AData);
        PutStr(AName,ATypeInfo,strData);
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
          otSLong,
          otULong : enumData := LongInt(AData);
        End;
        If ( ATypeInfo^.Kind = tkInteger ) Then
          PutInt64(AName,ATypeInfo,enumData)
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

procedure TSOAPBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
begin
  Case ATypeInfo^.Kind Of
    tkInt64,tkQWord :
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
          GetInt64(ATypeInfo,AName,enumData)
        Else
          GetEnum(ATypeInfo,AName,enumData);
        Case GetTypeData(ATypeInfo)^.OrdType Of
          otSByte : ShortInt(AData) := enumData;
          otUByte : Byte(AData) := enumData;
          otSWord : SmallInt(AData) := enumData;
          otUWord : Word(AData) := enumData;
          otSLong,
          otULong : LongInt(AData) := enumData;
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

procedure TSOAPBaseFormatter.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TSOAPBaseFormatter.LoadFromStream(AStream: TStream);
Var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := GetXmlDoc().DocumentElement;
  If Assigned(nd) Then
    PushStack(nd);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string);
begin
  Raise ESOAPException.Create(AMsg);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise ESOAPException.CreateFmt(AMsg,AArgs);
end;

end.
