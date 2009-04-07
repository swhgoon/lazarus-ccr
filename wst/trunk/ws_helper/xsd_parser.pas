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
unit xsd_parser;

interface
uses
  Classes, SysUtils,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  cursor_intf, rtti_filters,
  pastree, pascal_parser_intf, logger_intf;

type

  EXsdParserException = class(Exception)
  end;

  EXsdParserAssertException = class(EXsdParserException)
  end;

  EXsdTypeNotFoundException = class(EXsdParserException)
  end;

  EXsdInvalidDefinitionException = class(EXsdParserException)
  end;

  EXsdInvalidTypeDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  EXsdInvalidElementDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  TOnParserMessage = procedure (const AMsgType : TMessageType; const AMsg : string) of object;


  IParserContext = interface
    ['{F400BA9E-41AC-456C-ABF9-CEAA75313685}']
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : TwstPasTreeContainer;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    function GetTargetNameSpace() : string;
    function GetTargetModule() : TPasModule;
  end;

  IXsdPaser = interface
    ['{F0CEC726-A068-4CCC-B1E7-D31F018415B2}']
    function ParseType(const AName : string) : TPasType;
    procedure ParseTypes();
    procedure SetNotifier(ANotifier : TOnParserMessage);
  end;

  { TCustomXsdSchemaParser }

  TCustomXsdSchemaParser = class(TInterfacedObject, IInterface, IParserContext, IXsdPaser)
  private
    FDoc : TXMLDocument;
    FParentContext : Pointer;//IParserContext;
    FSymbols : TwstPasTreeContainer;
    FModuleName : string;
    FModule : TPasModule;
    FTargetNameSpace : string;
    FSchemaNode : TDOMNode;
  private
    FNameSpaceList : TStringList;
    FXSShortNames : TStrings;
    FChildCursor : IObjectCursor;
    FOnMessage: TOnParserMessage;
  private
    procedure DoOnMessage(const AMsgType : TMessageType; const AMsg : string);
  private
    function FindNamedNode(AList : IObjectCursor; const AName : WideString; const AOrder : Integer = 0):TDOMNode;
    function GetParentContext() : IParserContext;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Prepare();
    function FindElement(const AName: String) : TPasElement; {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : TwstPasTreeContainer;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpaceLocal(const ANameSpace : string) : TStrings;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    procedure SetNotifier(ANotifier : TOnParserMessage);
  public
    constructor Create(
      ADoc           : TXMLDocument;
      ASchemaNode    : TDOMNode;
      ASymbols       : TwstPasTreeContainer;
      AParentContext : IParserContext
    );
    destructor Destroy();override;
    function ParseType(const AName : string) : TPasType;
    procedure ParseTypes();

    function GetTargetNameSpace() : string;
    function GetTargetModule() : TPasModule;

    property SymbolTable : TwstPasTreeContainer read FSymbols;
    property Module : TPasModule read FModule;
    property OnMessage : TOnParserMessage read FOnMessage write FOnMessage;
  end;

  TXsdParser = class(TCustomXsdSchemaParser)
  public
    constructor Create(
            ADoc : TXMLDocument;
            ASymbols : TwstPasTreeContainer;
      const AModuleName : string;
      const ANotifier : TOnParserMessage = nil
    );
  end;

implementation
uses ws_parser_imp, dom_cursors, parserutils, xsd_consts
{$IFDEF FPC}
     ,wst_fpc_xml
{$ENDIF}
     ;

{ TCustomXsdSchemaParser }

constructor TCustomXsdSchemaParser.Create(
  ADoc           : TXMLDocument;
  ASchemaNode    : TDOMNode;
  ASymbols       : TwstPasTreeContainer;
  AParentContext : IParserContext
);
begin
  if ( ADoc = nil ) then
    raise EXsdParserAssertException.Create('Invalid DOM document.');
  if ( ASchemaNode = nil ) then
    raise EXsdParserAssertException.Create('Invalid schema node.');
  if ( ASymbols = nil ) then
    raise EXsdParserAssertException.Create('Invalid Symbol table.');
  if ( ASchemaNode = nil ) then
    raise EXsdParserAssertException.Create('Invalid schema node.');

  FDoc := ADoc;
  FParentContext := Pointer(AParentContext);
  FSymbols := ASymbols;
  FSchemaNode := ASchemaNode;

  FNameSpaceList := TStringList.Create();
  FNameSpaceList.Duplicates := dupError;
  FNameSpaceList.Sorted := True;

  Prepare();
end;

destructor TCustomXsdSchemaParser.Destroy();
var
  i : PtrInt;
begin
  FParentContext := nil;
  for i := 0 to Pred(FNameSpaceList.Count) do begin
    FNameSpaceList.Objects[i].Free();
  end;
  FreeAndNil(FNameSpaceList);
  inherited;
end;

procedure TCustomXsdSchemaParser.DoOnMessage(
  const AMsgType: TMessageType;
  const AMsg: string
);
begin
  if Assigned(FOnMessage) then begin
    FOnMessage(AMsgType,AMsg);
  end else if IsConsole and HasLogger() then begin
    GetLogger().Log(AMsgType, AMsg);
  end;
end;

function TCustomXsdSchemaParser.FindElement(const AName: String): TPasElement;
begin
  Result := SymbolTable.FindElementInModule(AName,FModule);
  if ( Result = nil ) then
    Result := SymbolTable.FindElement(AName);
end;

function TCustomXsdSchemaParser.FindNamedNode(
        AList : IObjectCursor;
  const AName : WideString;
  const AOrder : Integer
): TDOMNode;
var
  attCrs, crs : IObjectCursor;
  curObj : TDOMNodeRttiExposer;
  fltr : IObjectFilter;
  locOrder : Integer;
begin
  Result := nil;
  if Assigned(AList) then begin
    fltr := ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer);
    AList.Reset();
    locOrder := AOrder;
    while AList.MoveNext() do begin
      curObj := AList.GetCurrent() as TDOMNodeRttiExposer;
      attCrs := CreateAttributesCursor(curObj.InnerObject,cetRttiNode);
      if Assigned(attCrs) then begin
        crs := CreateCursorOn(attCrs,fltr);
        crs.Reset();
        if crs.MoveNext() and AnsiSameText(AName,TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue) then begin
          Dec(locOrder);
          if ( locOrder <= 0 ) then begin
            Result := curObj.InnerObject;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomXsdSchemaParser.FindNameSpace(
  const AShortName : string;
  out   AResult : string
) : Boolean;
var
  i : PtrInt;
  ls : TStrings;
begin
  AResult := '';
  Result := False;
  for i := 0 to Pred(FNameSpaceList.Count) do begin
    ls := FNameSpaceList.Objects[i] as TStrings;
    if ( ls.IndexOf(AShortName) >= 0 ) then begin
      AResult := FNameSpaceList[i];
      Result := True;
      Break;
    end;
  end;
  if not Result then
    Result := GetParentContext().FindNameSpace(AShortName,AResult);
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpace(const ANameSpace: string): TStrings;
var
  prtCtx : IParserContext;
begin
  Result := FindShortNamesForNameSpaceLocal(ANameSpace);
  if ( Result = nil ) then begin
    prtCtx := GetParentContext();
    if Assigned(prtCtx) then
      Result := prtCtx.FindShortNamesForNameSpace(ANameSpace);
  end;
end;

procedure TCustomXsdSchemaParser.SetNotifier(ANotifier: TOnParserMessage);
begin
  FOnMessage := ANotifier;
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpaceLocal(const ANameSpace: string): TStrings;
var
  i : PtrInt;
begin
  i := FNameSpaceList.IndexOf(ANameSpace);
  if ( i >= 0 ) then
    Result := FNameSpaceList.Objects[i] as TStrings
  else
    Result := nil;
end;

function TCustomXsdSchemaParser.GetParentContext() : IParserContext;
begin
  Result := IParserContext(FParentContext);
end;

function TCustomXsdSchemaParser.GetSymbolTable() : TwstPasTreeContainer;
begin
  Result := FSymbols;
end;

function TCustomXsdSchemaParser.GetTargetModule() : TPasModule;
begin
  Result := FModule;
end;

function TCustomXsdSchemaParser.GetTargetNameSpace() : string;
begin
  Result := FTargetNameSpace;
end;

function TCustomXsdSchemaParser.GetXsShortNames() : TStrings;
begin
  Result := FXSShortNames;
end;

function TCustomXsdSchemaParser.ParseType(const AName: string): TPasType;
var
  crsSchemaChild : IObjectCursor;
  typNd : TDOMNode;
  typName : string;
  embededType : Boolean;
  localTypeName : string;

  procedure Init();
  begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
  end;

  function ExtractTypeHint(AElement: TDOMNode): string;
  begin
    if not wst_findCustomAttributeXsd(FXSShortNames,AElement,s_WST_typeHint,Result) then
      Result := '';
  end;

  function FindTypeNode(out ASimpleTypeAlias : TPasType) : Boolean;
  var
    nd, oldTypeNode : TDOMNode;
    crs : IObjectCursor;
    locStrFilter, locTypeHint : string;
    locHintedType : TPasType;
  begin
    ASimpleTypeAlias := nil;
    Result := True;
    typNd := FindNamedNode(crsSchemaChild,localTypeName);
    if not Assigned(typNd) then
      raise EXsdTypeNotFoundException.CreateFmt('Type definition not found 1 : "%s"',[AName]);
    if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_element) then begin
      crs := CreateCursorOn(CreateAttributesCursor(typNd,cetRttiNode),ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        ASimpleTypeAlias := FindElement(ExtractNameFromQName(nd.NodeValue)) as TPasType;
        if Assigned(ASimpleTypeAlias) then begin
          if ASimpleTypeAlias.InheritsFrom(TPasNativeSimpleType) then begin
            locTypeHint := ExtractTypeHint(typNd);
            if not IsStrEmpty(locTypeHint) then begin
              locHintedType := FindElement(locTypeHint) as TPasType;
              if ( locHintedType <> nil ) then
                ASimpleTypeAlias := locHintedType;
            end;
          end;
          Result := False;
        end else begin
          oldTypeNode := typNd;
          typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue));
          if not Assigned(typNd) then
            raise EXsdTypeNotFoundException.CreateFmt('Type definition not found 2 : "%s"',[AName]);
          embededType := False;
          if ( typNd = oldTypeNode ) then begin
            typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue),2);
            if not Assigned(typNd) then
              raise EXsdTypeNotFoundException.CreateFmt('Type definition not found 2.1 : "%s"',[AName]);
          end;
        end;
      end else begin
        //locStrFilter := Format('%s = %s or %s = %s ',[s_NODE_NAME,QuotedStr(s_complexType),s_NODE_NAME,QuotedStr(s_simpleType)]);
        locStrFilter := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames);
        crs := CreateCursorOn(CreateChildrenCursor(typNd,cetRttiNode),ParseFilter(locStrFilter,TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          raise EXsdTypeNotFoundException.CreateFmt('Type definition not found 3 : "%s"',[AName]);
        end;
        typNd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        typName := ExtractNameFromQName(AName);
        embededType := True;
      end;
    end;
  end;

  function ParseComplexType():TPasType;
  var
    locParser : TComplexTypeParser;
  begin
    locParser := TComplexTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  function ParseSimpleType():TPasType;
  var
    locParser : TSimpleTypeParser;
  begin
    locParser := TSimpleTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  function CreateTypeAlias(const ABase : TPasType): TPasType;
  var
    hasInternameName : Boolean;
    internameName : string;
  begin
    internameName := ExtractNameFromQName(AName);
    hasInternameName := IsReservedKeyWord(internameName) or
                           ( not IsValidIdent(internameName) );
    if hasInternameName then begin
      internameName := '_' + internameName;
    end;
    Result := TPasType(SymbolTable.CreateElement(TPasAliasType,internameName,SymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    TPasAliasType(Result).DestType := ABase;
    ABase.AddRef();
  end;

  function CreateUnresolveType(): TPasType;
  var
    hasInternameName : Boolean;
    internameName : string;
  begin
    internameName := ExtractNameFromQName(AName);
    hasInternameName := IsReservedKeyWord(internameName) or
                           ( not IsValidIdent(internameName) );
    if hasInternameName then begin
      internameName := '_' + internameName;
    end;
    Result := TPasUnresolvedTypeRef(SymbolTable.CreateElement(TPasUnresolvedTypeRef,internameName,SymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    if not AnsiSameText(internameName,AName) then
      SymbolTable.RegisterExternalAlias(Result,AName);
  end;

var
  frwType, aliasType : TPasType;
  sct : TPasSection;
  shortNameSpace, longNameSpace : string;
  typeModule : TPasModule;
begin
  sct := nil;
  DoOnMessage(mtInfo, Format('Parsing "%s" ...',[AName]));
  try
    embededType := False;
    aliasType := nil;
    ExplodeQName(AName,localTypeName,shortNameSpace);
    if IsStrEmpty(shortNameSpace) then begin
      typeModule := FModule;
    end else begin
      if not FindNameSpace(shortNameSpace,longNameSpace) then
        raise EXsdParserAssertException.CreateFmt('Unable to resolve namespace, short name = "%s".',[shortNameSpace]);
      typeModule := SymbolTable.FindModule(longNameSpace);
    end;
    if ( typeModule = nil ) then
      raise EXsdTypeNotFoundException.Create(AName);
    Result := SymbolTable.FindElementInModule(localTypeName,typeModule) as TPasType;
    if ( ( Result = nil ) or Result.InheritsFrom(TPasUnresolvedTypeRef) ) and
       ( typeModule = FModule )
    then begin
      sct := FModule.InterfaceSection;
      frwType := Result;
      Result := nil;
      Init();
      if FindTypeNode(aliasType) then begin
        if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_complexType) then begin
          Result := ParseComplexType();
        end else if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_simpleType) then begin
          Result := ParseSimpleType();
        end;
        if Assigned(Result) then begin
          if Assigned(frwType) and AnsiSameText(SymbolTable.GetExternalName(Result),SymbolTable.GetExternalName(frwType)) then begin
            Result.Name := frwType.Name;
            SymbolTable.RegisterExternalAlias(Result,SymbolTable.GetExternalName(frwType));
          end;
        end else begin
          raise EXsdTypeNotFoundException.CreateFmt('Type node found but unable to parse it : "%s"',[AName]);
        end;
      end else begin
        Result := CreateTypeAlias(aliasType);
      end;
      if ( frwType <> nil ) then begin
        sct.Declarations.Extract(frwType);
        sct.Types.Extract(frwType);
        frwType.Release();
      end;
      sct.Declarations.Add(Result);
      sct.Types.Add(Result);
      if Result.InheritsFrom(TPasClassType) then begin
        sct.Classes.Add(Result);
      end;
    end;
  except
    on e : EXsdTypeNotFoundException do begin
      Result := CreateUnresolveType();
      if ( sct = nil ) then
        sct := FModule.InterfaceSection;
      sct.Declarations.Add(Result);
      sct.Types.Add(Result);
    end;
  end;
end;

procedure TCustomXsdSchemaParser.ParseTypes();
var
  crsSchemaChild, typTmpCrs : IObjectCursor;
  typFilterStr : string;
  typNode : TDOMNode;
begin
  if Assigned(FChildCursor) then begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
    typFilterStr := Format(
                      '%s or %s or %s',
                      [ CreateQualifiedNameFilterStr(s_complexType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_element,FXSShortNames)
                      ]
                    );
    crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(typFilterStr,TDOMNodeRttiExposer));
    crsSchemaChild.Reset();
    while crsSchemaChild.MoveNext() do begin
      typNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      typTmpCrs := CreateAttributesCursor(typNode,cetRttiNode);
      if Assigned(typTmpCrs) then begin
        typTmpCrs.Reset();
        typTmpCrs := CreateCursorOn(typTmpCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
        typTmpCrs.Reset();
        if typTmpCrs.MoveNext() then begin
          ParseType(
            (typTmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue
          );
        end;
      end;
    end;
  end;
end;

procedure TCustomXsdSchemaParser.Prepare();
var
  locAttCursor : IObjectCursor;
  prntCtx : IParserContext;
  nd : TDOMNode;
  i : PtrInt;
  ls : TStrings;
begin
  if ( FSchemaNode.Attributes = nil ) or ( GetNodeListCount(FSchemaNode.Attributes) = 0 ) then
    raise EXsdParserAssertException.CreateFmt('The Schema node has at least the "%s" attribute.',[s_targetNamespace]);
  nd := FSchemaNode.Attributes.GetNamedItem(s_targetNamespace);
  if ( nd = nil ) then
    raise EXsdParserAssertException.CreateFmt('The Schema node has at least the "%s" attribute.',[s_targetNamespace]);
  FTargetNameSpace := nd.NodeValue;
  if IsStrEmpty(FModuleName) then
    FModuleName := ExtractIdentifier(FTargetNameSpace);
  if ( SymbolTable.FindModule(s_xs) = nil ) then begin
    CreateWstInterfaceSymbolTable(SymbolTable);
  end;
  FChildCursor := CreateChildrenCursor(FSchemaNode,cetRttiNode);

  locAttCursor := CreateAttributesCursor(FSchemaNode,cetRttiNode);
  BuildNameSpaceList(locAttCursor,FNameSpaceList);
  FXSShortNames := FindShortNamesForNameSpaceLocal(s_xs);
  prntCtx := GetParentContext();
  if ( FXSShortNames = nil ) then begin
    if ( prntCtx = nil ) then
      raise EXsdParserAssertException.CreateFmt('Invalid Schema document, namespace not found :'#13'%s.',[s_xs]);
    FXSShortNames := prntCtx.FindShortNamesForNameSpace(s_xs);
    if ( FXSShortNames = nil ) then
      raise EXsdParserAssertException.CreateFmt('Invalid Schema document, namespace not found ( short names ) :'#13'%s.',[s_xs]);
  end;

  if Assigned(prntCtx) then begin
    for i:= 0 to Pred(FNameSpaceList.Count) do begin
      ls := prntCtx.FindShortNamesForNameSpace(FNameSpaceList[i]);
      if Assigned(ls) then
        (FNameSpaceList.Objects[i] as TStrings).AddStrings(ls);
    end;
  end;

  FModule := SymbolTable.FindModule(FTargetNameSpace);
  if ( FModule = nil ) then begin
    FModule := TPasModule(SymbolTable.CreateElement(TPasModule,FModuleName,SymbolTable.Package,visDefault,'',0));
    SymbolTable.Package.Modules.Add(FModule);
    SymbolTable.RegisterExternalAlias(FModule,FTargetNameSpace);
    FModule.InterfaceSection := TPasSection(SymbolTable.CreateElement(TPasSection,'',FModule,visDefault,'',0));
  end;
end;

{ TXsdParser }

constructor TXsdParser.Create(
        ADoc : TXMLDocument;
        ASymbols : TwstPasTreeContainer;
  const AModuleName : string;
  const ANotifier : TOnParserMessage
);
var
  locName : string;
begin
  inherited Create(ADoc,ADoc.DocumentElement,ASymbols,nil);
  if Assigned(ANotifier) then
    FOnMessage := ANotifier;
  if not IsStrEmpty(AModuleName) then begin
    locName := ExtractIdentifier(AModuleName);
    if not IsStrEmpty(locName) then begin
      FModuleName := locName;
      Module.Name := FModuleName;
    end;
  end;
end;

end.
