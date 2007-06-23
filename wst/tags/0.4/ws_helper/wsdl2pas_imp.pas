unit wsdl2pas_imp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM,
  parserdefs, cursor_intf, rtti_filters;

type

  EWslParserException = class(Exception)
  end;

  TWsdlParser = class;

  TAbstractTypeParserClass = class of TAbstractTypeParser;
  { TAbstractTypeParser }

  TAbstractTypeParser = class
  private
    FOwner : TWsdlParser;
    FTypeNode : TDOMNode;
    FSymbols : TSymbolTable;
    FTypeName : string;
    FEmbededDef : Boolean;
  public
    constructor Create(
            AOwner       : TWsdlParser;
            ATypeNode    : TDOMNode;
            ASymbols     : TSymbolTable;
      const ATypeName    : string;
      const AEmbededDef  : Boolean
    );
    class function ExtractEmbeddedTypeFromElement(
            AOwner       : TWsdlParser;
            AEltNode     : TDOMNode;
            ASymbols     : TSymbolTable;
      const ATypeName    : string
    ) : TTypeDefinition;
    class function GetParserSupportedStyle():string;virtual;abstract;
    class procedure RegisterParser(AParserClass : TAbstractTypeParserClass);
    class function GetRegisteredParserCount() : Integer;
    class function GetRegisteredParser(const AIndex : Integer):TAbstractTypeParserClass;
    function Parse():TTypeDefinition;virtual;abstract;
  end;
  
  TDerivationMode = ( dmNone, dmExtension, dmRestriction );
  TSequenceType = ( stElement, stAll );

  { TComplexTypeParser }

  TComplexTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FContentNode : TDOMNode;
    FContentType : string;
    FBaseType : TTypeDefinition;
    FDerivationMode : TDerivationMode;
    FDerivationNode : TDOMNode;
    FSequenceType : TSequenceType;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    procedure ExtractContentType();
    procedure ExtractBaseType();
    function ParseComplexContent(const ATypeName : string):TTypeDefinition;
    function ParseSimpleContent(const ATypeName : string):TTypeDefinition;
    function ParseEmptyContent(const ATypeName : string):TTypeDefinition;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TTypeDefinition;override;
  end;

  { TSimpleTypeParser }

  TSimpleTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FBaseName : string;
    FRestrictionNode : TDOMNode;
    FIsEnum : Boolean;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    function ExtractContentType() : Boolean;
    function ParseEnumContent():TTypeDefinition;
    function ParseOtherContent():TTypeDefinition;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TTypeDefinition;override;
  end;

  TParserMode = ( pmUsedTypes, pmAllTypes );
  
  { TWsdlParser }

  TWsdlParser = class
  private
    FDoc : TXMLDocument;
    FSymbols : TSymbolTable;
  private
    FWsdlShortNames : TStringList;
    FSoapShortNames : TStringList;
    FXSShortNames : TStringList;
    FChildCursor : IObjectCursor;
    FServiceCursor : IObjectCursor;
    FBindingCursor : IObjectCursor;
    FPortTypeCursor : IObjectCursor;
    FMessageCursor : IObjectCursor;
    FTypesCursor : IObjectCursor;
    FSchemaCursor : IObjectCursor;
  private
    function CreateWsdlNameFilter(const AName : WideString):IObjectFilter;
    function FindNamedNode(AList : IObjectCursor; const AName : WideString):TDOMNode;
    procedure Prepare();
    procedure ParseService(ANode : TDOMNode);
    procedure ParsePort(ANode : TDOMNode);
    function ParsePortType(
      ANode, ABindingNode : TDOMNode
    ) : TInterfaceDefinition;
    function ParseOperation(
            AOwner : TInterfaceDefinition;
            ANode  : TDOMNode;
      const ASoapBindingStyle : string
    ) : TMethodDefinition;
    function ParseType(const AName, ATypeOrElement : string) : TTypeDefinition;
    procedure ParseTypes();
  public
    constructor Create(ADoc : TXMLDocument; ASymbols : TSymbolTable);
    destructor Destroy();override;
    procedure Parse(const AMode : TParserMode);
    property SymbolTable : TSymbolTable read FSymbols;
  end;
  
  
implementation
uses dom_cursors, parserutils, StrUtils, Contnrs;

const
  s_address                    : WideString = 'address';
  s_all                        : WideString = 'all';
  //s_any                        : WideString = 'any';
  s_array                      : WideString = 'array';
  s_arrayType                  : WideString = 'arrayType';
  s_attribute                  : WideString = 'attribute';
  s_base                       : WideString = 'base';
  s_binding                    : WideString = 'binding';
  s_body                       : WideString = 'body';
  s_complexContent             : WideString = 'complexContent';
  s_complexType                : WideString = 'complexType';
  s_document                   : WideString = 'document';
  s_element                    : WideString = 'element';
  s_enumeration                : WideString = 'enumeration';
  s_extension                  : WideString = 'extension';
  s_input                      : WideString = 'input';
  s_item                       : WideString = 'item';
  s_location                   : WideString = 'location';
  s_message                    : WideString = 'message';
  s_maxOccurs                  : WideString = 'maxOccurs';
  s_minOccurs                  : WideString = 'minOccurs';
  s_name                       : WideString = 'name';
  s_operation                  : WideString = 'operation';
  s_optional                   : WideString = 'optional';
  s_output                     : WideString = 'output';
  s_part                       : WideString = 'part';
  s_port                       : WideString = 'port';
  s_portType                   : WideString = 'portType';
  s_prohibited                 : WideString = 'prohibited';
  s_ref                        : WideString = 'ref';
  s_required                   : WideString = 'required';
  s_restriction                : WideString = 'restriction';
  //s_return                     : WideString = 'return';
  s_rpc                        : WideString = 'rpc';
  s_schema                     : WideString = 'schema';
  s_xs                         : WideString = 'http://www.w3.org/2001/XMLSchema';
  s_sequence                   : WideString = 'sequence';
  s_service                    : WideString = 'service';
  s_simpleContent              : WideString = 'simpleContent';
  s_simpleType                 : WideString = 'simpleType';
  s_soap                       : WideString = 'http://schemas.xmlsoap.org/wsdl/soap/';
  s_soapAction                 : WideString = 'soapAction';
  s_soapInputEncoding          : WideString = 'Input_EncodingStyle';
  s_soapOutputEncoding         : WideString = 'OutputEncodingStyle';
  s_soapStyle                  : WideString = 'style';
  s_style                      : WideString = 'style';
  s_targetNamespace            : WideString = 'targetNamespace';
  s_type                       : WideString = 'type';
  s_types                      : WideString = 'types';
  s_unbounded                  : WideString = 'unbounded';
  s_use                        : WideString = 'use';
  s_value                      : WideString = 'value';
  s_wsdl                       : WideString = 'http://schemas.xmlsoap.org/wsdl/';
  s_xmlns                      : WideString = 'xmlns';

  //----------------------------------------------------------
  s_NODE_NAME = 'NodeName';
  s_NODE_VALUE = 'NodeValue';
  s_TRANSPORT  = 'TRANSPORT';
  s_FORMAT     = 'FORMAT';

type TCursorExposedType = ( cetRttiNode, cetDomNode );
function CreateAttributesCursor(ANode : TDOMNode; const AExposedType : TCursorExposedType):IObjectCursor;
begin
  Result := nil;
  if ( ANode <> nil ) and ( ANode.Attributes <> nil ) and ( ANode.Attributes.Length > 0 ) then begin
    Result := TDOMNamedNodeMapCursor.Create(ANode.Attributes,faNone) ;
    if ( AExposedType = cetRttiNode ) then
      Result := TDOMNodeRttiExposerCursor.Create(Result);
  end;
end;

function CreateChildrenCursor(ANode : TDOMNode; const AExposedType : TCursorExposedType):IObjectCursor;
begin
  Result := nil;
  if ( ANode <> nil ) and ANode.HasChildNodes() then begin
    Result := TDOMNodeListCursor.Create(ANode.GetChildNodes(),faFreeOnDestroy) ;
    if ( AExposedType = cetRttiNode ) then
      Result := TDOMNodeRttiExposerCursor.Create(Result);
  end;
end;

function ExtractNameFromQName(const AQName : string):string ;
var
  i : Integer;
begin
  Result := Trim(AQName);
  i := Pos(':',Result);
  if ( i > 0 ) then
    Result := Copy(Result,( i + 1 ), MaxInt);
end;

function CreateQualifiedNameFilterStr(
  const AName        : WideString;
        APrefixList  : TStrings
) : string;
var
  k : Integer;
  locStr : string;
  locWStr : WideString;
begin
  Result := '';
  if ( APrefixList.Count > 0 ) then begin
    for k := 0 to Pred(APrefixList.Count) do begin
      if IsStrEmpty(APrefixList[k]) then begin
        locWStr := ''
      end else begin
        locWStr := APrefixList[k] + ':';
      end;
      locWStr := locWStr + AName;
      locStr := s_NODE_NAME;
      Result := Result + ' or ' + locStr + ' = ' + QuotedStr(locWStr);
    end;
    if ( Length(Result) > 0 ) then begin
      Delete(Result,1,Length(' or'));
    end;
  end else begin
    Result := Format('%s = %s',[s_NODE_NAME,QuotedStr(AName)]);
  end;
end;

{ TWsdlParser }

function TWsdlParser.CreateWsdlNameFilter(const AName: WideString): IObjectFilter;
begin
  Result := ParseFilter(CreateQualifiedNameFilterStr(AName,FWsdlShortNames),TDOMNodeRttiExposer);
end;

function TWsdlParser.FindNamedNode(
        AList : IObjectCursor;
  const AName : WideString
): TDOMNode;
var
  attCrs, crs : IObjectCursor;
  curObj : TDOMNodeRttiExposer;
  fltr : IObjectFilter;
begin
  Result := nil;
  if Assigned(AList) then begin
    fltr := ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer);
    AList.Reset();
    while AList.MoveNext() do begin
      curObj := AList.GetCurrent() as TDOMNodeRttiExposer;
      attCrs := CreateAttributesCursor(curObj.InnerObject,cetRttiNode);
      if Assigned(attCrs) then begin
        crs := CreateCursorOn(attCrs,fltr);
        crs.Reset();
        if crs.MoveNext() and AnsiSameText(AName,TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue) then begin
          Result := curObj.InnerObject;
          exit;
        end;
      end;
    end;
  end;
end;

type
  TNotFoundAction = ( nfaNone, nfaRaiseException );
procedure ExtractNameSpaceShortNames(
        AAttribCursor   : IObjectCursor;
        AResList        : TStrings;
  const ANameSpace      : WideString;
  const ANotFoundAction : TNotFoundAction;
  const AClearBefore    : Boolean
);
var
  crs : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  wStr : WideString;
  i : Integer;
begin
  if AClearBefore then begin
    AResList.Clear();
  end;
  AAttribCursor.Reset();
  crs := CreateCursorOn(AAttribCursor,ParseFilter(Format('%s=%s',[s_NODE_VALUE,QuotedStr(ANameSpace)]),TDOMNodeRttiExposer));
  crs.Reset();
  if crs.MoveNext() then begin
    repeat
      locObj := crs.GetCurrent() as TDOMNodeRttiExposer;
      wStr := Trim(locObj.NodeName);
      i := AnsiPos(s_xmlns + ':',wStr);
      if ( i > 0 ) then begin
        i := AnsiPos(':',wStr);
        AResList.Add(Copy(wStr,( i + 1 ), MaxInt));
      end else begin
        if ( AResList.IndexOf('') = -1 ) then
          AResList.Add('');
      end;
    until not crs.MoveNext();
  end else begin
    if ( ANotFoundAction = nfaRaiseException ) then begin
      raise EWslParserException.CreateFmt('Namespace not found : "%s"',[ANameSpace]);
    end;
  end;
end;

procedure ExtractNameSpaceShortNamesNested(
        ANode         : TDOMNode;
        AResList      : TStrings;
  const ANameSpace    : WideString
);
var
  nd : TDOMNode;
begin
  AResList.Clear();
  nd := ANode;
  while Assigned(nd) do begin
    if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
      ExtractNameSpaceShortNames(CreateAttributesCursor(nd,cetRttiNode),AResList,ANameSpace,nfaNone,False);
    end;
    nd := nd.ParentNode;
  end;
end;

procedure TWsdlParser.Prepare();
var
  locAttCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
begin
  FPortTypeCursor := nil;
  FWsdlShortNames.Clear();
  locAttCursor := CreateAttributesCursor(FDoc.DocumentElement,cetRttiNode);

  FChildCursor := TDOMNodeListCursor.Create(FDoc.DocumentElement.GetChildNodes,faFreeOnDestroy) ;
  FChildCursor := TDOMNodeRttiExposerCursor.Create(FChildCursor);

  ExtractNameSpaceShortNames(locAttCursor,FWsdlShortNames,s_wsdl,nfaRaiseException,True);
  ExtractNameSpaceShortNames(locAttCursor,FSoapShortNames,s_soap,nfaRaiseException,False);
  ExtractNameSpaceShortNames(locAttCursor,FXSShortNames,s_xs,nfaNone,True);

  FServiceCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_service,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FServiceCursor.Reset();
  
  FBindingCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_binding,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FBindingCursor.Reset();

  FPortTypeCursor := CreateCursorOn(
                       FChildCursor.Clone() as IObjectCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_portType,FWsdlShortNames),TDOMNodeRttiExposer)
                     );
  FPortTypeCursor.Reset();

  FSchemaCursor := nil;
  FTypesCursor := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_types,FWsdlShortNames),TDOMNodeRttiExposer)
                  );
  FTypesCursor.Reset();
  if FTypesCursor.MoveNext() then begin
    locObj := FTypesCursor.GetCurrent() as TDOMNodeRttiExposer;
    if locObj.InnerObject.HasChildNodes() then begin
      FSchemaCursor := CreateChildrenCursor(locObj.InnerObject,cetRttiNode);
      FSchemaCursor.Reset();
      FSchemaCursor := CreateCursorOn(
                         FSchemaCursor,//.Clone() as IObjectCursor,
                         ParseFilter(CreateQualifiedNameFilterStr(s_schema,FXSShortNames),TDOMNodeRttiExposer)
                       );
      FSchemaCursor.Reset();
    end;
  end;

  FMessageCursor := CreateCursorOn(
                      FChildCursor.Clone() as IObjectCursor,
                      ParseFilter(CreateQualifiedNameFilterStr(s_message,FWsdlShortNames),TDOMNodeRttiExposer)
                    );
  FMessageCursor.Reset();
end;

procedure TWsdlParser.ParseService(ANode: TDOMNode);
var
  locCursor, locPortCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
begin
  locCursor := CreateChildrenCursor(ANode,cetRttiNode);
  if Assigned(locCursor) then begin
    locPortCursor := CreateCursorOn(
                       locCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_port,FWsdlShortNames),TDOMNodeRttiExposer)
                     );
    locPortCursor.Reset();
    while locPortCursor.MoveNext() do begin
      locObj := locPortCursor.GetCurrent() as TDOMNodeRttiExposer;
      ParsePort(locObj.InnerObject);
    end;
  end;
end;

procedure TWsdlParser.ParsePort(ANode: TDOMNode);

  function FindBindingNode(const AName : WideString):TDOMNode;
  var
    crs : IObjectCursor;
  begin
    Result := FindNamedNode(FBindingCursor,AName);
    if Assigned(Result) then begin
      crs := CreateChildrenCursor(Result,cetRttiNode);
      if Assigned(crs) then begin
        crs := CreateCursorOn(crs,ParseFilter(CreateQualifiedNameFilterStr(s_binding,FSoapShortNames),TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          Result := nil;
        end;
      end else begin
        Result := nil;
      end;
    end;
  end;
  
  function ExtractBindingQName(out AName : WideString):Boolean ;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    attCrs := CreateAttributesCursor(ANode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_binding)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function ExtractTypeQName(ABndgNode : TDOMNode; out AName : WideString):Boolean ;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    attCrs := CreateAttributesCursor(ABndgNode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function FindTypeNode(const AName : WideString):TDOMNode;
  begin
    Result := FindNamedNode(FPortTypeCursor,AName);
  end;

  function ExtractAddress() : string;
  var
    tmpCrs : IObjectCursor;
    nd : TDOMNode;
  begin
    Result := '';
    if ANode.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(ANode,cetRttiNode),
                  ParseFilter(CreateQualifiedNameFilterStr(s_address,FSoapShortNames),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        tmpCrs := CreateCursorOn(
                    CreateAttributesCursor(nd,cetRttiNode),
                    ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_location)]),TDOMNodeRttiExposer)
                  );
        if Assigned(tmpCrs) and tmpCrs.MoveNext() then begin
          Result := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        end;
      end;
    end;
  end;
  
var
  bindingName, typeName : WideString;
  i : Integer;
  bindingNode, typeNode : TDOMNode;
  intfDef : TInterfaceDefinition;
begin
  if ExtractBindingQName(bindingName) then begin
    i := Pos(':',bindingName);
    bindingName := Copy(bindingName,( i + 1 ), MaxInt);
    bindingNode := FindBindingNode(bindingName);
    if Assigned(bindingNode) then begin
      if ExtractTypeQName(bindingNode,typeName) then begin
        i := Pos(':',typeName);
        typeName := Copy(typeName,( i + 1 ), MaxInt);
        typeNode := FindTypeNode(typeName);
        if Assigned(typeNode) then begin
          intfDef := ParsePortType(typeNode,bindingNode);
          intfDef.Address := ExtractAddress();
        end;
      end;
    end;
  end;
end;

function StrToBindingStyle(const AStr : string):TBindingStyle;
begin
  if IsStrEmpty(AStr) then begin
    Result := bsDocument;
  end else if AnsiSameText(AStr,s_document) then begin
    Result := bsDocument;
  end else if AnsiSameText(AStr,s_rpc) then begin
    Result := bsRPC;
  end else begin
    Result := bsUnknown;
  end;
end;

function TWsdlParser.ParsePortType(ANode, ABindingNode : TDOMNode) : TInterfaceDefinition;

  function ExtractSoapBindingStyle(out AName : WideString):Boolean ;
  var
    childrenCrs, crs, attCrs : IObjectCursor;
    s : string;
  begin
    AName := '';
    Result := False;
    childrenCrs := CreateChildrenCursor(ABindingNode,cetRttiNode);
    if Assigned(childrenCrs) then begin
      s := CreateQualifiedNameFilterStr(s_binding,FSoapShortNames);
      crs := CreateCursorOn(childrenCrs,ParseFilter(s,TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        attCrs := CreateAttributesCursor(TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject,cetRttiNode);
        if Assigned(attCrs) then begin
          s := s_NODE_NAME + ' = ' + QuotedStr(s_style);
          crs := CreateCursorOn(attCrs,ParseFilter(s,TDOMNodeRttiExposer));
          crs.Reset();
          if crs.MoveNext() then begin
            AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
            Result := True;
            exit;
          end;
        end;
      end;
    end;
  end;
  
  function ExtractBindingOperationCursor() : IObjectCursor ;
  begin
    Result := nil;
    if ABindingNode.HasChildNodes() then begin
      Result := CreateCursorOn(
                  CreateChildrenCursor(ABindingNode,cetRttiNode),
                  ParseFilter(CreateQualifiedNameFilterStr(s_operation,FWsdlShortNames),TDOMNodeRttiExposer)
                );
    end;
  end;
  
  procedure ParseOperation_EncodingStyle(ABndngOpCurs : IObjectCursor; AOp : TMethodDefinition);
  var
    nd, ndSoap : TDOMNode;
    tmpCrs, tmpSoapCrs, tmpXcrs : IObjectCursor;
    in_out_count : Integer;
    strBuffer : string;
  begin
    nd := FindNamedNode(ABndngOpCurs,AOp.ExternalName);
    if Assigned(nd) and nd.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(
                   CreateQualifiedNameFilterStr(s_input,FWsdlShortNames) + ' or ' +
                     CreateQualifiedNameFilterStr(s_output,FWsdlShortNames)
                   ,
                   TDOMNodeRttiExposer
                  )
                );
      tmpCrs.Reset();
      in_out_count := 0;
      while tmpCrs.MoveNext() and ( in_out_count < 2 ) do begin
        Inc(in_out_count);
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if nd.HasChildNodes() then begin
          tmpSoapCrs := CreateCursorOn(
                          CreateChildrenCursor(nd,cetRttiNode),
                          ParseFilter(CreateQualifiedNameFilterStr(s_body,FSoapShortNames),TDOMNodeRttiExposer)
                        );
          tmpSoapCrs.Reset();
          if tmpSoapCrs.MoveNext() then begin
            ndSoap := (tmpSoapCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
            if Assigned(ndSoap.Attributes) and ( ndSoap.Attributes.Length > 0 ) then begin
              tmpXcrs := CreateCursorOn(
                          CreateAttributesCursor(ndSoap,cetRttiNode),
                          ParseFilter(
                            Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),
                            TDOMNodeRttiExposer
                          )
                        );
              tmpXcrs.Reset();
              if tmpXcrs.MoveNext() then begin
                if AnsiSameText(s_input,ExtractNameFromQName(nd.NodeName)) then begin
                  strBuffer := s_soapInputEncoding;
                end else begin
                  strBuffer := s_soapOutputEncoding;
                end;
                AOp.Properties.Values[s_FORMAT + '_' + strBuffer] := (tmpXcrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject.NodeValue;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ParseOperationAttributes(ABndngOpCurs : IObjectCursor; AOp : TMethodDefinition);
  var
    nd : TDOMNode;
    tmpCrs : IObjectCursor;
  begin
    nd := FindNamedNode(ABndngOpCurs,AOp.ExternalName);
    if Assigned(nd) and nd.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(CreateQualifiedNameFilterStr(s_operation,FSoapShortNames),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
          tmpCrs := CreateCursorOn(
                      CreateAttributesCursor(nd,cetRttiNode),
                      ParseFilter(
                        Format( '%s = %s or %s = %s',
                                [ s_NODE_NAME,QuotedStr(s_soapAction),
                                  s_NODE_NAME,QuotedStr(s_style)
                                ]
                        ),
                        TDOMNodeRttiExposer
                      )
                    );
          tmpCrs.Reset();
          if tmpCrs.MoveNext() then begin
            nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
            if AnsiSameText(nd.NodeName,s_style) then begin
              AOp.Properties.Values[s_soapStyle] := nd.NodeValue;
            end else if AnsiSameText(nd.NodeName,s_soapAction) then begin
              AOp.Properties.Values[s_TRANSPORT + '_' + s_soapAction] := nd.NodeValue;
            end;
          end;
        end;
      end;
      ParseOperation_EncodingStyle(ABndngOpCurs,AOp);
    end;
  end;

var
  locIntf : TInterfaceDefinition;
  locAttCursor : IObjectCursor;
  locCursor, locOpCursor, locBindingOperationCursor : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  locSoapBindingStyle : string;
  locWStrBuffer : WideString;
  locMthd : TMethodDefinition;
  inft_guid : TGuid;
begin
  locAttCursor := CreateAttributesCursor(ANode,cetRttiNode);
  locCursor := CreateCursorOn(locAttCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
  locCursor.Reset();
  if not locCursor.MoveNext() then
    raise EWslParserException.CreateFmt('PortType Attribute not found : "%s"',[s_name]);
  locObj := locCursor.GetCurrent() as TDOMNodeRttiExposer;
  locIntf := TInterfaceDefinition.Create(locObj.NodeValue);
  try
    FSymbols.Add(locIntf);
  except
    FreeAndNil(locIntf);
    raise;
  end;
  Result := locIntf;
  if ( CreateGUID(inft_guid) = 0 ) then
    locIntf.InterfaceGUID := GUIDToString(inft_guid);
  locCursor := CreateChildrenCursor(ANode,cetRttiNode);
  if Assigned(locCursor) then begin
    locOpCursor := CreateCursorOn(locCursor,ParseFilter(CreateQualifiedNameFilterStr(s_operation,FWsdlShortNames),TDOMNodeRttiExposer));
    locOpCursor.Reset();
    ExtractSoapBindingStyle(locWStrBuffer);
    locSoapBindingStyle := locWStrBuffer;
    locIntf.BindingStyle := StrToBindingStyle(locSoapBindingStyle);
    locBindingOperationCursor := ExtractBindingOperationCursor();
    while locOpCursor.MoveNext() do begin
      locObj := locOpCursor.GetCurrent() as TDOMNodeRttiExposer;
      locMthd := ParseOperation(locIntf,locObj.InnerObject,locSoapBindingStyle);
      if Assigned(locMthd) then begin
        ParseOperationAttributes(locBindingOperationCursor,locMthd);
      end;
    end;
  end;
end;

type

  TParamDefCrack = class(TParameterDefinition);

  TMethodDefinitionCrack = class(TMethodDefinition);

  TTypeDefinitionCrack = class(TTypeDefinition);
  
function TWsdlParser.ParseOperation(
        AOwner : TInterfaceDefinition;
        ANode  : TDOMNode;
  const ASoapBindingStyle : string
) : TMethodDefinition;

  function ExtractOperationName(out AName : string):Boolean;
  var
    attCrs, crs : IObjectCursor;
  begin
    Result := False;
    attCrs := CreateAttributesCursor(ANode,cetRttiNode);
    if Assigned(attCrs) then begin
      crs := CreateCursorOn(attCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(s_name) ,TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
        Result := True;
        exit;
      end;
    end;
  end;

  function ExtractMsgName(const AMsgType : string; out AName : string) : Boolean;
  var
    chldCrs, crs : IObjectCursor;
  begin
    chldCrs := CreateChildrenCursor(ANode,cetRttiNode);
    if ( chldCrs <> nil ) then begin
      //crs := CreateCursorOn(chldCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(AMsgType) ,TDOMNodeRttiExposer));
      crs := CreateCursorOn(chldCrs,CreateWsdlNameFilter(AMsgType));
      crs.Reset();
      if crs.MoveNext() then begin
        chldCrs := CreateAttributesCursor(TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject,cetRttiNode);
        if ( chldCrs <> nil ) then begin
          crs := CreateCursorOn(chldCrs,ParseFilter(s_NODE_NAME + '=' + QuotedStr(s_message) ,TDOMNodeRttiExposer));
          crs.Reset();
          if crs.MoveNext() then begin
            AName := TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue;
            Result := True;
            exit;
          end;
        end;
      end;
    end;
    Result := False;
  end;
  
  function FindMessageNode(const AName : string) : TDOMNode;
  begin
    Result := FindNamedNode(FMessageCursor.Clone() as IObjectCursor,ExtractNameFromQName(AName));
  end;
  
  function CreatePartCursor(AMsgNode : TDOMNode):IObjectCursor ;
  begin
    Result := CreateChildrenCursor(AMsgNode,cetRttiNode);
    if Assigned(Result) then
      Result := CreateCursorOn(Result,CreateWsdlNameFilter(s_part));
  end;
  
  function GetDataType(const AName, ATypeOrElement : string):TTypeDefinition;
  begin
    try
      Result := ParseType(AName,ATypeOrElement);
    except
      on e : Exception do begin
        WriteLn(e.Message + ' ' + AName + ' ' + ATypeOrElement);
      end;
    end;
  end;
  
  procedure ExtractMethod(
    const AMthdName : string;
    out   AMthd     : TMethodDefinition
  );
  var
    tmpMthd : TMethodDefinition;
    
    procedure ParseInputMessage();
    var
      inMsg, strBuffer : string;
      inMsgNode, tmpNode : TDOMNode;
      crs, tmpCrs : IObjectCursor;
      prmName, prmTypeName, prmTypeType, prmTypeInternalName : string;
      prmInternameName : string;
      prmHasInternameName : Boolean;
      prmDef : TParameterDefinition;
      prmTypeDef : TTypeDefinition;
    begin
      if ExtractMsgName(s_input,inMsg) then begin
        inMsgNode := FindMessageNode(inMsg);
        if ( inMsgNode <> nil ) then begin
          crs := CreatePartCursor(inMsgNode);
          if ( crs <> nil ) then begin
            crs.Reset();
            while crs.MoveNext() do begin
              tmpNode := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
              if ( tmpNode.Attributes = nil ) or ( tmpNode.Attributes.Length < 1 ) then begin
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              end;
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_name);
              tmpCrs := CreateCursorOn(
                          CreateAttributesCursor(tmpNode,cetRttiNode),
                          ParseFilter(strBuffer,TDOMNodeRttiExposer)
                        );
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then begin
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              end;
              prmName := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_element) + ' or ' + s_NODE_NAME + ' = ' + QuotedStr(s_type);
              tmpCrs := CreateCursorOn(
                          CreateAttributesCursor(tmpNode,cetRttiNode),
                          ParseFilter(strBuffer,TDOMNodeRttiExposer)
                        );
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then begin
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              end;
              prmTypeName := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
              prmTypeType := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeName;
              if IsStrEmpty(prmName) or IsStrEmpty(prmTypeName) or IsStrEmpty(prmTypeType) then begin
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              end;
              if SameText(s_document,ASoapBindingStyle) and
                 AnsiSameText(prmTypeType,s_element)
              then begin
                prmName := ExtractNameFromQName(prmTypeName);
              end;
              prmInternameName := Trim(prmName);
              if AnsiSameText(prmInternameName,tmpMthd.Name) then begin
                prmInternameName := prmInternameName + 'Param';
              end;
              prmHasInternameName := IsReservedKeyWord(prmInternameName) or
                                     ( not IsValidIdent(prmInternameName) ) or
                                     ( tmpMthd.GetParameterIndex(prmInternameName) >= 0 );
              if prmHasInternameName then begin
                prmInternameName := '_' + prmInternameName;
              end;
              prmHasInternameName := not AnsiSameText(prmInternameName,prmName);
              prmTypeDef := GetDataType(prmTypeName,prmTypeType);
              prmDef := tmpMthd.AddParameter(prmInternameName,pmConst,prmTypeDef);
              if prmHasInternameName then begin
                prmDef.RegisterExternalAlias(prmName);
              end;
              if AnsiSameText(tmpMthd.Name,prmTypeDef.Name) then begin
                prmTypeInternalName := prmTypeDef.Name + 'Type';
                while ( FSymbols.IndexOf(prmTypeInternalName) >= 0 ) do begin
                  prmTypeInternalName := '_' + prmTypeInternalName;
                end;
                TTypeDefinitionCrack(prmTypeDef).SetName(prmTypeInternalName);
              end;
            end;
          end;
        end;
      end;
    end;

    procedure ParseOutputMessage();
    var
      outMsg, strBuffer : string;
      outMsgNode, tmpNode : TDOMNode;
      crs, tmpCrs : IObjectCursor;
      prmName, prmTypeName, prmTypeType : string;
      prmDef : TParameterDefinition;
      prmInternameName : string;
      prmHasInternameName : Boolean;
    begin
      if ExtractMsgName(s_output,outMsg) then begin
        outMsgNode := FindMessageNode(outMsg);
        if ( outMsgNode <> nil ) then begin
          crs := CreatePartCursor(outMsgNode);
          if ( crs <> nil ) then begin
            prmDef := nil;
            crs.Reset();
            While crs.MoveNext() do begin
              tmpNode := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
              if ( tmpNode.Attributes = nil ) or ( tmpNode.Attributes.Length < 1 ) then
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_name);
              tmpCrs := CreateCursorOn(CreateAttributesCursor(tmpNode,cetRttiNode),ParseFilter(strBuffer,TDOMNodeRttiExposer));
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              prmName := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
              strBuffer := s_NODE_NAME + '=' + QuotedStr(s_element) + ' or ' + s_NODE_NAME + ' = ' + QuotedStr(s_type);
              tmpCrs := CreateCursorOn(
                          CreateAttributesCursor(tmpNode,cetRttiNode),
                          ParseFilter(strBuffer,TDOMNodeRttiExposer)
                        );
              tmpCrs.Reset();
              if not tmpCrs.MoveNext() then
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              prmTypeName := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
              prmTypeType := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeName;
              if IsStrEmpty(prmName) or IsStrEmpty(prmTypeName) or IsStrEmpty(prmTypeType) then
                raise EWslParserException.CreateFmt('Invalid message part : "%s"',[tmpNode.NodeName]);
              if SameText(s_document,ASoapBindingStyle) and
                 AnsiSameText(prmTypeType,s_element)
              then begin
                prmName := ExtractNameFromQName(prmTypeName);
              end;
              prmInternameName := Trim(prmName);
              if AnsiSameText(prmInternameName,tmpMthd.Name) then begin
                prmInternameName := prmInternameName + 'Param';
              end;
              //prmHasInternameName := IsReservedKeyWord(prmInternameName) or ( not IsValidIdent(prmInternameName) );
              prmHasInternameName := IsReservedKeyWord(prmInternameName) or
                                     ( not IsValidIdent(prmInternameName) ) or
                                     ( tmpMthd.GetParameterIndex(prmInternameName) >= 0 );
              if prmHasInternameName then
                prmInternameName := '_' + prmInternameName;
              prmHasInternameName := not AnsiSameText(prmInternameName,prmName);
              prmDef := tmpMthd.FindParameter(prmInternameName);//(prmName);
              if ( prmDef = nil ) then begin
                prmDef := tmpMthd.AddParameter(prmInternameName,pmOut,GetDataType(prmTypeName,prmTypeType));
                prmDef.RegisterExternalAlias(prmName);
              end else begin
                if prmDef.DataType.SameName(prmTypeName) then begin
                  TParamDefCrack(prmDef).SetModifier(pmVar);
                end else begin
                  prmInternameName := '_' + prmInternameName;
                  prmDef := tmpMthd.AddParameter(prmInternameName,pmOut,GetDataType(prmTypeName,prmTypeType));
                  prmDef.RegisterExternalAlias(prmName);
                end;
              end;
            end;
            if ( SameText(ASoapBindingStyle,s_rpc) and
                 ( prmDef <> nil ) and ( prmDef.Modifier = pmOut ) and//and SameText(prmDef.Name,s_return) and
                 ( prmDef = tmpMthd.Parameter[Pred(tmpMthd.ParameterCount)] )
               ) or
               ( SameText(ASoapBindingStyle,s_document) and
                 ( prmDef <> nil ) and
                 ( prmDef.Modifier = pmOut ) and
                 ( prmDef = tmpMthd.Parameter[Pred(tmpMthd.ParameterCount)] )
               )
            then begin
              TMethodDefinitionCrack(tmpMthd).SetMethodType(mtFunction);
            end;
          end;
        end;
      end;
    end;
    
  begin
    AMthd := nil;
    tmpMthd := TMethodDefinition.Create(AMthdName,mtProcedure);
    try
      ParseInputMessage();
      ParseOutputMessage();
    except
      FreeAndNil(tmpMthd);
      AMthd := nil;
      raise;
    end;
    AMthd := tmpMthd;
  end;

var
  locMthd : TMethodDefinition;
  mthdName : string;
begin
  Result := nil;
  locMthd := nil;
  if not ExtractOperationName(mthdName) then
    raise EWslParserException.CreateFmt('Operation Attribute not found : "%s"',[s_name]);
  if SameText(s_document,ASoapBindingStyle) then begin
    ExtractMethod(mthdName,locMthd);
    if ( locMthd <> nil ) then
      AOwner.AddMethod(locMthd);
  end else if SameText(s_rpc,ASoapBindingStyle) then begin
    ExtractMethod(mthdName,locMthd);
    if ( locMthd <> nil ) then
      AOwner.AddMethod(locMthd);
  end;
  Result := locMthd;
end;

function TWsdlParser.ParseType(const AName, ATypeOrElement: string): TTypeDefinition;
var
  crsSchemaChild : IObjectCursor;
  typNd : TDOMNode;
  typName : string;
  embededType : Boolean;
  
  procedure Init();
  var
    nd : TDOMNodeRttiExposer;
    schmCrsr : IObjectCursor;
  begin
    if not Assigned(FSchemaCursor) then
      raise EWslParserException.Create('Schema cursor not assigned.');
    schmCrsr := FSchemaCursor.Clone() as IObjectCursor;
    FSchemaCursor.Reset();
    if not FSchemaCursor.MoveNext() then
      raise EWslParserException.Create('Schema cursor is empty.');
    nd := FSchemaCursor.GetCurrent() as TDOMNodeRttiExposer;
    crsSchemaChild := CreateChildrenCursor(nd.InnerObject,cetRttiNode);
  end;
  
  procedure FindTypeNode();
  var
    nd : TDOMNode;
    crs : IObjectCursor;
    locStrFilter : string;
  begin
    typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(AName));
    if not Assigned(typNd) then
      raise EWslParserException.CreateFmt('Type definition not found 1 : "%s"',[AName]);
    if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_element) then begin
      crs := CreateCursorOn(CreateAttributesCursor(typNd,cetRttiNode),ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue));
        if not Assigned(typNd) then
          raise EWslParserException.CreateFmt('Type definition not found 2 : "%s"',[AName]);
        embededType := False;
      end else begin
        //locStrFilter := Format('%s = %s or %s = %s ',[s_NODE_NAME,QuotedStr(s_complexType),s_NODE_NAME,QuotedStr(s_simpleType)]);
        locStrFilter := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames);
        crs := CreateCursorOn(CreateChildrenCursor(typNd,cetRttiNode),ParseFilter(locStrFilter,TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          raise EWslParserException.CreateFmt('Type definition not found 3 : "%s"',[AName]);
        end;
        typNd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        typName := ExtractNameFromQName(AName);
        embededType := True;
      end;
    end;
  end;

  function ParseComplexType():TTypeDefinition;
  var
    locParser : TComplexTypeParser;
  begin
    locParser := TComplexTypeParser.Create(Self,typNd,FSymbols,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;
  
  function ParseSimpleType():TTypeDefinition;
  var
    locParser : TSimpleTypeParser;
  begin
    locParser := TSimpleTypeParser.Create(Self,typNd,FSymbols,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

var
  frwType : TTypeDefinition;
begin
  embededType := False;
  Result := nil;
  Result := FSymbols.Find(ExtractNameFromQName(AName),TTypeDefinition) as TTypeDefinition;
  if ( not Assigned(Result) ) or ( Result is TForwardTypeDefinition ) then begin
    frwType := Result;
    Result := nil;
    Init();
    FindTypeNode();
    if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_complexType) then begin
      Result := ParseComplexType();
    end else if AnsiSameText(ExtractNameFromQName(typNd.NodeName),s_simpleType) then begin
      Result := ParseSimpleType();
    end;
    if Assigned(Result) then begin
      if Assigned(frwType) and AnsiSameText(Result.ExternalName,frwType.ExternalName) then begin
        TTypeDefinitionCrack(Result).SetName(frwType.Name);
      end;
      FSymbols.Add(Result);
    end;
  end;
end;

procedure TWsdlParser.ParseTypes();
var
  nd : TDOMNodeRttiExposer;
  schmCrsr, crsSchemaChild, typTmpCrs : IObjectCursor;
  typFilterStr : string;
  typNode : TDOMNode;
begin
  if Assigned(FSchemaCursor) then begin
    schmCrsr := FSchemaCursor.Clone() as IObjectCursor;
    schmCrsr.Reset();
    while schmCrsr.MoveNext() do begin
      nd := schmCrsr.GetCurrent() as TDOMNodeRttiExposer;
      crsSchemaChild := CreateChildrenCursor(nd.InnerObject,cetRttiNode);
      if Assigned(crsSchemaChild) then begin
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
                (typTmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,
                ExtractNameFromQName(typNode.NodeName)
              );
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TWsdlParser.Create(ADoc: TXMLDocument; ASymbols : TSymbolTable);
begin
  Assert(Assigned(ADoc));
  Assert(Assigned(ASymbols));
  FDoc := ADoc;
  FWsdlShortNames := TStringList.Create();
  FSoapShortNames := TStringList.Create();
  FXSShortNames   := TStringList.Create();
  FSymbols := ASymbols;
  FSymbols.Add(CreateWstInterfaceSymbolTable());
end;

destructor TWsdlParser.Destroy();
begin
  FreeAndNil(FXSShortNames);
  FreeAndNil(FSoapShortNames);
  FreeAndNil(FWsdlShortNames);
  inherited Destroy();
end;

procedure TWsdlParser.Parse(const AMode : TParserMode);

  procedure ParseForwardDeclarations();
  var
    i, c : Integer;
    sym : TAbstractSymbolDefinition;
    typeCursor : IObjectCursor;
    tmpNode : TDOMNode;
    s : string;
  begin
    if Assigned(FSchemaCursor) then begin
      FSchemaCursor.Reset();
      if FSchemaCursor.MoveNext() then begin
        tmpNode := (FSchemaCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if tmpNode.HasChildNodes() then begin
          typeCursor := CreateChildrenCursor(tmpNode,cetRttiNode);
          s := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
               CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames) + ' or ' +
               CreateQualifiedNameFilterStr(s_element,FXSShortNames);
          typeCursor := CreateCursorOn(typeCursor,ParseFilter(s,TDOMNodeRttiExposer));
          typeCursor.Reset();
          if typeCursor.MoveNext() then begin
            c := FSymbols.Count;
            i := 0;
            while ( i < c ) do begin
              sym := FSymbols[i];
              if ( sym is TForwardTypeDefinition ) then begin
                typeCursor.Reset();
                tmpNode := FindNamedNode(typeCursor,sym.ExternalName);
                if Assigned(tmpNode) then begin
                  ParseType(sym.ExternalName,ExtractNameFromQName(tmpNode.NodeName));
                  Dec(i);
                  c := FSymbols.Count;
                end else begin
                  WriteLn('XXXXXXXXXXXXXX = ',sym.Name);
                end;
              end;
              Inc(i);
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ExtractNameSpace();
  var
    tmpCrs : IObjectCursor;
    nd : TDOMNode;
    s : string;
  begin
    nd := FDoc.DocumentElement;
    if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
      tmpCrs := CreateCursorOn(
                  CreateAttributesCursor(nd,cetRttiNode),
                  ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_targetNamespace)]),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        s := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
        if not IsStrEmpty(s) then begin
          FSymbols.RegisterExternalAlias(s);
        end;
      end;
    end;
  end;
  
var
  locSrvcCrs : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
begin
  Prepare();

  locSrvcCrs := FServiceCursor.Clone() as IObjectCursor;
  locSrvcCrs.Reset();
  while locSrvcCrs.MoveNext() do begin
    locObj := locSrvcCrs.GetCurrent() as TDOMNodeRttiExposer;
    ParseService(locObj.InnerObject);
  end;

  if ( AMode = pmAllTypes ) then begin
    ParseTypes();
  end;
  ParseForwardDeclarations();
  ExtractNameSpace();
end;

{ TAbstractTypeParser }

constructor TAbstractTypeParser.Create(
        AOwner       : TWsdlParser;
        ATypeNode    : TDOMNode;
        ASymbols     : TSymbolTable;
  const ATypeName    : string;
  const AEmbededDef  : Boolean
);
begin
  Assert(Assigned(AOwner));
  Assert(Assigned(ATypeNode));
  Assert(Assigned(ASymbols));
  FOwner := AOwner;
  FTypeNode := ATypeNode;
  FSymbols := ASymbols;
  FTypeName := ATypeName;
  FEmbededDef := AEmbededDef;
end;

class function TAbstractTypeParser.ExtractEmbeddedTypeFromElement(
        AOwner       : TWsdlParser;
        AEltNode     : TDOMNode;
        ASymbols     : TSymbolTable;
  const ATypeName    : string
): TTypeDefinition;

  function ExtractTypeName() : string;
  var
    locCrs : IObjectCursor;
  begin
    locCrs := CreateCursorOn(
                CreateAttributesCursor(AEltNode,cetRttiNode),
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EWslParserException.Create('Unable to find the <name> tag in the type/element node attributes.');
    Result := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(Result) then begin
      raise EWslParserException.Create('Invalid type/element name( the name is empty ).');
    end;
  end;
  
  function FindParser(out AFoundTypeNode : TDOMNode):TAbstractTypeParserClass;
  var
    k : Integer;
    locPrsClss : TAbstractTypeParserClass;
    locFilter : string;
    locCrs : IObjectCursor;
  begin
    Result := nil;
    AFoundTypeNode := nil;
    for k := 0 to Pred(GetRegisteredParserCount()) do begin
      locPrsClss := GetRegisteredParser(k);
      locFilter := locPrsClss.GetParserSupportedStyle();
      if not IsStrEmpty(locFilter) then begin
        locFilter := CreateQualifiedNameFilterStr(locFilter,AOwner.FXSShortNames);
        locCrs := CreateCursorOn(CreateChildrenCursor(AEltNode,cetRttiNode),ParseFilter(locFilter,TDOMNodeRttiExposer));
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          AFoundTypeNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          Result := locPrsClss;
          Break;
        end;
      end;
    end;
  end;
  
var
  typName : string;
  prsClss : TAbstractTypeParserClass;
  prs : TAbstractTypeParser;
  typNode : TDOMNode;
begin
  if not AEltNode.HasChildNodes() then begin;
    raise EWslParserException.Create('Invalid type definition, this element must have children.');
  end;
  Result := nil;
  typName := ATypeName;
  if IsStrEmpty(typName) then begin
    typName := ExtractTypeName();
  end;
  prsClss := FindParser(typNode);
  if ( prsClss = nil ) then begin;
    raise EWslParserException.CreateFmt('This type style is not supported : "%s".',[typName]);
  end;
  prs := prsClss.Create(AOwner,typNode,ASymbols,typName,True);
  try
    Result := prs.Parse();
  finally
    FreeAndNil(prs);
  end;
end;

var
  FTypeParserList : TClassList = nil;
class procedure TAbstractTypeParser.RegisterParser(AParserClass: TAbstractTypeParserClass);
begin
  if ( FTypeParserList = nil ) then begin
    FTypeParserList := TClassList.Create();
  end;
  if ( FTypeParserList.IndexOf(AParserClass) < 0 ) then begin
    FTypeParserList.Add(AParserClass);
  end;
end;

class function TAbstractTypeParser.GetRegisteredParserCount(): Integer;
begin
  if Assigned(FTypeParserList) then begin
    Result := FTypeParserList.Count;
  end else begin
    Result := 0;
  end;
end;

class function TAbstractTypeParser.GetRegisteredParser(const AIndex: Integer): TAbstractTypeParserClass;
begin
  Result := TAbstractTypeParserClass(FTypeParserList[AIndex]);
end;


{ TComplexTypeParser }

procedure TComplexTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TComplexTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EWslParserException.Create('Unable to find the <name> tag in the type node attributes.');
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EWslParserException.Create('Invalid type name( the name is empty ).');
end;

procedure TComplexTypeParser.ExtractContentType();
var
  locCrs : IObjectCursor;
begin
  FContentType := '';
  if Assigned(FChildCursor) then begin
    locCrs := CreateCursorOn(
                FChildCursor.Clone() as IObjectCursor,
                ParseFilter(CreateQualifiedNameFilterStr(s_complexContent,FOwner.FXSShortNames),TDOMNodeRttiExposer)
              );
    if Assigned(locCrs) then begin
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        FContentType := FContentNode.NodeName;
      end else begin
        locCrs := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_simpleContent,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                  );
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          FContentType := FContentNode.NodeName;
        end else begin
          FContentNode := FTypeNode;
          FContentType := s_complexContent;
        end;
      end;
      FContentType := ExtractNameFromQName(FContentType);
    end;
  end;
end;

procedure TComplexTypeParser.ExtractBaseType();
var
  locContentChildCrs, locCrs : IObjectCursor;
  locSymbol : TAbstractSymbolDefinition;
  locBaseTypeName, locBaseTypeInternalName, locFilterStr : string;
begin
  locFilterStr := CreateQualifiedNameFilterStr(s_extension,FOwner.FXSShortNames);
  locContentChildCrs := CreateChildrenCursor(FContentNode,cetRttiNode);
  locCrs := CreateCursorOn(
              locContentChildCrs.Clone() as IObjectCursor,
              ParseFilter(locFilterStr,TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FDerivationMode := dmExtension;
    FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
  end else begin
    locFilterStr := CreateQualifiedNameFilterStr(s_restriction,FOwner.FXSShortNames);
    locCrs := CreateCursorOn(
                locContentChildCrs.Clone() as IObjectCursor,
                ParseFilter(locFilterStr,TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if locCrs.MoveNext() then begin
      FDerivationMode := dmRestriction;
      FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    end else begin
      FDerivationMode := dmNone;
      FDerivationNode := nil;
   end;
  end;
  if ( FDerivationMode > dmNone ) then begin
    locCrs := CreateCursorOn(
      CreateAttributesCursor(FDerivationNode,cetRttiNode),
      ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer)
    );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EWslParserException.CreateFmt('Invalid extention/restriction of type "%s" : "base" attribute not found.',[FTypeName]);
    locBaseTypeName := ExtractNameFromQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    locSymbol := FSymbols.Find(locBaseTypeName);
    if Assigned(locSymbol) then begin
      if locSymbol.InheritsFrom(TTypeDefinition) then begin
        FBaseType := locSymbol as TTypeDefinition;
        while Assigned(FBaseType) and FBaseType.InheritsFrom(TTypeAliasDefinition) do begin
          FBaseType := (FBaseType as TTypeAliasDefinition).BaseType;
        end;
        if FBaseType.InheritsFrom(TNativeSimpleTypeDefinition) then begin
          Assert(Assigned(TNativeSimpleTypeDefinition(FBaseType).BoxedType));
          FBaseType := TNativeSimpleTypeDefinition(FBaseType).BoxedType;
        end;
      end else begin
        raise EWslParserException.CreateFmt('"%s" was expected to be a type definition.',[locSymbol.Name]);
      end;
    end else begin
      locBaseTypeInternalName := ExtractIdentifier(locBaseTypeName);
      if IsReservedKeyWord(locBaseTypeInternalName) then
        locBaseTypeInternalName := '_' + locBaseTypeInternalName ;
      FBaseType := TForwardTypeDefinition.Create(locBaseTypeInternalName);
      if not AnsiSameText(locBaseTypeInternalName,locBaseTypeName) then
        FBaseType.RegisterExternalAlias(locBaseTypeName);
      FSymbols.Add(FBaseType);
    end;
  end;
end;

function TComplexTypeParser.ParseComplexContent(const ATypeName : string) : TTypeDefinition;

  function ExtractElementCursor(out AAttCursor : IObjectCursor):IObjectCursor;
  var
    frstCrsr, tmpCursor : IObjectCursor;
    parentNode, tmpNode : TDOMNode;
  begin
    Result := nil;
    AAttCursor := nil;
    case FDerivationMode of
      dmNone          : parentNode := FContentNode;
      dmRestriction,
      dmExtension     : parentNode := FDerivationNode;
    end;
    if parentNode.HasChildNodes() then begin;
      AAttCursor := CreateCursorOn(
                     CreateChildrenCursor(parentNode,cetRttiNode),
                     ParseFilter(CreateQualifiedNameFilterStr(s_attribute,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                   );
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      tmpCursor := CreateCursorOn(
                     frstCrsr.Clone() as IObjectCursor,
                     ParseFilter(CreateQualifiedNameFilterStr(s_sequence,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                   );
      tmpCursor.Reset();
      if tmpCursor.MoveNext() then begin
        FSequenceType := stElement;
        tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if  tmpNode.HasChildNodes() then begin
          tmpCursor := CreateCursorOn(
                         CreateChildrenCursor(tmpNode,cetRttiNode),
                         ParseFilter(CreateQualifiedNameFilterStr(s_element,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                       );
          Result := tmpCursor;
        end;
      end else begin
        tmpCursor := CreateCursorOn(
                       frstCrsr.Clone() as IObjectCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_all,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                     );
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          FSequenceType := stElement;
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if  tmpNode.HasChildNodes() then begin
            tmpCursor := CreateCursorOn(
                           CreateChildrenCursor(tmpNode,cetRttiNode),
                           ParseFilter(CreateQualifiedNameFilterStr(s_element,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                         );
            Result := tmpCursor;
          end;
        end;
      end
    end else begin
      Result := nil;
    end;
  end;
  
var
  classDef : TClassTypeDefinition;
  isArrayDef : Boolean;
  arrayItems : TObjectList;
  
  procedure ParseElement(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locTypeInternalName : string;
    locType : TAbstractSymbolDefinition;
    locInternalEltName : string;
    locProp : TPropertyDefinition;
    locHasInternalName : Boolean;
    locMinOccur, locMaxOccur : Integer;
    locMaxOccurUnbounded : Boolean;
    locStrBuffer : string;
    locIsRefElement : Boolean;
  begin
    locType := nil;
    locTypeName := '';
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    locIsRefElement := False;
    if not locPartCursor.MoveNext() then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_ref)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if not locPartCursor.MoveNext() then begin
        raise EWslParserException.Create('Invalid <element> definition : missing "name" or "ref" attribute.');
      end;
      locIsRefElement := True;
    end;
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if locIsRefElement then begin
      locName := ExtractNameFromQName(locName);
    end;
    if IsStrEmpty(locName) then
      raise EWslParserException.Create('Invalid <element> definition : empty "name".');
    if locIsRefElement then begin
      locTypeName := locName;
    end else begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      end else begin
        locTypeName := Format('%s_%s_Type',[FTypeName,locName]);
        locType := TAbstractTypeParser.ExtractEmbeddedTypeFromElement(FOwner,AElement,FSymbols,locTypeName);
        if ( locType = nil ) then begin
          raise EWslParserException.CreateFmt('Invalid <element> definition : unable to determine the type.'#13'Type name : "%s"; Element name :"%s".',[FTypeName,locName]);
        end;
        FSymbols.Add(locType);
      end;
    end;
    if IsStrEmpty(locTypeName) then
      raise EWslParserException.Create('Invalid <element> definition : empty "type".');
    locType := FSymbols.Find(locTypeName);
    if Assigned(locType) then begin
      if locIsRefElement then begin
        locTypeInternalName := locTypeName;
        locTypeInternalName := locTypeInternalName + '_Type';
        TTypeDefinitionCrack(locType).SetName(locTypeInternalName);
      end;
    end else begin
      locTypeInternalName := locTypeName;
      if locIsRefElement then begin
        locTypeInternalName := locTypeInternalName + '_Type';
      end;
      if IsReservedKeyWord(locTypeInternalName) then begin
        locTypeInternalName := '_' + locTypeInternalName;
      end;
      locType := TForwardTypeDefinition.Create(locTypeInternalName);
      if not AnsiSameText(locTypeInternalName,locTypeName) then
        locType.RegisterExternalAlias(locTypeName);
      FSymbols.Add(locType);
    end;
    
    locInternalEltName := locName;
    locHasInternalName := IsReservedKeyWord(locInternalEltName);
    if locHasInternalName then
      locInternalEltName := Format('_%s',[locInternalEltName]);

    locProp := classDef.AddProperty(locInternalEltName,locType as TTypeDefinition);
    if locHasInternalName then
      locProp.RegisterExternalAlias(locName);

    locMinOccur := 1;
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_minOccurs)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      if not TryStrToInt((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locMinOccur) then
        raise EWslParserException.CreateFmt('Invalid "minOccurs" value : "%s.%s".',[FTypeName,locName]);
      if ( locMinOccur < 0 ) then
        raise EWslParserException.CreateFmt('Invalid "minOccurs" value : "%s.%s".',[FTypeName,locName]);
    end;
    if ( locMinOccur = 0 ) then
      locProp.StorageOption := soOptional;

    locMaxOccur := 1;
    locMaxOccurUnbounded := False;
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_maxOccurs)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStrBuffer := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
      if AnsiSameText(locStrBuffer,s_unbounded) then begin
        locMaxOccurUnbounded := True;
      end else begin
        if not TryStrToInt(locStrBuffer,locMaxOccur) then
          raise EWslParserException.CreateFmt('Invalid "maxOccurs" value : "%s.%s".',[FTypeName,locName]);
        if ( locMinOccur < 0 ) then
          raise EWslParserException.CreateFmt('Invalid "maxOccurs" value : "%s.%s".',[FTypeName,locName]);
      end;
    end;
    isArrayDef := locMaxOccurUnbounded or ( locMaxOccur > 1 );
    if isArrayDef then begin
      arrayItems.Add(locProp);
    end;
    if AnsiSameText(s_attribute,ExtractNameFromQName(AElement.NodeName)) then begin
      locProp.IsAttribute := True;
    end;
  end;
  
  procedure GenerateArrayTypes(
    const AClassName : string;
          AArrayPropList : TObjectList
  );
  var
    locPropTyp : TPropertyDefinition;
    k : Integer;
    locString : string;
    locSym : TAbstractSymbolDefinition;
  begin
    for k := 0 to Pred(AArrayPropList.Count) do begin
      locPropTyp := AArrayPropList[k] as TPropertyDefinition;
      locString := Format('%s_%sArray',[AClassName,locPropTyp.Name]);
      locSym := FSymbols.Find(locString);
      if ( locSym = nil ) then begin
        FSymbols.Add(
          TArrayDefinition.Create(
            locString,
            locPropTyp.DataType,
            locPropTyp.Name,
            locPropTyp.ExternalName,
            asEmbeded
          )
        );
      end;
    end;
  end;
  
  function ExtractSoapArray(const AInternalName : string; const AHasInternalName : Boolean) : TArrayDefinition;
  var
    ls : TStringList;
    crs, locCrs : IObjectCursor;
    s : string;
    i : Integer;
    locSym : TAbstractSymbolDefinition;
    ok : Boolean;
    nd : TDOMNode;
  begin
    if not FDerivationNode.HasChildNodes then begin
      raise EWslParserException.CreateFmt('Invalid type definition, attributes not found : "%s".',[FTypeName]);
    end;
    crs := CreateCursorOn(
             CreateChildrenCursor(FDerivationNode,cetRttiNode),
             ParseFilter(CreateQualifiedNameFilterStr(s_attribute,FOwner.FXSShortNames),TDOMNodeRttiExposer)
           );
    ls := TStringList.Create();
    try
      ok := False;
      crs.Reset();
      while crs.MoveNext() do begin
        nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
          ls.Clear();
          ExtractNameSpaceShortNamesNested(nd,ls,s_wsdl);
          locCrs := CreateAttributesCursor(nd,cetRttiNode);
          locCrs := CreateCursorOn(
                      locCrs,
                      ParseFilter(CreateQualifiedNameFilterStr(s_arrayType,ls),TDOMNodeRttiExposer)
                    );
          if Assigned(locCrs) then begin
            locCrs.Reset();
            if locCrs.MoveNext() then begin
              ok := True;
              Break;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(ls);
    end;
    if not ok then begin
      raise EWslParserException.CreateFmt('Invalid type definition, unable to find the "%s" attribute : "%s".',[s_arrayType,FTypeName]);
    end;
    s := ExtractNameFromQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject.NodeValue);
    i := Pos('[',s);
    if ( i < 1 ) then begin
      i := MaxInt;
    end;
    s := Copy(s,1,Pred(i));
    locSym := FSymbols.Find(s);
    if not Assigned(locSym) then begin
      locSym := TForwardTypeDefinition.Create(s);
      FSymbols.Add(locSym);
    end;
    if not locSym.InheritsFrom(TTypeDefinition) then
      raise EWslParserException.CreateFmt('Invalid array type definition, invalid item type definition : "%s".',[FTypeName]);
    Result := TArrayDefinition.Create(AInternalName,locSym as TTypeDefinition,s_item,s_item,asScoped);
    if AHasInternalName then
      Result.RegisterExternalAlias(ATypeName);
  end;
  
var
  eltCrs, eltAttCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
  arrayDef : TArrayDefinition;
  propTyp, tmpPropTyp : TPropertyDefinition;
  tmpClassDef : TClassTypeDefinition;
  i : Integer;
begin
  ExtractBaseType();
  eltCrs := ExtractElementCursor(eltAttCrs);
  
  internalName := ExtractIdentifier(ATypeName);
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) ) or
                     //( FSymbols.IndexOf(internalName) <> -1 ) or
                     ( not AnsiSameText(internalName,ATypeName) );
  if hasInternalName then begin
    internalName := Format('_%s',[internalName]);
  end;

  if ( FDerivationMode = dmRestriction ) and FBaseType.SameName(s_array) then begin
    Result := ExtractSoapArray(internalName,hasInternalName);
  end else begin
    arrayItems := TObjectList.Create(False);
    try
      classDef := TClassTypeDefinition.Create(internalName);
      try
        Result := classDef;
        if hasInternalName then
          classDef.RegisterExternalAlias(ATypeName);
        if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
          classDef.SetParent(FBaseType);
        end;
        if ( classDef.Parent = nil ) then
          classDef.SetParent(
            (FSymbols.ByName('base_service_intf') as TSymbolTable)
            .ByName('TBaseComplexRemotable') as TClassTypeDefinition
          );
        if Assigned(eltCrs) or Assigned(eltAttCrs) then begin
          isArrayDef := False;
          if Assigned(eltCrs) then begin
            eltCrs.Reset();
            while eltCrs.MoveNext() do begin
              ParseElement((eltCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
            end;
          end;
          if Assigned(eltAttCrs) then begin
            eltAttCrs.Reset();
            while eltAttCrs.MoveNext() do begin
              ParseElement((eltAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
            end;
          end;
          if ( arrayItems.Count > 0 ) then begin
            if ( arrayItems.Count = 1 ) and ( classDef.PropertyCount = 1 ) then begin
              Result := nil;
              propTyp := arrayItems[0] as TPropertyDefinition;
              arrayDef := TArrayDefinition.Create(internalName,propTyp.DataType,propTyp.Name,propTyp.ExternalName,asScoped);
              FreeAndNil(classDef);
              Result := arrayDef;
              if hasInternalName then
                arrayDef.RegisterExternalAlias(ATypeName);
            end else begin
              GenerateArrayTypes(internalName,arrayItems);
              tmpClassDef := classDef;
              classDef := TClassTypeDefinition.Create(tmpClassDef.Name);
              Result := classDef;
              classDef.SetParent(tmpClassDef.Parent);
              if hasInternalName then
                classDef.RegisterExternalAlias(ATypeName);
              for i := 0 to Pred(tmpClassDef.PropertyCount) do begin
                propTyp := tmpClassDef.Properties[i];
                if ( arrayItems.IndexOf(propTyp) = -1 ) then begin
                  tmpPropTyp := classDef.AddProperty(propTyp.Name,propTyp.DataType);
                  tmpPropTyp.IsAttribute := propTyp.IsAttribute;
                  tmpPropTyp.StorageOption := propTyp.StorageOption;
                  tmpPropTyp.RegisterExternalAlias(propTyp.ExternalName);
                end else begin
                  classDef.AddProperty(
                    propTyp.Name,
                    FSymbols.ByName(Format('%s_%sArray',[internalName,propTyp.Name])) as TTypeDefinition
                  ).RegisterExternalAlias(propTyp.ExternalName);
                end;
              end;
              FreeAndNil(tmpClassDef);
            end;
          end;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      FreeAndNil(arrayItems);
    end;
  end;
end;

function TComplexTypeParser.ParseSimpleContent(const ATypeName : string) : TTypeDefinition;

  function ExtractAttributeCursor():IObjectCursor;
  var
    frstCrsr, tmpCursor : IObjectCursor;
    parentNode, tmpNode : TDOMNode;
    locFilterStr : string;
  begin
    Result := nil;
    parentNode := FContentNode;
    if parentNode.HasChildNodes() then begin;
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      locFilterStr := CreateQualifiedNameFilterStr(s_extension,FOwner.FXSShortNames) + ' or ' +
                      CreateQualifiedNameFilterStr(s_restriction,FOwner.FXSShortNames) ;
      tmpCursor := CreateCursorOn(frstCrsr.Clone() as IObjectCursor,ParseFilter(locFilterStr,TDOMNodeRttiExposer));
      if Assigned(tmpCursor) then begin
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if tmpNode.HasChildNodes() then begin
            locFilterStr := CreateQualifiedNameFilterStr(s_attribute,FOwner.FXSShortNames);
            tmpCursor := CreateCursorOn(CreateChildrenCursor(tmpNode,cetRttiNode),ParseFilter(locFilterStr,TDOMNodeRttiExposer));
            if Assigned(tmpCursor) then begin
              Result := tmpCursor;
              Result.Reset();
            end;
          end;
        end;
      end;
    end else begin
      Result := nil;
    end;
  end;

var
  locClassDef : TClassTypeDefinition;

  procedure ParseAttribute(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locStoreOpt : string;
    locType : TAbstractSymbolDefinition;
    locStoreOptIdx : Integer;
    locAttObj : TPropertyDefinition;
    locInternalEltName : string;
    locHasInternalName : boolean;
  begin
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EWslParserException.CreateFmt('Invalid <%s> definition : missing "name" attribute.',[s_attribute]);
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(locName) then
      raise EWslParserException.CreateFmt('Invalid <%s> definition : empty "name".',[s_attribute]);

    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EWslParserException.CreateFmt('Invalid <%s> definition : missing "type" attribute.',[s_attribute]);
    locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    if IsStrEmpty(locTypeName) then
      raise EWslParserException.CreateFmt('Invalid <%s> definition : empty "type".',[s_attribute]);
    locType := FSymbols.Find(locTypeName);
    if not Assigned(locType) then begin
      locType := TForwardTypeDefinition.Create(locTypeName);
      FSymbols.Add(locType);
    end;
    
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStoreOpt := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      if IsStrEmpty(locStoreOpt) then
        raise EWslParserException.CreateFmt('Invalid <%s> definition : empty "use".',[s_attribute]);
      locStoreOptIdx := AnsiIndexText(locStoreOpt,[s_required,s_optional,s_prohibited]);
      if ( locStoreOptIdx < Ord(Low(TStorageOption)) ) or ( locStoreOptIdx > Ord(High(TStorageOption)) ) then
        raise EWslParserException.CreateFmt('Invalid <%s> definition : invalid "use" value "%s".',[s_attribute,locStoreOpt]);
    end else begin
      locStoreOptIdx := 0;
    end;

    locInternalEltName := locName;
    locHasInternalName := IsReservedKeyWord(locInternalEltName);
    if locHasInternalName then
      locInternalEltName := Format('_%s',[locInternalEltName]);
      
    locAttObj := locClassDef.AddProperty(locInternalEltName,locType as TTypeDefinition);
    if locHasInternalName then
      locAttObj.RegisterExternalAlias(locName);
    locAttObj.IsAttribute := True;
    locAttObj.StorageOption := TStorageOption(locStoreOptIdx);
  end;

var
  locAttCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
begin
  ExtractBaseType();
  if not ( FDerivationMode in [dmExtension, dmRestriction] ) then
    raise EWslParserException.Create('Invalid "complexeType.simpleType" definition : restriction/extension not found.');

  internalName := ATypeName;
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) );{ or
                     ( FSymbols.IndexOf(internalName) <> -1 );}
  if hasInternalName then
    internalName := Format('_%s',[internalName]);

  locAttCrs := ExtractAttributeCursor();
  locClassDef := TClassTypeDefinition.Create(Trim(internalName));
  try
    Result := locClassDef;
    if hasInternalName then
      locClassDef.RegisterExternalAlias(ATypeName);
    if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
      locClassDef.SetParent(FBaseType);
    end;
    if ( locClassDef.Parent = nil ) then begin
      locClassDef.SetParent(
        (FSymbols.ByName('base_service_intf') as TSymbolTable)
        .ByName('TBaseComplexRemotable') as TClassTypeDefinition
      );
    end;
    if ( locAttCrs <> nil ) then begin
      locAttCrs.Reset();
      while locAttCrs.MoveNext() do begin
        ParseAttribute((locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TComplexTypeParser.ParseEmptyContent(const ATypeName: string): TTypeDefinition;
var
  internalName : string;
  hasInternalName : Boolean;
begin
  internalName := ATypeName;
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) );{ or
                     ( FSymbols.IndexOf(internalName) <> -1 );}
  if hasInternalName then
    internalName := Format('_%s',[internalName]);
  Result := TClassTypeDefinition.Create(internalName);
  if hasInternalName then
    Result.RegisterExternalAlias(ATypeName);
  TClassTypeDefinition(Result).SetParent(
    (FSymbols.ByName('base_service_intf') as TSymbolTable)
    .ByName('TBaseComplexRemotable') as TClassTypeDefinition
  );
end;

class function TComplexTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_complexType;
end;

function TComplexTypeParser.Parse() : TTypeDefinition;
var
  locSym : TAbstractSymbolDefinition;
  locContinue : Boolean;
begin
  if not AnsiSameText(ExtractNameFromQName(FTypeNode.NodeName),s_complexType) then
    raise EWslParserException.CreateFmt('%s expected but %s found.',[s_complexType,ExtractNameFromQName(FTypeNode.NodeName)]);
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FSymbols.Find(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TTypeDefinition) then
      raise EWslParserException.CreateFmt('Symbol found in the symbol table but is not a type definition : %s.',[FTypeName]);
    locContinue := locSym.InheritsFrom(TForwardTypeDefinition);
    if not locContinue then;
      Result := locSym as TTypeDefinition;
  end;
  if locContinue then begin
    ExtractContentType();
    if IsStrEmpty(FContentType) then begin
      Result := ParseEmptyContent(FTypeName);
    end else begin
      if AnsiSameText(FContentType,s_complexContent) then
        Result := ParseComplexContent(FTypeName)
      else
        Result := ParseSimpleContent(FTypeName);
    end;
  end;
end;

{ TSimpleTypeParser }

procedure TSimpleTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TSimpleTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EWslParserException.Create('Unable to find the <name> tag in the type node attributes.');
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EWslParserException.Create('Invalid type name( the name is empty ).');
end;

function TSimpleTypeParser.ExtractContentType() : Boolean;
var
  locCrs, locAttCrs : IObjectCursor;
  tmpNode : TDOMNode;
begin
  locCrs := CreateCursorOn(
              FChildCursor.Clone() as IObjectCursor,
              ParseFilter(CreateQualifiedNameFilterStr(s_restriction,FOwner.FXSShortNames),TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FRestrictionNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    tmpNode := nil;
    locAttCrs := CreateAttributesCursor(FRestrictionNode,cetRttiNode);
    if Assigned(locAttCrs) then begin
      locAttCrs := CreateCursorOn(locAttCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer));
      locAttCrs.Reset();
      if locAttCrs.MoveNext() then begin
        tmpNode := (locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      end;
    end;
    FBaseName := '';
    if Assigned(tmpNode) then begin
      FBaseName := ExtractNameFromQName(tmpNode.NodeValue);
    end;
    locCrs := CreateChildrenCursor(FRestrictionNode,cetRttiNode) as IObjectCursor;
    if Assigned(locCrs) then begin
      locCrs := CreateCursorOn(
                  locCrs,
                  ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,FOwner.FXSShortNames),TDOMNodeRttiExposer)
                );
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FIsEnum := True;
      end else begin
        if IsStrEmpty(FBaseName) then
          raise EWslParserException.CreateFmt('Base type is not specified for the simple type, parsing : "%s".',[FTypeName]);
        FIsEnum := False
      end;
    end else begin
      if IsStrEmpty(FBaseName) then
        raise EWslParserException.CreateFmt('Base type is not specified for the simple type, parsing : "%s".',[FTypeName]);
      FIsEnum := False
    end;
    Result := True;
  end else begin
    //raise EWslParserException.CreateFmt('The parser only support "Restriction" mode simple type derivation, parsing : "%s".',[FTypeName]);
    Result := False;
  end;
end;

function TSimpleTypeParser.ParseEnumContent(): TTypeDefinition;

  function ExtractEnumCursor():IObjectCursor ;
  begin
    Result := CreateCursorOn(
                CreateChildrenCursor(FRestrictionNode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,FOwner.FXSShortNames),TDOMNodeRttiExposer)
              );
  end;
  
var
  locRes : TEnumTypeDefinition;
  locOrder : Integer;
  
  procedure ParseEnumItem(AItemNode : TDOMNode);
  var
    tmpNode : TDOMNode;
    locItemName, locInternalItemName : string;
    locCrs : IObjectCursor;
    locItem : TEnumItemDefinition;
    locHasInternalName : Boolean;
    locBuffer : string;
  begin
    locCrs := CreateCursorOn(CreateAttributesCursor(AItemNode,cetRttiNode),ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_value)]),TDOMNodeRttiExposer)) as IObjectCursor;
    if not Assigned(locCrs) then
      raise EWslParserException.CreateFmt('Invalid "enum" item node : no value attribute, type = "%s".',[FTypeName]);
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EWslParserException.CreateFmt('Invalid "enum" item node : no value attribute, type = "%s".',[FTypeName]);
    tmpNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    locItemName := tmpNode.NodeValue;
    if IsStrEmpty(locItemName) then
      raise EWslParserException.CreateFmt('Invalid "enum" item node : the value attribute is empty, type = "%s".',[FTypeName]);

    locInternalItemName := ExtractIdentifier(locItemName);
    locHasInternalName := IsReservedKeyWord(locInternalItemName) or
                          ( not IsValidIdent(locInternalItemName) ) or
                          ( FSymbols.IndexOf(locInternalItemName) <> -1 ) or
                          ( not AnsiSameText(locInternalItemName,locItemName) );
    if locHasInternalName then begin
      locBuffer := ExtractIdentifier(locRes.ExternalName);
      if IsStrEmpty(locBuffer) and ( locBuffer[Length(locBuffer)] <> '_' ) then begin
        locInternalItemName := Format('%s_%s',[locBuffer,locInternalItemName]);
      end else begin
        locInternalItemName := Format('%s%s',[locBuffer,locInternalItemName]);
      end;
    end;
    locItem := TEnumItemDefinition.Create(locInternalItemName,locRes,locOrder);
    if locHasInternalName then
      locItem.RegisterExternalAlias(locItemName);
    FSymbols.Add(locItem);
    locRes.AddItem(locItem);
    Inc(locOrder);
  end;
  
var
  locEnumCrs : IObjectCursor;
  intrName : string;
  hasIntrnName : Boolean;
begin
  locEnumCrs := ExtractEnumCursor();

  intrName := FTypeName;
  hasIntrnName := IsReservedKeyWord(FTypeName) or
                  ( ( FSymbols.IndexOf(intrName) >= 0 ) and ( not FSymbols.ByName(intrName).InheritsFrom(TForwardTypeDefinition) ) );
  if hasIntrnName then
    intrName := '_' + intrName;

  locRes := TEnumTypeDefinition.Create(Trim(intrName));
  try
    Result := locRes;
    if hasIntrnName then
      locRes.RegisterExternalAlias(FTypeName);
    locEnumCrs.Reset();
    locOrder := 0;
    while locEnumCrs.MoveNext() do begin
      ParseEnumItem((locEnumCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSimpleTypeParser.ParseOtherContent(): TTypeDefinition;
begin  // todo : implement TSimpleTypeParser.ParseOtherContent
  if IsStrEmpty(FBaseName) then
    raise EWslParserException.CreateFmt('Invalid simple type definition : base type not provided, "%s".',[FTypeName]);
  Result := TTypeAliasDefinition.Create(FTypeName,FSymbols.ByName(FBaseName) as TTypeDefinition);
end;

class function TSimpleTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_simpleType;
end;

function TSimpleTypeParser.Parse(): TTypeDefinition;
var
  locSym : TAbstractSymbolDefinition;
  locContinue : Boolean;
begin
  if not AnsiSameText(ExtractNameFromQName(FTypeNode.NodeName),s_simpleType) then
    raise EWslParserException.CreateFmt('%s expected but %s found.',[s_simpleType,ExtractNameFromQName(FTypeNode.NodeName)]);
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FSymbols.Find(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TTypeDefinition) then
      raise EWslParserException.CreateFmt('Symbol found in the symbol table but is not a type definition : %s.',[FTypeName]);
    locContinue := locSym.InheritsFrom(TForwardTypeDefinition);
    if not locContinue then begin
      Result := locSym as TTypeDefinition;
    end;
  end;
  if locContinue then begin
    if ExtractContentType() then begin
      if FIsEnum then begin
        Result := ParseEnumContent()
      end else begin
        Result := ParseOtherContent();
      end;
    end else begin
      FBaseName := 'string';
      Result := ParseOtherContent();
    end;
  end;
end;


initialization
  TAbstractTypeParser.RegisterParser(TSimpleTypeParser);
  TAbstractTypeParser.RegisterParser(TComplexTypeParser);

finalization
  FreeAndNil(FTypeParserList);
  
end.
