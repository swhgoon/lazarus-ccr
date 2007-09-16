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
unit ws_parser_imp;

interface
uses
  Classes, SysUtils,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  cursor_intf, rtti_filters,
  pastree, pascal_parser_intf, logger_intf,
  xsd_parser;

type

  TNameSpaceValueType = ( nvtExpandValue, nvtShortSynonym );

  TAbstractTypeParserClass = class of TAbstractTypeParser;

  { TAbstractTypeParser }

  TAbstractTypeParser = class
  private
    FContext : IParserContext;
    FTypeNode : TDOMNode;
    FSymbols : TwstPasTreeContainer;
    FTypeName : string;
    FEmbededDef : Boolean;
  private
    function GetModule: TPasModule;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function FindElementNS(
      const ANameSpace,
            ALocalName : string;
      const ASpaceType : TNameSpaceValueType
    ) : TPasElement;
    function FindElement(const ALocalName : string) : TPasElement; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(
            AOwner       : IParserContext;
            ATypeNode    : TDOMNode;
      const ATypeName    : string;
      const AEmbededDef  : Boolean
    );
    class function ExtractEmbeddedTypeFromElement(
            AOwner       : IParserContext;
            AEltNode     : TDOMNode;
            ASymbols     : TwstPasTreeContainer;
      const ATypeName    : string
    ) : TPasType;
    class function GetParserSupportedStyle():string;virtual;abstract;
    class procedure RegisterParser(AParserClass : TAbstractTypeParserClass);
    class function GetRegisteredParserCount() : Integer;
    class function GetRegisteredParser(const AIndex : Integer):TAbstractTypeParserClass;
    function Parse():TPasType;virtual;abstract;
    property Module : TPasModule read GetModule;
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
    FBaseType : TPasType;
    FDerivationMode : TDerivationMode;
    FDerivationNode : TDOMNode;
    FSequenceType : TSequenceType;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    procedure ExtractContentType();
    procedure ExtractBaseType();
    function ParseSimpleContent(const ATypeName : string):TPasType;
    function ParseEmptyContent(const ATypeName : string):TPasType;
    function ParseComplexContent(const ATypeName : string):TPasType;virtual;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TPasType;override;
  end;

  { TSimpleTypeParser }

  TSimpleTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FBaseName : string;
    FBaseNameSpace : string;
    FRestrictionNode : TDOMNode;
    FIsEnum : Boolean;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    function ExtractContentType() : Boolean;
    function ParseEnumContent():TPasType;
    function ParseOtherContent():TPasType;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():TPasType;override;
  end;

  resourcestring
    SResolveError = 'Unable to resolve this namespace : "%s".';

implementation
uses dom_cursors, parserutils, StrUtils, Contnrs, xsd_consts;

{ TAbstractTypeParser }

constructor TAbstractTypeParser.Create(
        AOwner       : IParserContext;
        ATypeNode    : TDOMNode;
  const ATypeName    : string;
  const AEmbededDef  : Boolean
);
var
  symtbl : TwstPasTreeContainer;
begin
  Assert(Assigned(AOwner));
  Assert(Assigned(ATypeNode));
  symtbl := AOwner.GetSymbolTable();
  Assert(Assigned(symtbl));
  FContext := AOwner;
  FTypeNode := ATypeNode;
  FSymbols := symtbl;
  FTypeName := ATypeName;
  FEmbededDef := AEmbededDef;
end;

class function TAbstractTypeParser.ExtractEmbeddedTypeFromElement(
        AOwner       : IParserContext;
        AEltNode     : TDOMNode;
        ASymbols     : TwstPasTreeContainer;
  const ATypeName    : string
): TPasType;

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
      raise EXsdParserException.Create('Unable to find the <name> tag in the type/element node attributes.');
    Result := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(Result) then begin
      raise EXsdParserException.Create('Invalid type/element name( the name is empty ).');
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
        locFilter := CreateQualifiedNameFilterStr(locFilter,AOwner.GetXsShortNames());
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
    raise EXsdParserException.Create('Invalid type definition, this element must have children.');
  end;
  Result := nil;
  typName := ATypeName;
  if IsStrEmpty(typName) then begin
    typName := ExtractTypeName();
  end;
  prsClss := FindParser(typNode);
  if ( prsClss = nil ) then begin;
    raise EXsdInvalidTypeDefinitionException.CreateFmt('This type style is not supported : "%s".',[typName]);
  end;
  prs := prsClss.Create(AOwner,typNode,typName,True);
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

function TAbstractTypeParser.FindElementNS(
  const ANameSpace,
        ALocalName : string;
  const ASpaceType : TNameSpaceValueType
) : TPasElement;
var
  locNS : string;
begin
  if ( ASpaceType = nvtExpandValue ) then begin
    locNS := ANameSpace
  end else begin
    if not FContext.FindNameSpace(ANameSpace,locNS) then
      raise EXsdParserAssertException.CreateFmt(SResolveError,[ANameSpace]);
  end;
  Result := FSymbols.FindElementNS(ALocalName,locNS);
end;

function TAbstractTypeParser.GetModule() : TPasModule;
begin
  Result := FContext.GetTargetModule();
end;

function TAbstractTypeParser.FindElement(const ALocalName: string): TPasElement;
begin
  Result := FSymbols.FindElementInModule(ALocalName,Module);
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
      raise EXsdParserException.Create('Unable to find the <name> tag in the type node attributes.');
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserException.Create('Invalid type name( the name is empty ).');
end;

procedure TComplexTypeParser.ExtractContentType();
var
  locCrs : IObjectCursor;
begin
  FContentType := '';
  if Assigned(FChildCursor) then begin
    locCrs := CreateCursorOn(
                FChildCursor.Clone() as IObjectCursor,
                ParseFilter(CreateQualifiedNameFilterStr(s_complexContent,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
              );
    if Assigned(locCrs) then begin
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        FContentType := FContentNode.NodeName;
      end else begin
        locCrs := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_simpleContent,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
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
  locSymbol : TPasElement;
  locBaseTypeLocalSpace, locBaseTypeLocalName, locBaseTypeInternalName, locFilterStr : string;
begin
  locFilterStr := CreateQualifiedNameFilterStr(s_extension,FContext.GetXsShortNames());
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
    locFilterStr := CreateQualifiedNameFilterStr(s_restriction,FContext.GetXsShortNames());
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
      raise EXsdParserException.CreateFmt('Invalid extention/restriction of type "%s" : "base" attribute not found.',[FTypeName]);
    ExplodeQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locBaseTypeLocalName,locBaseTypeLocalSpace);
    locSymbol := FindElementNS(locBaseTypeLocalSpace,locBaseTypeLocalName,nvtShortSynonym);
    if Assigned(locSymbol) then begin
      if locSymbol.InheritsFrom(TPasType) then begin
        FBaseType := locSymbol as TPasType;
        while Assigned(FBaseType) and FBaseType.InheritsFrom(TPasAliasType) do begin
          FBaseType := (FBaseType as TPasAliasType).DestType;
        end;
        if FBaseType.InheritsFrom(TPasNativeSimpleType) then begin
          Assert(Assigned(TPasNativeSimpleType(FBaseType).BoxedType));
          FBaseType := TPasNativeSimpleType(FBaseType).BoxedType;
        end;
      end else begin
        raise EXsdParserException.CreateFmt('"%s" was expected to be a type definition.',[locSymbol.Name]);
      end;
    end else begin
      locBaseTypeInternalName := ExtractIdentifier(locBaseTypeLocalName);
      if IsReservedKeyWord(locBaseTypeInternalName) then
        locBaseTypeInternalName := '_' + locBaseTypeInternalName ;
      FBaseType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locBaseTypeInternalName,Self.Module.InterfaceSection,visDefault,'',0));
      Self.Module.InterfaceSection.Declarations.Add(FBaseType);
      Self.Module.InterfaceSection.Types.Add(FBaseType);
      if not AnsiSameText(locBaseTypeInternalName,locBaseTypeLocalName) then
        FSymbols.RegisterExternalAlias(FBaseType,locBaseTypeLocalName);
    end;
  end;
end;

function TComplexTypeParser.ParseComplexContent(const ATypeName : string) : TPasType;

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
                     ParseFilter(CreateQualifiedNameFilterStr(s_attribute,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
                   );
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      tmpCursor := CreateCursorOn(
                     frstCrsr.Clone() as IObjectCursor,
                     ParseFilter(CreateQualifiedNameFilterStr(s_sequence,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
                   );
      tmpCursor.Reset();
      if tmpCursor.MoveNext() then begin
        FSequenceType := stElement;
        tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if  tmpNode.HasChildNodes() then begin
          tmpCursor := CreateCursorOn(
                         CreateChildrenCursor(tmpNode,cetRttiNode),
                         ParseFilter(CreateQualifiedNameFilterStr(s_element,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
                       );
          Result := tmpCursor;
        end;
      end else begin
        tmpCursor := CreateCursorOn(
                       frstCrsr.Clone() as IObjectCursor,
                       ParseFilter(CreateQualifiedNameFilterStr(s_all,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          FSequenceType := stElement;
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if  tmpNode.HasChildNodes() then begin
            tmpCursor := CreateCursorOn(
                           CreateChildrenCursor(tmpNode,cetRttiNode),
                           ParseFilter(CreateQualifiedNameFilterStr(s_element,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
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
  classDef : TPasClassType;
  isArrayDef : Boolean;
  arrayItems : TObjectList;
  
  procedure ParseElement(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locTypeInternalName : string;
    locType : TPasElement;
    locInternalEltName : string;
    locProp : TPasProperty;
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
        raise EXsdParserException.Create('Invalid <element> definition : missing "name" or "ref" attribute.');
      end;
      locIsRefElement := True;
    end;
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if locIsRefElement then begin
      locName := ExtractNameFromQName(locName);
    end;
    if IsStrEmpty(locName) then
      raise EXsdParserException.Create('Invalid <element> definition : empty "name".');
    if locIsRefElement then begin
      locTypeName := locName;
    end else begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      end else begin
        locTypeName := Format('%s_%s_Type',[FTypeName,locName]);
        locType := TAbstractTypeParser.ExtractEmbeddedTypeFromElement(FContext,AElement,FSymbols,locTypeName);
        if ( locType = nil ) then begin
          raise EXsdInvalidElementDefinitionException.CreateFmt('Invalid <element> definition : unable to determine the type.'#13'Type name : "%s"; Element name :"%s".',[FTypeName,locName]);
        end;
        Self.Module.InterfaceSection.Declarations.Add(locType);
        Self.Module.InterfaceSection.Types.Add(locType);
        if locType.InheritsFrom(TPasClassType) then begin
          Self.Module.InterfaceSection.Classes.Add(locType);
        end;
      end;
    end;
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidElementDefinitionException.Create('Invalid <element> definition : empty "type".');
    locType := FSymbols.FindElement(locTypeName);
    if Assigned(locType) then begin
      if locIsRefElement then begin
        locTypeInternalName := locTypeName;
        locTypeInternalName := locTypeInternalName + '_Type';
        locType.Name := locTypeInternalName;
        FSymbols.RegisterExternalAlias(locType,locTypeName);
      end;
    end else begin
      locTypeInternalName := locTypeName;
      if locIsRefElement or AnsiSameText(locTypeInternalName,locInternalEltName) then begin
        locTypeInternalName := locTypeInternalName + '_Type';
      end;
      if IsReservedKeyWord(locTypeInternalName) then begin
        locTypeInternalName := '_' + locTypeInternalName;
      end;
      locType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locTypeInternalName,Self.Module.InterfaceSection,visDefault,'',0));
      Self.Module.InterfaceSection.Declarations.Add(locType);
      Self.Module.InterfaceSection.Types.Add(locType);
      if not AnsiSameText(locTypeInternalName,locTypeName) then
        FSymbols.RegisterExternalAlias(locType,locTypeName);
    end;
    
    locInternalEltName := locName;
    locHasInternalName := IsReservedKeyWord(locInternalEltName);
    if locHasInternalName then
      locInternalEltName := Format('_%s',[locInternalEltName]);

    locProp := TPasProperty(FSymbols.CreateElement(TPasProperty,locInternalEltName,classDef,visPublished,'',0));
    classDef.Members.Add(locProp);
    locProp.VarType := locType as TPasType;
    locType.AddRef();
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locProp,locName);
    {if AnsiSameText(locType.Name,locProp.Name) then begin
      FSymbols.RegisterExternalAlias(locType,FSymbols.GetExternalName(locType));
      TPasEmentCrack(locType).SetName(locType.Name + '_Type');
    end;}

    if AnsiSameText(s_attribute,ExtractNameFromQName(AElement.NodeName)) then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locStrBuffer := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
        if IsStrEmpty(locStrBuffer) then
          raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : empty "use".',[s_attribute]);
        case AnsiIndexText(locStrBuffer,[s_required,s_optional,s_prohibited]) of
          0 : locMinOccur := 1;
          1 : locMinOccur := 0;
          2 : locMinOccur := -1;
          else
            raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : invalid "use" value "%s".',[s_attribute,locStrBuffer]);
        end;
      end else begin
        locMinOccur := 0;
      end;
    end else begin
      locMinOccur := 1;
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_minOccurs)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        if not TryStrToInt((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locMinOccur) then
          raise EXsdParserException.CreateFmt('Invalid "minOccurs" value : "%s.%s".',[FTypeName,locName]);
        if ( locMinOccur < 0 ) then
          raise EXsdParserException.CreateFmt('Invalid "minOccurs" value : "%s.%s".',[FTypeName,locName]);
      end;
    end;
    locProp.ReadAccessorName := 'F' + locProp.Name;
    locProp.WriteAccessorName := 'F' + locProp.Name;
    if ( locMinOccur = 0 ) then begin
      locProp.StoredAccessorName := 'Has' + locProp.Name;
    end else if ( locMinOccur = -1 ) then begin
      locProp.StoredAccessorName := 'False';
    end else begin
      locProp.StoredAccessorName := 'True';
    end;

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
          raise EXsdParserException.CreateFmt('Invalid "maxOccurs" value : "%s.%s".',[FTypeName,locName]);
        if ( locMinOccur < 0 ) then
          raise EXsdParserException.CreateFmt('Invalid "maxOccurs" value : "%s.%s".',[FTypeName,locName]);
      end;
    end;
    isArrayDef := locMaxOccurUnbounded or ( locMaxOccur > 1 );
    if isArrayDef then begin
      arrayItems.Add(locProp);
    end;
    if AnsiSameText(s_attribute,ExtractNameFromQName(AElement.NodeName)) then begin
      FSymbols.SetPropertyAsAttribute(locProp,True);
    end;
  end;
  
  procedure GenerateArrayTypes(
    const AClassName : string;
          AArrayPropList : TObjectList
  );
  var
    locPropTyp : TPasProperty;
    k : Integer;
    locString : string;
    locSym : TPasElement;
  begin
    for k := 0 to Pred(AArrayPropList.Count) do begin
      locPropTyp := AArrayPropList[k] as TPasProperty;
      locString := Format('%s_%sArray',[AClassName,locPropTyp.Name]);
      locSym := FSymbols.FindElement(locString);
      if ( locSym = nil ) then begin
        locSym := FSymbols.CreateArray(
          locString,
          locPropTyp.VarType,
          locPropTyp.Name,
          FSymbols.GetExternalName(locPropTyp),
          asEmbeded
        );
        Self.Module.InterfaceSection.Declarations.Add(locSym);
        Self.Module.InterfaceSection.Types.Add(locSym);
      end;
    end;
  end;
  
  function ExtractSoapArray(const AInternalName : string; const AHasInternalName : Boolean) : TPasArrayType;
  var
    ls : TStringList;
    crs, locCrs : IObjectCursor;
    s : string;
    i : Integer;
    locSym : TPasElement;
    ok : Boolean;
    nd : TDOMNode;
  begin
    if not FDerivationNode.HasChildNodes then begin
      raise EXsdInvalidTypeDefinitionException.CreateFmt('Invalid type definition, attributes not found : "%s".',[FTypeName]);
    end;
    crs := CreateCursorOn(
             CreateChildrenCursor(FDerivationNode,cetRttiNode),
             ParseFilter(CreateQualifiedNameFilterStr(s_attribute,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
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
      raise EXsdInvalidTypeDefinitionException.CreateFmt('Invalid type definition, unable to find the "%s" attribute : "%s".',[s_arrayType,FTypeName]);
    end;
    s := ExtractNameFromQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject.NodeValue);
    i := Pos('[',s);
    if ( i < 1 ) then begin
      i := MaxInt;
    end;
    s := Copy(s,1,Pred(i));
    locSym := FSymbols.FindElement(s);
    if not Assigned(locSym) then begin
      locSym := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,s,Self.Module.InterfaceSection,visDefault,'',0));
      Self.Module.InterfaceSection.Declarations.Add(locSym);
      Self.Module.InterfaceSection.Types.Add(locSym);
    end;
    if not locSym.InheritsFrom(TPasType) then
      raise EXsdInvalidTypeDefinitionException.CreateFmt('Invalid array type definition, invalid item type definition : "%s".',[FTypeName]);
    Result := FSymbols.CreateArray(AInternalName,locSym as TPasType,s_item,s_item,asScoped);
    if AHasInternalName then
      FSymbols.RegisterExternalAlias(Result,ATypeName);
  end;
  
  function IsHeaderBlock() : Boolean;
  var
    strBuffer : string;
  begin
    Result := wst_findCustomAttributeXsd(FContext.GetXsShortNames(),FTypeNode,s_WST_headerBlock,strBuffer) and AnsiSameText('true',Trim(strBuffer));
  end;

  function IsRecordType() : Boolean;
  var
    strBuffer : string;
  begin
    Result := wst_findCustomAttributeXsd(FContext.GetXsShortNames(),FTypeNode,s_WST_record,strBuffer) and AnsiSameText('true',Trim(strBuffer));
  end;
  
  procedure ParseElementsAndAttributes(AEltCrs, AEltAttCrs : IObjectCursor);
  begin
    if Assigned(AEltCrs) then begin
      AEltCrs.Reset();
      while AEltCrs.MoveNext() do begin
        ParseElement((AEltCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
    if Assigned(AEltAttCrs) then begin
      AEltAttCrs.Reset();
      while AEltAttCrs.MoveNext() do begin
        ParseElement((AEltAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
  end;
  
var
  eltCrs, eltAttCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
  arrayDef : TPasArrayType;
  propTyp, tmpPropTyp : TPasProperty;
  tmpClassDef : TPasClassType;
  i : Integer;
  recordType : TPasRecordType;
  tmpRecVar : TPasVariable;
  locStrBuffer : string;
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

  if ( FDerivationMode = dmRestriction ) and FSymbols.SameName(FBaseType,s_array) then begin
    Result := ExtractSoapArray(internalName,hasInternalName);
  end else begin
    arrayItems := TObjectList.Create(False);
    try
      classDef := TPasClassType(FSymbols.CreateElement(TPasClassType,internalName,Self.Module.InterfaceSection,visDefault,'',0));
      try
        classDef.ObjKind := okClass;
        Result := classDef;
        if hasInternalName then
          FSymbols.RegisterExternalAlias(classDef,ATypeName);
        if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
          classDef.AncestorType := FBaseType;
        end;
        if ( classDef.AncestorType = nil ) then begin
          if IsHeaderBlock() then
            classDef.AncestorType := FSymbols.FindElementInModule('THeaderBlock',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType
          else
            classDef.AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
        end;
        classDef.AncestorType.AddRef();
        if Assigned(eltCrs) or Assigned(eltAttCrs) then begin
          isArrayDef := False;
          ParseElementsAndAttributes(eltCrs,eltAttCrs);
          if ( arrayItems.Count > 0 ) then begin
            if ( arrayItems.Count = 1 ) and ( GetElementCount(classDef.Members,TPasProperty) = 1 ) then begin
              Result := nil;
              propTyp := arrayItems[0] as TPasProperty;
              arrayDef := FSymbols.CreateArray(internalName,propTyp.VarType,propTyp.Name,FSymbols.GetExternalName(propTyp),asScoped);
              FSymbols.FreeProperties(classDef);
              FreeAndNil(classDef);
              Result := arrayDef;
              if hasInternalName then
                FSymbols.RegisterExternalAlias(arrayDef,ATypeName);
            end else begin
              GenerateArrayTypes(internalName,arrayItems);
              tmpClassDef := classDef;
              classDef := TPasClassType(FSymbols.CreateElement(TPasClassType,tmpClassDef.Name,Self.Module.InterfaceSection,visPublic,'',0));
              classDef.ObjKind := okClass;
              Result := classDef;
              classDef.AncestorType := tmpClassDef.AncestorType;
              classDef.AncestorType.AddRef();
              if hasInternalName then
                FSymbols.RegisterExternalAlias(classDef,ATypeName);
              for i := 0 to Pred(tmpClassDef.Members.Count) do begin
                if TPasElement(tmpClassDef.Members[i]).InheritsFrom(TPasProperty) then begin
                  propTyp := TPasProperty(tmpClassDef.Members[i]);
                  if ( arrayItems.IndexOf(propTyp) = -1 ) then begin
                    tmpPropTyp := TPasProperty(FSymbols.CreateElement(TPasProperty,propTyp.Name,classDef,visPublished,'',0));
                    if FSymbols.IsAttributeProperty(propTyp) then begin
                      FSymbols.SetPropertyAsAttribute(tmpPropTyp,True);
                    end;
                    tmpPropTyp.VarType := propTyp.VarType;
                    tmpPropTyp.VarType.AddRef();
                    tmpPropTyp.StoredAccessorName := propTyp.StoredAccessorName;
                    FSymbols.RegisterExternalAlias(tmpPropTyp,FSymbols.GetExternalName(propTyp));
                    classDef.Members.Add(tmpPropTyp);
                  end else begin
                    tmpPropTyp := TPasProperty(FSymbols.CreateElement(TPasProperty,propTyp.Name,classDef,visPublished,'',0));
                    tmpPropTyp.StoredAccessorName := propTyp.StoredAccessorName;
                    tmpPropTyp.VarType := FSymbols.FindElement(Format('%s_%sArray',[internalName,propTyp.Name])) as TPasType;
                    tmpPropTyp.VarType.AddRef();
                    FSymbols.RegisterExternalAlias(tmpPropTyp,FSymbols.GetExternalName(propTyp));
                    classDef.Members.Add(tmpPropTyp);
                  end;
                end;
              end;
              FSymbols.FreeProperties(tmpClassDef);
              FreeAndNil(tmpClassDef);
            end;
          end;
        end;

        //check for record
        if ( FDerivationMode = dmNone ) and
           Result.InheritsFrom(TPasClassType) and
           IsRecordType()
        then begin
          tmpClassDef := classDef;
          classDef := nil;
          recordType := TPasRecordType(FSymbols.CreateElement(TPasRecordType,tmpClassDef.Name,Self.Module.InterfaceSection,visPublic,'',0));
          Result := recordType;
          if hasInternalName then
            FSymbols.RegisterExternalAlias(recordType,ATypeName);
          for i := 0 to Pred(tmpClassDef.Members.Count) do begin
            if TPasElement(tmpClassDef.Members[i]).InheritsFrom(TPasProperty) then begin
              propTyp := TPasProperty(tmpClassDef.Members[i]);
              tmpRecVar := TPasVariable(FSymbols.CreateElement(TPasVariable,propTyp.Name,recordType,visPublic,'',0));
              tmpRecVar.VarType := propTyp.VarType;
              tmpRecVar.VarType.AddRef();
              FSymbols.RegisterExternalAlias(tmpRecVar,FSymbols.GetExternalName(propTyp));
              recordType.Members.Add(tmpRecVar);
              if FSymbols.IsAttributeProperty(propTyp) then begin
                FSymbols.SetPropertyAsAttribute(tmpRecVar,True);
              end;
              if AnsiSameText(propTyp.StoredAccessorName,'False') then
                locStrBuffer := s_prohibited
              else if AnsiSameText(Copy(propTyp.StoredAccessorName,1,3),'Has') then
                locStrBuffer := s_optional
              else
                locStrBuffer := s_required;
              FSymbols.Properties.SetValue(tmpRecVar,s_WST_storeType,locStrBuffer);    
            end;
          end;
          FSymbols.FreeProperties(tmpClassDef);
          FreeAndNil(tmpClassDef);
        end;
      except
        FSymbols.FreeProperties(Result);
        FreeAndNil(Result);
        raise;
      end;
    finally
      FreeAndNil(arrayItems);
    end;
  end;
end;

function TComplexTypeParser.ParseSimpleContent(const ATypeName : string) : TPasType;

  function ExtractAttributeCursor():IObjectCursor;
  var
    frstCrsr, tmpCursor : IObjectCursor;
    parentNode, tmpNode : TDOMNode;
    locFilterStr : string;
    xsShortNameList : TStrings;
  begin
    Result := nil;
    parentNode := FContentNode;
    if parentNode.HasChildNodes() then begin;
      xsShortNameList := FContext.GetXsShortNames();
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      locFilterStr := CreateQualifiedNameFilterStr(s_extension,xsShortNameList) + ' or ' +
                      CreateQualifiedNameFilterStr(s_restriction,xsShortNameList) ;
      tmpCursor := CreateCursorOn(frstCrsr.Clone() as IObjectCursor,ParseFilter(locFilterStr,TDOMNodeRttiExposer));
      if Assigned(tmpCursor) then begin
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if tmpNode.HasChildNodes() then begin
            locFilterStr := CreateQualifiedNameFilterStr(s_attribute,xsShortNameList);
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
  locClassDef : TPasClassType;

  procedure ParseAttribute(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locStoreOpt : string;
    locType : TPasElement;
    locStoreOptIdx : Integer;
    locAttObj : TPasProperty;
    locInternalEltName : string;
    locHasInternalName : boolean;
  begin
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : missing "name" attribute.',[s_attribute]);
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(locName) then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : empty "name".',[s_attribute]);

    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : missing "type" attribute.',[s_attribute]);
    locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : empty "type".',[s_attribute]);
    locType := FSymbols.FindElement(locTypeName) as TPasType;
    if not Assigned(locType) then begin
      locType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locTypeName,Self.Module.InterfaceSection,visPublic,'',0));
      Self.Module.InterfaceSection.Declarations.Add(locType);
      Self.Module.InterfaceSection.Types.Add(locType);
    end;
    
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStoreOpt := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      if IsStrEmpty(locStoreOpt) then
        raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : empty "use".',[s_attribute]);
      locStoreOptIdx := AnsiIndexText(locStoreOpt,[s_required,s_optional,s_prohibited]);
      if ( locStoreOptIdx < 0 ) then
        raise EXsdInvalidDefinitionException.CreateFmt('Invalid <%s> definition : invalid "use" value "%s".',[s_attribute,locStoreOpt]);
    end else begin
      locStoreOptIdx := 1{optional by default!}; //0;
    end;

    locInternalEltName := locName;
    locHasInternalName := IsReservedKeyWord(locInternalEltName);
    if locHasInternalName then
      locInternalEltName := Format('_%s',[locInternalEltName]);
      
    locAttObj := TPasProperty(FSymbols.CreateElement(TPasProperty,locInternalEltName,locClassDef,visPublished,'',0));
    locClassDef.Members.Add(locAttObj);
    locAttObj.VarType := locType as TPasType;
    locAttObj.VarType.AddRef();
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locAttObj,locName);
    FSymbols.SetPropertyAsAttribute(locAttObj,True);
    case locStoreOptIdx of
      0 : locAttObj.StoredAccessorName := 'True';
      1 : locAttObj.StoredAccessorName := 'Has' + locAttObj.Name;
      2 : locAttObj.StoredAccessorName := 'False';
    end;
  end;

var
  locAttCrs : IObjectCursor;
  internalName : string;
  hasInternalName : Boolean;
begin
  ExtractBaseType();
  if not ( FDerivationMode in [dmExtension, dmRestriction] ) then
    raise EXsdInvalidTypeDefinitionException.Create('Invalid "complexeType.simpleType" definition : restriction/extension not found.');

  internalName := ATypeName;
  hasInternalName := IsReservedKeyWord(internalName) or
                     ( not IsValidIdent(internalName) );{ or
                     ( FSymbols.IndexOf(internalName) <> -1 );}
  if hasInternalName then
    internalName := Format('_%s',[internalName]);

  locAttCrs := ExtractAttributeCursor();
  locClassDef := TPasClassType(FSymbols.CreateElement(TPasClassType,Trim(internalName),Self.Module.InterfaceSection,visDefault,'',0));
  try
    locClassDef.ObjKind := okClass;
    Result := locClassDef;
    if hasInternalName then
      FSymbols.RegisterExternalAlias(locClassDef,ATypeName);
    if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
      locClassDef.AncestorType := FBaseType;
    end;
    if ( locClassDef.AncestorType = nil ) then begin
      locClassDef.AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
    end;
    locClassDef.AncestorType.AddRef();
    if ( locAttCrs <> nil ) then begin
      locAttCrs.Reset();
      while locAttCrs.MoveNext() do begin
        ParseAttribute((locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
  except
    FSymbols.FreeProperties(Result);
    FreeAndNil(Result);
    raise;
  end;
end;

function TComplexTypeParser.ParseEmptyContent(const ATypeName: string): TPasType;
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
  Result := TPasClassType(FSymbols.CreateElement(TPasClassType,internalName,Self.Module.InterfaceSection,visDefault,'',0));
  TPasClassType(Result).ObjKind := okClass;
  if hasInternalName then
    FSymbols.RegisterExternalAlias(Result,ATypeName);
  TPasClassType(Result).AncestorType := FSymbols.FindElementInModule('TBaseComplexRemotable',FSymbols.FindModule('base_service_intf') as TPasModule) as TPasType;
  TPasClassType(Result).AncestorType.AddRef();
end;

class function TComplexTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_complexType;
end;

function TComplexTypeParser.Parse() : TPasType;
var
  locSym : TPasElement;
  locContinue : Boolean;
begin
  if not AnsiSameText(ExtractNameFromQName(FTypeNode.NodeName),s_complexType) then
    raise EXsdParserAssertException.CreateFmt('%s expected but %s found.',[s_complexType,ExtractNameFromQName(FTypeNode.NodeName)]);
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FSymbols.FindElement(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TPasType) then
      raise EXsdParserException.CreateFmt('Symbol found in the symbol table but is not a type definition : %s.',[FTypeName]);
    locContinue := locSym.InheritsFrom(TPasUnresolvedTypeRef);
    if not locContinue then;
      Result := locSym as TPasType;
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
      raise EXsdParserAssertException.Create('Unable to find the <name> tag in the type node attributes.');
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserAssertException.Create('Invalid type name( the name is empty ).');
end;

function TSimpleTypeParser.ExtractContentType() : Boolean;
var
  locCrs, locAttCrs : IObjectCursor;
  tmpNode : TDOMNode;
  spaceShort : string;
begin
  locCrs := CreateCursorOn(
              FChildCursor.Clone() as IObjectCursor,
              ParseFilter(CreateQualifiedNameFilterStr(s_restriction,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
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
    FBaseNameSpace := '';
    if Assigned(tmpNode) then begin
      ExplodeQName(tmpNode.NodeValue,FBaseName,spaceShort);
      if not FContext.FindNameSpace(spaceShort,FBaseNameSpace) then
        raise EXsdParserAssertException.CreateFmt(SResolveError,[spaceShort]);
    end;
    locCrs := CreateChildrenCursor(FRestrictionNode,cetRttiNode) as IObjectCursor;
    if Assigned(locCrs) then begin
      locCrs := CreateCursorOn(
                  locCrs,
                  ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
                );
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FIsEnum := True;
      end else begin
        if IsStrEmpty(FBaseName) then
          raise EXsdParserAssertException.CreateFmt('Base type is not specified for the simple type, parsing : "%s".',[FTypeName]);
        FIsEnum := False
      end;
    end else begin
      if IsStrEmpty(FBaseName) then
        raise EXsdParserAssertException.CreateFmt('Base type is not specified for the simple type, parsing : "%s".',[FTypeName]);
      FIsEnum := False
    end;
    Result := True;
  end else begin
    //raise EWslParserException.CreateFmt('The parser only support "Restriction" mode simple type derivation, parsing : "%s".',[FTypeName]);
    Result := False;
  end;
end;

function TSimpleTypeParser.ParseEnumContent(): TPasType;

  function ExtractEnumCursor():IObjectCursor ;
  begin
    Result := CreateCursorOn(
                CreateChildrenCursor(FRestrictionNode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,FContext.GetXsShortNames()),TDOMNodeRttiExposer)
              );
  end;
  
var
  locRes : TPasEnumType;
  locOrder : Integer;
  
  procedure ParseEnumItem(AItemNode : TDOMNode);
  var
    tmpNode : TDOMNode;
    locItemName, locInternalItemName : string;
    locCrs : IObjectCursor;
    locItem : TPasEnumValue;
    locHasInternalName : Boolean;
    locBuffer : string;
  begin
    locCrs := CreateCursorOn(CreateAttributesCursor(AItemNode,cetRttiNode),ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_value)]),TDOMNodeRttiExposer)) as IObjectCursor;
    if not Assigned(locCrs) then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid "enum" item node : no value attribute, type = "%s".',[FTypeName]);
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid "enum" item node : no value attribute, type = "%s".',[FTypeName]);
    tmpNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    locItemName := tmpNode.NodeValue;
    if IsStrEmpty(locItemName) then
      raise EXsdInvalidDefinitionException.CreateFmt('Invalid "enum" item node : the value attribute is empty, type = "%s".',[FTypeName]);

    locInternalItemName := ExtractIdentifier(locItemName);
    locHasInternalName := IsReservedKeyWord(locInternalItemName) or
                          ( not IsValidIdent(locInternalItemName) ) or
                          ( FSymbols.FindElementInModule(locInternalItemName,Self.Module) <> nil ) or
                          FSymbols.IsEnumItemNameUsed(locInternalItemName,Self.Module) or
                          ( not AnsiSameText(locInternalItemName,locItemName) );
    if locHasInternalName then begin
      locBuffer := ExtractIdentifier(FSymbols.GetExternalName(locRes));
      if ( not IsStrEmpty(locBuffer) ) and ( locBuffer[Length(locBuffer)] <> '_' ) then begin
        locInternalItemName := Format('%s_%s',[locBuffer,locInternalItemName]);
      end else begin
        locInternalItemName := Format('%s%s',[locBuffer,locInternalItemName]);
      end;
    end;
    locItem := TPasEnumValue(FSymbols.CreateElement(TPasEnumValue,locInternalItemName,locRes,visDefault,'',0));
    locItem.Value := locOrder;
    locRes.Values.Add(locItem);
    //locItem := TEnumItemDefinition.Create(locInternalItemName,locRes,locOrder);
    if locHasInternalName then
      FSymbols.RegisterExternalAlias(locItem,locItemName);
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
                  ( ( FindElement(intrName) <> nil ) and ( not FindElement(intrName).InheritsFrom(TPasUnresolvedTypeRef) ) );
  if hasIntrnName then
    intrName := '_' + intrName;

  locRes := TPasEnumType(FSymbols.CreateElement(TPasEnumType,Trim(intrName),Self.Module.InterfaceSection,visDefault,'',0));
  try
    Result := locRes;
    if hasIntrnName then
      FSymbols.RegisterExternalAlias(locRes,FTypeName);
    locEnumCrs.Reset();
    locOrder := 0;
    while locEnumCrs.MoveNext() do begin
      ParseEnumItem((locEnumCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
    end;
  except
    FSymbols.FreeProperties(Result);
    FreeAndNil(Result);
    raise;
  end;
end;

function TSimpleTypeParser.ParseOtherContent(): TPasType;
begin  // todo : implement TSimpleTypeParser.ParseOtherContent
  if IsStrEmpty(FBaseName) then
    raise EXsdInvalidTypeDefinitionException.CreateFmt('Invalid simple type definition : base type not provided, "%s".',[FTypeName]);
  Result := TPasTypeAliasType(FSymbols.CreateElement(TPasTypeAliasType,FTypeName,Self.Module.InterfaceSection,visDefault,'',0));
  TPasTypeAliasType(Result).DestType := FindElementNS(FBaseNameSpace,FBaseName,nvtExpandValue) as TPasType;
  TPasTypeAliasType(Result).DestType.AddRef();
end;

class function TSimpleTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_simpleType;
end;

function TSimpleTypeParser.Parse(): TPasType;
var
  locSym : TPasElement;
  locContinue : Boolean;
begin
  if not AnsiSameText(ExtractNameFromQName(FTypeNode.NodeName),s_simpleType) then
    raise EXsdParserAssertException.CreateFmt('%s expected but %s found.',[s_simpleType,ExtractNameFromQName(FTypeNode.NodeName)]);
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FindElement(FTypeName);
  if Assigned(locSym) then begin
    if not locSym.InheritsFrom(TPasType) then
      raise EXsdParserAssertException.CreateFmt('Symbol found in the symbol table but is not a type definition : %s.',[FTypeName]);
    locContinue := locSym.InheritsFrom(TPasUnresolvedTypeRef);
    if not locContinue then begin
      Result := locSym as TPasType;
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
      FBaseNameSpace := s_xs;
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
