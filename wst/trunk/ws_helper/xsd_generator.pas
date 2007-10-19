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
unit xsd_generator;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM, wst_fpc_xml{$ENDIF},
  pastree, pascal_parser_intf;
  
type

  EXsdGeneratorException = class(Exception) end;
  TBaseTypeHandler = class;
  TBaseTypeHandlerClass = class of TBaseTypeHandler;

  IGenerator = interface
    ['{F69523B3-A6FF-4BFB-9ACB-D4B9F32DBCA9}']
    procedure Execute(
      ASymTable   : TwstPasTreeContainer;
      AModuleName : string
    );
  end;

  IXsdGenerator = interface(IGenerator)
    ['{FBFF92BC-B72B-4B85-8D16-379F9E548DDB}']
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
  end;
  
  IXsdTypeHandler = interface
    ['{541EA377-4F70-49B1-AFB4-FC62B24F567B}']
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument : TDOMDocument
    );
    function GetOwner() : IXsdGenerator;
  end;

  IXsdTypeHandlerRegistry = interface
    ['{C5666646-3426-4696-93EE-AFA8EE7CAE53}']
    function Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  end;

  { TCustomXsdGenerator }

  TCustomXsdGenerator = class(
    TInterfacedObject,
    IInterface,
    IGenerator,
    IXsdGenerator
  )
  private
    FDocument : TDOMDocument;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;virtual;abstract;
    procedure Execute(
      ASymTable   : TwstPasTreeContainer;
      AModuleName : string
    );

    procedure Prepare(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule
    );virtual;
    property Document : TDOMDocument read FDocument;
  public
    constructor Create(ADocument : TDOMDocument);
  end;

  { TXsdGenerator }

  TXsdGenerator = class(TCustomXsdGenerator)
  private
    FSchemaNode : TDOMElement;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;override;
    procedure Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);override;
  end;
  
  { TBaseTypeHandler }

  TBaseTypeHandler = class(TInterfacedObject,IXsdTypeHandler)
  private
    FOwner : Pointer;
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );virtual;abstract;
    function GetOwner() : IXsdGenerator;
    class function CanHandle(ASymbol : TObject) : Boolean;virtual;abstract;
  public
    constructor Create(AOwner : IGenerator);virtual;
  end;

  function GetNameSpaceShortName(
    const ANameSpace    : string;
          ADocument : TDOMDocument
  ):string;

  function GetXsdTypeHandlerRegistry():IXsdTypeHandlerRegistry;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation
uses xsd_consts, Contnrs, StrUtils;

type

  { TXsdTypeHandlerRegistry }

  TXsdTypeHandlerRegistry = class(TInterfacedObject,IInterface,IXsdTypeHandlerRegistry)
  private
    FList : TClassList;
  private
    function FindIndexOfHandler(ASymbol : TPasElement) : Integer;
  protected
    function Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  public
    constructor Create();
    destructor Destroy();override;
  end;
  
  { TTypeDefinition_TypeHandler }

  TTypeDefinition_TypeHandler = class(TBaseTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
  end;
  
  { TTypeAliasDefinition_TypeHandler }

  TTypeAliasDefinition_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TEnumTypeHandler }

  TEnumTypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TClassTypeDefinition_TypeHandler }

  TClassTypeDefinition_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  { TPasRecordType_TypeHandler }

  TPasRecordType_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TBaseArrayRemotable_TypeHandler }

  TBaseArrayRemotable_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

var
  XsdTypeHandlerRegistryInst : IXsdTypeHandlerRegistry = nil;
function GetXsdTypeHandlerRegistry():IXsdTypeHandlerRegistry;
begin
  Result := XsdTypeHandlerRegistryInst;
end;

procedure RegisterFondamentalTypes();
var
  r : IXsdTypeHandlerRegistry;
begin
  r := GetXsdTypeHandlerRegistry();
  r.Register(TEnumTypeHandler);
  r.Register(TClassTypeDefinition_TypeHandler);
  r.Register(TPasRecordType_TypeHandler);
  r.Register(TBaseArrayRemotable_TypeHandler);
  r.Register(TTypeAliasDefinition_TypeHandler);
end;

function GetTypeNameSpace(
  AContainer : TwstPasTreeContainer;
  AType      : TPasElement
) : string;
var
  locElt : TPasElement;
begin
  Result := '';
  locElt := AType;
  if ( locElt <> nil ) then begin
    if locElt.InheritsFrom(TPasUnresolvedTypeRef) then
      locElt := AContainer.FindElement(AContainer.GetExternalName(locElt));
    if ( locElt <> nil ) and
       ( not locElt.InheritsFrom(TPasUnresolvedTypeRef) ) and
       //locElt.InheritsFrom(TPasType) and
       ( locElt.Parent <> nil ) and
       ( locElt.Parent.Parent <> nil )
    then begin
      Result := AContainer.GetExternalName(locElt.Parent.Parent);
    end;
  end;
  Result := Trim(Result);
  if ( Length(Result) = 0 ) then
    Result := AContainer.GetExternalName(AContainer.CurrentModule);
end;

function FindAttributeByValueInNode(
  const AAttValue        : string;
  const ANode            : TDOMNode;
  out   AResAtt          : string;
  const AStartIndex      : Integer = 0;
  const AStartingWith    : string = ''
):boolean;
var
  i,c : Integer;
  b : Boolean;
begin
  AResAtt := '';
  if Assigned(ANode) and Assigned(ANode.Attributes) then begin
    b := ( Length(AStartingWith) = 0);
    c := Pred(ANode.Attributes.Length);
    if ( AStartIndex >= 0 ) then
      i := AStartIndex;
    for i := AStartIndex to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) and
         ( b or ( Pos(AStartingWith,ANode.Attributes.Item[i].NodeName) = 1 ))
      then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        ADocument : TDOMDocument
):string;
begin
  if FindAttributeByValueInNode(ANameSpace,ADocument.DocumentElement,Result,0, s_xmlns) then begin
    Result := Copy(Result,Length(s_xmlns+':')+1,MaxInt);
  end else begin
    Result := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
    ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,Result]),ANameSpace);
  end;
end;

function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;//inline;
begin
  Result := ADoc.CreateElement(ANodeName);
  AParent.AppendChild(Result);
end;

{ TWsdlTypeHandlerRegistry }

function TXsdTypeHandlerRegistry.FindIndexOfHandler(ASymbol: TPasElement): Integer;
Var
  i, c : Integer;
begin
  Result := -1;
  c := FList.Count;
  for i := 0 to Pred(c) do begin
    if TBaseTypeHandlerClass(FList[i]).CanHandle(ASymbol) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TXsdTypeHandlerRegistry.Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
) : Boolean;
var
  fct : TBaseTypeHandlerClass;
  i : Integer;
begin
  i := FindIndexOfHandler(ASymbol);
  Result := ( i >= 0 );
  if Result then begin
    fct := TBaseTypeHandlerClass(FList[i]);
    AHandler := fct.Create(Aowner) as IXsdTypeHandler;
  end;
end;

procedure TXsdTypeHandlerRegistry.Register(AFactory: TBaseTypeHandlerClass);
begin
  if ( FList.IndexOf(AFactory) = -1 ) then begin
    FList.Add(AFactory);
  end;
end;

constructor TXsdTypeHandlerRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TXsdTypeHandlerRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

{ TBaseTypeHandler }

function TBaseTypeHandler.GetOwner(): IXsdGenerator;
begin
  Result := IXsdGenerator(FOwner);
end;

constructor TBaseTypeHandler.Create(AOwner: IGenerator);
begin
  Assert(Assigned(AOwner));
  FOwner := Pointer(AOwner);
end;

{ TTypeDefinition_TypeHandler }

procedure TTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        ADocument: TDOMDocument
);
begin
  Assert(ASymbol.InheritsFrom(TPasType));
end;

class function TTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasType);
end;

function TTypeDefinition_TypeHandler.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := GetOwner().GetSchemaNode(ADocument);
end;

{ TTypeAliasDefinition_TypeHandler }

procedure TTypeAliasDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        ADocument: TDOMDocument
);
var
  typItm : TPasAliasType;
  ns_shortName, s : string;
  defSchemaNode, resNode : TDOMElement;
  unitExternalName, baseUnitExternalName : string;
begin
  inherited;
  typItm := ASymbol as TPasAliasType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    if FindAttributeByValueInNode(unitExternalName,ADocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(s_xmlns+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
      ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,ns_shortName]),unitExternalName);
    end;
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_element]);
    resNode := CreateElement(s,defSchemaNode,ADocument);
    resNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

    baseUnitExternalName := GetTypeNameSpace(AContainer,typItm.DestType);
    s := GetNameSpaceShortName(baseUnitExternalName,ADocument);
    s := Format('%s:%s',[s,AContainer.GetExternalName(typItm.DestType)]);
    resNode.SetAttribute(s_type,s) ;
  end;
end;

class function TTypeAliasDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasAliasType);
end;

{ TEnumTypeHandler }

procedure TEnumTypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol : TPasElement;
        ADocument : TDOMDocument
);
var
  typItm : TPasEnumType;
  ns_shortName, s : string;
  defSchemaNode, resNode, restrictNode : TDOMElement;
  i, c : Integer;
  unitExternalName : string;
begin
  typItm := ASymbol as TPasEnumType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    if FindAttributeByValueInNode(unitExternalName,ADocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(s_xmlns+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
      ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,ns_shortName]),unitExternalName);
    end;
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_simpleType]);
    resNode := CreateElement(s,defSchemaNode,ADocument);
    resNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;
      s := Format('%s:%s',[s_xs_short,s_restriction]);
      restrictNode := CreateElement(s,resNode,ADocument);
      restrictNode.SetAttribute(s_base,Format('%s:%s',[s_xs_short,'string'])) ;
      c := typItm.Values.Count;
      for i := 0 to pred(c) do begin
        s := Format('%s:%s',[s_xs_short,s_enumeration]);
        CreateElement(s,restrictNode,ADocument).SetAttribute(
          s_value,
          AContainer.GetExternalName(TPasEnumValue(typItm.Values[i]))
        );
      end;
  end;
end;

class function TEnumTypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasEnumType);
end;

{ TClassTypeDefinition_TypeHandler }
type TTypeCategory = ( tcComplexContent, tcSimpleContent );
procedure TClassTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        ADocument     : TDOMDocument
);
var
  cplxNode : TDOMElement;
  typItm : TPasClassType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defSchemaNode, sqcNode, propNode, derivationNode : TDOMElement;
  i : Integer;
  p : TPasProperty;
  typeCategory : TTypeCategory;
  hasSequence : Boolean;
  trueParent : TPasType;
begin
  inherited;
  typItm := ASymbol as TPasClassType;
  if Assigned(typItm) then begin
    GetNameSpaceShortName(AContainer.GetExternalName(AContainer.CurrentModule) ,ADocument);
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

    typeCategory := tcComplexContent;
    derivationNode := nil;
    hasSequence := True;
    if Assigned(typItm.AncestorType) then begin
      trueParent := typItm.AncestorType;

      if trueParent.InheritsFrom(TPasNativeClassType) and AnsiSameText('THeaderBlock',trueParent.Name) then begin
        cplxNode.SetAttribute(s_WST_headerBlock,'true');
      end;

      if trueParent.InheritsFrom(TPasAliasType) then begin
        trueParent := GetUltimeType(trueParent);
      end;
      if trueParent.InheritsFrom(TPasNativeSimpleContentClassType) or
         trueParent.InheritsFrom(TPasNativeSimpleType)
      then begin
        typeCategory := tcSimpleContent;
      end;
      derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_extension]),cplxNode,ADocument);
      s := Trim(GetNameSpaceShortName(GetTypeNameSpace(AContainer,trueParent),ADocument));
      if ( Length(s) > 0 ) then begin
        s := s + ':';
      end;
      s := s + AContainer.GetExternalName(trueParent);
      derivationNode.SetAttribute(s_base,s);
      hasSequence := False;
    end;
    if ( typItm.Members.Count > 0 ) then begin
      hasSequence := False;
      for i := 0 to Pred(typItm.Members.Count) do begin
        if TPasElement(typItm.Members[i]).InheritsFrom(TPasProperty) then begin
          p := TPasProperty(typItm.Members[i]);
          if not AContainer.IsAttributeProperty(p) then begin
            if ( typeCategory = tcSimpleContent ) then begin
              raise EXsdGeneratorException.CreateFmt('Invalid type definition, a simple type cannot have "not attribute" properties : "%s"',[AContainer.GetExternalName(ASymbol)]);
            end;
            hasSequence := True;
          end;
        end;
      end;
    end;
    if hasSequence then begin
      s := Format('%s:%s',[s_xs_short,s_sequence]);
      if Assigned(derivationNode) then begin
        sqcNode := CreateElement(s,derivationNode,ADocument);
      end else begin
        sqcNode := CreateElement(s,cplxNode,ADocument);
      end;
    end else begin
      sqcNode := nil;
    end;


      for i := 0 to Pred(typItm.Members.Count) do begin
        if TPasElement(typItm.Members[i]).InheritsFrom(TPasProperty) then begin
          p := TPasProperty(typItm.Members[i]);
          if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) or AnsiSameText('True',p.StoredAccessorName) then begin
            if AContainer.IsAttributeProperty(p) then begin
              s := Format('%s:%s',[s_xs_short,s_attribute]);
              if Assigned(derivationNode) then
                propNode := CreateElement(s,derivationNode,ADocument)
              else
                propNode := CreateElement(s,cplxNode,ADocument);
            end else begin
              s := Format('%s:%s',[s_xs_short,s_element]);
              propNode := CreateElement(s,sqcNode,ADocument);
            end;
            propNode.SetAttribute(s_name,AContainer.GetExternalName(p));
            propTypItm := p.VarType;
            if Assigned(propTypItm) then begin
              prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),ADocument);
              if GetUltimeType(propTypItm).InheritsFrom(TPasArrayType) then
                s := AContainer.GetExternalName(TPasArrayType(GetUltimeType(propTypItm)).ElType)
              else
                s := AContainer.GetExternalName(propTypItm);
              propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,s]));
              if AContainer.IsAttributeProperty(p) then begin
                if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then
                  propNode.SetAttribute(s_use,'optional')
                else
                  propNode.SetAttribute(s_use,'required');
              end else begin
                if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then
                  propNode.SetAttribute(s_minOccurs,'0');
                {else
                  propNode.SetAttribute(s_minOccurs,'1');}
                if GetUltimeType(propTypItm).InheritsFrom(TPasArrayType) then
                  propNode.SetAttribute(s_maxOccurs,s_unbounded)
                {else
                  propNode.SetAttribute(s_maxOccurs,'1');}
              end;
            end;
          end;
        end;
      end;
  end;
end;

class function TClassTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and
            ( ASymbol.InheritsFrom(TPasClassType) and ( TPasClassType(ASymbol).ObjKind = okClass ));
end;

{ TPasRecordType_TypeHandler }

procedure TPasRecordType_TypeHandler.Generate(
        AContainer : TwstPasTreeContainer;
  const ASymbol : TPasElement;
        ADocument : TDOMDocument
);
var
  cplxNode : TDOMElement;
  typItm : TPasRecordType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defSchemaNode, sqcNode, propNode : TDOMElement;
  i : Integer;
  p : TPasVariable;
  hasSequence : Boolean;
  storeOption : string;
begin
  inherited;
  typItm := ASymbol as TPasRecordType;
  if Assigned(typItm) then begin
    GetNameSpaceShortName(AContainer.GetExternalName(AContainer.CurrentModule) ,ADocument);
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

    cplxNode.SetAttribute(s_WST_record,'true');

    hasSequence := False;
    for i := 0 to Pred(typItm.Members.Count) do begin
      if TPasElement(typItm.Members[i]).InheritsFrom(TPasVariable) then begin
        p := TPasVariable(typItm.Members[i]);
        if not AContainer.IsAttributeProperty(p) then begin
          hasSequence := True;
          Break;
        end;
      end;
    end;
    if hasSequence then begin
      s := Format('%s:%s',[s_xs_short,s_sequence]);
      sqcNode := CreateElement(s,cplxNode,ADocument);
    end else begin
      sqcNode := nil;
    end;


      for i := 0 to Pred(typItm.Members.Count) do begin
        if TPasElement(typItm.Members[i]).InheritsFrom(TPasVariable) then begin
          p := TPasVariable(typItm.Members[i]);
          if AContainer.IsAttributeProperty(p) then begin
            s := Format('%s:%s',[s_xs_short,s_attribute]);
            propNode := CreateElement(s,cplxNode,ADocument);
          end else begin
            s := Format('%s:%s',[s_xs_short,s_element]);
            propNode := CreateElement(s,sqcNode,ADocument);
          end;
          propNode.SetAttribute(s_name,AContainer.GetExternalName(p));
          propTypItm := p.VarType;
          if Assigned(propTypItm) then begin
            prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),ADocument);
            propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
            storeOption := Trim(AContainer.Properties.GetValue(p,s_WST_storeType));
            if AContainer.IsAttributeProperty(p) then begin
              if ( Length(storeOption) > 0 ) then begin
                case AnsiIndexText(storeOption,[s_required,s_optional,s_prohibited]) of
                  0 : propNode.SetAttribute(s_use,storeOption);
                  1 : ;
                  2 : propNode.SetAttribute(s_use,storeOption);
                  else
                    raise EXsdGeneratorException.CreateFmt('Invalid attribute "%s" value : "%s".',[s_use,storeOption]);
                end;
              end;
            end else begin
              case AnsiIndexText(storeOption,[s_required,s_optional,s_prohibited]) of
                0 : ;//propNode.SetAttribute(s_minOccurs,'1');
                1 : propNode.SetAttribute(s_minOccurs,'0');
              end;
              //propNode.SetAttribute(s_maxOccurs,'1');
            end;
          end;
        end;
      end;
  end;
end;

class function TPasRecordType_TypeHandler.CanHandle(ASymbol : TObject) : Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasRecordType);
end;

{ TBaseArrayRemotable_TypeHandler }

procedure TBaseArrayRemotable_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        ADocument : TDOMDocument
);

  function GetNameSpaceShortName(const ANameSpace : string):string;//inline;
  begin
    if FindAttributeByValueInNode(ANameSpace,ADocument.DocumentElement,Result,0,s_xmlns) then begin
      Result := Copy(Result,Length(s_xmlns+':')+1,MaxInt);
    end else begin
      Result := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
      ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,Result]),ANameSpace);
    end;
  end;

var
  typItm : TPasArrayType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defSchemaNode, cplxNode, sqcNode, propNode : TDOMElement;
  unitExternalName : string;
begin
  inherited;
  typItm := ASymbol as TPasArrayType;
  if not Assigned(typItm) then
    Exit;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,typItm);
    GetNameSpaceShortName(unitExternalName);
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

      s := Format('%s:%s',[s_xs_short,s_sequence]);
      sqcNode := CreateElement(s,cplxNode,ADocument);
      propTypItm := typItm.ElType;
      s := Format('%s:%s',[s_xs_short,s_element]);
      propNode := CreateElement(s,sqcNode,ADocument);
      propNode.SetAttribute(s_name,s_item);
      if Assigned(propTypItm) then begin
        prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm));//  AContainer.GetExternalName(propTypItm.Parent.Parent));
        propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
        propNode.SetAttribute(s_minOccurs,'0');
        propNode.SetAttribute(s_maxOccurs,s_unbounded);
      end;
  end;
end;

class function TBaseArrayRemotable_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasArrayType);
end;

{ TCustomXsdGenerator }

procedure TCustomXsdGenerator.Execute(
  ASymTable   : TwstPasTreeContainer;
  AModuleName : string
);
var
  j, k : Integer;
  tri : TPasElement;
  g : IXsdTypeHandler;
  gr : IXsdTypeHandlerRegistry;
  typeList : TList;
  mdl : TPasModule;
begin
  if ( ASymTable = nil ) then
    raise EXsdGeneratorException.Create('Invalid symbol table.');
  mdl := ASymTable.FindModule(AModuleName);
  if ( mdl = nil ) then
    raise EXsdGeneratorException.CreateFmt('Unable to find module : "%s".',[AModuleName]);
  Prepare(ASymTable,mdl);
  gr := GetXsdTypeHandlerRegistry();
  typeList := mdl.InterfaceSection.Declarations;
  k := typeList.Count;
  for j := 0 to Pred(k) do begin
    tri := TPasElement(typeList[j]);
    if tri.InheritsFrom(TPasType) and
       ( not tri.InheritsFrom(TPasNativeClassType) ) and
       ( not tri.InheritsFrom(TPasNativeSimpleType) )
    then begin
      if gr.Find(tri,Self,g) then
        g.Generate(ASymTable,tri,Self.Document);
    end;
  end;
end;

procedure TCustomXsdGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
begin

end;

constructor TCustomXsdGenerator.Create(ADocument : TDOMDocument);
begin
  if ( ADocument = nil ) then
    raise EXsdGeneratorException.Create('Invalid document.');
  FDocument := ADocument;
end;

{ TXsdGenerator }

function TXsdGenerator.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := FSchemaNode;
end;

procedure TXsdGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
var
  unitExternalName : string;
begin
  inherited Prepare(ASymTable, AModule);
  unitExternalName := ASymTable.GetExternalName(AModule);
  FSchemaNode := CreateElement(s_schema,Document,Document);
  FSchemaNode.SetAttribute(s_targetNamespace,unitExternalName);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_xs_short]),s_xs);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_tns]),unitExternalName);
end;

initialization
  XsdTypeHandlerRegistryInst := TXsdTypeHandlerRegistry.Create() as IXsdTypeHandlerRegistry;
  RegisterFondamentalTypes();

finalization
  XsdTypeHandlerRegistryInst := nil;
  
end.
