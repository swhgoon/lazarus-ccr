{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_parsers;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  pastree, pascal_parser_intf, xsd_parser, wsdl_parser;

type

  { TTest_CustomXsdParser }

  TTest_CustomXsdParser = class(TTestCase)
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;virtual;abstract;
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
  published
    procedure EmptySchema();

    procedure SimpleType_Enum();
    procedure SimpleType_Enum_Embedded();
    
    procedure ComplexType_Class();
    procedure ComplexType_Class_Embedded();
  end;

  { TTest_XsdParser }

  TTest_XsdParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(const ADoc : string) : TwstPasTreeContainer;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
  end;
  
  { TTest_WsdlParser }

  TTest_WsdlParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(const ADoc : string) : TwstPasTreeContainer;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
  end;
  
implementation
uses parserutils;

const
  x_complexType_SampleClassType     = 'TClassSampleType';
  x_complexType_SampleClassTypeAll     = 'TClassSampleTypeAll';
  x_complexType_SampleClass         = 'TClassSample';
  x_complexType_class               = 'complex_class';
  x_complexType_class_embedded      = 'complex_class_embedded';

  x_empty                 = 'empty';

  x_enumSample            = 'EnumSample';
  x_enumSampleType        = 'EnumSampleType';
    x_enumSampleLIST_COUNT = 7;
    x_enumSampleLIST      : array[0..( x_enumSampleLIST_COUNT - 1 )] of string = ( 'esOne', 'esTwo', 'esThree', 'begin', 'finally', 'True', 'False' );
  x_simpleType            = 'simpletype';
  x_simpleTypeEmbedded    = 'simpletype_embedded';

  x_targetNamespace       = 'urn:wst-test';


  x_byteField  = 'byteField';
  x_charField  = 'charField';
  x_classField = 'classField';
  x_enumField  = 'enumField';
  x_floatField = 'floatField';
  x_intField   = 'intField';
  x_longField  = 'longField';
  x_strField   = 'strField';

  x_intAtt     = 'intAtt';
  x_strAtt     = 'strAtt';


function LoadXmlFile(const AFileName : string) : TXMLDocument;
begin
  Result := nil;
  ReadXMLFile(Result,AFileName);
end;

{ TTest_CustomXsdParser }

procedure TTest_CustomXsdParser.EmptySchema();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
begin
  tr := LoadEmptySchema();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_empty,mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  CheckEquals(0,mdl.InterfaceSection.Declarations.Count);
end;

procedure TTest_CustomXsdParser.SimpleType_Enum();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  enumType : TPasEnumType;
  enumVal : TPasEnumValue;
  aliasType : TPasAliasType;
  i : Integer;
begin
  tr := LoadSimpleType_Enum_Schema();

  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpleType,mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(2,ls.Count);
  elt := tr.FindElement(x_enumSampleType);
    CheckNotNull(elt,x_enumSampleType);
    CheckEquals(x_enumSampleType,elt.Name);
    CheckEquals(x_enumSampleType,tr.GetExternalName(elt));
    CheckIs(elt,TPasEnumType);
    enumType := elt as TPasEnumType;
    CheckEquals(x_enumSampleLIST_COUNT,enumType.Values.Count);
    for i := 0 to Pred(x_enumSampleLIST_COUNT) do begin
      enumVal := TPasEnumValue(enumType.Values[i]);
      CheckNotNull(enumVal);
      if IsReservedKeyWord(x_enumSampleLIST[i]) then begin
        CheckEquals(Format('%s_%s',[enumType.Name,x_enumSampleLIST[i]]),enumVal.Name);
      end else begin
        CheckEquals(x_enumSampleLIST[i],enumVal.Name);
      end;
      CheckEquals(x_enumSampleLIST[i],tr.GetExternalName(enumVal));
    end;
    
  elt := tr.FindElement(x_enumSample);
    CheckNotNull(elt,x_enumSample);
    CheckEquals(x_enumSample,elt.Name);
    CheckEquals(x_enumSample,tr.GetExternalName(elt));
    CheckIs(elt,TPasAliasType);
    aliasType := elt as TPasAliasType;
    CheckNotNull(aliasType.DestType);
    CheckEquals(x_enumSampleType, tr.GetExternalName(aliasType.DestType));
end;

procedure TTest_CustomXsdParser.SimpleType_Enum_Embedded();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  enumType : TPasEnumType;
  enumVal : TPasEnumValue;
  aliasType : TPasAliasType;
  i : Integer;
begin
  tr := LoadSimpleType_Enum_Embedded_Schema();

  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpleTypeEmbedded,mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(1,ls.Count);
  elt := tr.FindElement(x_enumSampleType);
    CheckNotNull(elt,x_enumSampleType);
    CheckEquals(x_enumSampleType,elt.Name);
    CheckEquals(x_enumSampleType,tr.GetExternalName(elt));
    CheckIs(elt,TPasEnumType);
    enumType := elt as TPasEnumType;
    CheckEquals(x_enumSampleLIST_COUNT,enumType.Values.Count);
    for i := 0 to Pred(x_enumSampleLIST_COUNT) do begin
      enumVal := TPasEnumValue(enumType.Values[i]);
      CheckNotNull(enumVal);
      if IsReservedKeyWord(x_enumSampleLIST[i]) then begin
        CheckEquals(Format('%s_%s',[enumType.Name,x_enumSampleLIST[i]]),enumVal.Name);
      end else begin
        CheckEquals(x_enumSampleLIST[i],enumVal.Name);
      end;
      CheckEquals(x_enumSampleLIST[i],tr.GetExternalName(enumVal));
    end;
end;

type
  TPropertyType = ( ptField, ptAttribute );
const
  PropertyType_Att : array[TPropertyType] of Boolean = ( False, True );
procedure TTest_CustomXsdParser.ComplexType_Class();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  
  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,tr.GetExternalName(prp.VarType));
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;
  
var
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_class,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);
    elt := tr.FindElement(x_complexType_SampleClassType);
      CheckNotNull(elt,x_complexType_SampleClassType);
      CheckEquals(x_complexType_SampleClassType,elt.Name);
      CheckEquals(x_complexType_SampleClassType,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(8,prpLs.Count);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
        CheckProperty(x_floatField,'float',ptField);
        CheckProperty(x_byteField,'byte',ptField);
        CheckProperty(x_charField,'char',ptField);
        CheckProperty(x_longField,'long',ptField);
        CheckProperty(x_strAtt,'string',ptAttribute);
        CheckProperty(x_intAtt,'int',ptAttribute);


    elt := tr.FindElement(x_complexType_SampleClass);
      CheckNotNull(elt,x_complexType_SampleClass);
      CheckEquals(x_complexType_SampleClass,elt.Name);
      CheckEquals(x_complexType_SampleClass,tr.GetExternalName(elt));
      CheckIs(elt,TPasAliasType);
      aliasType := elt as TPasAliasType;
      CheckNotNull(aliasType.DestType);
      CheckEquals(x_complexType_SampleClassType, tr.GetExternalName(aliasType.DestType));

    elt := tr.FindElement(x_complexType_SampleClassTypeAll);
      CheckNotNull(elt,x_complexType_SampleClassTypeAll);
      CheckEquals(x_complexType_SampleClassTypeAll,elt.Name);
      CheckEquals(x_complexType_SampleClassTypeAll,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(8,prpLs.Count);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
        CheckProperty(x_floatField,'float',ptField);
        CheckProperty(x_byteField,'byte',ptField);
        CheckProperty(x_charField,'char',ptField);
        CheckProperty(x_longField,'long',ptField);
        CheckProperty(x_strAtt,'string',ptAttribute);
        CheckProperty(x_intAtt,'int',ptAttribute);

  finally
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Embedded();
var
  tr : TwstPasTreeContainer;
  nestedClassName, nestedEnumName : string;

  procedure CheckProperty(
    const AName,ATypeName : string;
    const AFieldType : TPropertyType;
    const AClsType : TPasClassType
  );
  var
    prp : TPasProperty;
  begin
    prp := FindMember(AClsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,tr.GetExternalName(prp.VarType));
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

  procedure CheckEmbeddedClassType();
  var
    mdl : TPasModule;
    e : TPasElement;
    k : Integer;
    prpLst : TList;
    nestedClsType : TPasClassType;
  begin
    prpLst := TList.Create();
    try
      nestedClassName := Format('%s_%s_Type',[x_complexType_SampleClassType,x_classField]);
      e := tr.FindElement(nestedClassName);
        CheckNotNull(e,nestedClassName);
        CheckEquals(nestedClassName,e.Name);
        CheckEquals(nestedClassName,tr.GetExternalName(e));
        CheckIs(e,TPasClassType);
        nestedClsType := e as TPasClassType;
          for k := 0 to Pred(nestedClsType.Members.Count) do begin
            if TPasElement(nestedClsType.Members[k]).InheritsFrom(TPasProperty) then
              prpLst.Add(nestedClsType.Members[k]);
          end;
        CheckEquals(4,prpLst.Count,nestedClassName + '  properties count.');
          CheckProperty(x_intField + 'E','int',ptField,nestedClsType);
          CheckProperty(x_strField + 'E','string',ptField,nestedClsType);
          CheckProperty(x_strAtt + 'E','string',ptAttribute,nestedClsType);
          CheckProperty(x_intAtt + 'E','int',ptAttribute,nestedClsType);
    finally
      FreeAndNil(prpLst);
    end;
  end;
  
  procedure CheckEmbeddedEnum();
  var
    e : TPasElement;
    enumType : TPasEnumType;
    enumVal : TPasEnumValue;
    k : Integer;
  begin
    nestedEnumName := Format('%s_%s_Type',[x_complexType_SampleClassType,x_enumField]);
    e := tr.FindElement(nestedEnumName);
      CheckNotNull(e,nestedEnumName);
      CheckEquals(nestedEnumName,e.Name);
      CheckEquals(nestedEnumName,tr.GetExternalName(e));
      CheckIs(e,TPasEnumType);
      enumType := e as TPasEnumType;
      CheckEquals(x_enumSampleLIST_COUNT,enumType.Values.Count);
      for k := 0 to Pred(x_enumSampleLIST_COUNT) do begin
        enumVal := TPasEnumValue(enumType.Values[k]);
        CheckNotNull(enumVal,'Enum value');
        if IsReservedKeyWord(x_enumSampleLIST[k]) then begin
          CheckEquals(Format('%s_%s',[enumType.Name,x_enumSampleLIST[k]]),enumVal.Name);
        end else begin
          CheckEquals(x_enumSampleLIST[k],enumVal.Name);
        end;
        CheckEquals(x_enumSampleLIST[k],tr.GetExternalName(enumVal));
      end;
  end;

var
  mdl : TPasModule;
  clsType : TPasClassType;
  ls : TList;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Embedded_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_class_embedded,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);

    CheckEmbeddedClassType();
    CheckEmbeddedEnum();
    
    elt := tr.FindElement(x_complexType_SampleClassType);
      CheckNotNull(elt,x_complexType_SampleClassType);
      CheckEquals(x_complexType_SampleClassType,elt.Name);
      CheckEquals(x_complexType_SampleClassType,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(10,prpLs.Count);
        CheckProperty(x_intField,'int',ptField,clsType);
        CheckProperty(x_strField,'string',ptField,clsType);
        CheckProperty(x_floatField,'float',ptField,clsType);
        CheckProperty(x_byteField,'byte',ptField,clsType);
        CheckProperty(x_charField,'char',ptField,clsType);
        CheckProperty(x_longField,'long',ptField,clsType);
        CheckProperty(x_strAtt,'string',ptAttribute,clsType);
        CheckProperty(x_intAtt,'int',ptAttribute,clsType);
        CheckProperty(x_classField,nestedClassName,ptField,clsType);
        CheckProperty(x_enumField,nestedEnumName,ptField,clsType);

  finally
    FreeAndNil(prpLs);
  end;
end;

{ TTest_XsdParser }

function TTest_XsdParser.ParseDoc(const ADoc: string): TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prs : IXsdPaser;
  fileName : string;
begin
  fileName := Format('.%sfiles%s%s.xsd',[PathDelim,PathDelim,ADoc]);
{$IFNDEF FPC}
  fileName := Format('..%s%s',[PathDelim,fileName]);
{$ENDIF}
  locDoc := LoadXmlFile(fileName);
  try
    Result := TwstPasTreeContainer.Create();
    CreateWstInterfaceSymbolTable(Result);
    prs := TXsdParser.Create(locDoc,Result,ADoc);
    prs.ParseTypes();
  finally
    ReleaseDomNode(locDoc);
  end;
end;

function TTest_XsdParser.LoadEmptySchema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_empty);
end;

function TTest_XsdParser.LoadSimpleType_Enum_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpleType);
end;

function TTest_XsdParser.LoadSimpleType_Enum_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpleTypeEmbedded);
end;

function TTest_XsdParser.LoadComplexType_Class_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class);
end;

function TTest_XsdParser.LoadComplexType_Class_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_embedded);
end;

{ TTest_WsdlParser }

function TTest_WsdlParser.ParseDoc(const ADoc: string): TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prs : IParser;
  fileName : string;
begin
  fileName := Format('.%sfiles%s%s.wsdl',[PathDelim,PathDelim,ADoc]);
{$IFNDEF FPC}
  fileName := Format('..%s%s',[PathDelim,fileName]);
{$ENDIF}
  locDoc := LoadXmlFile(fileName);
  try
    Result := TwstPasTreeContainer.Create();
    CreateWstInterfaceSymbolTable(Result);
    prs := TWsdlParser.Create(locDoc,Result);
    prs.Execute(pmAllTypes,ADoc);
  finally
    ReleaseDomNode(locDoc);
  end;
end;

function TTest_WsdlParser.LoadEmptySchema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_empty);
end;

function TTest_WsdlParser.LoadSimpleType_Enum_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpleType);
end;

function TTest_WsdlParser.LoadSimpleType_Enum_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpleTypeEmbedded);
end;

function TTest_WsdlParser.LoadComplexType_Class_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class);
end;

function TTest_WsdlParser.LoadComplexType_Class_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_embedded);
end;

initialization
  RegisterTest('XSD parser',TTest_XsdParser.Suite);
  RegisterTest('WSDL parser',TTest_WsdlParser.Suite);

end.
