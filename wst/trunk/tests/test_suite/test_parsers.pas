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
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;virtual;abstract;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;virtual;abstract;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;virtual;abstract;
  published
    procedure EmptySchema();

    procedure SimpleType_Enum();
    procedure SimpleType_Enum_Embedded();
    procedure SimpleType_AliasToNativeType();
    
    procedure ComplexType_Class();
    procedure ComplexType_Class_default_values();
    procedure ComplexType_Class_properties_extended_metadata();
    procedure ComplexType_Class_Embedded();
    procedure ComplexType_Class_Extend_Simple_Schema();
    
    procedure ComplexType_Record();
    procedure ComplexType_Record_Embedded();
    
    procedure ComplexType_ArraySequence();
    procedure ComplexType_ArraySequence_Embedded();
    procedure ComplexType_Array_soaparray();
    
    procedure ComplexType_CollectionSequence();
    procedure pascal_class_default_parent();
    
    procedure class_headerblock_derived();
    procedure class_headerblock_simplecontent_derived();
  end;

  { TTest_XsdParser }

  TTest_XsdParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(const ADoc : string) : TwstPasTreeContainer;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;
    
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;override;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;override;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;override;
  end;

  { TTest_WsdlParser }

  TTest_WsdlParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(const ADoc : string) : TwstPasTreeContainer;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;

    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;override;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;override;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;override;
  published
    procedure no_binding_style();
    procedure signature_last();
    procedure signature_result();
    procedure signature_return();
  end;
  
implementation
uses parserutils;

const
  x_complexType_SampleArrayIntFieldType     = 'TArrayIntFieldType';
  x_complexType_SampleArrayItemType         = 'TArrayItemType';
  
  x_complexType_SampleCollectionComplexType = 'TComplexType';
  x_complexType_SampleCollectionCollectionComplexType = 'TCollectionComplexType';
  x_complexType_SampleCollectionItemType    = 'TCollectionItemType';

  x_complexType_SampleDerivedType           = 'TClassSampleDerivedType';
  x_complexType_SampleClassType             = 'TClassSampleType';
  x_complexType_SampleClassTypeA            = 'TClassSampleTypeA';
  x_complexType_SampleClassTypeAll          = 'TClassSampleTypeAll';
  x_complexType_SampleClass                 = 'TClassSample';

  x_complexType_SampleRecordType             = 'TRecordSampleType';
  x_complexType_SampleRecordTypeAll          = 'TRecordSampleTypeAll';
  x_complexType_SampleRecord                 = 'TRecordSample';

  x_complexType_array_sequence      = 'complex_array_sequence';
  x_complexType_array_sequence_embedded  = 'complex_array_sequence_embedded';
  x_complexType_array_sequence_collection      = 'complex_array_sequence_collection';
  x_complexType_array_soaparray      = 'complex_array_soaparray';
  
  x_complexType_class               = 'complex_class';
  x_complexType_class_default       = 'complex_class_default';
  x_complexType_class_properties_extended_metadata = 'class_properties_extended_metadata';
  x_complexType_extend_simple = 'complex_class_extend_simple';
  x_complexType_class_embedded      = 'complex_class_embedded';
  x_complexType_record               = 'complex_record';
  x_complexType_record_embedded      = 'complex_record_embedded';

  x_empty                 = 'empty';

  x_enumSample            = 'EnumSample';
  x_enumSampleType        = 'EnumSampleType';
    x_enumSampleLIST_COUNT = 7;
    x_enumSampleLIST      : array[0..( x_enumSampleLIST_COUNT - 1 )] of string = ( 'esOne', 'esTwo', 'esThree', 'begin', 'finally', 'True', 'False' );
  x_simpleTypeAliasString = 'AliasString';
  x_simpleTypeAliasInt    = 'AliasInt';
  x_simpleType            = 'simpletype';
  x_simpleTypeEmbedded    = 'simpletype_embedded';
  x_simpletypeNativeAlias = 'simpletypeNativeAlias';

  x_targetNamespace       = 'urn:wst-test';


  x_byteField  = 'byteField';
  x_charField  = 'charField';
  x_classField = 'classField';
  x_enumField  = 'enumField';
  x_field      = 'field';
  x_floatField = 'floatField';
  x_intField   = 'intField';
  x_longField  = 'longField';
  x_strField   = 'strField';

  x_intAtt     = 'intAtt';
  x_strAtt     = 'strAtt';

  x_Item       = 'Item';

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

procedure TTest_CustomXsdParser.SimpleType_AliasToNativeType();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  aliasType : TPasAliasType;
begin
  tr := LoadSimpleType_AliasToNativeType_Schema();

  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpletypeNativeAlias,mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(2,ls.Count);
  elt := tr.FindElement(x_simpleTypeAliasString);
    CheckNotNull(elt,x_simpleTypeAliasString);
    CheckEquals(x_simpleTypeAliasString,elt.Name);
    CheckEquals(x_simpleTypeAliasString,tr.GetExternalName(elt));
    CheckIs(elt,TPasAliasType);
    aliasType := elt as TPasAliasType;
    CheckNotNull(aliasType.DestType);
    Check(tr.SameName(aliasType.DestType,'string'));

  elt := tr.FindElement(x_simpleTypeAliasInt);
    CheckNotNull(elt,x_simpleTypeAliasInt);
    CheckEquals(x_simpleTypeAliasInt,elt.Name);
    CheckEquals(x_simpleTypeAliasInt,tr.GetExternalName(elt));
    CheckIs(elt,TPasAliasType);
    aliasType := elt as TPasAliasType;
    CheckNotNull(aliasType.DestType);
    Check(tr.SameName(aliasType.DestType,'int'));
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
    CheckEquals(4,ls.Count);
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

    elt := tr.FindElement(x_complexType_SampleDerivedType);
      CheckNotNull(elt,x_complexType_SampleDerivedType);
      CheckEquals(x_complexType_SampleDerivedType,elt.Name);
      CheckEquals(x_complexType_SampleDerivedType,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckNotNull(clsType.AncestorType);
      CheckEquals(x_complexType_SampleClassType,tr.GetExternalName(clsType.AncestorType));
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(4,prpLs.Count);
        CheckProperty(x_intField + 'Ex','int',ptField);
        CheckProperty(x_strField + 'Ex','string',ptField);
        CheckProperty(x_strAtt + 'Ex','string',ptAttribute);
        CheckProperty(x_intAtt + 'Ex','int',ptAttribute);
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

procedure TTest_CustomXsdParser.ComplexType_Class_Extend_Simple_Schema();
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
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Extend_Simple_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_extend_simple,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(2,ls.Count);
    elt := tr.FindElement(x_complexType_SampleClassType);
      CheckNotNull(elt,x_complexType_SampleClassType);
      CheckEquals(x_complexType_SampleClassType,elt.Name);
      CheckEquals(x_complexType_SampleClassType,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        CheckNotNull(clsType.AncestorType,'AncestorType is null');
        CheckSame(tr.FindElementNS('TComplexStringContentRemotable',sXSD_NS),clsType.AncestorType);

        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(1,prpLs.Count);
        CheckProperty(x_intField,'int',ptAttribute);


    elt := tr.FindElement(x_complexType_SampleClassTypeA);
      CheckNotNull(elt,x_complexType_SampleClassTypeA);
      CheckEquals(x_complexType_SampleClassTypeA,elt.Name);
      CheckEquals(x_complexType_SampleClassTypeA,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        CheckNotNull(clsType.AncestorType,'AncestorType is null');
        CheckSame(tr.FindElementNS('TBase64StringExtRemotable',sXSD_NS),clsType.AncestorType);

        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(1,prpLs.Count);
        CheckProperty(x_floatField,'float',ptAttribute);
  finally
    tr.Free();
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Record();
var
  tr : TwstPasTreeContainer;
  recType : TPasRecordType;

  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasVariable;
  begin
    prp := FindMember(recType,AName) as TPasVariable;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,tr.GetExternalName(prp.VarType));
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp),Format('IsAttributeProperty("%s.%s")',[recType.Name, AName]));
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
    tr := LoadComplexType_Record_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_record,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);
    elt := tr.FindElement(x_complexType_SampleRecordType);
      CheckNotNull(elt,x_complexType_SampleRecordType);
      CheckEquals(x_complexType_SampleRecordType,elt.Name);
      CheckEquals(x_complexType_SampleRecordType,tr.GetExternalName(elt));
      CheckIs(elt,TPasRecordType,'Element Type');
      recType := elt as TPasRecordType;
        prpLs.Clear();
        for i := 0 to Pred(recType.Members.Count) do begin
          if TPasElement(recType.Members[i]).InheritsFrom(TPasVariable) then
            prpLs.Add(recType.Members[i]);
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


    elt := tr.FindElement(x_complexType_SampleRecord);
      CheckNotNull(elt,x_complexType_SampleRecord);
      CheckEquals(x_complexType_SampleRecord,elt.Name);
      CheckEquals(x_complexType_SampleRecord,tr.GetExternalName(elt));
      CheckIs(elt,TPasAliasType);
      aliasType := elt as TPasAliasType;
      CheckNotNull(aliasType.DestType);
      CheckEquals(x_complexType_SampleRecordType, tr.GetExternalName(aliasType.DestType));

    elt := tr.FindElement(x_complexType_SampleRecordTypeAll);
      CheckNotNull(elt,x_complexType_SampleRecordTypeAll);
      CheckEquals(x_complexType_SampleRecordTypeAll,elt.Name);
      CheckEquals(x_complexType_SampleRecordTypeAll,tr.GetExternalName(elt));
      CheckIs(elt,TPasRecordType,'Element type');
      recType := elt as TPasRecordType;
        prpLs.Clear();
        for i := 0 to Pred(recType.Members.Count) do begin
          if TPasElement(recType.Members[i]).InheritsFrom(TPasVariable) then
            prpLs.Add(recType.Members[i]);
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

procedure TTest_CustomXsdParser.ComplexType_Record_Embedded();
var
  tr : TwstPasTreeContainer;
  recType : TPasRecordType;

  procedure CheckProperty(const AName,ATypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasVariable;
  begin
    prp := FindMember(recType,AName) as TPasVariable;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,tr.GetExternalName(prp.VarType));
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp),Format('IsAttributeProperty("%s.%s")',[recType.Name, AName]));
  end;

var
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Record_Embedded_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_record_embedded,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(2,ls.Count);
    elt := tr.FindElement(x_complexType_SampleRecordType);
      CheckNotNull(elt,x_complexType_SampleRecordType);
      CheckEquals(x_complexType_SampleRecordType,elt.Name);
      CheckEquals(x_complexType_SampleRecordType,tr.GetExternalName(elt));
      CheckIs(elt,TPasRecordType,'Element Type');
      recType := elt as TPasRecordType;
        prpLs.Clear();
        for i := 0 to Pred(recType.Members.Count) do begin
          if TPasElement(recType.Members[i]).InheritsFrom(TPasVariable) then
            prpLs.Add(recType.Members[i]);
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


    elt := tr.FindElement(x_complexType_SampleRecordTypeAll);
      CheckNotNull(elt,x_complexType_SampleRecordTypeAll);
      CheckEquals(x_complexType_SampleRecordTypeAll,elt.Name);
      CheckEquals(x_complexType_SampleRecordTypeAll,tr.GetExternalName(elt));
      CheckIs(elt,TPasRecordType,'Element type');
      recType := elt as TPasRecordType;
        prpLs.Clear();
        for i := 0 to Pred(recType.Members.Count) do begin
          if TPasElement(recType.Members[i]).InheritsFrom(TPasVariable) then
            prpLs.Add(recType.Members[i]);
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

procedure TTest_CustomXsdParser.ComplexType_ArraySequence();
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
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_ArraySequence_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_sequence,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);
    elt := tr.FindElement(x_complexType_SampleArrayIntFieldType);
      CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
      CheckEquals(x_complexType_SampleArrayIntFieldType,elt.Name);
      CheckEquals(x_complexType_SampleArrayIntFieldType,tr.GetExternalName(elt));
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals('int',tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_intField,tr.GetArrayItemName(arrayType));
      CheckEquals(x_intField,tr.GetArrayItemExternalName(arrayType));
      

    nestedClassName := Format('%s_%s_Type',[x_complexType_SampleArrayItemType,x_Item]);
    elt := tr.FindElement(nestedClassName);
      CheckNotNull(elt,nestedClassName);
      CheckEquals(nestedClassName,elt.Name,'Item Name');
      CheckEquals(nestedClassName,tr.GetExternalName(elt),'Item ExternalName');
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

    elt := tr.FindElement(x_complexType_SampleArrayItemType);
      CheckNotNull(elt,x_complexType_SampleArrayItemType);
      CheckEquals(x_complexType_SampleArrayItemType,elt.Name, 'Array name');
      CheckEquals(x_complexType_SampleArrayItemType,tr.GetExternalName(elt), 'Array external name');
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals(nestedClassName,tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_Item,tr.GetArrayItemExternalName(arrayType));

  finally
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_ArraySequence_Embedded();
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
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_ArraySequence_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_sequence,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);
    elt := tr.FindElement(x_complexType_SampleArrayIntFieldType);
      CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
      CheckEquals(x_complexType_SampleArrayIntFieldType,elt.Name);
      CheckEquals(x_complexType_SampleArrayIntFieldType,tr.GetExternalName(elt));
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals('int',tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_intField,tr.GetArrayItemName(arrayType));
      CheckEquals(x_intField,tr.GetArrayItemExternalName(arrayType));


    nestedClassName := Format('%s_%s_Type',[x_complexType_SampleArrayItemType,x_Item]);
    elt := tr.FindElement(nestedClassName);
      CheckNotNull(elt,nestedClassName);
      CheckEquals(nestedClassName,elt.Name,'Item Name');
      CheckEquals(nestedClassName,tr.GetExternalName(elt),'Item ExternalName');
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

    elt := tr.FindElement(x_complexType_SampleArrayItemType);
      CheckNotNull(elt,x_complexType_SampleArrayItemType);
      CheckEquals(x_complexType_SampleArrayItemType,elt.Name, 'Array name');
      CheckEquals(x_complexType_SampleArrayItemType,tr.GetExternalName(elt), 'Array external name');
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals(nestedClassName,tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_Item,tr.GetArrayItemExternalName(arrayType));

  finally
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Array_soaparray();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  arrayType : TPasArrayType;
begin
  tr := LoadComplexType_Array_soaparray();
  try
    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_soaparray,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
    elt := tr.FindElement(x_complexType_SampleArrayIntFieldType);
      CheckNotNull(elt,x_complexType_SampleArrayIntFieldType);
      CheckEquals(x_complexType_SampleArrayIntFieldType,elt.Name);
      CheckEquals(x_complexType_SampleArrayIntFieldType,tr.GetExternalName(elt));
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      CheckNotNull(arrayType.ElType);
      CheckEquals('int',tr.GetExternalName(arrayType.ElType));
      CheckEquals('item',tr.GetArrayItemName(arrayType));
      CheckEquals('item',tr.GetArrayItemExternalName(arrayType));

    CheckNull(tr.FindElementNS('Array','http://schemas.xmlsoap.org/wsdl/'));
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_CollectionSequence();
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
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_CollectionSequence_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_array_sequence_collection,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(4,ls.Count);
    elt := tr.FindElement(x_complexType_SampleCollectionCollectionComplexType);
      CheckNotNull(elt,x_complexType_SampleCollectionCollectionComplexType);
      CheckEquals(x_complexType_SampleCollectionCollectionComplexType,elt.Name);
      CheckEquals(x_complexType_SampleCollectionCollectionComplexType,tr.GetExternalName(elt));
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      Check(tr.IsCollection(arrayType));
      CheckNotNull(arrayType.ElType);
      CheckEquals(x_complexType_SampleCollectionComplexType,tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_field,tr.GetArrayItemName(arrayType));
      CheckEquals(x_field,tr.GetArrayItemExternalName(arrayType));


    nestedClassName := Format('%s_%s_Type',[x_complexType_SampleCollectionItemType,x_Item]);
    elt := tr.FindElement(nestedClassName);
      CheckNotNull(elt,nestedClassName);
      CheckEquals(nestedClassName,elt.Name,'Item Name');
      CheckEquals(nestedClassName,tr.GetExternalName(elt),'Item ExternalName');
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

    elt := tr.FindElement(x_complexType_SampleCollectionItemType);
      CheckNotNull(elt,x_complexType_SampleCollectionItemType);
      CheckEquals(x_complexType_SampleCollectionItemType,elt.Name, 'Array name');
      CheckEquals(x_complexType_SampleCollectionItemType,tr.GetExternalName(elt), 'Array external name');
      CheckIs(elt,TPasArrayType);
      arrayType := elt as TPasArrayType;
      Check(tr.IsCollection(arrayType));
      CheckNotNull(arrayType.ElType);
      CheckEquals(nestedClassName,tr.GetExternalName(arrayType.ElType));
      CheckEquals(x_Item,tr.GetArrayItemExternalName(arrayType));

  finally
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.pascal_class_default_parent();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  clsType : TPasClassType;
  elt : TPasElement;
begin
  tr := LoadComplexType_pascal_class_parent();
  try
    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    elt := tr.FindElement(x_complexType_SampleClass);
      CheckNotNull(elt,x_complexType_SampleClass);
      CheckEquals(x_complexType_SampleClass,elt.Name);
      CheckEquals(x_complexType_SampleClass,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckNotNull(clsType.AncestorType,'AncestorType is null');
      CheckSame(tr.FindElementNS('TBaseComplexRemotable',sXSD_NS),clsType.AncestorType);
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_headerblock_derived();
const s_class_name = 'TSampleHeader'; s_emty_class_name = 'TEmptyHeader';
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  clsType : TPasClassType;
  elt : TPasElement;
begin
  tr := load_class_headerblock_derived_Schema();
  try
    mdl := tr.FindModule('class_headerblock_derived');
    CheckNotNull(mdl,'class_headerblock_derived');
    elt := tr.FindElement(s_emty_class_name);
      CheckNotNull(elt,s_emty_class_name);
      CheckEquals(s_emty_class_name,elt.Name);
      CheckEquals(s_emty_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckNotNull(clsType.AncestorType,'AncestorType is null');
      CheckSame(tr.FindElementNS('THeaderBlock',sXSD_NS),clsType.AncestorType);
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckNotNull(clsType.AncestorType,'AncestorType is null');
      CheckSame(tr.FindElementNS('THeaderBlock',sXSD_NS),clsType.AncestorType);

  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_headerblock_simplecontent_derived();
const s_class_name = 'TSampleHeader';
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  clsType : TPasClassType;
  elt : TPasElement;
begin
  tr := load_class_headerblock_simplecontent_derived_Schema();
  try
    mdl := tr.FindModule('class_headerblock_simplecontent_derived');
    CheckNotNull(mdl,'class_headerblock_simplecontent_derived');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckNotNull(clsType.AncestorType,'AncestorType is null');
      CheckSame(tr.FindElementNS('TSimpleContentHeaderBlock',sXSD_NS),clsType.AncestorType,'AncestorType');
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_default_values();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;

  procedure CheckProperty(
    const AName,
          ATypeName : string;
    const AFieldType : TPropertyType;
    const ADefault : string
  );
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
      CheckEquals(ADefault,prp.DefaultValue,'default');
  end;

var
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_default_values();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_class_default,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
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
        CheckProperty(x_intField,'int',ptField,'1210');
        CheckProperty(x_strField,'string',ptField,'azerty');
        CheckProperty(x_floatField,'float',ptField,'1234');
        CheckProperty(x_byteField,'byte',ptField,'23');
        CheckProperty(x_charField,'char',ptField,'i');
        CheckProperty(x_longField,'long',ptField,'567');
        CheckProperty(x_strAtt,'string',ptAttribute,'attribute azerty');
        CheckProperty(x_intAtt,'int',ptAttribute,'789');
  finally
    FreeAndNil(prpLs);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_properties_extended_metadata();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;

  procedure CheckProperty(
    const AName,
          ATypeName : string;
    const AFieldType : TPropertyType;
    const ADefault : string;
    const AExtMetaDataNameSpace,
          AExtMetaDataLocalName,
          AExtMetaDataValue : string
  );
  var
    prp : TPasProperty;
    locExtMeta : string;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,tr.GetExternalName(prp.VarType));
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
      CheckEquals(ADefault,prp.DefaultValue,'default');
      locExtMeta := Format('%s#%s',[AExtMetaDataNameSpace,AExtMetaDataLocalName]);
      if not IsStrEmpty(locExtMeta) then
       CheckEquals(AExtMetaDataValue, tr.Properties.GetValue(prp,locExtMeta), 'extended metadata');
  end;

var
  mdl : TPasModule;
  ls : TList;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_properties_extended_metadata();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals(x_complexType_class_properties_extended_metadata,mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
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
      CheckEquals(5,prpLs.Count);
        CheckProperty(x_intField,'int',ptField,'', 'uri-4','a','1210');
          CheckProperty(x_intField,'int',ptField,'', 'uri-4','b','uri-5#xx');
        CheckProperty(x_strField,'string',ptField,'azerty', 'uri-4','a', 'http://www.w3.org/2001/XMLSchema#int');
        CheckProperty(x_strAtt,'string',ptAttribute,'attribute azerty', 'uri-4','a', 'optional');
        CheckProperty(x_intAtt,'int',ptAttribute,'', '', '', '');
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

function TTest_XsdParser.LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpletypeNativeAlias);
end;

function TTest_XsdParser.LoadComplexType_Class_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class);
end;

function TTest_XsdParser.LoadComplexType_Class_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_embedded);
end;

function TTest_XsdParser.LoadComplexType_Class_Extend_Simple_Schema( ) : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_extend_simple);
end;

function TTest_XsdParser.LoadComplexType_Record_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record);
end;

function TTest_XsdParser.LoadComplexType_Record_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record_embedded);
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence);
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_embedded);
end;

function TTest_XsdParser.LoadComplexType_Array_soaparray() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_soaparray);
end;

function TTest_XsdParser.LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_collection);
end;

function TTest_XsdParser.LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;
begin
  Result := ParseDoc('pascal_class_parent');
end;

function TTest_XsdParser.load_class_headerblock_derived_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_headerblock_derived');
end;

function TTest_XsdParser.load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_headerblock_simplecontent_derived');
end;

function TTest_XsdParser.LoadComplexType_Class_default_values() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_default);
end;

function TTest_XsdParser.LoadComplexType_Class_properties_extended_metadata(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_properties_extended_metadata);
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

function TTest_WsdlParser.LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_simpletypeNativeAlias);
end;

function TTest_WsdlParser.LoadComplexType_Class_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class);
end;

function TTest_WsdlParser.LoadComplexType_Class_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_embedded);
end;

function TTest_WsdlParser.LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_extend_simple);
end;

function TTest_WsdlParser.LoadComplexType_Record_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record);
end;

function TTest_WsdlParser.LoadComplexType_Record_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record_embedded);
end;

function TTest_WsdlParser.LoadComplexType_ArraySequence_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence);
end;

function TTest_WsdlParser.LoadComplexType_ArraySequence_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_embedded);
end;

function TTest_WsdlParser.LoadComplexType_Array_soaparray() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_soaparray);
end;

function TTest_WsdlParser.LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_collection);
end;

function TTest_WsdlParser.LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;
begin
  Result := ParseDoc('pascal_class_parent');
end;

function TTest_WsdlParser.load_class_headerblock_derived_Schema( ) : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_headerblock_derived');
end;

function TTest_WsdlParser.load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_headerblock_simplecontent_derived');
end;

procedure TTest_WsdlParser.no_binding_style();
var
  symTable : TwstPasTreeContainer;
  elt : TPasElement;
  intf : TPasClassType;
begin
  symTable := ParseDoc('no_binding_style');
  try
    elt := symTable.FindElement('ISampleService');
    CheckNotNull(elt);
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    Check(intf.ObjKind = okInterface);
    CheckEquals(2,GetElementCount(intf.Members,TPasProcedure));
  finally
    symTable.Free();
  end;
end;

procedure TTest_WsdlParser.signature_last();
var
  tr : TwstPasTreeContainer;
  elt : TPasElement;
  intf : TPasClassType;
  i : Integer;
  mth : TPasProcedure;
  mthType : TPasProcedureType;
  res : TPasResultElement;
  arg : TPasArgument;
begin
  tr := ParseDoc('signature_last');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := nil;
    for i := 0 to (intf.Members.Count - 1) do begin
      if TObject(intf.Members[i]).InheritsFrom(TPasProcedure) then begin
        mth := TPasProcedure(intf.Members[i]);
        Break;
      end;
    end;
    CheckNotNull(mth,'test_proc not found');
    CheckEquals('test_proc',mth.Name);
    mthType := mth.ProcType;
    CheckIs(mthType,TPasFunctionType);
      res := TPasFunctionType(mthType).ResultEl;
      CheckNotNull(res, 'Result');
      CheckEquals('integer', LowerCase(res.ResultType.Name));
    CheckEquals(2, mthType.Args.Count, 'Parameter count');
    arg := TPasArgument(mthType.Args[0]);
      CheckNotNull(arg);
      CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
      CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
    arg := TPasArgument(mthType.Args[1]);
      CheckNotNull(arg);
      CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
      CheckEquals(LowerCase('boolean'), LowerCase(arg.ArgType.Name));
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.signature_result();

  function FindProc(const AName : string; AIntf : TPasClassType) : TPasProcedure;
  var
    k : Integer;
  begin
    Result := nil;
    for k := 0 to (AIntf.Members.Count - 1) do begin
      if TObject(AIntf.Members[k]).InheritsFrom(TPasProcedure) and ( TPasProcedure(AIntf.Members[k]).Name = AName ) then begin
        Result := TPasProcedure(AIntf.Members[k]);
        Break;
      end;
    end;
  end;
  
var
  tr : TwstPasTreeContainer;
  elt : TPasElement;
  intf : TPasClassType;
  mth : TPasProcedure;
  mthType : TPasProcedureType;
  res : TPasResultElement;
  arg : TPasArgument;
begin
  tr := ParseDoc('signature_result');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('test_proc',intf);
      CheckNotNull(mth,'test_proc not found');
      CheckEquals('test_proc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('Boolean'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        
    mth := FindProc('test_proc2',intf);
      CheckNotNull(mth,'test_proc2 not found');
      CheckEquals('test_proc2',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('string'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('boolean'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        
    mth := FindProc('test_proc3',intf);
      CheckNotNull(mth,'test_proc3 not found');
      CheckEquals('test_proc3',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('Boolean'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.signature_return();

  function FindProc(const AName : string; AIntf : TPasClassType) : TPasProcedure;
  var
    k : Integer;
  begin
    Result := nil;
    for k := 0 to (AIntf.Members.Count - 1) do begin
      if TObject(AIntf.Members[k]).InheritsFrom(TPasProcedure) and ( TPasProcedure(AIntf.Members[k]).Name = AName ) then begin
        Result := TPasProcedure(AIntf.Members[k]);
        Break;
      end;
    end;
  end;

var
  tr : TwstPasTreeContainer;
  elt : TPasElement;
  intf : TPasClassType;
  mth : TPasProcedure;
  mthType : TPasProcedureType;
  res : TPasResultElement;
  arg : TPasArgument;
begin
  tr := ParseDoc('signature_return');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('test_proc',intf);
      CheckNotNull(mth,'test_proc not found');
      CheckEquals('test_proc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('Boolean'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));

    mth := FindProc('test_proc2',intf);
      CheckNotNull(mth,'test_proc2 not found');
      CheckEquals('test_proc2',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('string'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('boolean'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));

    mth := FindProc('test_proc3',intf);
      CheckNotNull(mth,'test_proc3 not found');
      CheckEquals('test_proc3',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('Boolean'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
  finally
    tr.Free();
  end;
end;

function TTest_WsdlParser.LoadComplexType_Class_default_values() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_default);
end;

function TTest_WsdlParser.LoadComplexType_Class_properties_extended_metadata(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_properties_extended_metadata);
end;

initialization
  RegisterTest('XSD parser',TTest_XsdParser.Suite);
  RegisterTest('WSDL parser',TTest_WsdlParser.Suite);

end.
