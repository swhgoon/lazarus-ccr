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
  fpcunit, testutils, testregistry, DOM, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  pastree, pascal_parser_intf, xsd_parser, wsdl_parser, test_suite_utils, wst_types;

type

  { TTest_CustomXsdParser }

  TTest_CustomXsdParser = class(TTestCase)
  protected
    function ParseDoc(const ADoc : string) : TwstPasTreeContainer;overload;virtual;
    function ParseDoc(const ADoc : string; const ACaseSensistive : Boolean) : TwstPasTreeContainer;overload;virtual;abstract;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;virtual;abstract;
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_properties_extended_metadata2() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_OpenType() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_FalseArray() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Choice_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Choice2_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Choice3_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Class_Choice4_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_ArraySequence_ItemName_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;virtual;abstract;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;virtual;abstract;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;virtual;abstract;
    function load_class_widestring_property() : TwstPasTreeContainer;virtual;abstract;
    function load_class_ansichar_property() : TwstPasTreeContainer;virtual;abstract;
    function load_class_widechar_property() : TwstPasTreeContainer;virtual;abstract;
    function load_class_currency_property() : TwstPasTreeContainer;virtual;abstract;
    function load_class_property_composed_name() : TwstPasTreeContainer;virtual;abstract;

    function load_schema_import() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_include() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_include_parent_no_types() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_include_fail_namespace() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_include_circular1() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_include_circular2() : TwstPasTreeContainer;virtual;abstract;

    function load_schema_case_sensitive() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_case_sensitive2() : TwstPasTreeContainer;virtual;abstract;
    function load_schema_case_sensitive_import() : TwstPasTreeContainer;virtual;abstract;

    function load_global_attribute() : TwstPasTreeContainer;virtual;abstract;
  published
    procedure EmptySchema();

    procedure SimpleType_Enum();
    procedure SimpleType_Enum_Embedded();
    procedure SimpleType_AliasToNativeType();

    procedure ComplexType_Class();
    procedure ComplexType_Class_default_values();
    procedure ComplexType_Class_properties_extended_metadata();
    procedure ComplexType_Class_properties_extended_metadata2();
    procedure ComplexType_Class_Embedded();
    procedure ComplexType_Class_Extend_Simple_Schema();
    procedure ComplexType_Class_open_type_any();
    procedure ComplexType_Class_open_extension_type_any();
    procedure ComplexType_Class_open_extension_type_anyAttribute();
    procedure ComplexType_Class_sequence_open_type_anyAttribute();
    procedure ComplexType_Class_all_open_type_anyAttribute();
    procedure ComplexType_Class_FalseArray();
    procedure ComplexType_Class_Choice();
    procedure ComplexType_Class_Choice2();
    procedure ComplexType_Class_Choice3();
    procedure ComplexType_Class_Choice4();

    procedure ComplexType_Record();
    procedure ComplexType_Record_Embedded();

    procedure ComplexType_ArraySequence();
    procedure ComplexType_ArraySequence_ItemName_Schema();
    procedure ComplexType_ArraySequence_Embedded();
    procedure ComplexType_Array_soaparray();

    procedure ComplexType_CollectionSequence();
    procedure pascal_class_default_parent();

    procedure class_headerblock_derived();
    procedure class_headerblock_simplecontent_derived();
    procedure class_widestring_property();
    procedure class_ansichar_property();
    procedure class_widechar_property();
    procedure class_currency_property();
    procedure class_property_composed_name();
    procedure schema_import();
    procedure schema_include();
    procedure schema_include_parent_no_types();
    procedure schema_include_fail_namespace();
    procedure schema_include_circular1();
    procedure schema_include_circular2();

    procedure case_sensitive();
    procedure case_sensitive2();
    procedure case_sensitive_import();

    procedure global_attribute();
  end;

  { TTest_XsdParser }

  TTest_XsdParser = class(TTest_CustomXsdParser)
  protected
    function ParseDoc(const ADoc : string; const ACaseSensistive : Boolean) : TwstPasTreeContainer;override;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;
    
    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata2() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_OpenType() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_FalseArray() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice2_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice3_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice4_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_ItemName_Schema() : TwstPasTreeContainer; override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;override;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;override;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_widestring_property() : TwstPasTreeContainer;override;
    function load_class_ansichar_property() : TwstPasTreeContainer;override;
    function load_class_widechar_property() : TwstPasTreeContainer;override;
    function load_class_currency_property() : TwstPasTreeContainer;override;
    function load_class_property_composed_name() : TwstPasTreeContainer;override;

    function load_schema_import() : TwstPasTreeContainer;override;
    function load_schema_include() : TwstPasTreeContainer;override;
    function load_schema_include_parent_no_types() : TwstPasTreeContainer;override;
    function load_schema_include_fail_namespace() : TwstPasTreeContainer;override;
    function load_schema_include_circular1() : TwstPasTreeContainer;override;
    function load_schema_include_circular2() : TwstPasTreeContainer;override;

    function load_schema_case_sensitive() : TwstPasTreeContainer;override;
    function load_schema_case_sensitive2() : TwstPasTreeContainer;override;
    function load_schema_case_sensitive_import() : TwstPasTreeContainer;override;

    function load_global_attribute() : TwstPasTreeContainer;override;
  end;

  { TTest_WsdlParser }

  TTest_WsdlParser = class(TTest_CustomXsdParser)
  private
    function ParseDoc(const ADoc : string; const ACaseSensitive : Boolean) : TwstPasTreeContainer;override;
  protected
    function LoadEmptySchema() : TwstPasTreeContainer;override;

    function LoadSimpleType_Enum_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_default_values() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_properties_extended_metadata2() : TwstPasTreeContainer;override;
    function LoadSimpleType_Enum_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadSimpleType_AliasToNativeType_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Class_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Extend_Simple_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_OpenType() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_FalseArray() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice2_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice3_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Class_Choice4_Schema() : TwstPasTreeContainer;override;

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Record_Embedded_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_ItemName_Schema() : TwstPasTreeContainer; override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_Array_soaparray() : TwstPasTreeContainer;override;
    
    function LoadComplexType_CollectionSequence_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_pascal_class_parent() : TwstPasTreeContainer;override;
    
    function load_class_headerblock_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_headerblock_simplecontent_derived_Schema() : TwstPasTreeContainer;override;
    function load_class_widestring_property() : TwstPasTreeContainer;override;
    function load_class_ansichar_property() : TwstPasTreeContainer;override;
    function load_class_widechar_property() : TwstPasTreeContainer;override;
    function load_class_currency_property() : TwstPasTreeContainer;override;  
    function load_class_property_composed_name() : TwstPasTreeContainer;override;

    function load_schema_import() : TwstPasTreeContainer;override;
    function load_schema_include() : TwstPasTreeContainer;override;
    function load_schema_include_parent_no_types() : TwstPasTreeContainer;override;
    function load_schema_include_fail_namespace() : TwstPasTreeContainer;override;
    function load_schema_include_circular1() : TwstPasTreeContainer;override;
    function load_schema_include_circular2() : TwstPasTreeContainer;override;

    function load_schema_case_sensitive() : TwstPasTreeContainer;override;
    function load_schema_case_sensitive2() : TwstPasTreeContainer;override;
    function load_schema_case_sensitive_import() : TwstPasTreeContainer;override;

    function load_global_attribute() : TwstPasTreeContainer;override;
  published
    procedure no_binding_style();
    procedure signature_last();
    procedure signature_result();
    procedure signature_return();
    procedure xsd_not_declared_at_top_node();
    procedure xsd_not_declared_at_top_node_2();
    procedure message_parts_type_hint();
    procedure parameter_var();
    procedure parameter_const_default();
    procedure parameter_composed_name();
    procedure parameter_composed_name_function();
    procedure soap_action();
  end;
  
implementation
uses parserutils, xsd_consts, typinfo, locators;

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
  x_simpleTypeAliasWideString = 'AliasWideString';
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
  Result := ReadXMLFile(AFileName);
end;

{ TTest_CustomXsdParser }

function TTest_CustomXsdParser.ParseDoc(const ADoc: string): TwstPasTreeContainer;
begin
  Result := ParseDoc(ADoc,False);
  Result.DefaultSearchNameKinds := NAME_KINDS_DEFAULT;
end;

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
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.SimpleType_Enum();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
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

  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.SimpleType_Enum_Embedded();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
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

  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.SimpleType_AliasToNativeType();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
begin
  tr := LoadSimpleType_AliasToNativeType_Schema();

  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals(x_simpletypeNativeAlias,mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(3,ls.Count);
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

  elt := tr.FindElement(x_simpleTypeAliasWideString);
    CheckNotNull(elt,x_simpleTypeAliasWideString);
    CheckEquals(x_simpleTypeAliasWideString,elt.Name);
    CheckEquals(x_simpleTypeAliasWideString,tr.GetExternalName(elt));
    CheckIs(elt,TPasAliasType);
    aliasType := elt as TPasAliasType;
    CheckNotNull(aliasType.DestType);
    CheckIs(aliasType.DestType,TPasNativeSimpleType);
    CheckEquals('WideString',aliasType.DestType.Name);

  FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
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
  ls : TList2;
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
        CheckSame(tr.FindElementNS('TComplexStringContentRemotable',sXSD_NS),clsType.AncestorType,clsType.AncestorType.Name);

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

procedure TTest_CustomXsdParser.ComplexType_Class_open_type_any();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  elt := tr.FindElement('TClass_1');
  CheckNotNull(elt,'TClass_1');
  CheckIs(elt,TPasClassType);
  clsType := elt as TPasClassType;
  strBuffer := tr.Properties.GetValue(clsType,Format('%s#%s',[s_xs,s_any]));
  Check(Length(strBuffer) > 0, s_any);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('lax',ls.Values[s_processContents]);
    CheckEquals('0',ls.Values[s_minOccurs]);
    CheckEquals(s_unbounded,ls.Values[s_maxOccurs]);
  finally
    ls.Free();
  end;
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_open_extension_type_any();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  elt := tr.FindElement('TClassSampleDerivedType');
  CheckNotNull(elt,'TClassSampleDerivedType');
  CheckIs(elt,TPasClassType);
  clsType := elt as TPasClassType;
  strBuffer := tr.Properties.GetValue(clsType,Format('%s#%s',[s_xs,s_any]));
  Check(Length(strBuffer) > 0, s_any);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('skip',ls.Values[s_processContents]);
    CheckEquals(s_unbounded,ls.Values[s_maxOccurs]);
  finally
    ls.Free();
  end;
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_open_extension_type_anyAttribute();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  elt := tr.FindElement('TClassSampleDerivedType');
  CheckNotNull(elt,'TClassSampleDerivedType');
  CheckIs(elt,TPasClassType);
  clsType := elt as TPasClassType;
  strBuffer := tr.Properties.GetValue(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('lax',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_sequence_open_type_anyAttribute();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  elt := tr.FindElement('TClass_1');
  CheckNotNull(elt,'TClass_1');
  CheckIs(elt,TPasClassType);
  clsType := elt as TPasClassType;
  strBuffer := tr.Properties.GetValue(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('strict',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_all_open_type_anyAttribute();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  strBuffer : string;
  ls : TStringList;
begin
  tr := LoadComplexType_Class_OpenType();
  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  elt := tr.FindElement('TClassSampleTypeAll');
  CheckNotNull(elt,'TClassSampleTypeAll');
  CheckIs(elt,TPasClassType);
  clsType := elt as TPasClassType;
  strBuffer := tr.Properties.GetValue(clsType,Format('%s#%s',[s_xs,s_anyAttribute]));
  Check(Length(strBuffer) > 0, s_anyAttribute);
  ls := TStringList.Create();
  try
    ls.Delimiter := ';';
    ls.DelimitedText := strBuffer;
    CheckEquals('skip',ls.Values[s_processContents]);
  finally
    ls.Free();
  end;
  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.ComplexType_Class_FalseArray();
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
  ls : TList2;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
  prp : TPasProperty;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_FalseArray();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('complex_class_false_array',mdl.Name);
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
      CheckEquals(2,prpLs.Count);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);

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
      CheckEquals(1,prpLs.Count);
        prp := TPasProperty(prpLs[0]);
        CheckIs(prp.VarType,TPasArrayType);
        CheckEquals(x_intField + 'Ex', tr.GetArrayItemExternalName(TPasArrayType(prp.VarType)));
  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Choice();
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Choice_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('complex_class_choice',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
    elt := tr.FindElement('TSampleType1');
      CheckNotNull(elt,'TSampleType1');
      CheckEquals('TSampleType1',elt.Name);
      CheckEquals('TSampleType1',tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(2,prpLs.Count);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Choice2();
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;

  procedure CheckProperty(const AName,ATypeName : string);
  var
    prp : TPasProperty;
    prpType : TPasArrayType;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckIs(prp.VarType,TPasArrayType);
      prpType := TPasArrayType(prp.VarType);
      CheckNotNull(prpType.ElType);
      CheckEquals(ATypeName,tr.GetExternalName(prpType.ElType));
  end;

var
  mdl : TPasModule;
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Choice2_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('complex_class_choice2',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    elt := tr.FindElement('TSampleType1');
      CheckNotNull(elt,'TSampleType1');
      CheckEquals('TSampleType1',elt.Name);
      CheckEquals('TSampleType1',tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(2,prpLs.Count);
        CheckProperty(x_intField,'int');
        CheckProperty(x_strField,'string');
  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Choice3();
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Choice3_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('complex_class_choice3',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
    elt := tr.FindElement('TSampleType1');
      CheckNotNull(elt,'TSampleType1');
      CheckEquals('TSampleType1',elt.Name);
      CheckEquals('TSampleType1',tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(4,prpLs.Count);
        CheckProperty('intField1','int',ptField);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
        CheckProperty('dateField','date',ptField);
  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_Choice4();
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := LoadComplexType_Class_Choice4_Schema();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('complex_class_choice4',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(1,ls.Count);
    elt := tr.FindElement('TSampleType1');
      CheckNotNull(elt,'TSampleType1');
      CheckEquals('TSampleType1',elt.Name);
      CheckEquals('TSampleType1',tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(4,prpLs.Count);
        CheckProperty('intField1','int',ptField);
        CheckProperty(x_intField,'int',ptField);
        CheckProperty(x_strField,'string',ptField);
        CheckProperty('dateField','date',ptField);
  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  tr := nil;
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
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_ArraySequence_ItemName_Schema();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt : TPasElement;
  arrayType : TPasArrayType;
begin
  tr := LoadComplexType_ArraySequence_ItemName_Schema();

  mdl := tr.FindModule(x_targetNamespace);
  CheckNotNull(mdl);
  CheckEquals('array_sequence_item_name',mdl.Name);
  CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(2,ls.Count);
  elt := tr.FindElement('ArrayOfEmailAddress');
    CheckNotNull(elt,'ArrayOfEmailAddress');
    CheckEquals('ArrayOfEmailAddress',elt.Name);
    CheckEquals('ArrayOfEmailAddress',tr.GetExternalName(elt));
    CheckIs(elt,TPasArrayType);
    arrayType := elt as TPasArrayType;
    CheckNotNull(arrayType.ElType);
    CheckEquals('EmailAddress',tr.GetExternalName(arrayType.ElType));
    CheckEquals('EmailAddress',tr.GetArrayItemName(arrayType));
    CheckEquals('EmailAddress',tr.GetArrayItemExternalName(arrayType));

  FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  tr := nil;
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
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Array_soaparray();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
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
  ls : TList2;
  elt : TPasElement;
  arrayType : TPasArrayType;
  i : Integer;
  prpLs : TList;
  nestedClassName : string;
begin
  tr := nil;
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
    FreeAndNil(tr);
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

procedure TTest_CustomXsdParser.class_widestring_property();
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(const AName,ATypeName,ADeclaredTypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(ADeclaredTypeName,tr.GetExternalName(prp.VarType),'DeclaredTypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_widestring_property();
  try
    mdl := tr.FindModule('class_widestring_property');
    CheckNotNull(mdl,'class_widestring_property');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('elementProp','WideString','string',ptField);
      CheckProperty('elementAtt','WideString','string',ptAttribute);
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
  ls : TList2;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
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
  ls : TList2;
  elt : TPasElement;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
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
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.ComplexType_Class_properties_extended_metadata2();
const s_ProjectType = 'ProjectType';
var
  tr : TwstPasTreeContainer;
  clsType : TPasClassType;
  mdl : TPasModule;
  elt : TPasElement;
  i : Integer;
  p : TPasProperty;
begin
  tr := LoadComplexType_Class_properties_extended_metadata2();
  mdl := tr.FindModule('uri:sample');
  CheckNotNull(mdl);
  elt := tr.FindElement(s_ProjectType);
    CheckNotNull(elt,s_ProjectType);
    CheckIs(elt,TPasClassType);
    clsType := elt as TPasClassType;
      p := nil;
      for i := 0 to Pred(clsType.Members.Count) do begin
        if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) and
           SameText('ProjectLeader',TPasElement(clsType.Members[i]).Name)
        then begin
          p := TPasProperty(clsType.Members[i]);
          Break;
        end;
      end;
      CheckNotNull(p,'Property non found : "ProjectLeader"');
      CheckEquals('uri:sample#Person', tr.Properties.GetValue(p,'commonj.sdo#propertyType'), 'extended metadata');

      p := nil;
      for i := 0 to Pred(clsType.Members.Count) do begin
        if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) and
           SameText('ProjectLeaderArray',TPasElement(clsType.Members[i]).Name)
        then begin
          p := TPasProperty(clsType.Members[i]);
          Break;
        end;
      end;
      CheckNotNull(p,'Property non found : "ProjectLeaderArray"');
      CheckEquals('uri:sample#Person', tr.Properties.GetValue(p,'commonj.sdo#propertyType'), 'extended metadata');

  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.class_ansichar_property();
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(const AName,ATypeName,ADeclaredTypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(ADeclaredTypeName,tr.GetExternalName(prp.VarType),'DeclaredTypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_ansichar_property();
  try
    mdl := tr.FindModule('class_ansichar_property');
    CheckNotNull(mdl,'class_ansichar_property');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('elementProp','AnsiChar','string',ptField);
      CheckProperty('elementAtt','AnsiChar','string',ptAttribute);
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_widechar_property();
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(const AName,ATypeName,ADeclaredTypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(ADeclaredTypeName,tr.GetExternalName(prp.VarType),'DeclaredTypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_widechar_property();
  try
    mdl := tr.FindModule('class_widechar_property');
    CheckNotNull(mdl,'class_widechar_property');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('elementProp','WideChar','string',ptField);
      CheckProperty('elementAtt','WideChar','string',ptAttribute);
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_currency_property(); 
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(const AName,ATypeName,ADeclaredTypeName : string; const AFieldType : TPropertyType);
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name);
      CheckEquals(AName,tr.GetExternalName(prp));
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(ADeclaredTypeName,tr.GetExternalName(prp.VarType),'DeclaredTypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_currency_property();
  try
    mdl := tr.FindModule('class_currency_property');
    CheckNotNull(mdl,'class_currency_property');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckEquals(s_class_name,elt.Name);
      CheckEquals(s_class_name,tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('elementProp','Currency','decimal',ptField);
      CheckProperty('elementAtt','Currency','decimal',ptAttribute);
  finally
    tr.Free();
  end;
end;

procedure TTest_CustomXsdParser.class_property_composed_name();   
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(
    const AName,
          ADeclaredName,
          ATypeName      : string; 
    const AFieldType     : TPropertyType
  );
  var
    prp : TPasProperty;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name,'Name');
      CheckEquals(ADeclaredName,tr.GetExternalName(prp),'External Name');
      CheckNotNull(prp.VarType);
      CheckEquals(ATypeName,prp.VarType.Name,'TypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_class_property_composed_name();
  try
    mdl := tr.FindModule('urn_sample');
    CheckNotNull(mdl,'urn_sample');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);       
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('one_prop','one-prop','string',ptField);
      CheckProperty('one_two_prop','one-two-prop','string',ptAttribute);
  finally
    tr.Free();
  end;   
end;

procedure TTest_CustomXsdParser.schema_import();
const
  s_base_namespace = 'urn:base-library';
  s_base_type = 'SampleBase_Type';
  s_second_namespace = 'urn:second-library';
  s_second_type = 'Second_Type';
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt, prpElt : TPasElement;
  prp : TPasProperty;
  baseType, scdClass : TPasClassType;
begin
  tr := load_schema_import();

  mdl := tr.FindModule(s_base_namespace);
  CheckNotNull(mdl,s_base_namespace);
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(1,ls.Count);
  elt := tr.FindElement(s_base_type);
    CheckNotNull(elt,s_base_type);
    CheckIs(elt,TPasClassType);
    baseType := TPasClassType(elt);

  mdl := tr.FindModule(s_second_namespace);
  CheckNotNull(mdl,s_second_namespace);
  ls := mdl.InterfaceSection.Declarations;
  CheckEquals(1,ls.Count);
  elt := tr.FindElement(s_second_type);
    CheckNotNull(elt,s_second_type);
    CheckIs(elt,TPasClassType);
    scdClass := TPasClassType(elt);
    prpElt := FindMember(scdClass,'SampleProperty');
    CheckNotNull(prpElt);
    CheckIs(prpElt,TPasProperty);
    prp := TPasProperty(prpElt);
    CheckNotNull(prp.VarType);
    CheckEquals(PtrUInt(prp.VarType),PtrUInt(prp.VarType));

  FreeAndNil(tr);
end;

procedure TTest_CustomXsdParser.schema_include();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt, prpElt : TPasElement;
  prp : TPasProperty;
  baseType, scdClass : TPasClassType;
begin
  tr := load_schema_include();
  try
    mdl := tr.FindModule('urn:include');
    CheckNotNull(mdl);
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(4,ls.Count,'type count');
    elt := tr.FindElement('TypeA');
      CheckNotNull(elt,'TypeA');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeB');
      CheckNotNull(elt,'TypeB');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeC');
      CheckNotNull(elt,'TypeC');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TClassSample');
      CheckNotNull(elt,'TClassSample');
      CheckIs(elt,TPasClassType);
  finally
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.schema_include_parent_no_types();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt, prpElt : TPasElement;
  prp : TPasProperty;
  baseType, scdClass : TPasClassType;
begin
  tr := load_schema_include_parent_no_types();
  try
    mdl := tr.FindModule('urn:include');
    CheckNotNull(mdl);
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count,'type count');
    elt := tr.FindElement('TypeA');
      CheckNotNull(elt,'TypeA');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeB');
      CheckNotNull(elt,'TypeB');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeC');
      CheckNotNull(elt,'TypeC');
      CheckIs(elt,TPasEnumType);
  finally
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.schema_include_fail_namespace();
var
  tr : TwstPasTreeContainer;
  ok : Boolean;
begin
  tr := nil;
  ok := False;
  try
    tr := load_schema_include_fail_namespace();
    ok := True;
  except
    on e : EXsdParserAssertException do
      ok := True;
  end;
  FreeAndNil(tr);
  Check(ok);
end;

procedure TTest_CustomXsdParser.schema_include_circular1(); 
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt, prpElt : TPasElement;
  prp : TPasProperty;
  baseType, scdClass : TPasClassType;
begin
  tr := load_schema_include_circular1();
  try
    mdl := tr.FindModule('urn:include');
    CheckNotNull(mdl);
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count,'type count');
    elt := tr.FindElement('TypeA');
      CheckNotNull(elt,'TypeA');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeB');
      CheckNotNull(elt,'TypeB');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TClassSample');
      CheckNotNull(elt,'TClassSample');
      CheckIs(elt,TPasClassType);
  finally
    FreeAndNil(tr);
  end;  
end;

procedure TTest_CustomXsdParser.schema_include_circular2(); 
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  ls : TList2;
  elt, prpElt : TPasElement;
  prp : TPasProperty;
  baseType, scdClass : TPasClassType;
begin
  tr := load_schema_include_circular2();
  try
    mdl := tr.FindModule('urn:include');
    CheckNotNull(mdl);
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(4,ls.Count,'type count');
    elt := tr.FindElement('TypeA');
      CheckNotNull(elt,'TypeA');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeB');
      CheckNotNull(elt,'TypeB');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TypeC');
      CheckNotNull(elt,'TypeC');
      CheckIs(elt,TPasEnumType);
    elt := tr.FindElement('TClassSample');
      CheckNotNull(elt,'TClassSample');
      CheckIs(elt,TPasClassType);
  finally
    FreeAndNil(tr);
  end;  
end;

procedure TTest_CustomXsdParser.case_sensitive();
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := load_schema_case_sensitive();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('case_sensitive',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(4,ls.Count);

    elt := tr.FindElement('Date');
      CheckNotNull(elt,'Date');
      CheckEquals('Date',tr.GetExternalName(elt));
      CheckEquals(x_targetNamespace,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'Date.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('String');
      CheckNotNull(elt,'String');
      CheckEquals('String',tr.GetExternalName(elt));
      CheckEquals(x_targetNamespace,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'String.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('Boolean');
      CheckNotNull(elt,'Boolean');
      CheckEquals('Boolean',tr.GetExternalName(elt));
      CheckEquals(x_targetNamespace,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'Boolean.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

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
        CheckProperty('dateField','date',ptField);
          CheckProperty('localDateField','Date',ptField);
        CheckProperty('booleanField','boolean',ptField);
          CheckProperty('localBooleanField','Boolean',ptField);
        CheckProperty('stringField','string',ptField);
          CheckProperty('localStringField','String',ptField);
        CheckProperty('dateAtt','date',ptAttribute);
          CheckProperty('localDateAtt','Date',ptAttribute);

  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.case_sensitive2();
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := load_schema_case_sensitive2();

    mdl := tr.FindModule(x_targetNamespace);
    CheckNotNull(mdl);
    CheckEquals('case_sensitive2',mdl.Name);
    CheckEquals(x_targetNamespace,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);

    elt := tr.FindElement('SampleType');
      CheckNotNull(elt,'SampleType');
      CheckEquals('SampleType',tr.GetExternalName(elt));
      CheckEquals(x_targetNamespace,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'SampleType.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('SAMPLETYPE');
      CheckNotNull(elt,'SAMPLETYPE');
      CheckEquals('SAMPLETYPE',tr.GetExternalName(elt));
      CheckEquals(x_targetNamespace,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'SAMPLETYPE.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

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
      CheckEquals(2,prpLs.Count);
        CheckProperty('Field1','SampleType',ptField);
        CheckProperty('Field2','SAMPLETYPE',ptField);

  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.case_sensitive_import();
const CONST_NS = 'urn:wst-test3';
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
  ls : TList2;
  elt : TPasElement;
  aliasType : TPasAliasType;
  i : Integer;
  prpLs : TList;
begin
  tr := nil;
  prpLs := TList.Create();
  try
    tr := load_schema_case_sensitive_import();
    //-----------------------------------------
    mdl := tr.FindModule('urn:wst-test');
    CheckNotNull(mdl);
    CheckEquals('case_sensitive2',mdl.Name);
    CheckEquals('urn:wst-test',tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);

    elt := tr.FindElement('SampleType');
      CheckNotNull(elt,'SampleType');
      CheckEquals('SampleType',tr.GetExternalName(elt));
      CheckEquals('urn:wst-test',tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'SampleType.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('SAMPLETYPE');
      CheckNotNull(elt,'SAMPLETYPE');
      CheckEquals('SAMPLETYPE',tr.GetExternalName(elt));
      CheckEquals('urn:wst-test',tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'SAMPLETYPE.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    //-----------------------------------------
    mdl := tr.FindModule(CONST_NS);
    CheckNotNull(mdl);
    CheckEquals('case_sensitive3',mdl.Name);
    CheckEquals(CONST_NS,tr.GetExternalName(mdl));
    ls := mdl.InterfaceSection.Declarations;
    CheckEquals(3,ls.Count);

    elt := tr.FindElement('TypeA');
      CheckNotNull(elt,'TypeA');
      CheckEquals('TypeA',tr.GetExternalName(elt));
      CheckEquals(CONST_NS,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'TypeA.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('TYPEA');
      CheckNotNull(elt,'TYPEA');
      CheckEquals('TYPEA',tr.GetExternalName(elt));
      CheckEquals(CONST_NS,tr.GetNameSpace(elt as TPasType));
      CheckIs(elt,TPasAliasType);
      CheckNotNull(TPasAliasType(elt).DestType,'TYPEA.DestType');
      CheckEquals('string',TPasAliasType(elt).DestType.Name);

    elt := tr.FindElement('CompoundType');
      CheckNotNull(elt,'CompoundType');
      CheckEquals('CompoundType',elt.Name);
      CheckEquals('CompoundType',tr.GetExternalName(elt));
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
        prpLs.Clear();
        for i := 0 to Pred(clsType.Members.Count) do begin
          if TPasElement(clsType.Members[i]).InheritsFrom(TPasProperty) then
            prpLs.Add(clsType.Members[i]);
        end;
      CheckEquals(4,prpLs.Count);
        CheckProperty('f1','SampleType',ptField);
        CheckProperty('f2','SAMPLETYPE',ptField);
        CheckProperty('f3','TypeA',ptField);
        CheckProperty('f4','TYPEA',ptField);

  finally
    FreeAndNil(prpLs);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdParser.global_attribute();
const s_class_name = 'TSampleClass';
var
  clsType : TPasClassType;
  tr : TwstPasTreeContainer;

  procedure CheckProperty(
    const AName,
          ADeclaredName,
          ATypeName      : string;
    const AFieldType     : TPropertyType
  );
  var
    prp : TPasProperty;
    t : TPasType;
  begin
    prp := FindMember(clsType,AName) as TPasProperty;
      CheckNotNull(prp);
      CheckEquals(AName,prp.Name,'Name');
      CheckEquals(ADeclaredName,tr.GetExternalName(prp),'External Name');
      CheckNotNull(prp.VarType);
      t := GetUltimeType(prp.VarType);
      CheckNotNull(t,'Property''s Ultime Type not found.');
      CheckEquals(ATypeName,tr.GetExternalName(t),'TypeName');
      CheckEquals(PropertyType_Att[AFieldType],tr.IsAttributeProperty(prp));
  end;

var
  mdl : TPasModule;
  elt : TPasElement;
begin
  tr := load_global_attribute();
  try
    mdl := tr.FindModule('urn:wst-test');
    CheckNotNull(mdl,'urn:wst-test');
    elt := tr.FindElement(s_class_name);
      CheckNotNull(elt,s_class_name);
      CheckIs(elt,TPasClassType);
      clsType := elt as TPasClassType;
      CheckProperty('intAtt','intAtt','int',ptAttribute);
      CheckProperty('strAtt','strAtt','string',ptAttribute);
  finally
    tr.Free();
  end;
end;

{ TTest_XsdParser }

function TTest_XsdParser.ParseDoc(
  const ADoc: string;
  const ACaseSensistive: Boolean
): TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prs : IXsdPaser;
  prsCtx : IParserContext;
  fileName : string;
begin
  fileName := wstExpandLocalFileName(TestFilesPath + ADoc + '.xsd');
  locDoc := LoadXmlFile(fileName);
  try
    Result := TwstPasTreeContainer.Create();
    Result.CaseSensitive := ACaseSensistive;
    CreateWstInterfaceSymbolTable(Result);
    prs := TXsdParser.Create(locDoc,Result,ADoc);
    prsCtx := prs as IParserContext;
    prsCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(fileName)));
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

function TTest_XsdParser.LoadComplexType_Class_OpenType( ): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_open_type');
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

function TTest_XsdParser.load_class_widestring_property(): TwstPasTreeContainer;
begin
  Result := ParseDoc('class_widestring_property');
end;

function TTest_XsdParser.LoadComplexType_Class_default_values() : TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_default);
end;

function TTest_XsdParser.LoadComplexType_Class_properties_extended_metadata(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_properties_extended_metadata);
end;

function TTest_XsdParser.LoadComplexType_Class_properties_extended_metadata2(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_properties_extended_metadata + '_2');
end;

function TTest_XsdParser.load_class_ansichar_property(): TwstPasTreeContainer;
begin
  Result := ParseDoc('class_ansichar_property');
end;

function TTest_XsdParser.load_class_currency_property() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('class_currency_property');    
end;

function TTest_XsdParser.load_class_property_composed_name() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('class_property_composed_name');  
end;

function TTest_XsdParser.load_schema_import(): TwstPasTreeContainer;
begin
  Result := ParseDoc('import_second_library');
end;

function TTest_XsdParser.load_schema_include() : TwstPasTreeContainer;
begin
  Result := ParseDoc('include');
end;

function TTest_XsdParser.load_schema_include_parent_no_types() : TwstPasTreeContainer;
begin
  Result := ParseDoc('include2');
end;

function TTest_XsdParser.load_schema_include_fail_namespace() : TwstPasTreeContainer;
begin
  Result := ParseDoc('include_error');
end;

function TTest_XsdParser.load_schema_include_circular1() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('include_circular1');
end;

function TTest_XsdParser.load_schema_include_circular2() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('include_circular2');
end;

function TTest_XsdParser.load_schema_case_sensitive(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive',True);
end;

function TTest_XsdParser.load_schema_case_sensitive2(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive2',True);
end;

function TTest_XsdParser.load_schema_case_sensitive_import(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive3',True);
end;

function TTest_XsdParser.load_global_attribute() : TwstPasTreeContainer;
begin
  Result := ParseDoc('global_attribute');
end;

function TTest_XsdParser.load_class_widechar_property() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_widechar_property');
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_ItemName_Schema() : TwstPasTreeContainer;
begin
  Result := ParseDoc('array_sequence_item_name');
end;

function TTest_XsdParser.LoadComplexType_Class_FalseArray( ) : TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_false_array');
end;

function TTest_XsdParser.LoadComplexType_Class_Choice_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice');
end;

function TTest_XsdParser.LoadComplexType_Class_Choice2_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice2');
end;

function TTest_XsdParser.LoadComplexType_Class_Choice3_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice3');
end;

function TTest_XsdParser.LoadComplexType_Class_Choice4_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice4');
end;

{ TTest_WsdlParser }

function TTest_WsdlParser.ParseDoc(
  const ADoc: string;
  const ACaseSensitive: Boolean
) : TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prs : IParser;
  prsCtx : IParserContext;
  fileName : string;
begin
  fileName := wstExpandLocalFileName(TestFilesPath + ADoc + '.wsdl');
  locDoc := LoadXmlFile(fileName);
  try
    Result := TwstPasTreeContainer.Create();
    Result.CaseSensitive := ACaseSensitive;
    CreateWstInterfaceSymbolTable(Result);
    prs := TWsdlParser.Create(locDoc,Result);
    prsCtx := prs as IParserContext;
    prsCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(fileName)));
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

function TTest_WsdlParser.LoadComplexType_Class_OpenType(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_open_type');
end;

function TTest_WsdlParser.LoadComplexType_Class_FalseArray() : TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_false_array');
end;

function TTest_WsdlParser.LoadComplexType_Class_Choice_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice');
end;

function TTest_WsdlParser.LoadComplexType_Class_Choice2_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice2');
end;

function TTest_WsdlParser.LoadComplexType_Class_Choice3_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice3');
end;

function TTest_WsdlParser.LoadComplexType_Class_Choice4_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc('complex_class_choice4');
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

function TTest_WsdlParser.LoadComplexType_ArraySequence_ItemName_Schema( ) : TwstPasTreeContainer;
begin
  Result := ParseDoc('array_sequence_item_name');
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

function TTest_WsdlParser.load_class_widestring_property(): TwstPasTreeContainer;
begin
  Result := ParseDoc('class_widestring_property');
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

procedure TTest_WsdlParser.xsd_not_declared_at_top_node();
begin
  ParseDoc('xsd_not_declared_at_top_node').Free();
end;

procedure TTest_WsdlParser.xsd_not_declared_at_top_node_2();
begin
  ParseDoc('xsd_not_declared_at_top_node_2').Free();
end;

procedure TTest_WsdlParser.message_parts_type_hint();

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
  tr := ParseDoc('echo_service');
  try
    elt := tr.FindElement('IEchoService');
    CheckNotNull(elt,'IEchoService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('EchoWideString',intf);
      CheckNotNull(mth,'EchoWideString not found');
      CheckEquals('EchoWideString',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('WideString'), LowerCase(res.ResultType.Name),'Result');
      CheckEquals(1, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AValue'), LowerCase(arg.Name));
        CheckEquals(LowerCase('WideString'), LowerCase(arg.ArgType.Name),'Parameter');
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.parameter_var();

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
  tr := ParseDoc('var_parameter');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('sampleProc',intf);
      CheckNotNull(mth,'sampleProc not found');
      CheckEquals('sampleProc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasProcedureType);
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AInParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AInOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        CheckEquals('argVar',GetEnumName(TypeInfo(TArgumentAccess),Ord(arg.Access)),'arg.Access');

    mth := FindProc('sampleProc2',intf);
      CheckNotNull(mth,'sampleProc2 not found');
      CheckEquals('sampleProc2',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
        res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('ShortInt'), LowerCase(res.ResultType.Name));
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AInParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AInOutParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        CheckEquals('argConst',GetEnumName(TypeInfo(TArgumentAccess),Ord(arg.Access)),'arg.Access');
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.parameter_const_default();

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
  tr := ParseDoc('parameter_const_default');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('sampleProc',intf);
      CheckNotNull(mth,'sampleProc not found');
      CheckEquals('sampleProc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasProcedureType);
      CheckEquals(3, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('AConstParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('string'), LowerCase(arg.ArgType.Name));
        CheckEquals('argConst',GetEnumName(TypeInfo(TArgumentAccess),Ord(arg.Access)),'AConstParam');
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('ADefaultParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        CheckEquals('argDefault',GetEnumName(TypeInfo(TArgumentAccess),Ord(arg.Access)),'ADefaultParam');
      arg := TPasArgument(mthType.Args[2]);
        CheckNotNull(arg);
        CheckEquals(LowerCase('ANonSpecifiedParam'), LowerCase(arg.Name));
        CheckEquals(LowerCase('integer'), LowerCase(arg.ArgType.Name));
        CheckEquals('argConst',GetEnumName(TypeInfo(TArgumentAccess),Ord(arg.Access)),'ANonSpecifiedParam');
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.parameter_composed_name(); 

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
  tr := ParseDoc('parameter_composed_name');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('sampleProc',intf);
      CheckNotNull(mth,'sampleProc not found');
      CheckEquals('sampleProc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasProcedureType);
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals('one_param',arg.Name,'Param Name');
        CheckEquals('one-param',tr.GetExternalName(arg),'Param External Name');
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals('one_two_param',arg.Name,'Param Name');
        CheckEquals('one-two-param',tr.GetExternalName(arg),'Param External Name');
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.parameter_composed_name_function();

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
  tr := ParseDoc('parameter_composed_name');
  try
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    CheckEquals(Ord(okInterface),Ord(intf.ObjKind));
    mth := FindProc('sampleFunc',intf);
      CheckNotNull(mth,'sampleFunc not found');
      CheckEquals('sampleFunc',mth.Name);
      mthType := mth.ProcType;
      CheckIs(mthType,TPasFunctionType);
      CheckEquals(2, mthType.Args.Count, 'Parameter count');
      arg := TPasArgument(mthType.Args[0]);
        CheckNotNull(arg);
        CheckEquals('one_param',arg.Name,'Param Name');
        CheckEquals('one-param',tr.GetExternalName(arg),'Param External Name');
      arg := TPasArgument(mthType.Args[1]);
        CheckNotNull(arg);
        CheckEquals('one_two_param',arg.Name,'Param Name');
        CheckEquals('one-two-param',tr.GetExternalName(arg),'Param External Name');
      res := TPasFunctionType(mthType).ResultEl;
        CheckNotNull(res, 'Result');
        CheckEquals(LowerCase('string'), LowerCase(res.ResultType.Name));
  finally
    tr.Free();
  end;
end;

procedure TTest_WsdlParser.soap_action(); 
var
  tr : TwstPasTreeContainer;
  elt : TPasElement;
  intf : TPasClassType;
  i : Integer;
  mth : TPasProcedure;
begin
  tr := ParseDoc('soap_action');
  try //SymbolTable.Properties.SetValue(AOp,s_TRANSPORT + '_' + s_soapAction,nd.NodeValue);
    elt := tr.FindElement('TestService');
    CheckNotNull(elt,'TestService');
    CheckIs(elt,TPasClassType);
    intf := elt as TPasClassType;
    mth := nil;
    for i := 0 to (intf.Members.Count - 1) do begin
      if TObject(intf.Members[i]).InheritsFrom(TPasProcedure) then begin
        mth := TPasProcedure(intf.Members[i]);
        Break;
      end;
    end;
    CheckNotNull(mth,'test_proc not found');
    CheckEquals('test_proc',mth.Name);
    CheckEquals(
      'http://wst.Sample/Soap/Action/',
      tr.Properties.GetValue(mth,s_TRANSPORT + '_' + s_soapAction)
    );
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

function TTest_WsdlParser.LoadComplexType_Class_properties_extended_metadata2(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_class_properties_extended_metadata + '_2');
end;

function TTest_WsdlParser.load_class_ansichar_property() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_ansichar_property');
end;

function TTest_WsdlParser.load_class_widechar_property() : TwstPasTreeContainer;
begin
  Result := ParseDoc('class_widechar_property');
end;

function TTest_WsdlParser.load_class_currency_property() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('class_currency_property');
end;

function TTest_WsdlParser.load_class_property_composed_name() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('class_property_composed_name');  
end;

function TTest_WsdlParser.load_schema_import(): TwstPasTreeContainer;
begin
  Result := ParseDoc('import_second_library');
end;

function TTest_WsdlParser.load_schema_include() : TwstPasTreeContainer;
begin
  Result := ParseDoc('include_schema');
end;

function TTest_WsdlParser.load_schema_include_parent_no_types() : TwstPasTreeContainer;
begin
  Result := ParseDoc('include2');
end;

function TTest_WsdlParser.load_schema_include_fail_namespace() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('include_error');
end;

function TTest_WsdlParser.load_schema_include_circular1() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('include_circular1');
end;

function TTest_WsdlParser.load_schema_include_circular2() : TwstPasTreeContainer;  
begin
  Result := ParseDoc('include_circular2');
end;

function TTest_WsdlParser.load_schema_case_sensitive(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive',True);
end;

function TTest_WsdlParser.load_schema_case_sensitive2(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive2',True);
end;

function TTest_WsdlParser.load_schema_case_sensitive_import(): TwstPasTreeContainer;
begin
  Result := ParseDoc('case_sensitive3',True);
end;

function TTest_WsdlParser.load_global_attribute() : TwstPasTreeContainer;
begin
  Result := ParseDoc('global_attribute',True);
end;

initialization
  RegisterTest('XSD parser',TTest_XsdParser.Suite);
  RegisterTest('WSDL parser',TTest_WsdlParser.Suite);

end.
