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
    
    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;virtual;abstract;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;virtual;abstract;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;virtual;abstract;
  published
    procedure EmptySchema();

    procedure SimpleType_Enum();
    procedure SimpleType_Enum_Embedded();
    
    procedure ComplexType_Class();
    procedure ComplexType_Class_Embedded();
    
    procedure ComplexType_Record();
    
    procedure ComplexType_ArraySequence();
    procedure ComplexType_ArraySequence_Embedded();
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

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
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

    function LoadComplexType_Record_Schema() : TwstPasTreeContainer;override;
    
    function LoadComplexType_ArraySequence_Schema() : TwstPasTreeContainer;override;
    function LoadComplexType_ArraySequence_Embedded_Schema() : TwstPasTreeContainer;override;
  end;
  
implementation
uses parserutils;

const
  x_complexType_SampleArrayIntFieldType     = 'TArrayIntFieldType';
  x_complexType_SampleArrayItemType         = 'TArrayItemType';
  x_complexType_SampleDerivedType           = 'TClassSampleDerivedType';
  x_complexType_SampleClassType             = 'TClassSampleType';
  x_complexType_SampleClassTypeAll          = 'TClassSampleTypeAll';
  x_complexType_SampleClass                 = 'TClassSample';

  x_complexType_SampleRecordType             = 'TRecordSampleType';
  x_complexType_SampleRecordTypeAll          = 'TRecordSampleTypeAll';
  x_complexType_SampleRecord                 = 'TRecordSample';

  x_complexType_array_sequence      = 'complex_array_sequence';
  x_complexType_array_sequence_embedded  = 'complex_array_sequence_embedded';
  x_complexType_class               = 'complex_class';
  x_complexType_class_embedded      = 'complex_class_embedded';
  x_complexType_record               = 'complex_record';
  x_complexType_record_embedded      = 'complex_record_embedded';

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
  aliasType : TPasAliasType;
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
  aliasType : TPasAliasType;
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

function TTest_XsdParser.LoadComplexType_Record_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record);
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence);
end;

function TTest_XsdParser.LoadComplexType_ArraySequence_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_embedded);
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

function TTest_WsdlParser.LoadComplexType_Record_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_record);
end;

function TTest_WsdlParser.LoadComplexType_ArraySequence_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence);
end;

function TTest_WsdlParser.LoadComplexType_ArraySequence_Embedded_Schema(): TwstPasTreeContainer;
begin
  Result := ParseDoc(x_complexType_array_sequence_embedded);
end;

initialization
  RegisterTest('XSD parser',TTest_XsdParser.Suite);
  RegisterTest('WSDL parser',TTest_WsdlParser.Suite);

end.
