{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_generators_runtime;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, XmlWrite, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  base_service_intf, metadata_wsdl, metadata_repository, wst_types;
  
const
  sNAMESPACE_SAMPLE = 'urn:sample-namespace';
  
type

  TTestEnum = ( teA, teB, teC );
  
  { TClass_A }

  TClass_A = class(TBaseComplexRemotable)
  private
    FA_StringProp : string;
  published
    property A_StringProp : string Read FA_StringProp Write FA_StringProp;
  end;
  
  { TClass_AB }

  TClass_AB = class(TClass_A)
  private
    FAB_IntProp : Integer;
  published
    property AB_IntProp : Integer read FAB_IntProp write FAB_IntProp;
  end;

  { TClass_ABC }

  TClass_ABC = class(TClass_AB)
  private
    FABC_BoolProp : Boolean;
    FABC_EnumAttProp : TTestEnum;
  published
    property ABC_BoolProp : Boolean read FABC_BoolProp write FABC_BoolProp;
    property ABC_EnumAttProp : TTestEnum read FABC_EnumAttProp write FABC_EnumAttProp;
  end;

  TArrayOfStringRemotableSample = class(TArrayOfStringRemotable)
  end;

  TArrayOfIntRemotableSample = class(TArrayOfInt32SRemotable)
  end;
  
  TTestSmallRecord = record
    fieldSmallint : Smallint;
    fieldWord : Word;
    fieldString : string;
  end;

  TLoginHeader = class(THeaderBlock)
  private
    FLogin: string;
    FPassword: string;
  published
    property Login : string read FLogin write FLogin;
    property Password : string read FPassword write FPassword;
  end;

  TClass_A_Collection = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TClass_A;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TClass_A; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TClass_A; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TClass_A Read GetItem;Default;
  end;

  { TTestWSDLGenerator }

  TTestWSDLGenerator= class(TTestCase)
  protected
    function CreateRepository() : PServiceRepository;
  published
    procedure generate_complex_type_derivation();
    procedure generate_enum();
    procedure generate_array();
    procedure generate_collection();
    procedure generate_record();
    procedure generate_soap_headerblock();
  end;

implementation
uses
  TypInfo, record_rtti, test_suite_utils;
  
{$IFDEF WST_RECORD_RTTI}
function __TTestSmallRecord_TYPEINFO_FUNC__() : PTypeInfo;
var
  p : ^TTestSmallRecord;
  r : TTestSmallRecord;
begin
  p := @r;
  Result := MakeRawTypeInfo(
    'TTestSmallRecord',
    SizeOf(TTestSmallRecord),
    [ PtrUInt(@(p^.fieldSmallint)) - PtrUInt(p), PtrUInt(@(p^.fieldWord)) - PtrUInt(p), PtrUInt(@(p^.fieldString)) - PtrUInt(p) ],
    [ TypeInfo(SmallInt), TypeInfo(Word), TypeInfo(String) ]
  );
end;
{$ENDIF WST_RECORD_RTTI}

{ TClass_A_Collection }

function TClass_A_Collection.GetItem(AIndex: Integer): TClass_A;
begin
  Result := TClass_A(Inherited GetItem(AIndex));
end;

class function TClass_A_Collection.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TClass_A;
end;

function TClass_A_Collection.Add() : TClass_A;
begin
  Result := TClass_A(inherited Add());
end;

function TClass_A_Collection.AddAt(const APosition : Integer) : TClass_A;
begin
  Result := TClass_A(inherited AddAt(APosition));
end;

{ TTestWSDLGenerator }

function TTestWSDLGenerator.CreateRepository() : PServiceRepository;
var
  locRes : PServiceRepository;
begin
  New(locRes);
  locRes^.Name := 'runtime_generator';
  locRes^.NameSpace := sNAMESPACE_SAMPLE;
  locRes^.RootAddress := 'http://runtime-generator-sample.com';
  locRes^.Services := nil;
  locRes^.ServicesCount := 0;
  Result := locRes;
end;

procedure TTestWSDLGenerator.generate_complex_type_derivation();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TTestEnum));
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TClass_A),'TClass_A');
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TClass_AB),'TClass_AB');
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TClass_ABC),'Class_ABC');
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('wsdl_gen_complex_type_derivation.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_complex_type_derivation.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

procedure TTestWSDLGenerator.generate_enum();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    with typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TTestEnum),'TestEnum_Type') do begin
      RegisterExternalPropertyName('teA', 'A');
      RegisterExternalPropertyName('teC', 'The C Item');
    end;
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('wsdl_gen_generate_enum.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_generate_enum.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

procedure TTestWSDLGenerator.generate_array();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TArrayOfStringRemotableSample));
    with typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TArrayOfIntRemotableSample)) do begin
      RegisterExternalPropertyName(sARRAY_ITEM,'int_value');
      RegisterExternalPropertyName(sARRAY_STYLE,sScoped);
    end;
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('wsdl_gen_generate_array.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_generate_array.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

procedure TTestWSDLGenerator.generate_collection();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TClass_A));
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TClass_A_Collection));
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('wsdl_gen_generate_collection.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_generate_collection.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

procedure TTestWSDLGenerator.generate_record();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    with typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TTestSmallRecord)) do begin
      RegisterExternalPropertyName('__FIELDS__','fieldSmallint;fieldWord;fieldString');
    end;
{$IFNDEF WST_RECORD_RTTI}
    typeReg.ItemByTypeInfo[TypeInfo(TTestSmallRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(TypeInfo(TTestSmallRecord)),typeReg.ItemByTypeInfo[TypeInfo(TTestSmallRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
{$IFDEF WST_RECORD_RTTI}
    typeReg.ItemByTypeInfo[TypeInfo(TTestSmallRecord)].RegisterObject(FIELDS_STRING,TRecordRttiDataObject.Create(MakeRecordTypeInfo(__TTestSmallRecord_TYPEINFO_FUNC__()),typeReg.ItemByTypeInfo[TypeInfo(TTestSmallRecord)].GetExternalPropertyName('__FIELDS__')));
{$ENDIF WST_RECORD_RTTI}
    (typeReg.ItemByTypeInfo[TypeInfo(TTestSmallRecord)].GetObject(FIELDS_STRING) as TRecordRttiDataObject).GetField('fieldWord')^.IsAttribute := True;
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('wsdl_gen_generate_record.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_generate_record.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

procedure TTestWSDLGenerator.generate_soap_headerblock();
var
  locRep : PServiceRepository;
  locDoc, locExistDoc : TXMLDocument;
  typeReg : TTypeRegistry;
  handlerReg : IWsdlTypeHandlerRegistry;
begin
  locExistDoc := nil;
  typeReg := nil;
  locDoc := nil;
  locRep := CreateRepository();
  try
    typeReg := TTypeRegistry.Create();
    RegisterStdTypes(typeReg);
    typeReg.Register(sNAMESPACE_SAMPLE,TypeInfo(TLoginHeader));
    handlerReg := CreateWsdlTypeHandlerRegistry(typeReg);
    RegisterFondamentalTypesHandler(handlerReg);
    locDoc := CreateDoc();
    GenerateWSDL(locRep,locDoc,typeReg,handlerReg);
    //WriteXML(locDoc,wstExpandLocalFileName('generate_soap_headerblock.wsdl'));
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'wsdl_gen_soap_headerblock.wsdl'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    typeReg.Free();
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    Dispose(locRep);
  end;
end;

initialization
  TClass_ABC.RegisterAttributeProperty('ABC_EnumAttProp');
  
  RegisterTest('Runtime Generators',TTestWSDLGenerator.Suite);
  
end.
