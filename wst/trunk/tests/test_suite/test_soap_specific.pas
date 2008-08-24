{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_soap_specific;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, XmlWrite, wst_fpc_xml,
{$ENDIF}
{$IFNDEF FPC}
  TestFrameWork, ActiveX, wst_delphi_xml,
{$ENDIF}
  TypInfo,
  base_service_intf, wst_types, server_service_intf, service_intf;

const
  ns_soap_test = 'soap.test.namespace';
  
type

  TSOAPTestEnum = ( steOne, steTwo, steThree, steFour );

  { NBHeader }

  NBHeader = class(THeaderBlock)
  private
    FSessionID : string;
    FUserID : string;
  public
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class function GetNameSpace() : string;
  published
    property UserID : string read FUserID write FUserID;
    property SessionID : string read FSessionID write FSessionID;
  end;

  { TNameSpaceA_Class }

  TNameSpaceA_Class = class(TBaseComplexRemotable)
  private
    FQualified_Val_Bool : boolean;
    FQualified_Val_Enum : TSOAPTestEnum;
    FQualified_Val_Int64 : Integer;
    FQualified_Val_Integer : Integer;
    FQualified_Val_String : string;
  public
    class function GetNameSpace() : string;virtual;
  published
    property Qualified_Val_Bool : boolean read FQualified_Val_Bool write FQualified_Val_Bool;
    property Qualified_Val_Enum : TSOAPTestEnum read FQualified_Val_Enum write FQualified_Val_Enum;
    property Qualified_Val_Integer : Integer read FQualified_Val_Integer write FQualified_Val_Integer;
    property Qualified_Val_Int64 : Integer read FQualified_Val_Int64 write FQualified_Val_Int64;
    property Qualified_Val_String : string Read FQualified_Val_String Write FQualified_Val_String;
  end;
  
  { TNameSpaceB_Class }

  TNameSpaceB_Class = class(TNameSpaceA_Class)
  private
    FVal_Bool : Boolean;
    FVal_String : string;
  public
    class function GetNameSpace() : string;override;
  published
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
  end;
  
  { TNameSpaceC_Class }

  TNameSpaceC_Class = class(TBaseComplexRemotable)
  private
    FProp_A : TNameSpaceA_Class;
    FProp_B : TNameSpaceB_Class;
    FProp_String : string;
  public
    constructor Create();override;
    destructor Destroy();override;
    class function GetNameSpace() : string;virtual;
  published
    property Prop_String : string Read FProp_String Write FProp_String;
    property Prop_A : TNameSpaceA_Class read FProp_A write FProp_A;
    property Prop_B : TNameSpaceB_Class read FProp_B write FProp_B;
  end;

  { TTest_SoapFormatterServerNameSpace }

  TTest_SoapFormatterServerNameSpace = class(TTestCase)
  published
    procedure namespace_declared_env();
    procedure received_header();
    procedure multi_namespace_object_write();
    procedure multi_namespace_object_read();
  end;
  
implementation
uses
  object_serializer, server_service_soap, test_suite_utils;

function GetFileFullName(const AFileName: string): string;
var
  locFileName : string;
begin
{$IFDEF FPC}
  Result := Format('.%sfiles%s%s',[PathDelim,PathDelim,AFileName]);
{$ENDIF}
{$IFDEF DELPHI}
  Result := Format('..%sfiles%s%s',[PathDelim,PathDelim,AFileName]);
{$ENDIF}
end;

function LoadXmlFromFilesList(const AFileName: string): TXMLDocument;
begin
  ReadXMLFile(Result,GetFileFullName(AFileName));
end;
  
{ NBHeader }

class procedure NBHeader.Load(
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  locSerializer : TObjectSerializer;
begin
  locSerializer := TObjectSerializer.Create(Self,GetTypeRegistry());
  try
    locSerializer.Read(AObject,AStore,AName,ATypeInfo);
  finally
    locSerializer.Free();
  end;
end;

class function NBHeader.GetNameSpace() : string;
begin
  Result := 'NBS3';
end;

{ TTest_SoapFormatterServerNameSpace }

procedure TTest_SoapFormatterServerNameSpace.namespace_declared_env();
const
  XML_SOURCE =
    '<soapenv:Envelope ' + sLineBreak +
    'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' + sLineBreak +
    'xmlns:hfp="hfpax"> ' + sLineBreak +
    '  <soapenv:Header/> ' + sLineBreak +
    '  <soapenv:Body> ' + sLineBreak +
    '     <hfp:GetVersion/> ' + sLineBreak +
    '  </soapenv:Body> ' + sLineBreak +
    '</soapenv:Envelope>';
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
begin
  f := server_service_soap.TSOAPFormatter.Create() as IFormatterResponse;
  strm := TMemoryStream.Create();
  try
    strBuffer := XML_SOURCE;
    strm.Write(strBuffer[1],Length(strBuffer));
    strm.Position := 0;
    f.LoadFromStream(strm);
    cctx := TSimpleCallContext.Create() as ICallContext;
    f.BeginCallRead(cctx);
      strBuffer := f.GetCallProcedureName();
      CheckEquals('GetVersion',strBuffer, 'GetCallProcedureName()');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterServerNameSpace.received_header();
const
  XML_SOURCE =
         '<?xml version="1.0" encoding="utf-8" ?>'  + sLineBreak +
         '<env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema"'  + sLineBreak +
         '   xmlns:env="http://schemas.xmlsoap.org/soap/envelope/"'  + sLineBreak +
         '   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'  + sLineBreak +
         ' <env:Header >'  + sLineBreak +
         '       <n1:NBHeader xmlns:n1="NBS3"'  + sLineBreak +
         '           env:mustUnderstand="1">'  + sLineBreak +
         '         <n1:UserID>AL00287DE</n1:UserID>'  + sLineBreak +
         '         <n1:SessionID>KvyxXkK9PAta4zLtm6PA</n1:SessionID>'  + sLineBreak +
         '       </n1:NBHeader>'  + sLineBreak +
         ' </env:Header>'  + sLineBreak +
         ' <env:Body>'  + sLineBreak +
         '   <n2:getSelbst xmlns:n2="NBS3">'  + sLineBreak +
         '   </n2:getSelbst>'  + sLineBreak +
         ' </env:Body>'  + sLineBreak +
         '</env:Envelope>';
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  hdr : NBHeader;
begin
  f := server_service_soap.TSOAPFormatter.Create() as IFormatterResponse;
  strm := TMemoryStream.Create();
  try
    strBuffer := XML_SOURCE;
    strm.Write(strBuffer[1],Length(strBuffer));
    strm.Position := 0;
    f.LoadFromStream(strm);
    cctx := TSimpleCallContext.Create() as ICallContext;
    f.BeginCallRead(cctx);
      CheckEquals(0,cctx.GetHeaderCount([hdOut]),'Ouput header count');
      CheckEquals(1,cctx.GetHeaderCount([hdIn]),'Input header count');
      CheckIs(cctx.GetHeader(0),NBHeader);
      hdr := NBHeader(cctx.GetHeader(0));
      CheckEquals(1,hdr.mustUnderstand,'mustUnderstand');
      CheckEquals('AL00287DE',hdr.UserID,'UserID');
      CheckEquals('KvyxXkK9PAta4zLtm6PA',hdr.SessionID);
      strBuffer := f.GetCallProcedureName();
      CheckEquals('getSelbst',strBuffer, 'GetCallProcedureName()');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterServerNameSpace.multi_namespace_object_write();
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  a : TNameSpaceA_Class;
  b : TNameSpaceB_Class;
  c : TNameSpaceC_Class;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  c := nil;
  b := nil;
  strm := nil;
  f := server_service_soap.TSOAPFormatter.Create() as IFormatterResponse;
  f.GetPropertyManager().SetProperty('Style','Document');
  f.GetPropertyManager().SetProperty('EncodingStyle','Literal');
  a := TNameSpaceA_Class.Create();
  try
    a.Qualified_Val_Bool := True;
    a.Qualified_Val_Enum := steTwo;
    a.Qualified_Val_Integer := 1210;
    a.Qualified_Val_Int64 := 123456;
    a.Qualified_Val_String := 'sample string.';
    b := TNameSpaceB_Class.Create();
      b.Val_Bool := True;
      b.Val_String := 'WST sample string, local to ' + b.GetNameSpace();
      b.Qualified_Val_Bool := True;
      b.Qualified_Val_Enum := steThree;
      b.Qualified_Val_Integer := 456;
      b.Qualified_Val_Int64 := 78945;
      b.Qualified_Val_String := 'Sample string inherited from TNameSpaceA_Class.';
    c := TNameSpaceC_Class.Create();
      c.Prop_String := 'This property should be in : ' + c.GetNameSpace() ;
      c.Prop_A.Qualified_Val_String := 'This property should be in : ' + a.GetNameSpace() ;
      c.Prop_B.Val_Bool := True;
      c.Prop_B.Val_String := 'local elemet. This property should be in : ' + b.GetNameSpace() ;
      c.Prop_B.Qualified_Val_Bool := False;
      c.Prop_B.Qualified_Val_Enum := steFour;
      c.Prop_B.Qualified_Val_Integer := 789;
      c.Prop_B.Qualified_Val_Int64 := 64;
      c.Prop_B.Qualified_Val_String := 'This inherited property should be in : ' + a.GetNameSpace() ;
    f.BeginCallResponse('SampleProc','SampleService');
      f.Put('a',TypeInfo(TNameSpaceA_Class),a);
      f.Put('b',TypeInfo(TNameSpaceB_Class),b);
      f.Put('c',TypeInfo(TNameSpaceC_Class),c);
    f.EndCallResponse();
    strm := TMemoryStream.Create();
    f.SaveToStream(strm);
    strm.SaveToFile('soap_multi_namespace_object.xml');
    
    strm.Position := 0;
    ReadXMLFile(locDoc,strm);
    locExistDoc := LoadXmlFromFilesList('soap_multi_namespace_object.xml');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    c.Free();
    b.Free();
    a.Free();
    strm.Free();
  end;
end;

procedure TTest_SoapFormatterServerNameSpace.multi_namespace_object_read();
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  a, a_readed : TNameSpaceA_Class;
  b, b_readed : TNameSpaceB_Class;
  c, c_readed : TNameSpaceC_Class;
  locDoc, locExistDoc : TXMLDocument;
  strName : string;
begin
  locDoc := nil;
  locExistDoc := nil;
  c := nil;
  b := nil;
  strm := nil;
  f := server_service_soap.TSOAPFormatter.Create() as IFormatterResponse;
  f.GetPropertyManager().SetProperty('Style','Document');
  f.GetPropertyManager().SetProperty('EncodingStyle','Literal');
  a := TNameSpaceA_Class.Create();
  try
    a.Qualified_Val_Bool := True;
    a.Qualified_Val_Enum := steTwo;
    a.Qualified_Val_Integer := 1210;
    a.Qualified_Val_Int64 := 123456;
    a.Qualified_Val_String := 'sample string.';
    b := TNameSpaceB_Class.Create();
      b.Val_Bool := True;
      b.Val_String := 'WST sample string, local to ' + b.GetNameSpace();
      b.Qualified_Val_Bool := True;
      b.Qualified_Val_Enum := steThree;
      b.Qualified_Val_Integer := 456;
      b.Qualified_Val_Int64 := 78945;
      b.Qualified_Val_String := 'Sample string inherited from TNameSpaceA_Class.';
    c := TNameSpaceC_Class.Create();
      c.Prop_String := 'This property should be in : ' + c.GetNameSpace() ;
      c.Prop_A.Qualified_Val_String := 'This property should be in : ' + a.GetNameSpace() ;
      c.Prop_B.Val_Bool := True;
      c.Prop_B.Val_String := 'local elemet. This property should be in : ' + b.GetNameSpace() ;
      c.Prop_B.Qualified_Val_Bool := False;
      c.Prop_B.Qualified_Val_Enum := steFour;
      c.Prop_B.Qualified_Val_Integer := 789;
      c.Prop_B.Qualified_Val_Int64 := 64;
      c.Prop_B.Qualified_Val_String := 'This inherited property should be in : ' + a.GetNameSpace() ;
    strm := TMemoryStream.Create();
    strm.LoadFromFile(GetFileFullName('soap_multi_namespace_object.xml'));
    strm.Position := 0;
    f.LoadFromStream(strm);
    a_readed := TNameSpaceA_Class.Create();
    b_readed := TNameSpaceB_Class.Create();
    c_readed := TNameSpaceC_Class.Create();
    f.BeginCallRead(TSimpleCallContext.Create());
      strName := 'a';
      f.Get(TypeInfo(TNameSpaceA_Class),strName,a_readed);
      strName := 'b';
      f.Get(TypeInfo(TNameSpaceB_Class),strName,b_readed);
      strName := 'c';
      f.Get(TypeInfo(TNameSpaceC_Class),strName,c_readed);
    f.EndScopeRead();

    Check(a.Equal(a_readed) and a_readed.Equal(a),'a');
    Check(b.Equal(b_readed) and b_readed.Equal(b),'b');
    Check(c.Equal(c_readed) and c_readed.Equal(c),'c');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    c.Free();
    b.Free();
    a.Free();
    strm.Free();
  end;
end;

{ TNameSpaceA_Class }

class function TNameSpaceA_Class.GetNameSpace() : string;
begin
  Result := 'NameSpace.A';
end;

{ TNameSpaceB_Class }

class function TNameSpaceB_Class.GetNameSpace() : string;
begin
  Result := 'NameSpace.B';
end;

{ TNameSpaceC_Class }

constructor TNameSpaceC_Class.Create();
begin
  inherited Create();
  FProp_A := TNameSpaceA_Class.Create();
  FProp_B := TNameSpaceB_Class.Create();
end;

destructor TNameSpaceC_Class.Destroy();
begin
  FreeAndNil(FProp_B);
  FreeAndNil(FProp_A);
  inherited Destroy();
end;

class function TNameSpaceC_Class.GetNameSpace() : string;
begin
  Result := 'NameSpace.C';
end;

initialization
  GetTypeRegistry().Register(NBHeader.GetNameSpace(),TypeInfo(NBHeader),'NBHeader');
  GetTypeRegistry().Register(TNameSpaceA_Class.GetNameSpace(),TypeInfo(TNameSpaceA_Class));
  GetTypeRegistry().Register(TNameSpaceB_Class.GetNameSpace(),TypeInfo(TNameSpaceB_Class));
  GetTypeRegistry().Register(TNameSpaceC_Class.GetNameSpace(),TypeInfo(TNameSpaceC_Class));
  GetTypeRegistry().Register(ns_soap_test,TypeInfo(TSOAPTestEnum));
  
  RegisterTest('Serializer',TTest_SoapFormatterServerNameSpace.Suite);
  
end.

