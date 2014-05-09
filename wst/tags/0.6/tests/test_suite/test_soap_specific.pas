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

  { TLoginInfos }

  TLoginInfos = class(TBaseComplexRemotable)
  private
    FPassword: string;
    FUserName: string;
  published
    property UserName : string read FUserName write FUserName;
    property Password : string read FPassword write FPassword;
  end;

  { THeaderProxyTestObject }

  THeaderProxyTestObject = class(TBaseComplexRemotable)
  private
    FDestructionCount: PInteger;
    procedure SetDestructionCount(const AValue: PInteger);
  public
    destructor Destroy(); override;
    property DestructionCount : PInteger read FDestructionCount write SetDestructionCount;
  end;

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
    procedure FreeObjectProperties();override;
    class function GetNameSpace() : string;virtual;
  published
    property Prop_String : string Read FProp_String Write FProp_String;
    property Prop_A : TNameSpaceA_Class read FProp_A write FProp_A;
    property Prop_B : TNameSpaceB_Class read FProp_B write FProp_B;
  end;

  TSampleSimpleContentHeaderBlock_A = class(TSimpleContentHeaderBlock)
  public
    class function GetNameSpace() : string;
  end;

  { TSampleSimpleContentHeaderBlock_B }

  TSampleSimpleContentHeaderBlock_B = class(TSampleSimpleContentHeaderBlock_A)
  private
    FintAtt : Integer;
  published
    property intAtt : Integer read FintAtt write FintAtt;
  end;
  
  { TTest_SoapFormatterServerNameSpace }

  TTest_SoapFormatterServerNameSpace = class(TTestCase)
  published
    procedure namespace_declared_env();
    procedure received_header();
    procedure multi_namespace_object_write();
    procedure multi_namespace_object_read();
  end;
  
  { TTest_SoapFormatterHeader }

  TTest_SoapFormatterHeader = class(TTestCase)
  published
    procedure write_header_simple_content_1();
    procedure write_header_simple_content_1_b();
    procedure write_header_simple_content_2();
    procedure read_header_simple_content_1();
    procedure read_header_simple_content_2();

    procedure write_header_proxy_header_block();
    procedure write_header_proxy_header_block_name();
    procedure read_header_proxy_header_block();
    procedure read_header_proxy_header_block_name();
  end;
  
  THRefTestSession = class(TBaseComplexRemotable)
  private
    FSessionID : string;
    FPartnerID : integer;
  published
    property SessionID : string read FSessionID write FSessionID;
    property PartnerID : integer read FPartnerID write FPartnerID;
  end;

  { TTest_SoapFormatterClient }

  TTest_SoapFormatterClient = class(TTestCase)
  published
    procedure test_soap_href_id();
    procedure inline_namespace();
  end;

  { TTest_THeaderBlockProxy }

  TTest_THeaderBlockProxy = class(TTestCase)
  published
    procedure ActualObject;
    procedure OwnObject_Destroy;
    procedure OwnObject_SetActualObject;
  end;

implementation
uses
  object_serializer, server_service_soap, test_suite_utils, soap_formatter;

function GetFileFullName(const AFileName: string): string;
begin
  Result := wstExpandLocalFileName(TestFilesPath + AFileName);
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
    //strm.SaveToFile('soap_multi_namespace_object.xml');
    
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
    c_readed.Free();
    b_readed.Free();
    a_readed.Free();
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

procedure TNameSpaceC_Class.FreeObjectProperties();
begin
  FreeAndNil(FProp_B);
  FreeAndNil(FProp_A);
  inherited FreeObjectProperties();
end;

class function TNameSpaceC_Class.GetNameSpace() : string;
begin
  Result := 'NameSpace.C';
end;

{ TSampleSimpleContentHeaderBlock_A }

class function TSampleSimpleContentHeaderBlock_A.GetNameSpace() : string;
begin
  Result := 'urn:simple-content-header.sample';
end;

{ TTest_SoapFormatterHeader }

procedure TTest_SoapFormatterHeader.write_header_simple_content_1();
var
  ser : IFormatterClient;
  cc : ICallContext;
  hdr : TSampleSimpleContentHeaderBlock_A;
  locStream : TMemoryStream;
  locDoc, locExistDoc : TXMLDocument;
begin
  cc := TSimpleCallContext.Create();
  hdr := TSampleSimpleContentHeaderBlock_A.Create();
  cc.AddHeader(hdr,True);
  hdr.Direction := hdOut;
  hdr.Value := 'sample header simple content value';
  ser := soap_formatter.TSOAPFormatter.Create();
  ser.BeginCall('test_proc','TestService',cc);
  ser.EndScope();
  locDoc := nil;
  locExistDoc := nil;
  locStream := TMemoryStream.Create();
  try
    ser.SaveToStream(locStream);
    //locStream.SaveToFile(wstExpandLocalFileName('write_header_simple_content_1.xml'));
    locStream.Position := 0;
    ReadXMLFile(locDoc,locStream);
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'write_header_simple_content_1.xml'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locDoc);
    ReleaseDomNode(locExistDoc);
    locStream.Free();
  end;
end;

procedure TTest_SoapFormatterHeader.write_header_simple_content_1_b();
var
  ser : IFormatterClient;
  cc : ICallContext;
  hdr : TSampleSimpleContentHeaderBlock_A;
  locStream : TMemoryStream;
  locDoc, locExistDoc : TXMLDocument;
begin
  cc := TSimpleCallContext.Create();
  hdr := TSampleSimpleContentHeaderBlock_A.Create();
  cc.AddHeader(TBaseRemotable(hdr),True);
  hdr.Direction := hdOut;
  hdr.Value := 'sample header simple content value';
  ser := soap_formatter.TSOAPFormatter.Create();
  ser.BeginCall('test_proc','TestService',cc);
  ser.EndScope();
  locDoc := nil;
  locExistDoc := nil;
  locStream := TMemoryStream.Create();
  try
    ser.SaveToStream(locStream);
    //locStream.SaveToFile(wstExpandLocalFileName('write_header_simple_content_1.xml'));
    locStream.Position := 0;
    ReadXMLFile(locDoc,locStream);
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'write_header_simple_content_1.xml'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locDoc);
    ReleaseDomNode(locExistDoc);
    locStream.Free();
  end;
end;

procedure TTest_SoapFormatterHeader.write_header_simple_content_2();
var
  ser : IFormatterClient;
  cc : ICallContext;
  hdrA : TSampleSimpleContentHeaderBlock_A;
  hdrB : TSampleSimpleContentHeaderBlock_B;
  locStream : TMemoryStream;
  locDoc, locExistDoc : TXMLDocument;
begin
  cc := TSimpleCallContext.Create();
  hdrA := TSampleSimpleContentHeaderBlock_A.Create();
  cc.AddHeader(hdrA,True);
  hdrA.Direction := hdOut;
  hdrA.Value := 'sample header simple content value';
  hdrB := TSampleSimpleContentHeaderBlock_B.Create();
  cc.AddHeader(hdrB,True);
  hdrB.Direction := hdOut;
  hdrB.mustUnderstand := 1;
  hdrB.Value := 'another content';
  hdrB.intAtt := 1210;

  ser := soap_formatter.TSOAPFormatter.Create();
  ser.BeginCall('test_proc','TestService',cc);
  ser.EndScope();
  locDoc := nil;
  locExistDoc := nil;
  locStream := TMemoryStream.Create();
  try
    ser.SaveToStream(locStream);
    //locStream.SaveToFile(wstExpandLocalFileName('write_header_simple_content_2.xml'));
    locStream.Position := 0;
    ReadXMLFile(locDoc,locStream);
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'write_header_simple_content_2.xml'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locDoc);
    ReleaseDomNode(locExistDoc);
    locStream.Free();
  end;
end;

procedure TTest_SoapFormatterHeader.read_header_simple_content_1();
const
  XML_SOURCE =
         '<?xml version="1.0"?>'  + sLineBreak +
         '<SOAP-ENV:Envelope '  + sLineBreak +
         '  xmlns:xsd="http://www.w3.org/2001/XMLSchema" '  + sLineBreak +
         '  xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" '  + sLineBreak +
         '  xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" '  + sLineBreak +
         '  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'  + sLineBreak +
         '  <SOAP-ENV:Header xmlns:ns1="urn:simple-content-header.sample">'  + sLineBreak +
         '    <ns1:TSampleSimpleContentHeaderBlock_A SOAP-ENV:mustUnderstand="1">sample header simple content value</ns1:TSampleSimpleContentHeaderBlock_A>'  + sLineBreak +
         '  </SOAP-ENV:Header>'  + sLineBreak +
         '  <SOAP-ENV:Body>'  + sLineBreak +
         '    <ns2:test_proc xmlns:ns2="TestService"/>'  + sLineBreak +
         '  </SOAP-ENV:Body>'  + sLineBreak +
         '</SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  hdr : TSampleSimpleContentHeaderBlock_A;
begin
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
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
      CheckIs(cctx.GetHeader(0),TSampleSimpleContentHeaderBlock_A);
      hdr := TSampleSimpleContentHeaderBlock_A(cctx.GetHeader(0));
      CheckEquals(1,hdr.mustUnderstand,'mustUnderstand');
      CheckEquals('sample header simple content value',hdr.Value,'Value');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterHeader.read_header_simple_content_2();
const
  XML_SOURCE =
         '<?xml version="1.0"?>'  + sLineBreak +
         '<SOAP-ENV:Envelope '  + sLineBreak +
         '  xmlns:xsd="http://www.w3.org/2001/XMLSchema" '  + sLineBreak +
         '  xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" '  + sLineBreak +
         '  xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" '  + sLineBreak +
         '  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'  + sLineBreak +
         '  <SOAP-ENV:Header xmlns:ns1="urn:simple-content-header.sample">'  + sLineBreak +
         '    <ns1:TSampleSimpleContentHeaderBlock_A SOAP-ENV:mustUnderstand="1">sample header simple content value</ns1:TSampleSimpleContentHeaderBlock_A>'  + sLineBreak +
         '    <ns1:TSampleSimpleContentHeaderBlock_B intAtt="1210" SOAP-ENV:mustUnderstand="1">another content</ns1:TSampleSimpleContentHeaderBlock_B>'  + sLineBreak +
         '  </SOAP-ENV:Header>'  + sLineBreak +
         '  <SOAP-ENV:Body>'  + sLineBreak +
         '    <ns2:test_proc xmlns:ns2="TestService"/>'  + sLineBreak +
         '  </SOAP-ENV:Body>'  + sLineBreak +
         '</SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  hdrA : TSampleSimpleContentHeaderBlock_A;
  hdrB : TSampleSimpleContentHeaderBlock_B;
begin
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
  strm := TMemoryStream.Create();
  try
    strBuffer := XML_SOURCE;
    strm.Write(strBuffer[1],Length(strBuffer));
    strm.Position := 0;
    f.LoadFromStream(strm);
    cctx := TSimpleCallContext.Create() as ICallContext;
    f.BeginCallRead(cctx);
      CheckEquals(0,cctx.GetHeaderCount([hdOut]),'Ouput header count');
      CheckEquals(2,cctx.GetHeaderCount([hdIn]),'Input header count');
      CheckIs(cctx.GetHeader(0),TSampleSimpleContentHeaderBlock_A);
        hdrA := TSampleSimpleContentHeaderBlock_A(cctx.GetHeader(0));
        CheckEquals(1,hdrA.mustUnderstand,'mustUnderstand');
        CheckEquals('sample header simple content value',hdrA.Value,'Value');
      CheckIs(cctx.GetHeader(1),TSampleSimpleContentHeaderBlock_B);
        hdrB := TSampleSimpleContentHeaderBlock_B(cctx.GetHeader(1));
        CheckEquals(1,hdrB.mustUnderstand,'mustUnderstand');
        CheckEquals('another content',hdrB.Value,'Value');
        CheckEquals(1210,hdrB.intAtt,'intAtt');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterHeader.write_header_proxy_header_block();
var
  ser : IFormatterClient;
  cc : ICallContext;
  locLoginInfo : TLoginInfos;
  locStream : TMemoryStream;
  locDoc, locExistDoc : TXMLDocument;
begin
  cc := TSimpleCallContext.Create();
  locLoginInfo := TLoginInfos.Create();
  locLoginInfo.UserName := 'Inoussa-wst';
  locLoginInfo.Password := 'sample password';
  cc.AddHeader(locLoginInfo,True);
  ser := soap_formatter.TSOAPFormatter.Create();
  ser.BeginCall('test_proc','TestService',cc);
  ser.EndScope();
  locDoc := nil;
  locExistDoc := nil;
  locStream := TMemoryStream.Create();
  try
    ser.SaveToStream(locStream);
    //locStream.SaveToFile(wstExpandLocalFileName('write_header_proxy_header_block.xml'));
    locStream.Position := 0;
    ReadXMLFile(locDoc,locStream);
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'write_header_proxy_header_block.xml'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locDoc);
    ReleaseDomNode(locExistDoc);
    locStream.Free();
  end;
end;

procedure TTest_SoapFormatterHeader.write_header_proxy_header_block_name();
var
  ser : IFormatterClient;
  cc : ICallContext;
  locLoginInfo : TLoginInfos;
  locStream : TMemoryStream;
  locDoc, locExistDoc : TXMLDocument;
begin
  cc := TSimpleCallContext.Create();
  locLoginInfo := TLoginInfos.Create();
  locLoginInfo.UserName := 'Inoussa-wst';
  locLoginInfo.Password := 'sample password';
  cc.AddHeader(locLoginInfo,True,'NamedLoginInfos');
  ser := soap_formatter.TSOAPFormatter.Create();
  ser.BeginCall('test_proc','TestService',cc);
  ser.EndScope();
  locDoc := nil;
  locExistDoc := nil;
  locStream := TMemoryStream.Create();
  try
    ser.SaveToStream(locStream);
    //locStream.SaveToFile(wstExpandLocalFileName('write_header_proxy_header_block_name.xml'));
    locStream.Position := 0;
    ReadXMLFile(locDoc,locStream);
    ReadXMLFile(locExistDoc,wstExpandLocalFileName(TestFilesPath + 'write_header_proxy_header_block_name.xml'));
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locDoc);
    ReleaseDomNode(locExistDoc);
    locStream.Free();
  end;
end;

procedure TTest_SoapFormatterHeader.read_header_proxy_header_block();
const
  XML_SOURCE =
         '<?xml version="1.0"?>'  + sLineBreak +
         '<SOAP-ENV:Envelope '  + sLineBreak +
         '      xmlns:xsd="http://www.w3.org/2001/XMLSchema" '  + sLineBreak +
         '      xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" '  + sLineBreak +
         '      xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" '  + sLineBreak +
         '      xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'  + sLineBreak +
         '  <SOAP-ENV:Header xmlns:ns1="soap.test.namespace">'  + sLineBreak +
         '    <ns1:LoginInfos >'  + sLineBreak +
         '      <ns1:UserName>Inoussa-wst</ns1:UserName>'  + sLineBreak +
         '      <ns1:Password>sample password</ns1:Password>'  + sLineBreak +
         '    </ns1:LoginInfos>'  + sLineBreak +
         '  </SOAP-ENV:Header>'  + sLineBreak +
         '  <SOAP-ENV:Body>'  + sLineBreak +
         '    <ns2:test_proc xmlns:ns2="TestService"/>'  + sLineBreak +
         '  </SOAP-ENV:Body>'  + sLineBreak +
         '</SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  hdr : THeaderBlockProxy;
  actualHeader : TLoginInfos;
begin
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
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
      CheckIs(cctx.GetHeader(0),THeaderBlockProxy);
      hdr := THeaderBlockProxy(cctx.GetHeader(0));
      CheckIs(hdr.ActualObject,TLoginInfos);
      actualHeader := TLoginInfos(hdr.ActualObject);
      //CheckEquals(1,hdr.mustUnderstand,'mustUnderstand');
      CheckEquals('LoginInfos',hdr.Name,'Name');
      CheckEquals('Inoussa-wst',actualHeader.UserName,'UserName');
      CheckEquals('sample password',actualHeader.Password,'Password');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterHeader.read_header_proxy_header_block_name();
const
  XML_SOURCE =
         '<?xml version="1.0"?>'  + sLineBreak +
         '<SOAP-ENV:Envelope '  + sLineBreak +
         '      xmlns:xsd="http://www.w3.org/2001/XMLSchema" '  + sLineBreak +
         '      xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" '  + sLineBreak +
         '      xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" '  + sLineBreak +
         '      xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'  + sLineBreak +
         '  <SOAP-ENV:Header xmlns:ns1="soap.test.namespace">'  + sLineBreak +
         '    <ns1:NamedLoginInfos >'  + sLineBreak +
         '      <ns1:UserName>Inoussa-wst</ns1:UserName>'  + sLineBreak +
         '      <ns1:Password>sample password</ns1:Password>'  + sLineBreak +
         '    </ns1:NamedLoginInfos>'  + sLineBreak +
         '  </SOAP-ENV:Header>'  + sLineBreak +
         '  <SOAP-ENV:Body>'  + sLineBreak +
         '    <ns2:test_proc xmlns:ns2="TestService"/>'  + sLineBreak +
         '  </SOAP-ENV:Body>'  + sLineBreak +
         '</SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  hdr : THeaderBlockProxy;
  actualHeader : TLoginInfos;
begin
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
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
      CheckIs(cctx.GetHeader(0),THeaderBlockProxy);
      hdr := THeaderBlockProxy(cctx.GetHeader(0));
      CheckIs(hdr.ActualObject,TLoginInfos);
      actualHeader := TLoginInfos(hdr.ActualObject);
      CheckEquals('NamedLoginInfos',hdr.Name,'Name');
      CheckEquals('Inoussa-wst',actualHeader.UserName,'UserName');
      CheckEquals('sample password',actualHeader.Password,'Password');
    f.EndScopeRead();
  finally
    FreeAndNil(strm);
  end;
end;

{ TTest_SoapFormatterClient }

procedure TTest_SoapFormatterClient.test_soap_href_id();
const
  XML_SOURCE =
         '<?xml version="1.0"?>'  + sLineBreak +
         '<SOAP-ENV:Envelope '  + sLineBreak +
         '  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" '  + sLineBreak +
         '  xmlns:xsd="http://www.w3.org/2001/XMLSchema" '  + sLineBreak +
         '  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '  + sLineBreak +
         '  xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">'  + sLineBreak +
         '  '  + sLineBreak +
         '  <SOAP-ENV:Body SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">'  + sLineBreak +
         '    <NS1:GetSessionResponse xmlns:NS1="urn:WS_PlotjetIntfU-IWS_Plotjet" xmlns:NS2="urn:WS_PlotjetIntfU">'  + sLineBreak +
         '      <NS2:TSession id="1" xsi:type="NS2:TSession">'  + sLineBreak +
         '        <SessionID xsi:type="xsd:string">Some GID</SessionID>'  + sLineBreak +
         '        <PartnerID xsi:type="xsd:int">12</PartnerID>'  + sLineBreak +
         '      </NS2:TSession>'  + sLineBreak +
         '      <return href="#1"/>'  + sLineBreak +
         '    </NS1:GetSessionResponse>'  + sLineBreak +
         '  </SOAP-ENV:Body>'  + sLineBreak +
         '</SOAP-ENV:Envelope>';
var
  f : IFormatterClient;
  strm : TMemoryStream;
  strBuffer : ansistring;
  cctx : ICallContext;
  locReturn : THRefTestSession;
  locStrPrmName : string;
begin
  locReturn := nil;
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
  strm := TMemoryStream.Create();
  try
    strBuffer := XML_SOURCE;
    strm.Write(strBuffer[1],Length(strBuffer));
    strm.Position := 0;
    f.LoadFromStream(strm);
    cctx := TSimpleCallContext.Create() as ICallContext;
    f.BeginCallRead(cctx);
      locReturn := nil;
      locStrPrmName := 'return';
      f.Get(TypeInfo(THRefTestSession),locStrPrmName,locReturn);
      CheckNotNull(locReturn,'return');
      CheckEquals('Some GID',locReturn.SessionID,'SessionID');
      CheckEquals(12,locReturn.PartnerID,'PartnerID');
    f.EndScopeRead();
  finally
    FreeAndNil(locReturn);
    FreeAndNil(strm);
  end;
end;

procedure TTest_SoapFormatterClient.inline_namespace();
var
  f : IFormatterClient;
  strm : TMemoryStream;
  c, c_readed : TNameSpaceC_Class;
  strName : string;
begin
  c_readed := nil;
  strm := nil;
  f := soap_formatter.TSOAPFormatter.Create() as IFormatterClient;
  f.GetPropertyManager().SetProperty('Style','Document');
  f.GetPropertyManager().SetProperty('EncodingStyle','Literal');
  c := TNameSpaceC_Class.Create();
  try
      c.Prop_String := 'This property should be in : ' + c.GetNameSpace() ;
      c.Prop_A.Qualified_Val_String := 'This property should be in : ' + TNameSpaceA_Class.GetNameSpace() ;
      c.Prop_B.Val_Bool := True;
      c.Prop_B.Val_String := 'local elemet. This property should be in : ' + TNameSpaceB_Class.GetNameSpace() ;
      c.Prop_B.Qualified_Val_Bool := False;
      c.Prop_B.Qualified_Val_Enum := steFour;
      c.Prop_B.Qualified_Val_Integer := 789;
      c.Prop_B.Qualified_Val_Int64 := 64;
      c.Prop_B.Qualified_Val_String := 'This inherited property should be in : ' + TNameSpaceA_Class.GetNameSpace() ;
    strm := TMemoryStream.Create();
    strm.LoadFromFile(GetFileFullName('soap_inline_ns.xml'));
    strm.Position := 0;
    f.LoadFromStream(strm);
    c_readed := TNameSpaceC_Class.Create();
    f.BeginCallRead(TSimpleCallContext.Create());
      strName := 'c';
      Check(f.Get(TypeInfo(TNameSpaceC_Class),strName,c_readed),'Reading from Formatter');
    f.EndScopeRead();

    CheckEquals(c.Prop_String,c_readed.Prop_String,'Prop_String');
    CheckNotNull(c_readed.Prop_A,'Prop_A');
    CheckEquals(c.Prop_A.Qualified_Val_String,c_readed.Prop_A.Qualified_Val_String,'Prop_A.Qualified_Val_String');
    CheckNotNull(c_readed.Prop_B,'Prop_B');
    CheckEquals(c.Prop_B.Val_Bool,c_readed.Prop_B.Val_Bool,'Prop_B.Val_Bool');
    CheckEquals(c.Prop_B.Val_String,c_readed.Prop_B.Val_String,'Prop_B.Val_String');
    CheckEquals(c.Prop_B.Qualified_Val_Bool,c_readed.Prop_B.Qualified_Val_Bool,'Prop_B.Qualified_Val_Bool');
    CheckEquals(Ord(c.Prop_B.Qualified_Val_Enum),Ord(c_readed.Prop_B.Qualified_Val_Enum),'Prop_B.Qualified_Val_Enum');
    CheckEquals(c.Prop_B.Qualified_Val_Integer,c_readed.Prop_B.Qualified_Val_Integer,'Prop_B.Qualified_Val_Integer');
    CheckEquals(c.Prop_B.Qualified_Val_Int64,c_readed.Prop_B.Qualified_Val_Int64,'Prop_B.Qualified_Val_Int64');
    CheckEquals(c.Prop_B.Qualified_Val_String,c_readed.Prop_B.Qualified_Val_String,'Prop_B.Qualified_Val_String');
    Check(c.Equal(c_readed) and c_readed.Equal(c),'c');
  finally
    c_readed.Free();
    c.Free();
    strm.Free();
  end;
end;

{ THeaderProxyTestObject }

procedure THeaderProxyTestObject.SetDestructionCount(const AValue: PInteger);
begin
  if ( FDestructionCount = AValue ) then
    Exit;
  FDestructionCount := AValue;
end;

destructor THeaderProxyTestObject.Destroy();
begin
  if ( FDestructionCount <> nil ) then
    Inc(FDestructionCount^);
  inherited Destroy();
end;

{ TTest_THeaderBlockProxy }

procedure TTest_THeaderBlockProxy.ActualObject;
var
  locObj : THeaderBlockProxy;
  ao1, ao2 : THeaderProxyTestObject;
begin
  ao1 := nil;
  ao2 := nil;
  locObj := THeaderBlockProxy.Create();
  try
    CheckNull(locObj.ActualObject);
    CheckEquals(False, locObj.OwnObject);
    ao1 := THeaderProxyTestObject.Create();
    ao2 := THeaderProxyTestObject.Create();

    locObj.ActualObject := ao1;
      CheckSame(ao1, locObj.ActualObject);
    locObj.ActualObject := ao2;
      CheckSame(ao2,locObj.ActualObject);
    locObj.ActualObject := nil;
      CheckNull(locObj.ActualObject);
  finally
    locObj.Free();
    ao1.Free();
    ao2.Free();
  end;
end;

procedure TTest_THeaderBlockProxy.OwnObject_Destroy;
var
  locObj : THeaderBlockProxy;
  ao1 : THeaderProxyTestObject;
  locDestructionCount : Integer;
begin
  locDestructionCount := 0;
  ao1 := nil;
  locObj := THeaderBlockProxy.Create();
    ao1 := THeaderProxyTestObject.Create();
    locObj.ActualObject := ao1;
    locObj.OwnObject := True;
    ao1.DestructionCount := @locDestructionCount;
    locObj.Free();
    CheckEquals(1,locDestructionCount);
end;

procedure TTest_THeaderBlockProxy.OwnObject_SetActualObject;
var
  locObj : THeaderBlockProxy;
  ao1, ao2 : THeaderProxyTestObject;
  locDestructionCount : Integer;
begin
  locDestructionCount := 0;
  ao1 := nil;
  locObj := THeaderBlockProxy.Create();
    ao1 := THeaderProxyTestObject.Create();
      ao1.DestructionCount := @locDestructionCount;
    ao2 := THeaderProxyTestObject.Create();
      ao2.DestructionCount := @locDestructionCount;
    locObj.OwnObject := True;

    locObj.ActualObject := ao1;
    locObj.ActualObject := ao2;
      CheckEquals(1,locDestructionCount);
    locObj.ActualObject := ao2;
      CheckEquals(1,locDestructionCount,'Setting the same value should not free the object.');
    locObj.Free();
      CheckEquals(2,locDestructionCount);
end;

initialization

  GetTypeRegistry().Register(TSampleSimpleContentHeaderBlock_A.GetNameSpace(),TypeInfo(TSampleSimpleContentHeaderBlock_A));
  TSampleSimpleContentHeaderBlock_B.RegisterAttributeProperty('intAtt');
  GetTypeRegistry().Register(TSampleSimpleContentHeaderBlock_B.GetNameSpace(),TypeInfo(TSampleSimpleContentHeaderBlock_B));
  
  GetTypeRegistry().Register(NBHeader.GetNameSpace(),TypeInfo(NBHeader),'NBHeader');
  GetTypeRegistry().Register(TNameSpaceA_Class.GetNameSpace(),TypeInfo(TNameSpaceA_Class));
  GetTypeRegistry().Register(TNameSpaceB_Class.GetNameSpace(),TypeInfo(TNameSpaceB_Class));
  GetTypeRegistry().Register(TNameSpaceC_Class.GetNameSpace(),TypeInfo(TNameSpaceC_Class));
  GetTypeRegistry().Register(ns_soap_test,TypeInfo(TSOAPTestEnum));
  GetTypeRegistry().Register('urn:WS_PlotjetIntfU',TypeInfo(THRefTestSession),'TSession');
  GetTypeRegistry().Register(ns_soap_test,TypeInfo(TLoginInfos),'LoginInfos').AddExternalSynonym('NamedLoginInfos');
  GetTypeRegistry().Register(ns_soap_test,TypeInfo(THeaderProxyTestObject));

  
  RegisterTest('Serializer',TTest_SoapFormatterServerNameSpace.Suite);
  RegisterTest('Serializer',TTest_SoapFormatterHeader.Suite);
  RegisterTest('Serializer',TTest_SoapFormatterClient.Suite);
  
  RegisterTest('Support',TTest_THeaderBlockProxy.Suite);
end.

