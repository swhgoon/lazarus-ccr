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
{$INCLUDE wst_tests_defines.inc}
unit test_json;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ENDIF}
{$IFDEF WST_DELPHI}
  TestFrameWork, ActiveX,
{$ENDIF}
  TypInfo,
  base_service_intf, wst_types, server_service_intf, service_intf,
  fpjson, jsonparser, base_json_formatter, json_formatter, server_service_json,
  testformatter_unit;

type

  { TTestJsonRpcFormatter }

  TTestJsonRpcFormatter= class(TTestFormatter)
  protected
    class function GetFormaterName() : string;override;
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
    function Support_ComplextType_with_SimpleContent():Boolean;override;
    function Support_nil():Boolean;override;
    class function SupportInt64() : Boolean;override;
    class function SupportUInt64() : Boolean;override;
  published
    //procedure test_WriteBuffer();
  end;

  { TTest_JsonRpcFormatterExceptionBlock }

  TTest_JsonRpcFormatterExceptionBlock = class(TTestCase)
  protected
    function CreateFormatter():IFormatterResponse;
    function CreateFormatterClient():IFormatterClient;
  published
    procedure ExceptBlock_server();
    procedure ExceptBlock_client();
  end;
  
implementation

{ TTestJsonRpcFormatter }

class function TTestJsonRpcFormatter.GetFormaterName() : string;
begin
  Result := 'json';
end;

function TTestJsonRpcFormatter.CreateFormatter(ARootType : PTypeInfo) : IFormatterBase;
begin
{$IFDEF FPC}
  Result := TJsonRpcBaseFormatter.Create();
  Result.BeginObject('root',nil);
{$ENDIF}
end;

function TTestJsonRpcFormatter.Support_ComplextType_with_SimpleContent() : Boolean;
begin
  Result := True;
end;

function TTestJsonRpcFormatter.Support_nil() : Boolean;
begin
  Result := False;
end;

class function TTestJsonRpcFormatter.SupportInt64(): Boolean;
begin
{$IFDEF WST_HAS_JSON_INT64}
  Result := True;
{$ELSE WST_HAS_JSON_INT64}
  Result := False;
{$ENDIF WST_HAS_JSON_INT64}
end;

class function TTestJsonRpcFormatter.SupportUInt64(): Boolean;
begin
  Result := False;
end;

{ TTest_JsonRpcFormatterExceptionBlock }

function TTest_JsonRpcFormatterExceptionBlock.CreateFormatter() : IFormatterResponse;
begin
  Result := server_service_json.TJsonRpcFormatter.Create() as IFormatterResponse;
end;

function TTest_JsonRpcFormatterExceptionBlock.CreateFormatterClient() : IFormatterClient;
begin
{$IFDEF FPC}
  Result := json_formatter.TJsonRpcFormatter.Create() as IFormatterClient;
{$ENDIF}
end;

procedure TTest_JsonRpcFormatterExceptionBlock.ExceptBlock_server();
const VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
var
  f : IFormatterResponse;
  strm : TMemoryStream;
  locParser : TJSONParser;
  root, errorNodeObj : TJSONObject;
  errorNode : TJSONData;
  excpt_code, excpt_msg : string;
begin
  root := nil;
  f := CreateFormatter();
  f.BeginExceptionList(VAL_CODE,VAL_MSG);
  f.EndExceptionList();
  locParser := nil;
  strm := TMemoryStream.Create();
  try
    f.SaveToStream(strm);
    strm.Position := 0;
    locParser := TJSONParser.Create(strm);
    root := locParser.Parse() as TJSONObject;
    Check(Assigned(root));
    errorNode := root.Elements[s_json_error];
    Check(Assigned(errorNode),'Error');
    Check(errorNode.JSONType() = jtObject);
    errorNodeObj := errorNode as TJSONObject;
    Check(errorNodeObj.IndexOfName(s_json_code) >= 0, s_json_code);
    Check(errorNodeObj.IndexOfName(s_json_message) >= 0, s_json_message);
    excpt_code := errorNodeObj.Elements[s_json_code].AsString;
    excpt_msg := errorNodeObj.Elements[s_json_message].AsString;
    CheckEquals(VAL_CODE,excpt_code,'faultCode');
    CheckEquals(VAL_MSG,excpt_msg,'faultString');
  finally
    locParser.Free();
    FreeAndNil(strm);
    root.Free();
  end;
end;

procedure TTest_JsonRpcFormatterExceptionBlock.ExceptBlock_client();
const
  VAL_CODE = '1210'; VAL_MSG = 'This is a sample exception message.';
  VAL_STREAM = '{ "result" : null, "error" : { "code" : ' + VAL_CODE + ', "message" : "' + VAL_MSG + '" } }';
var
  f : IFormatterClient;
  strm : TStringStream;
  excpt_code, excpt_msg : string;
begin
  excpt_code := '';
  excpt_msg := '';
  f := CreateFormatterClient();
  strm := TStringStream.Create(VAL_STREAM);
  try
    strm.Position := 0;
    f.LoadFromStream(strm);
    try
      f.BeginCallRead(nil);
      Check(False,'BeginCallRead() should raise an exception.');
    except
      on e : EJsonRpcException do begin
        excpt_code := e.FaultCode;
        excpt_msg := e.FaultString;
      end;
    end;
    CheckEquals(VAL_CODE,excpt_code,'faultCode');
    CheckEquals(VAL_MSG,excpt_msg,'faultString');
  finally
    FreeAndNil(strm);
  end;
end;


initialization
  RegisterTest('Serializer',TTestJsonRpcFormatter.Suite);
  RegisterTest('Serializer',TTest_JsonRpcFormatterExceptionBlock.Suite);

end.
