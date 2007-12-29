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
unit server_service_json;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, server_service_intf,
  fpjson, base_json_formatter;

type

  { TJsonRpcFormatter }

  TJsonRpcFormatter = class(TJsonRpcBaseFormatter,IFormatterBase,IFormatterResponse)
  Private
    FCallProcedureName : string;
    FCallTarget : string;
    FIDObject : TJSONData;
  Protected
    procedure BeginCallResponse(Const AProcName,ATarget:string);
    procedure EndCallResponse();
    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;
    procedure BeginExceptionList(
      const AErrorCode : string;
      const AErrorMsg  : string
    );
    procedure EndExceptionList();
  public
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterJsonFormat();

implementation
uses jsonparser;

procedure Server_service_RegisterJsonFormat();
begin
  GetFormatterRegistry().Register(s_json,s_json_ContentType,TSimpleItemFactory.Create(TJsonRpcFormatter) as IItemFactory);
end;

function Clone(const AValue : TJSONData) : TJSONData;
var
  locParser : TJSONParser;
begin
  if Assigned(AValue) then begin
    case AValue.JSONType() of
      jtNumber :
        begin
          if ( TJSONNumber(AValue).NumberType() = ntInteger ) then
            Result := TJSONIntegerNumber.Create(AValue.AsInteger)
          else
            Result := TJSONFloatNumber.Create(AValue.AsFloat);
        end;
      jtString  : Result := TJSONString.Create(AValue.AsString);
      jtBoolean : Result := TJSONBoolean.Create(AValue.AsBoolean);
      jtNull    : Result := TJSONNull.Create();
      jtArray,
      jtObject  :
        begin
          locParser := TJSONParser.Create(AValue.AsJSON);
          try
            Result := locParser.Parse();
          finally
            locParser.Free();
          end;
        end;
      else
        raise Exception.Create('Invalid JSON object type.');
    end;
  end else begin
    Result := nil;
  end;
end;

{ TJsonRpcFormatter }

procedure TJsonRpcFormatter.BeginCallResponse(const AProcName, ATarget : string);
begin
  Clear();
  BeginObject('',nil);
end;

procedure TJsonRpcFormatter.EndCallResponse();
var
  locRoot : TJSONObject;
begin
    locRoot := GetRootData();
    locRoot.Elements[s_json_error] := TJSONNull.Create();
    if Assigned(FIDObject) then begin
      locRoot.Elements[s_json_id] := FIDObject;
      FIDObject := nil;
    end else begin
      locRoot.Elements[s_json_id] := TJSONNull.Create();
    end;
  EndScope();
end;

procedure TJsonRpcFormatter.BeginCallRead(ACallContext : ICallContext);
var
  nameBuffer, strBuffer : string;
  rootObj : TJSONObject;
  i : PtrInt;
begin
  ClearStack();
  FreeAndNil(FIDObject);
  rootObj := GetRootData();
  PushStack(rootObj,stObject);
  nameBuffer := s_json_method;
  Get(TypeInfo(string),nameBuffer,FCallProcedureName);
  i := rootObj.IndexOfName(s_json_id);
  if ( i > -1 ) then
    FIDObject := Clone(rootObj);
  nameBuffer := s_json_params;
  BeginArrayRead(nameBuffer,nil,asScoped,'');
end;

function TJsonRpcFormatter.GetCallProcedureName() : String;
begin
  Result := FCallProcedureName;
end;

function TJsonRpcFormatter.GetCallTarget() : String;
begin
  Result := FCallTarget;
end;

procedure TJsonRpcFormatter.BeginExceptionList(
  const AErrorCode : string;
  const AErrorMsg : string
);
var
  locRoot, locError : TJSONObject;
begin
  Clear();
  BeginObject('',nil);

    locRoot := GetRootData();
    locRoot.Elements[s_json_result] := TJSONNull.Create();
    locError := TJSONObject.Create();
    locRoot.Elements[s_json_error] := locError;
    locError.Add(s_json_name,'');
    locError.Add(s_json_code,StrToIntDef(AErrorCode,0));
    locError.Add(s_json_message,AErrorMsg);
    if Assigned(FIDObject) then begin
      locRoot.Elements[s_json_id] := FIDObject;
      FIDObject := nil;
    end else begin
      locRoot.Elements[s_json_id] := TJSONNull.Create();
    end;
end;

procedure TJsonRpcFormatter.EndExceptionList();
begin
  EndScope();
end;

destructor TJsonRpcFormatter.Destroy();
begin
  FreeAndNil(FIDObject);
  inherited Destroy();
end;

end.

