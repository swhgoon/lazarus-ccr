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
unit json_formatter;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, service_intf, imp_utils,
  base_json_formatter, fpjson;

type

{$M+}

  { TJsonRpcFormatter }

  TJsonRpcFormatter = class(TJsonRpcBaseFormatter,IFormatterClient)
  private
    FPropMngr : IPropertyManager;
    FCallProcedureName : string;
    FCallTarget : string;
  protected
  public
    function GetPropertyManager():IPropertyManager;

    procedure BeginCall(
      const AProcName,
            ATarget      : string;
            ACallContext : ICallContext
    );
    procedure EndCall();
    procedure BeginCallRead(ACallContext : ICallContext);

    function GetCallProcedureName():string;
    function GetCallTarget():string;
  end;
  
  { TJsonRpcCallMaker }

  TJsonRpcCallMaker = class(TSimpleFactoryItem,ICallMaker)
  Private
    FPropMngr : IPropertyManager;
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure MakeCall(
      ASerializer : IFormatterClient;
      ATransport  : ITransport
    );
  End;

  procedure RegisterJsonProtocol();
  
implementation

{ TJsonRpcFormatter }

function TJsonRpcFormatter.GetPropertyManager() : IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

procedure TJsonRpcFormatter.BeginCall(
  const AProcName, ATarget : string;
        ACallContext : ICallContext
);
begin
  FCallProcedureName := AProcName;
  FCallTarget := ATarget;

  BeginObject('',Nil);
    Put(s_json_method,TypeInfo(string),FCallProcedureName);
    BeginArray(s_json_params,Nil,nil,[0,0],asScoped);
end;

procedure TJsonRpcFormatter.EndCall();
begin
    EndScope(); // params
  EndScope();   // Root object
end;

procedure TJsonRpcFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  errCode, errMsg : string;
  e : EJsonRpcException;
  elt : TJSONData;
  remoteErr : TJSONObject;
  i : PtrInt;
begin
  ClearStack();
  PushStack(GetRootData(),stObject);
  elt := GetRootData().Elements[s_json_error];
  if Assigned(elt) and elt.InheritsFrom(TJSONObject) then begin
    remoteErr := TJSONObject(elt);
    i := remoteErr.IndexOfName(s_json_code);
    if ( i > -1 ) then
      errCode := remoteErr.Items[i].AsString
    else
      errCode := '';
    i := remoteErr.IndexOfName(s_json_message);
    if ( i > -1 ) then
      errMsg := remoteErr.Items[i].AsString
    else
      errMsg := '';
    e := EJsonRpcException.Create(errMsg);
    e.FaultCode := errCode;
    e.FaultString := errMsg;
    raise e;
  end;
end;

function TJsonRpcFormatter.GetCallProcedureName() : string;
begin
  Result := FCallProcedureName;
end;

function TJsonRpcFormatter.GetCallTarget() : string;
begin
  Result := FCallTarget;
end;

{ TJsonRpcCallMaker }

constructor TJsonRpcCallMaker.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TJsonRpcCallMaker.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TJsonRpcCallMaker.GetPropertyManager() : IPropertyManager;
begin
  Result:= FPropMngr;
end;

procedure TJsonRpcCallMaker.MakeCall(
  ASerializer : IFormatterClient;
  ATransport : ITransport
);
var
  rqt, rsps : TMemoryStream;
  propMngr : IPropertyManager;
begin
  Assert(Assigned(ASerializer));
  Assert(Assigned(ATransport));
  propMngr := ATransport.GetPropertyManager();
  propMngr.SetProperty(
    s_json_ContentType,
    s_json
  );
  propMngr.SetProperty(
    sFORMAT,
    s_json
  );
  rsps := Nil;
  rqt := TMemoryStream.Create();
  Try
    rsps := TMemoryStream.Create();
    ASerializer.SaveToStream(rqt);
    rqt.Position := 0;
    ATransport.SendAndReceive(rqt,rsps);
    rqt.Clear();
    rsps.Position := 0;
    ASerializer.Clear();
    ASerializer.LoadFromStream(rsps);
  Finally
    rsps.Free();
    rqt.Free();
  End;
end;

procedure RegisterJsonProtocol();
begin
  GetFormaterRegistry().Register(
    s_json,
    TSimpleItemFactory.Create(TJsonRpcFormatter) as IItemFactory,
    TSimpleItemFactory.Create(TJsonRpcCallMaker) as IItemFactory
  );
end;

Initialization
  RegisterJsonProtocol();
  
end.
