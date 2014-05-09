{
This unit has been produced by ws_helper.
  Input unit name : "echo_service".
  This unit name  : "echo_service_binder".
  Date            : "06/04/2009 17:35:29".
}
unit echo_service_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, echo_service;

type


  TEchoService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure EchoWideStringHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TEchoService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterEchoServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TEchoService_ServiceBinder implementation }
procedure TEchoService_ServiceBinder.EchoWideStringHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IEchoService;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  AValue : WideString;
  returnVal : WideString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'AValue';  AFormatter.Get(TypeInfo(WideString),locStrPrmName,AValue);
  
  tmpObj := Self.GetFactory().CreateInstance() as IEchoService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.EchoWideString(AValue);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(WideString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TEchoService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('IEchoService'));
  RegisterVerbHandler('EchoWideString',{$IFDEF FPC}@{$ENDIF}EchoWideStringHandler);
end;


{ TEchoService_ServiceBinderFactory }

function TEchoService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TEchoService_ServiceBinderFactory.Create();
begin
  FInstance := TEchoService_ServiceBinder.Create() as IInterface;
end;

destructor TEchoService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterEchoServiceService();
Begin
  GetServerServiceRegistry().Register('IEchoService',TEchoService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i echo_service.wst}

  {$IF DECLARED(Register_echo_service_ServiceMetadata)}
  Register_echo_service_ServiceMetadata();
  {$IFEND}

End.
