{
This unit has been produced by ws_helper.
  Input unit name : "calcservice".
  This unit name  : "calcservice_binder".
  Date            : "27/08/2008 16:47:32".
}
unit calcservice_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, calcservice;

type


  TCalcService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SubstractHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TCalcService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterCalcServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TCalcService_ServiceBinder implementation }
procedure TCalcService_ServiceBinder.AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalcService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : integer;
  B : integer;
  returnVal : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalcService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Add(A,B);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TCalcService_ServiceBinder.SubstractHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalcService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : integer;
  B : integer;
  returnVal : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalcService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Substract(A,B);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TCalcService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('ICalcService'));
  RegisterVerbHandler('Add',{$IFDEF FPC}@{$ENDIF}AddHandler);
  RegisterVerbHandler('Substract',{$IFDEF FPC}@{$ENDIF}SubstractHandler);
end;


{ TCalcService_ServiceBinderFactory }

function TCalcService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TCalcService_ServiceBinderFactory.Create();
begin
  FInstance := TCalcService_ServiceBinder.Create() as IInterface;
end;

destructor TCalcService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterCalcServiceService();
Begin
  GetServerServiceRegistry().Register('ICalcService',TCalcService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i calcservice.wst}

  {$IF DECLARED(Register_calcservice_ServiceMetadata)}
  Register_calcservice_ServiceMetadata();
  {$IFEND}

End.
