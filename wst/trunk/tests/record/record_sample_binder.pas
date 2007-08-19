{
This unit has been produced by ws_helper.
  Input unit name : "record_sample".
  This unit name  : "record_sample_binder".
  Date            : "17/08/2007 19:37:26".
}
unit record_sample_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, record_sample;

type


  TRecordService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddRecHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TRecordService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterRecordServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TRecordService_ServiceBinder implementation }
procedure TRecordService_ServiceBinder.AddHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : RecordService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AValue : RecordA;
  returnVal : RecordB;
begin
  callCtx := AContext;
  
  strPrmName := 'AValue';  AFormatter.Get(TypeInfo(RecordA),strPrmName,AValue);
  
  tmpObj := Self.GetFactory().CreateInstance() as RecordService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Add(AValue);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(RecordB),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TRecordService_ServiceBinder.AddRecHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : RecordService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AA : RecordA;
  AB : RecordB;
  AC : RecordC;
  returnVal : RecordC;
begin
  callCtx := AContext;
  
  strPrmName := 'AA';  AFormatter.Get(TypeInfo(RecordA),strPrmName,AA);
  strPrmName := 'AB';  AFormatter.Get(TypeInfo(RecordB),strPrmName,AB);
  strPrmName := 'AC';  AFormatter.Get(TypeInfo(RecordC),strPrmName,AC);
  
  tmpObj := Self.GetFactory().CreateInstance() as RecordService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddRec(AA,AB,AC);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(RecordC),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TRecordService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('RecordService'));
  RegisterVerbHandler('Add',{$IFDEF FPC}@{$ENDIF}AddHandler);
  RegisterVerbHandler('AddRec',{$IFDEF FPC}@{$ENDIF}AddRecHandler);
end;


{ TRecordService_ServiceBinderFactory }

function TRecordService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TRecordService_ServiceBinderFactory.Create();
begin
  FInstance := TRecordService_ServiceBinder.Create() as IInterface;
end;

destructor TRecordService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterRecordServiceService();
Begin
  GetServerServiceRegistry().Register('RecordService',TRecordService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i record_sample.wst}

  {$IF DECLARED(Register_record_sample_ServiceMetadata)}
  Register_record_sample_ServiceMetadata();
  {$IFEND}

End.
