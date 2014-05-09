{
This unit has been produced by ws_helper.
  Input unit name : "calculator".
  This unit name  : "calculator_binder".
  Date            : "15/08/2007 16:34:20".
}
unit calculator_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, calculator;

type


  TCalculator_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure AddIntHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DivIntHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DoAllOperationsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DoOperationHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TCalculator_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterCalculatorService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TCalculator_ServiceBinder implementation }
procedure TCalculator_ServiceBinder.AddIntHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : TBinaryArgsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddInt(A,B);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TBinaryArgsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TCalculator_ServiceBinder.DivIntHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : Integer;
begin
  callCtx := AContext;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.DivInt(A,B);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(Integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TCalculator_ServiceBinder.DoAllOperationsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : TBinaryArgsResultArray;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.DoAllOperations(A,B);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TBinaryArgsResultArray),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TCalculator_ServiceBinder.DoOperationHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  AOperation : TCalc_Op;
  returnVal : TBinaryArgsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  strPrmName := 'AOperation';  AFormatter.Get(TypeInfo(TCalc_Op),strPrmName,AOperation);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.DoOperation(A,B,AOperation);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TBinaryArgsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TCalculator_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('ICalculator'));
  RegisterVerbHandler('AddInt',{$IFDEF FPC}@{$ENDIF}AddIntHandler);
  RegisterVerbHandler('DivInt',{$IFDEF FPC}@{$ENDIF}DivIntHandler);
  RegisterVerbHandler('DoAllOperations',{$IFDEF FPC}@{$ENDIF}DoAllOperationsHandler);
  RegisterVerbHandler('DoOperation',{$IFDEF FPC}@{$ENDIF}DoOperationHandler);
end;


{ TCalculator_ServiceBinderFactory }

function TCalculator_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TCalculator_ServiceBinderFactory.Create();
begin
  FInstance := TCalculator_ServiceBinder.Create() as IInterface;
end;

destructor TCalculator_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterCalculatorService();
Begin
  GetServerServiceRegistry().Register('ICalculator',TCalculator_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i calculator.wst}

  {$IF DECLARED(Register_calculator_ServiceMetadata)}
  Register_calculator_ServiceMetadata();
  {$IFEND}

End.
