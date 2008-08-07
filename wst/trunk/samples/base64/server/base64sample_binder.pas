{
This unit has been produced by ws_helper.
  Input unit name : "base64sample".
  This unit name  : "base64sample_binder".
  Date            : "07/08/2008 13:17:40".
}
unit base64sample_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, base64sample;

type


  TSampleService_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure DuplicateContentHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TSampleService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterSampleServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TSampleService_ServiceBinder implementation }
procedure TSampleService_ServiceBinder.DuplicateContentHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : SampleService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AInitialContent : TBase64StringRemotable;
  ARepeatCount : integer;
  returnVal : TBase64StringRemotable;
begin
  callCtx := AContext;
  Fillchar(returnVal,SizeOf(TBase64StringRemotable),#0);
  Fillchar(AInitialContent,SizeOf(TBase64StringRemotable),#0);
  
  strPrmName := 'AInitialContent';  AFormatter.Get(TypeInfo(TBase64StringRemotable),strPrmName,AInitialContent);
  if Assigned(Pointer(AInitialContent)) then
    callCtx.AddObjectToFree(TObject(AInitialContent));
  strPrmName := 'ARepeatCount';  AFormatter.Get(TypeInfo(integer),strPrmName,ARepeatCount);
  
  tmpObj := Self.GetFactory().CreateInstance() as SampleService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.DuplicateContent(AInitialContent,ARepeatCount);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(TBase64StringRemotable),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TSampleService_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('SampleService'));
  RegisterVerbHandler('DuplicateContent',{$IFDEF FPC}@{$ENDIF}DuplicateContentHandler);
end;


{ TSampleService_ServiceBinderFactory }

function TSampleService_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TSampleService_ServiceBinderFactory.Create();
begin
  FInstance := TSampleService_ServiceBinder.Create() as IInterface;
end;

destructor TSampleService_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterSampleServiceService();
Begin
  GetServerServiceRegistry().Register('SampleService',TSampleService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i base64sample.wst}

  {$IF DECLARED(Register_base64sample_ServiceMetadata)}
  Register_base64sample_ServiceMetadata();
  {$IFEND}

End.
