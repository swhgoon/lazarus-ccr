{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_binder".
  Date            : "12/11/2006 11:12".
}
unit metadata_service_binder;
{$INCLUDE wst.inc}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, metadata_service;

type


  TWSTMetadataService_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure GetRepositoryListHandler(AFormatter:IFormatterResponse);
    procedure GetRepositoryInfoHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TWSTMetadataService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterWSTMetadataServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TWSTMetadataService_ServiceBinder implementation }
procedure TWSTMetadataService_ServiceBinder.GetRepositoryListHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : IWSTMetadataService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  returnVal : TArrayOfStringRemotable;
Begin
  callCtx := GetCallContext();
  If ( PTypeInfo(TypeInfo(TArrayOfStringRemotable))^.Kind in [tkClass,tkInterface] ) Then
    Pointer(returnVal) := Nil;
  
  
  tmpObj := Self.GetFactory().CreateInstance() as IWSTMetadataService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.GetRepositoryList();
  If ( PTypeInfo(TypeInfo(TArrayOfStringRemotable))^.Kind = tkClass ) And Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TArrayOfStringRemotable),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TWSTMetadataService_ServiceBinder.GetRepositoryInfoHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : IWSTMetadataService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AName : string;
  returnVal : TWSTMtdRepository;
Begin
  callCtx := GetCallContext();
  Pointer(returnVal) := Nil;
  
  strPrmName := 'AName';  AFormatter.Get(TypeInfo(string),strPrmName,AName);
  
  tmpObj := Self.GetFactory().CreateInstance() as IWSTMetadataService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.GetRepositoryInfo(AName);
  If Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TWSTMtdRepository),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;


constructor TWSTMetadataService_ServiceBinder.Create();
Begin
  Inherited Create(GetServiceImplementationRegistry().FindFactory('IWSTMetadataService'));
  RegisterVerbHandler('GetRepositoryList',@GetRepositoryListHandler);
  RegisterVerbHandler('GetRepositoryInfo',@GetRepositoryInfoHandler);
End;


{ TWSTMetadataService_ServiceBinderFactory }
function TWSTMetadataService_ServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TWSTMetadataService_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterWSTMetadataServiceService();
Begin
  GetServerServiceRegistry().Register('IWSTMetadataService',TWSTMetadataService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$IF DECLARED(Register_metadata_service_NameSpace)}
  Register_metadata_service_NameSpace();
  {$ENDIF}

  {$i metadata_service.wst}

End.
