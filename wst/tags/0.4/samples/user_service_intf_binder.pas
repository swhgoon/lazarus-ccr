{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_binder".
  Date            : "02/05/2007 20:07".
}
unit user_service_intf_binder;
{$mode objfpc}{$H+}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, user_service_intf;

type


  TUserService_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure GetListHandler(AFormatter:IFormatterResponse);
    procedure AddHandler(AFormatter:IFormatterResponse);
    procedure UpdateHandler(AFormatter:IFormatterResponse);
    procedure FindHandler(AFormatter:IFormatterResponse);
    procedure DeleteHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TUserService_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterUserServiceService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TUserService_ServiceBinder implementation }
procedure TUserService_ServiceBinder.GetListHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : UserService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  returnVal : TUserArray;
Begin
  callCtx := GetCallContext();
  If ( PTypeInfo(TypeInfo(TUserArray))^.Kind in [tkClass,tkInterface] ) Then
    Pointer(returnVal) := Nil;
  
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.GetList();
  If ( PTypeInfo(TypeInfo(TUserArray))^.Kind = tkClass ) And Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('result',TypeInfo(TUserArray),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TUserService_ServiceBinder.AddHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : UserService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AUser : TUser;
Begin
  callCtx := GetCallContext();
  TObject(AUser) := Nil;
  
  strPrmName := 'AUser';  AFormatter.Get(TypeInfo(TUser),strPrmName,AUser);
  If Assigned(Pointer(AUser)) Then
    callCtx.AddObjectToFree(TObject(AUser));
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  tmpObj.Add(AUser);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TUserService_ServiceBinder.UpdateHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : UserService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AUser : TUser;
Begin
  callCtx := GetCallContext();
  TObject(AUser) := Nil;
  
  strPrmName := 'AUser';  AFormatter.Get(TypeInfo(TUser),strPrmName,AUser);
  If Assigned(Pointer(AUser)) Then
    callCtx.AddObjectToFree(TObject(AUser));
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  tmpObj.Update(AUser);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TUserService_ServiceBinder.FindHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : UserService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AName : string;
  returnVal : TUser;
Begin
  callCtx := GetCallContext();
  TObject(returnVal) := Nil;
  
  strPrmName := 'AName';  AFormatter.Get(TypeInfo(string),strPrmName,AName);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.Find(AName);
  If Assigned(TObject(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('result',TypeInfo(TUser),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TUserService_ServiceBinder.DeleteHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : UserService;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  AName : string;
  returnVal : boolean;
Begin
  callCtx := GetCallContext();
  
  strPrmName := 'AName';  AFormatter.Get(TypeInfo(string),strPrmName,AName);
  
  tmpObj := Self.GetFactory().CreateInstance() as UserService;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.Delete(AName);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('result',TypeInfo(boolean),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;


constructor TUserService_ServiceBinder.Create();
Begin
  Inherited Create(GetServiceImplementationRegistry().FindFactory('UserService'));
  RegisterVerbHandler('GetList',@GetListHandler);
  RegisterVerbHandler('Add',@AddHandler);
  RegisterVerbHandler('Update',@UpdateHandler);
  RegisterVerbHandler('Find',@FindHandler);
  RegisterVerbHandler('Delete',@DeleteHandler);
End;


{ TUserService_ServiceBinderFactory }
function TUserService_ServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TUserService_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterUserServiceService();
Begin
  GetServerServiceRegistry().Register('UserService',TUserService_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$IF DECLARED(Register_user_service_intf_NameSpace)}
  Register_user_service_intf_NameSpace();
  {$ENDIF}

  {$i user_service_intf.wst}

End.
