{
This unit has been produced by ws_helper.
  Input unit name : "googlewebapi".
  This unit name  : "googlewebapi_binder".
  Date            : "08/06/2006 23:28".
}
Unit googlewebapi_binder;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, base_service_intf, server_service_intf, googlewebapi;

Type


  TGoogleSearch_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure doSpellingSuggestionHandler(AFormatter:IFormatterResponse);
    procedure doGoogleSearchHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TGoogleSearch_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterGoogleSearchService();

Implementation
uses TypInfo;

{ TGoogleSearch_ServiceBinder implementation }
procedure TGoogleSearch_ServiceBinder.doSpellingSuggestionHandler(AFormatter:IFormatterResponse);
Var
  tmpObj : IGoogleSearch;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  key : string;
  phrase : string;
  returnVal : string;
  locTypeInfo : PTypeInfo;
Begin
  callCtx := CreateCallContext();
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  strPrmName := 'phrase';  AFormatter.Get(TypeInfo(string),strPrmName,phrase);
  
  tmpObj := Self.GetFactory().CreateInstance() as IGoogleSearch;
  
  returnVal := tmpObj.doSpellingSuggestion(key,phrase);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(string),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TGoogleSearch_ServiceBinder.doGoogleSearchHandler(AFormatter:IFormatterResponse);
Var
  tmpObj : IGoogleSearch;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  key : string;
  q : string;
  start : Integer;
  maxResults : Integer;
  filter : Boolean;
  restrict : string;
  safeSearch : Boolean;
  lr : string;
  ie : string;
  oe : string;
  returnVal : TGoogleSearchResult;
  locTypeInfo : PTypeInfo;
Begin
  callCtx := CreateCallContext();
  locTypeInfo := TypeInfo(TGoogleSearchResult);
  If ( locTypeInfo^.Kind in [tkClass,tkInterface] ) Then
    Pointer(returnVal) := Nil;
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  strPrmName := 'q';  AFormatter.Get(TypeInfo(string),strPrmName,q);
  strPrmName := 'start';  AFormatter.Get(TypeInfo(Integer),strPrmName,start);
  strPrmName := 'maxResults';  AFormatter.Get(TypeInfo(Integer),strPrmName,maxResults);
  strPrmName := 'filter';  AFormatter.Get(TypeInfo(Boolean),strPrmName,filter);
  strPrmName := 'restrict';  AFormatter.Get(TypeInfo(string),strPrmName,restrict);
  strPrmName := 'safeSearch';  AFormatter.Get(TypeInfo(Boolean),strPrmName,safeSearch);
  strPrmName := 'lr';  AFormatter.Get(TypeInfo(string),strPrmName,lr);
  strPrmName := 'ie';  AFormatter.Get(TypeInfo(string),strPrmName,ie);
  strPrmName := 'oe';  AFormatter.Get(TypeInfo(string),strPrmName,oe);
  
  tmpObj := Self.GetFactory().CreateInstance() as IGoogleSearch;
  
  returnVal := tmpObj.doGoogleSearch(key,q,start,maxResults,filter,restrict,safeSearch,lr,ie,oe);
  locTypeInfo := TypeInfo(TGoogleSearchResult);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(returnVal)) Then
    callCtx.AddObject(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TGoogleSearchResult),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;


constructor TGoogleSearch_ServiceBinder.Create();
Begin
  Inherited Create(GetServiceImplementationRegistry().FindFactory('GoogleSearch'));
  RegisterVerbHandler('doSpellingSuggestion',@doSpellingSuggestionHandler);
  RegisterVerbHandler('doGoogleSearch',@doGoogleSearchHandler);
End;


{ TGoogleSearch_ServiceBinderFactory }
function TGoogleSearch_ServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TGoogleSearch_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterGoogleSearchService();
Begin
  GetServerServiceRegistry().Register('GoogleSearch',TGoogleSearch_ServiceBinderFactory.Create() as IItemFactory);
End;

End.
