{
This unit has been produced by ws_helper.
  Input unit name : "googlewebapi".
  This unit name  : "googlewebapi_stub".
  Date            : "17/05/2006 21:28".
}
Unit googlewebapi_stub;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, server_service_intf, googlewebapi;

Type


  TGoogleSearch_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure doSpellingSuggestionHandler(AFormatter:IFormatterResponse);
    procedure doGoogleSearchHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TGoogleSearchServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterGoogleSearchService();

Implementation
uses TypInfo, server_service_imputils;

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
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(returnVal) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(key) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(phrase) := Nil;
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(key)) Then
    callCtx.AddObject(TObject(key));
  strPrmName := 'phrase';  AFormatter.Get(TypeInfo(string),strPrmName,phrase);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(phrase)) Then
    callCtx.AddObject(TObject(phrase));
  
  tmpObj := Self.GetFactory().CreateInstance() as IGoogleSearch;
  
  returnVal := tmpObj.doSpellingSuggestion(key,phrase);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(returnVal)) Then
    callCtx.AddObject(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(string),returnVal);
  AFormatter.EndCallResponse();
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
  locTypeInfo := TypeInfo(TGoogleSearchResult);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(returnVal) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(key) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(q) := Nil;
  locTypeInfo := TypeInfo(Integer);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(start) := Nil;
  locTypeInfo := TypeInfo(Integer);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(maxResults) := Nil;
  locTypeInfo := TypeInfo(Boolean);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(filter) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(restrict) := Nil;
  locTypeInfo := TypeInfo(Boolean);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(safeSearch) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(lr) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(ie) := Nil;
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind in [tkClass,tkObject,tkInterface] ) Then
    Pointer(oe) := Nil;
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(key)) Then
    callCtx.AddObject(TObject(key));
  strPrmName := 'q';  AFormatter.Get(TypeInfo(string),strPrmName,q);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(q)) Then
    callCtx.AddObject(TObject(q));
  strPrmName := 'start';  AFormatter.Get(TypeInfo(Integer),strPrmName,start);
  locTypeInfo := TypeInfo(Integer);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(start)) Then
    callCtx.AddObject(TObject(start));
  strPrmName := 'maxResults';  AFormatter.Get(TypeInfo(Integer),strPrmName,maxResults);
  locTypeInfo := TypeInfo(Integer);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(maxResults)) Then
    callCtx.AddObject(TObject(maxResults));
  strPrmName := 'filter';  AFormatter.Get(TypeInfo(Boolean),strPrmName,filter);
  locTypeInfo := TypeInfo(Boolean);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(filter)) Then
    callCtx.AddObject(TObject(filter));
  strPrmName := 'restrict';  AFormatter.Get(TypeInfo(string),strPrmName,restrict);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(restrict)) Then
    callCtx.AddObject(TObject(restrict));
  strPrmName := 'safeSearch';  AFormatter.Get(TypeInfo(Boolean),strPrmName,safeSearch);
  locTypeInfo := TypeInfo(Boolean);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(safeSearch)) Then
    callCtx.AddObject(TObject(safeSearch));
  strPrmName := 'lr';  AFormatter.Get(TypeInfo(string),strPrmName,lr);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(lr)) Then
    callCtx.AddObject(TObject(lr));
  strPrmName := 'ie';  AFormatter.Get(TypeInfo(string),strPrmName,ie);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(ie)) Then
    callCtx.AddObject(TObject(ie));
  strPrmName := 'oe';  AFormatter.Get(TypeInfo(string),strPrmName,oe);
  locTypeInfo := TypeInfo(string);
  If ( locTypeInfo^.Kind = tkClass ) And Assigned(Pointer(oe)) Then
    callCtx.AddObject(TObject(oe));
  
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
End;


{ TGoogleSearchServiceBinderFactory }
function TGoogleSearchServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TGoogleSearch_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterGoogleSearchService();
Begin
  GetServerServiceRegistry().Register('GoogleSearch',TGoogleSearchServiceBinderFactory.Create() as IItemFactory);
End;
