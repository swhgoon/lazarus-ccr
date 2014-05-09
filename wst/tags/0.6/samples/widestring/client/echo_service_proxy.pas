{
This unit has been produced by ws_helper.
  Input unit name : "echo_service".
  This unit name  : "echo_service_proxy".
  Date            : "06/04/2009 17:57:59".
}

Unit echo_service_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, echo_service;

Type


  TEchoService_Proxy=class(TBaseProxy,IEchoService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function EchoWideString(
      const  AValue : WideString
    ):WideString;
  End;

  Function wst_CreateInstance_IEchoService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):IEchoService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_IEchoService(const AFormat : string; const ATransport : string; const AAddress : string):IEchoService;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(IEchoService));
  Result := TEchoService_Proxy.Create('IEchoService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(IEchoService)),ATransport + 'address=' + locAdr);
End;

{ TEchoService_Proxy implementation }

class function TEchoService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IEchoService);
end;

function TEchoService_Proxy.EchoWideString(
  const  AValue : WideString
):WideString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('EchoWideString', GetTarget(),locCallContext);
      locSerializer.Put('AValue', TypeInfo(WideString), AValue);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(WideString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i echo_service.wst}

  {$IF DECLARED(Register_echo_service_ServiceMetadata)}
  Register_echo_service_ServiceMetadata();
  {$IFEND}
End.
