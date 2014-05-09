{
This unit has been produced by ws_helper.
  Input unit name : "echo_service".
  This unit name  : "echo_service_imp".
  Date            : "06/04/2009 17:25:42".
}
Unit echo_service_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, echo_service;

Type


  TEchoService_ServiceImp=class(TBaseServiceImplementation,IEchoService)
  Protected
    function EchoWideString(
      const  AValue : WideString
    ):WideString;
  End;


  procedure RegisterEchoServiceImplementationFactory();

Implementation
uses config_objects;

{ TEchoService_ServiceImp implementation }
function TEchoService_ServiceImp.EchoWideString(
  const  AValue : WideString
):WideString;
Begin
  Result := AValue + AValue;
End;



procedure RegisterEchoServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('IEchoService',TImplementationFactory.Create(TEchoService_ServiceImp,wst_GetServiceConfigText('IEchoService')) as IServiceImplementationFactory);
End;

End.
