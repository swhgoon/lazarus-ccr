program ws_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring,
  {$ENDIF}
  Classes, SysUtils,
  server_listener, server_service_intf, server_service_soap, indy_http_server,
  echo_service, echo_service_binder, echo_service_imp;

var
  appObj : TwstListener;
begin
  Server_service_RegisterSoapFormat();
  RegisterEchoServiceImplementationFactory();
  Server_service_RegisterEchoServiceService();
  appObj := TwstIndyHttpListener.Create();
  try
    appObj.Start();
      WriteLn('WST Echo-Service sample. Hit any <return> to stop.');
      ReadLn;
    appObj.Stop();
  finally
    appObj.Free();
  end;
end.

