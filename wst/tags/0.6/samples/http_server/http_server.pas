program http_server;

{$mode objfpc}{$H+}
{$IFDEF UNIX}
  {$DEFINE UseCThreads}
  {$DEFINE USE_THREAD}
{$ENDIF}

{$IFDEF WINDOWS}
  {$DEFINE USE_THREAD}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpc_http_server, // indy_http_server,
  metadata_service, logger_extension, server_listener,
  server_service_soap, server_binary_formatter, server_service_xmlrpc, server_service_json, config_objects,
  user_service_intf, user_service_intf_binder, user_service_intf_imp, server_service_intf;


var
  AppObject : TwstListener;
begin
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();
  Server_service_RegisterJsonFormat();

  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();

  GetServiceImplementationRegistry().FindFactory('UserService').RegisterExtension(['TLoggerServiceExtension']);
  //wst_CreateDefaultFile(wst_GetConfigFileName(),nil);
  
  //AppObject := TwstIndyHttpListener.Create('');
  AppObject := TwstFPHttpListener.Create('');
  try
    {$IFDEF USE_THREAD}
    TwstFPHttpListener(AppObject).Options := [loExecuteInThread, loHandleRequestInThread];
    {$ENDIF}
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    AppObject.Start();
    ReadLn();
  finally
    FreeAndNil(AppObject);
  end;
end.
