program record_server;

{$APPTYPE CONSOLE}

uses
  delphi_init_com, Classes, SysUtils, 
  indy_http_server,
  metadata_service,
  server_listener,
  server_service_soap,
  server_binary_formatter,
  server_service_xmlrpc,
  config_objects,
  record_sample,
  record_sample_binder,
  record_sample_imp,
  record_rtti;

var
  AppObject : TwstListener;
begin
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();

  RegisterRecordServiceImplementationFactory();
  Server_service_RegisterRecordServiceService();

  //wst_CreateDefaultFile(wst_GetConfigFileName(),nil);

  AppObject := TwstIndyHttpListener.Create('127.0.0.1',20000);
  try
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:20000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    AppObject.Start();
    ReadLn;
  finally
    FreeAndNil(AppObject);
  end;
end.

