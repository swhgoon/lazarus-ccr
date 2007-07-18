program tcp_server;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils, ActiveX,
  base_service_intf,
  server_service_soap,
  base_binary_formatter,
  server_binary_formatter,
  metadata_service,
  metadata_service_imp,
  metadata_service_binder,
  synapse_tcp_server,
  user_service_intf,
  user_service_intf_binder,
  user_service_intf_imp,
  imp_helper,
  server_service_xmlrpc;

{$INCLUDE wst.inc}

var
  listnerThread : TServerListnerThread;
begin
  CoInitialize(nil);
  try
    SetLogger(TConsoleLogger.Create());

    Server_service_RegisterBinaryFormat();
    Server_service_RegisterSoapFormat();
    Server_service_RegisterXmlRpcFormat();

    RegisterWSTMetadataServiceImplementationFactory();
    //Server_service_RegisterWSTMetadataServiceService();
    RegisterWSTMetadataServiceImplementationFactory();
  
    RegisterUserServiceImplementationFactory();
    Server_service_RegisterUserServiceService();
  
    Logger().Log('WST sample TCP Server listning on "%s"',[sSERVER_PORT]);
    Logger().Log('Hit <enter> to stop.');
    listnerThread := TServerListnerThread.Create();
    ReadLn;
  finally
    CoUninitialize();
  end;
end.

