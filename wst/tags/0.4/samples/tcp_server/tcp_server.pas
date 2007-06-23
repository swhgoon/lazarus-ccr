{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program tcp_server;

{$INCLUDE wst.inc}

uses
{$IFDEF FPC}
  {$IFDEF UNIX}
    {$DEFINE UseCThreads}
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
{$ENDIF}
  Classes, SysUtils,
  base_service_intf, server_service_soap,
  base_binary_formatter, server_binary_formatter,
  metadata_service, metadata_service_imp, metadata_service_binder,
  synapse_tcp_server,

  user_service_intf, user_service_intf_binder, user_service_intf_imp , imp_helper;


var
  listnerThread : TServerListnerThread;
begin
  SetLogger(TConsoleLogger.Create());
  
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterWSTMetadataServiceService();
  RegisterWSTMetadataServiceImplementationFactory();
  
  Server_service_RegisterUserServiceService();
  RegisterUserServiceImplementationFactory();
  
  Logger().Log('WST sample TCP Server listning on "%s"',[sSERVER_PORT]);
  Logger().Log('Hit <enter> to stop.');
  listnerThread := TServerListnerThread.Create();
  ReadLn;
end.

