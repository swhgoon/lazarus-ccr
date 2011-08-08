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
{ $DEFINE WST_BLOCK_TYPE}
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
  base_service_intf, server_service_soap, server_service_json,
  base_binary_formatter, server_binary_formatter,
  metadata_service, metadata_service_imp, metadata_service_binder,
  server_listener ,
  fpc_tcp_server,
  //synapse_tcp_server,
  //indy_tcp_server,    
  user_service_intf, user_service_intf_binder, user_service_intf_imp , imp_helper,
  server_service_xmlrpc;

{$IFDEF WST_BLOCK_TYPE}
type
  
  { TSampleBlockHandler }

  TSampleBlockHandler = class(TInterfacedObject,IBlockHandler)
  protected  
    procedure Execute(
      const ABlockType     : LongInt; 
            ARequestBlock, 
            AResponseBlock : TStream
    );
  end; 
  
procedure TSampleBlockHandler.Execute(const ABlockType : LongInt;  
  ARequestBlock, AResponseBlock : TStream); 
var
  fs : TMemoryStream;
begin
  WriteLn('Block Type = ',ABlockType);
  fs := TMemoryStream.Create();
  try
    fs.CopyFrom(ARequestBlock,0);
    fs.SaveToFile(ExpandFileName('.'+PathDelim+'request.log'));
  finally
    fs.Free();
  end;
end;
{$ENDIF WST_BLOCK_TYPE}
  
var
  listener : TwstListener;
{$IFDEF WST_BLOCK_TYPE}  
  blockHandler : IBlockHandler;
{$ENDIF WST_BLOCK_TYPE}
begin
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();
  Server_service_RegisterJsonFormat();
  
  RegisterWSTMetadataServiceImplementationFactory();
  //Server_service_RegisterWSTMetadataServiceService();
  RegisterWSTMetadataServiceImplementationFactory();
  
  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();
  
  WriteLn(Format('WST sample TCP Server listning on "%d"',[sSERVER_PORT]));
  WriteLn('Hit <enter> to stop.');
  //listener := TwstSynapseTcpListener.Create();
  //listener := TwstIndyTcpListener.Create();
  listener := TwstFPCTcpListener.Create();
{$IFDEF WST_BLOCK_TYPE}
  blockHandler := TSampleBlockHandler.Create() as IBlockHandler;
  TwstBaseTcpListener(listener).UnknownBlockHandler := blockHandler;
  TwstBaseTcpListener(listener).Options := [tloHandleBlockType];
{$ENDIF WST_BLOCK_TYPE}
  listener.Start();
  ReadLn;
end.

