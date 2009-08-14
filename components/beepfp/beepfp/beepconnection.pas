(*******************************************************************************
 *  BeepConnection.pas: BEEP protocol connection
 *  Copyright (C) 2009,  Wimpie Nortje <wimpienortje@gmail.com>
 *
 *  This file is part of BeepFp.
 *
 *  BeepFp is free software: you can redistribute it and/or modify it under the
 *  terms of the GNU Lesser General Public License as published by the Free
 *  Software Foundation, either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  BeepFp is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with BeepFp.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  BeepFp is further covered by a special exception as described in the file
 *  COPYING.modifiedLGPL.txt which should have been included in the
 *  distribution. If not, see
 *  <http://svn.freepascal.org/svn/lazarus/trunk/COPYING.modifiedLGPL.txt>
 *******************************************************************************
 *  TBeepConnection implements the connection handling capabilities of the
 *  Vortex library
 ******************************************************************************)
unit BeepConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepUtils, BeepObject, BeepContext;

type
  { Exceptions }
  EBeepConnection  = class(EBeepError);

  {Event types }
  TBeepConnectionErrorEvent  = procedure (Sender: TObject; Reason: string) of object;

  { Enums }
  TConnectionStates = (csDisconnected, csConnecting, csConnected);

  { TBeepConnection }

  { Class: TBeepConnection
    Implements the Vortex Connection module
  }
  TBeepConnection = class(TBEEPObject)
  private
    FConnected: TConnectionStates;        //Current connection state
    FContext: TBeepContext;               //Context in which to operate
    FHost: string;
    FID: integer;
    FOnDisconnected: TNotifyEvent;
    FPort: string;
    FVortexConnection: PVortexConnection; //Vortex connection

    function GetValid: boolean;

    procedure SetCloseHandler;
    procedure StoreObjectRef;
    procedure UpdateConnectionData;

    //Event triggers
    procedure DoDisconnected;

    //Event handlers
    procedure BeepConnectionClose(aConnection:PVortexConnection);
  public
    //Events
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;

    //Properties
    property Context: TBeepContext read FContext;
    property VortexConnection: PVortexConnection read FVortexConnection;
    property Connected: TConnectionStates read FConnected;
    property Host: string read FHost;       //Return actual host (not target host)
    property Port: string read FPort;       //Return actual port (not target port)
    property ID: integer read FID;        //Vortex connection ID
    property Valid: boolean read GetValid;  //Is the connection OK?

    constructor Create(aCtx: TBeepContext);
    destructor Destroy; override;

    procedure Disconnect;
  end;

  { TBeepConnectionListener }

  TBeepConnectionListener = class(TBeepConnection)
  private
    FAutoShutdown: boolean;

    function GetVortexListener: PVortexConnection;

  public
    //Properties
    property AutoShutdown: boolean read FAutoShutdown write FAutoShutdown;  //Shutdown the Vortex connection on free'ing (for listener)
    property VortexListener: PVortexConnection read GetVortexListener;      //Listener under which connection was created

    constructor Create(aCtx: TBeepContext; aVortexConnection: PVortexConnection; aAutoShutdown: boolean = false); reintroduce;
    destructor Destroy; override;

    procedure Shutdown;
  end;

  { TBeepConnectionInitiator }

  TBeepConnectionInitiator = class(TBeepConnection)
  private
    FOnConnected: TNotifyEvent;
    FOnConnectionFailed: TBeepConnectionErrorEvent;
    FTargetHost: string;  //Host to connect to
    FTargetPort: string;  //Port to connect to


    //Event triggers
    procedure DoConnectionFailed(Reason: string);
    procedure DoConnected;

    //Event handlers
    procedure BeepConnectionNew(aConnection:PVortexConnection);
  public
    //Events
    property OnConnectionFailed: TBeepConnectionErrorEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;

    constructor Create(aCtx: TBeepContext; aHost: string; aPort: string); reintroduce;
    destructor Destroy; override;

    procedure Connect;
  end;

{ Vortex Connection API to implement:
function              vortex_connection_reconnect

function              vortex_connection_sock_connect

function              vortex_connection_do_greetings_exchange

function              vortex_connection_ref

function              vortex_connection_uncheck_ref            (connection : PVortexConnection):Taxl_bool;

procedure             vortex_connection_unref

function              vortex_connection_ref_count              (connection : PVortexConnection):longint;

function              vortex_connection_new_empty

function              vortex_connection_new_empty_from_connection

function              vortex_connection_set_socket

procedure             vortex_connection_timeout

procedure             vortex_connection_connect_timeout

function              vortex_connection_get_timeout            (ctx : PVortexCtx):int64;

function              vortex_connection_get_connect_timeout    (ctx : PVortexCtx):int64;

function              vortex_connection_get_message            (connection : PVortexConnection):Pchar;

function              vortex_connection_get_status             (connection : PVortexConnection):TVortexStatus;

function              vortex_connection_pop_channel_error

procedure             vortex_connection_push_channel_error

function              vortex_connection_get_remote_profiles    (connection : PVortexConnection):PaxlList;

function              vortex_connection_set_profile_mask

function              vortex_connection_is_profile_filtered

function              vortex_connection_is_profile_supported   (connection : PVortexConnection;

function              vortex_connection_channel_exists         (connection  : PVortexConnection;

function              vortex_connection_channels_count         (connection : PVortexConnection):longint;

function              vortex_connection_foreach_channel        (connection : PVortexConnection;

function              vortex_connection_get_next_channel       (connection : PVortexConnection):longint;

function              vortex_connection_get_channel            (connection  : PVortexConnection;

function              vortex_connection_get_channel_by_uri     (connection : PVortexConnection;

function              vortex_connection_get_channel_by_func    (connection : PVortexConnection;

function              vortex_connection_get_channel_count      (connection : PVortexConnection;

function              vortex_connection_get_socket             (connection:PVortexConnection):TVORTEX_SOCKET;

procedure             vortex_connection_set_close_socket       (connection : PVortexConnection;

procedure             vortex_connection_add_channel            (connection : PVortexConnection;

procedure             vortex_connection_remove_channel         (connection : PVortexConnection;

function              vortex_connection_get_server_name        (connection : PVortexConnection):Pchar;

procedure             vortex_connection_set_server_name        (connection : PVortexConnection;

function              vortex_connection_set_blocking_socket    (connection : PVortexConnection):Taxl_bool;

function              vortex_connection_set_nonblocking_socket (connection : PVortexConnection):Taxl_bool;

function              vortex_connection_set_sock_tcp_nodelay   (socket : TVORTEX_SOCKET;

function              vortex_connection_set_sock_block         (socket : TVORTEX_SOCKET;

procedure             vortex_connection_set_data               (connection : PVortexConnection;

procedure             vortex_connection_set_data_full          (connection    : PVortexConnection;

procedure             vortex_connection_set_connection_actions (ctx            : PVortexCtx;
\
function              vortex_connection_actions_notify         (caller_conn : PPVortexConnection;

function              vortex_connection_get_data               (connection : PVortexConnection;

function              vortex_connection_get_channels_hash      (connection : PVortexConnection):PVortexHash;

function              vortex_connection_get_channel_pool       (connection : PVortexConnection;

function              vortex_connection_get_pending_msgs       (connection : PVortexConnection):longint;

function              vortex_connection_get_role               (connection : PVortexConnection):TVortexPeerRole;

function              vortex_connection_get_features           (connection : PVortexConnection):Pchar;

function              vortex_connection_get_localize           (connection : PVortexConnection):Pchar;

function              vortex_connection_get_opened_channels    (connection : PVortexConnection):longint;

function                 vortex_connection_set_send_handler    (connection   : PVortexConnection;

function                 vortex_connection_set_receive_handler (connection      : PVortexConnection;

procedure                vortex_connection_set_default_io_handler (connection : PVortexConnection);

procedure             vortex_connection_sanity_socket_check        (ctx    : PVortexCtx;

function              vortex_connection_parse_greetings_and_enable (connection : PVortexConnection;

procedure             vortex_connection_set_preread_handler        (connection         : PVortexConnection;

function              vortex_connection_is_defined_preread_handler (connection : PVortexConnection):Taxl_bool;

procedure             vortex_connection_set_tlsfication_status     (connection : PVortexConnection;

function              vortex_connection_is_tlsficated              (connection : PVortexConnection):Taxl_bool;

procedure             vortex_connection_set_channel_added_handler  (connection    : PVortexConnection;

procedure             vortex_connection_set_channel_removed_handler  (connection      : PVortexConnection;

procedure             vortex_connection_block                        (conn   : PVortexConnection;

function              vortex_connection_is_blocked                   (conn : PVortexConnection):Taxl_bool;

function              vortex_connection_get_next_frame_size          (connection   : PVortexConnection;

procedure             vortex_connection_seq_frame_updates            (connection  : PVortexConnection;

function              vortex_connection_seq_frame_updates_status     (connection : PVortexConnection):Taxl_bool;

function                  vortex_connection_set_next_frame_size_handler (connection      : PVortexConnection;

function                  vortex_connection_set_default_next_frame_size_handler (ctx             : PVortexCtx;

DONE:
function              vortex_connection_new
function              vortex_connection_close
procedure             vortex_connection_free
function              vortex_connection_get_ctx
function              vortex_connection_get_host
function              vortex_connection_get_port
procedure             vortex_connection_set_on_close
procedure             vortex_connection_set_on_close_full
function              vortex_connection_is_ok
function              vortex_connection_get_id
function              vortex_connection_get_listener
procedure             vortex_connection_shutdown
}

{
CODE to get remote profiles
for local use vortex_profile_get_ref
  List: PAxlList;
  k: Integer;
  Profile: string;
    //Check profiles
    List := vortex_connection_get_remote_profiles(Connection);
    k := 0;
    while k < axl_list_length (list) do
    begin
      //Get next profile
      Profile := PChar(axl_list_get_nth (list, k));

      writeln(Profile);
      //Inc
      Inc(k);
    end;
}
implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//User_data points to connection's TBeepConnection object (ie owning connection)
procedure VortexConnectionNew(aConnection:PVortexConnection; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepConnectionInitiator;
begin
  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Owner := TBeepConnectionInitiator(User_data);
    Owner.AcquireLock;
    try
      Owner.BeepConnectionNew(aConnection);
    finally
      Owner.ReleaseLock;
    end;
  end;
end;

//User_data points to connection's TBeepConnection object (ie owning connection)
procedure VortexConnectionClose(Connection:PVortexConnection; Data:TaxlPointer);cdecl;
var
  Owner: TBeepConnection;
begin
  if assigned(Data) then
  begin
    Owner := TBeepConnection(Data);
    Owner.AcquireLock;
    try
      Owner.BeepConnectionClose(Connection);
    finally
      Owner.ReleaseLock;
    end;
  end;
end;

{ TBeepConnection }
procedure TBeepConnection.BeepConnectionClose(aConnection: PVortexConnection);
begin
  //Future note: This test is probably not necessary, or maybe another test
  //is necessary. There is something I don't understand yet but it seems that
  //all listeners and/or all connections in a context gets notified about all
  //(or some?) events. In any case, LazBEEP was designed to allow multiple
  //listeners in the same context but this must be reviewed. For now only a
  //single listener will be allowed in a context.
  if FConnected <> csDisconnected then
  begin
    //writeln('Beep Connection(%d): BeepConnectionClose');

    //Set state
    FConnected := csDisconnected;

    //Trigger event
    DoDisconnected;
  end;
end;

constructor TBeepConnection.Create(aCtx: TBeepContext);
begin
  //Safety checks
  if aCtx = nil then
    raise EBeepInvalidContext.Create('TBeepConnection requires a valid context.');

  inherited Create;

  FContext          := aCtx;
  FVortexConnection := nil;
  FConnected        := csDisconnected;
end;

destructor TBeepConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TBeepConnection.Disconnect;
begin
  if FConnected = csConnected then
  begin
    //Close connection
    vortex_connection_close(FVortexConnection);

    //Set state
    FConnected := csDisconnected;
    //*********^^ as disconnect refuse, is hierdie nie so nie.
    //*????stel in die handler? Hoe destroy mens dan?
  end;
end;

procedure TBeepConnection.DoDisconnected;
begin
  if assigned(FOnDisconnected) then
    FOnDisconnected(self);
end;

function TBeepConnection.GetValid: boolean;
begin
  Result := (axl_true = vortex_connection_is_ok(FVortexConnection, axl_false));
end;

procedure TBeepConnection.SetCloseHandler;
begin
  //Set the handler for connection OnClose events
  vortex_connection_set_on_close_full(FVortexConnection, @VortexConnectionClose, self);
end;

procedure TBeepConnection.StoreObjectRef;
begin
  //Store the object's referece in the Vortex object
  vortex_connection_set_data(FVortexConnection, KEY_OWNER, self);
end;

procedure TBeepConnection.UpdateConnectionData;
begin
  //Only try when connection is alive
  if Valid then
  begin
    FID   := vortex_connection_get_id(FVortexConnection);
    FHost := vortex_connection_get_host(FVortexConnection);
    FPort := vortex_connection_get_port(FVortexConnection)
  end;
end;

{ TBeepConnectionListener }

constructor TBeepConnectionListener.Create(aCtx: TBeepContext;
  aVortexConnection: PVortexConnection; aAutoShutdown: boolean);
begin
  //Safety checks
  if aVortexConnection = nil then
    raise EBeepInvalidConnection.Create('TBeepConnection requires a valid Vortex connection.');

  inherited Create(aCtx);

  //Vortex connection is already open
  FVortexConnection := aVortexConnection;

  //Assign event handler
  SetCloseHandler;

  //Store object ref
  StoreObjectRef;

  //Update all connection specific info
  UpdateConnectionData;

  //Set state
  FConnected := csConnected;

  //Init
  FAutoShutdown := aAutoShutdown; //Should Destroy shutdown the vortex connection?
end;

destructor TBeepConnectionListener.Destroy;
begin
  //Listener
  begin
    if FAutoShutdown and Valid then
      Shutdown;
  end;

  inherited Destroy;
end;

function TBeepConnectionListener.GetVortexListener: PVortexConnection;
begin
  Result := vortex_connection_get_listener(FVortexConnection);
end;

procedure TBeepConnectionListener.Shutdown;
begin
  vortex_connection_shutdown(FVortexConnection);
end;

{ TBeepConnectionInitiator }

procedure TBeepConnectionInitiator.BeepConnectionNew(
  aConnection: PVortexConnection);
var
  Error: string;
begin
  //Store connection
  FVortexConnection := aConnection;

  //Check new connection
  if not Valid then
  begin
    //Get error message
    Error := vortex_connection_get_message(FVortexConnection);

    //Set state
    FConnected := csDisconnected;

    //Trigger event
    DoConnectionFailed(Error);
  end
  else
  begin
    //Set close handler
    if assigned(FOnDisconnected) or assigned(FOnConnectionFailed) then
      SetCloseHandler;

    //Store object ref
    StoreObjectRef;

    //Update all connection specific info
    UpdateConnectionData;

    //Set state
    FConnected := csConnected;

    //Trigger event
    DoConnected;
  end;

end;

procedure TBeepConnectionInitiator.Connect;
begin
  //check for previous connection
  if FConnected = csDisconnected then
  begin
    //No event handler, blocking mode
    if FOnConnected = nil then
    begin
      //Create new connection
      FVortexConnection := vortex_connection_new(FContext.VortexCtx, PChar(FTargetHost), PChar(FTargetPort), nil, nil);

      //Check error
      if not Valid then
      begin
        raise EBeepConnection.Create('Can''t create BEEP connection. Vortex error is:'+#13#10+
          vortex_connection_get_message(FVortexConnection));
      end;

      //Set close handler
      if assigned(FOnDisconnected) or assigned(FOnConnectionFailed) then
        SetCloseHandler;

      //Store object ref
      StoreObjectRef;

      //Set state
      FConnected := csConnected;

      //Trigger event
      DoConnected;
    end

    //Event handler specified, non-blocking mode
    else
    begin
      //Create connection
      vortex_connection_new(FContext.VortexCtx, PChar(FTargetHost), PChar(FTargetPort), @VortexConnectionNew, self);

      //Set state
      FConnected := csConnecting;   //set in handler
    end;

  end;
end;

constructor TBeepConnectionInitiator.Create(aCtx: TBeepContext; aHost: string;
  aPort: string);
var
  Val: integer;
begin
  //Safety checks
  Val := StrToInt(aPort);
  if (Val < 0) or (MAX_PORT < Val) then
    raise EBeepInvalidPort.Create(aPort + ' is an invalid port number. Valid range is from 0 to 65536.');

  inherited Create(aCtx);

  FTargetHost := aHost;
  FTargetPort := aPort;
end;

destructor TBeepConnectionInitiator.Destroy;
begin
  //Close connection
  Disconnect;

  inherited Destroy;
end;

procedure TBeepConnectionInitiator.DoConnected;
begin
  if assigned(FOnConnected) then
    FOnConnected(self);
end;

procedure TBeepConnectionInitiator.DoConnectionFailed(Reason: string);
begin
  if assigned(FOnConnectionFailed) then
    FOnConnectionFailed(self, Reason);
end;

end.

