(*******************************************************************************
 *  BeepListener.pas: Vortex BEEP listener class
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
 *  TBeepListener implements the listener handling capabilities of the Vortex
 *  library
 ******************************************************************************)
unit BeepListener;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  axl, Vortex, BeepUtils, BeepObject, BeepContext, BeepConnection;

type
  { Event types }
  TBeepListenerReady     = procedure (aListener: TObject) of object;
  TBeepListenerFailed    = procedure (aListener: TObject; const ErrMsg: string) of object;
  TBeepListenerStop      = procedure (aListener: TObject) of object;
  TBeepAcceptConnection  = procedure (aConnection: TBeepConnectionListener; var Accept: boolean) of object;
  TBeepCloseConnection   = procedure (aConnection: TBeepConnection) of object;

  { Enums }
  TListenStates = (lsStopped, lsStarting, lsActive);

  { TBeepListener }

  TBeepListener = class(TBEEPObject)
  private
    FConnections: TFPObjectList;//All the accepted connections
    FContext: TBeepContext;     //Operating context
    FHost: string;              //Host at which to listen. 0.0.0.0 = everywhere
    FOnConnectionAccept: TBeepAcceptConnection;
    FOnConnectionClose: TBeepCloseConnection;
    FOnFailed: TBeepListenerFailed;
    FOnReady: TBeepListenerReady;
    FOnStop: TBeepListenerStop;
    FPort: string;            //Port at which to lister. 0 = all ports
    FState: TListenStates;    //State of the listener
    FVortexListener: PVortexConnection; //The connection listening on
    FZombies: TFPObjectList; //List of dropped connections that need to be destroyed ASAP

    procedure SetHost(const AValue: string);
    procedure SetPort(const AValue: string);

    procedure KillZombies;
    procedure MakeZombie(aZombie: TObject);

    //Event triggers
    procedure DoConnectionAccept(aConnection: TBeepConnectionListener; var Accept: boolean);
    procedure DoConnectionClose(aConnection: TBeepConnection);
    procedure DoReady;
    procedure DoFailed(const ErrorMsg: string);
    procedure DoStop;

    //Event handlers
    procedure BeepAcceptConnection(aConnection: PVortexConnection;
      var Accept: boolean);
    procedure BeepListenerReady(aHost:Pchar; aPort:longint; aStatus:TVortexStatus;
      aMessage:Pchar; aConnection:PVortexConnection);
    procedure ConnectionClose(Sender: TObject);
  public
    //Events
    property OnConnectionAccept: TBeepAcceptConnection read FOnConnectionAccept write FOnConnectionAccept;  //Beep received a connection request. Return true to accept, false to reject See  VortexOnAcceptedConnection()
    property OnConnectionClose: TBeepCloseConnection read FOnConnectionClose write FOnConnectionClose; //Connection was closed/broken
    property OnReady: TBeepListenerReady read FOnReady write FOnReady;    //Listener created and ready
    property OnFailed: TBeepListenerFailed read FOnFailed write FOnFailed;//Listener creation failed
    property OnStop: TBeepListenerStop read FOnStop write FOnStop;        //Listener stopped

    //Properties
    property Context: TBeepContext read FContext;
    property State: TListenStates read FState;
    property Host: string read FHost write SetHost;
    property Port: string read FPort write SetPort;

    constructor Create(aCtx: TBeepContext);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    //future: Stop specified connection using (Host, Port)

    procedure Wait;
    procedure Unblock;

  end;

{ Vortex Context functions not implemented:
function        vortex_listener_sock_listen      (ctx   : PVortexCtx;

procedure       vortex_listener_accept_connection    (connection     : PVortexConnection;

function        vortex_listener_parse_conf_and_start (ctx : PVortexCtx):Taxl_bool;

procedure       vortex_listener_set_default_realm    (ctx   : PVortexCtx;

function        vortex_listener_get_default_realm    (ctx : PVortexCtx):Pchar;

DONE:
function        vortex_listener_new
function        vortex_listener_new2
function        vortex_listener_new_full
procedure       vortex_listener_shutdown
procedure       vortex_listener_set_on_connection_accepted
procedure       vortex_listener_wait
procedure       vortex_listener_unlock
}


implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//Data points to connection's TBeepListener object (ie owning listener)
function VortexAcceptedConnection(Connection:PVortexConnection; Data:TaxlPointer):Taxl_bool;cdecl;
var
  Owner: TBeepListener;
  Accept: boolean;
begin
  //Default response.
  Accept := true;

  //Execute user assigned event handler
  if assigned(Data) then
  begin
    Owner := TBeepListener(Data);
    Owner.AcquireLock;
    try
      Owner.BeepAcceptConnection(Connection, Accept);
    finally
      Owner.ReleaseLock;
    end;
  end;

  //Return axl_true to accept the connection to be created
  Result := VortexBool(Accept);
end;

//User_data points to listener's TBeepListener object (ie owning listener)
procedure VortexListenerReady(Host:Pchar; Port:longint; Status:TVortexStatus;
  aMessage:Pchar; Connection:PVortexConnection; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepListener;
begin
  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Owner := TBeepListener(User_data);
    Owner.BeepListenerReady(Host, Port, Status, aMessage, Connection);
  end;
end;

{ TBeepListener }

procedure TBeepListener.BeepAcceptConnection(aConnection: PVortexConnection;
  var Accept: boolean);
var
  NewConn: TBeepConnectionListener;
  VListen: PVortexConnection;
  LHost: string;
  LPort: string;
  KeepObject: boolean = false;
begin
  //Create the new connection
  NewConn := TBeepConnectionListener.Create(Context, aConnection);

  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) AcceptConnection(%s)', [Host, Port, IntToStr(ptrint(NewConn))]));
  {$ENDIF}

  //Get the originating listener
  VListen := NewConn.VortexListener;

  //Get listener host:port
  LHost := vortex_connection_get_host(VListen);
  LPort := vortex_connection_get_port(VListen);

  //Request is on my port
  if (FHost = LHost) and (FPort = LPort) then
  begin
    //Execute user handler
    DoConnectionAccept(NewConn, Accept);

    //Local copy of Accept because Accept may not be manipulated here
    KeepObject := Accept;
  end;

  //Store connection information
  if KeepObject then
  begin
    //Assign event handler
    NewConn.OnDisconnected  := @ConnectionClose;

    //Automatically shutdown the vortex connection
    NewConn.AutoShutdown := true;

    //Add to list of active connections
    FConnections.Add(NewConn);
  end
  else
  begin
    //The connection is not needed
    NewConn.Free;
  end;

  //Free some resources
  KillZombies;
end;

procedure TBeepListener.BeepListenerReady(aHost: Pchar; aPort: longint;
  aStatus: TVortexStatus; aMessage: Pchar; aConnection: PVortexConnection);
begin
  //Check error
  if axl_true = vortex_connection_is_ok (aConnection, axl_false) then
  begin
    //Store reference
    FVortexListener := aConnection;

    //Set socket values
    FHost := vortex_connection_get_host(FVortexListener);
    FPort := vortex_connection_get_port(FVortexListener);

    //Set state
    FState := lsActive;

    //Trigger event
    DoReady;
  end
  else
  begin
    //Set state
    FState := lsStopped;

    //Trigger event
    DoFailed(string(aMessage));
  end;
end;

procedure TBeepListener.ConnectionClose(Sender: TObject);
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) ConnectionClose', [Host, Port]));
  {$ENDIF}

  //Connection is already closed. Don't try it again
  TBeepConnectionListener(Sender).AutoShutdown := false;

  //Schedule connection for free'ing
  MakeZombie(Sender);

  //Trigger event
  DoConnectionClose(TBeepConnection(Sender));
end;

constructor TBeepListener.Create(aCtx: TBeepContext);
begin
  //Safety checks
  if aCtx = nil then
    raise EBeepInvalidConnection.Create('TBeepListener requires a valid context.');

  inherited Create;

  //Init
  FContext := aCtx;
  FVortexListener := nil;
  FState := lsStopped;
  FPort  := '3000';
  FHost  := 'localhost';

  //Create objects
  FZombies     := TFPObjectList.Create(true);   //Free objects when deleted.
  FConnections := TFPObjectList.Create(false);  //Don't free objects when deleted.

  //Configure connection notification
  vortex_listener_set_on_connection_accepted(FContext.VortexCtx, @VortexAcceptedConnection, self);
end;

destructor TBeepListener.Destroy;
begin
  //Stop listener, close connections
  Stop;

  //Free lists
  FConnections.Free;
  FZombies.Free;

  inherited Destroy;
end;

procedure TBeepListener.DoConnectionAccept(
  aConnection: TBeepConnectionListener; var Accept: boolean);
begin
  if assigned(FOnConnectionAccept) then
    FOnConnectionAccept(aConnection, Accept);

  {$IFDEF DBG_LISTEN}
  write(Format('Beep Listener(%s:%s) accept connection from %s:%s', [Host, Port, aConnection.Host, aConnection.Port]));
  if Accept then
    writeln(' (Accepted)')
  else
    writeln(' (Denied)');
  {$ENDIF}
end;

procedure TBeepListener.DoConnectionClose(aConnection: TBeepConnection);
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) closed connection from %s:%s', [Host, Port, aConnection.Host, aConnection.Port]));
  {$ENDIF}

  if assigned(FOnConnectionClose) then
    FOnConnectionClose(aConnection);
end;

procedure TBeepListener.DoFailed(const ErrorMsg: string);
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) connection failed', [Host, Port]));
  {$ENDIF}

  if assigned(FOnFailed) then
    FOnFailed(self, ErrorMsg);
end;

procedure TBeepListener.DoReady;
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) (self=%s) ready', [Host, Port, IntToStr(ptrint(self))]));
  {$ENDIF}

  if assigned(FOnReady) then
    FOnReady(self);
end;

procedure TBeepListener.DoStop;
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) stopping', [Host, Port]));
  {$ENDIF}

  if assigned(FOnStop) then
    FOnStop(self);
end;

procedure TBeepListener.KillZombies;
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) Kill Zombies', [Host, Port]));
  writeln(Format('Beep Listener(%s:%s) Connections=%s Zombies=%s', [Host, Port, IntToStr(FConnections.Count), IntToStr(FZombies.Count)]));
  if FConnections.Count > 0 then
    writeln(Format('Beep Listener(%s:%s) Connection[0]=%s', [Host, Port, IntToStr(ptrint(TBeepConnection(FConnections.Items[0]).VortexConnection))]));
  if FZombies.Count > 0 then
    writeln(Format('Beep Listener(%s:%s) Zombie    [0]=%s', [Host, Port, IntToStr(ptrint(TBeepConnection(FZombies.Items[0]).VortexConnection))]));
  {$ENDIF}

  //Free current zombies
  FZombies.Clear;
end;

procedure TBeepListener.MakeZombie(aZombie: TObject);
var
  Item: TObject;
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Beep Listener(%s:%s) MakeZombie', [Host, Port]));
  {$ENDIF}

  //Remove connection from list without freeing
  Item := FConnections.Extract(aZombie);

  //BIIIIG problem
  if Item = nil then
    raise EBeepListener.Create('TBeepListener: The dropped connection was not in the list of active connections. Please investigate.'+#13#10
      +TBeepConnection(aZombie).Port);

  //Move connection to zombie list. Can't free it now because it is the caller.
  FZombies.Add(aZombie);
end;

procedure TBeepListener.SetHost(const AValue: string);
begin
  if FState = lsStopped then
    FHost := AValue;
end;

procedure TBeepListener.SetPort(const AValue: string);
begin
  if FState = lsStopped then
  begin
    //Check validity
    if not VortexPortOK(AValue) then
      raise EBeepInvalidPort.Create(AValue + ' is an invalid port number. Valid range is from 0 to 65536.');

    //OK
    FPort := AValue;
  end;
end;

procedure TBeepListener.Start;
begin
  //Not listening already
  if FState = lsStopped then
  begin
    //Blocking mode
    if FOnReady = nil then
    begin
       //Create the listener
      FVortexListener := vortex_listener_new(FContext.VortexCtx, PChar(FHost), PChar(FPort), nil, nil);

      //Check error
    	if (axl_false = vortex_connection_is_ok (FVortexListener, axl_false)) then
      begin
        raise EBeepListener.Create('Can''t create BEEP listener. Vortex error is:'+#13#10+
          vortex_connection_get_message(FVortexListener));
      end;

      //Set state
      FState := lsActive;
    end

    //Non-blocking mode
    else
    begin
      //Start creating a listener
      vortex_listener_new_full(FContext.VortexCtx, PChar(FHost), PChar(FPort), @VortexListenerReady, self);

      //Set state
      FState := lsStarting;
    end;
  end;
end;

procedure TBeepListener.Stop;
var
  k: Integer;
begin
  {$IFDEF DBG_LISTEN}
  writeln(Format('Listener(%s:%s): Connection objects: %s',[Host, Port, IntToStr(FConnections.Count)]));
  writeln(Format('Listener(%s:%s): Zombie objects    : %s',[Host, Port, IntToStr(FZombies.Count)]));
  {$ENDIF}

  if FState = lsActive then
  begin
    //Close all open connections. This moves them to Zombies list
    for k := 0 to FConnections.Count-1 do
    begin
      TBeepConnectionListener(FConnections.Items[k]).Shutdown;
    end;

    //Clear the list
    FConnections.Clear;

    //Remove all zombies
    FZombies.Clear;

    //Trigger event
    DoStop;

    //Stop listening. Stop all active connections
    vortex_listener_shutdown(FVortexListener, axl_true);

    //Set state
    FState := lsStopped;
  end;
end;

procedure TBeepListener.Unblock;
begin
  vortex_listener_unlock(FContext.VortexCtx);
end;

procedure TBeepListener.Wait;
begin
  vortex_listener_wait(FContext.VortexCtx);
end;

end.

