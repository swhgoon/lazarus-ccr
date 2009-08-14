(*******************************************************************************
 *  BeepServer.pas: BEEP protocol server implementation
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
 *  TBeepServer implements a BEEP protocol listener using the TBeepXXXXX
 *  classes. The sever does not originate any connections. It accepts
 *  multiple concurrent connections from different peers and each connection can
 *  have multiple channels to the peer.
 ******************************************************************************)
unit BeepServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  BeepPeer, BeepListener, BeepConnection;

type
  { Event types}
  TListenerReadyEvent = procedure(aListener: TBeepListener) of object;
  TListenerFailedEvent = procedure(aListener: TBeepListener; const ErrMsg: string) of object;
  TListenerStopEvent = procedure(aListener: TBeepListener) of object;

  { TBeepServer }

  TBeepServer = class(TBeepPeer)
  private
    FListeners: TFPObjectList; //All the active listeners
    FOnConnectionAccept: TBeepAcceptConnection;
    FOnConnectionClose: TBeepCloseConnection;
    FOnListenerFailed: TListenerFailedEvent;
    FOnListenerReady: TListenerReadyEvent;
    FOnListenerStop: TListenerStopEvent;

    //Event triggers
    procedure DoConnectionAccept(aConnection: TBeepConnectionListener; var Accept: boolean);
    procedure DoConnectionClose(aConnection: TBeepConnection);
    procedure DoListenerFailed(aListener: TBeepListener; const ErrMsg: string);
    procedure DoListenerReady(aListener: TBeepListener);
    procedure DoListenerStop(aListener: TBeepListener);

    //Event handlers
    procedure ListenConnectionAccept(aConnection: TBeepConnectionListener;
      var Accept: boolean);
    procedure ListenConnectionClose(aConnection: TBeepConnection);
    procedure ListenFailed(aListener: TObject; const ErrMsg: string);
    procedure ListenReady(aListener: TObject);
    procedure ListenStop(aListener: TObject);
  public
    //Events
    property OnListenerReady: TListenerReadyEvent read FOnListenerReady write FOnListenerReady;    //Listener created and ready
    property OnListenerFailed: TListenerFailedEvent read FOnListenerFailed write FOnListenerFailed;//Listener creation failed
    property OnListenerStop: TListenerStopEvent read FOnListenerStop write FOnListenerStop;    //Listener is stopping
    property OnConnectionAccept: TBeepAcceptConnection read FOnConnectionAccept write FOnConnectionAccept;  //Connection request
    property OnConnectionClose: TBeepCloseConnection read FOnConnectionClose write FOnConnectionClose;      //Connection closed/dropped

    constructor Create; override;
    destructor Destroy; override;

    //Listeners
    procedure AddListener(aHost: string; aPort: string); //Register a Host:Port combination on which to listen for FConnections. All the FPortListeners will have all the profiles
    procedure StartAll;
    procedure StopAll;
  end;

implementation

uses
  BeepUtils;

{ TBeepServer }

procedure TBeepServer.AddListener(aHost: string; aPort: string);
var
  NewListen: TBeepListener;
begin
  {* NOTE:
   * BeepFp was designed with multiple listeners in one Server(Context) in mind.
   * There are some things I don't yet understand about Vortex operation. It seems
   * that all the Vortex Listeners in one context are informed about all the
   * events happeing on any of the listeners. This means that the listeners are
   * not indepent from each other.  Whether it is required to have
   * multiple listeners in one context must be reviewed. Maybe it is better to
   * have multiple servers, each with their one listener. In any case, a lot of
   * the work has been done, but its use are prevented by the following check.
   * The problems with the current implementation is that each BeepListener
   * object must check whether the triggered event (connection accept, close, etc)
   * does actually involve it (the beeplistener). Failing to do this results in
   * crashes because uninitialised object are passed around and used.}

  //Warn for unsupported usage
  if FListeners.Count > 0 then
    raise EBeepListener.Create('Multiple listeners in one TBeepServer is not yet supported.');

  //Safety check
  if not VortexPortOK(aPort) then
    raise EBeepInvalidPort.Create('Invalid port number. Valid range for Port is 0 to '+IntToStr(MAX_PORT));

  {TODO: Prevent duplicate entries}
  //How to check what exists

  //Create a listener
  NewListen := TBeepListener.Create(Context);
  NewListen.Host := aHost;
  NewListen.Port := aPort;
  NewListen.OnReady            := @ListenReady;
  NewListen.OnFailed           := @ListenFailed;
  NewListen.OnStop             := @ListenStop;
  NewListen.OnConnectionAccept := @ListenConnectionAccept;
  NewListen.OnConnectionClose  := @ListenConnectionClose;

  //Add to list
  FListeners.Add(NewListen);
end;

constructor TBeepServer.Create;
begin
  inherited Create;

  //Create objects
  FListeners := TFPObjectList.Create(true); //Free objects automatically
end;

destructor TBeepServer.Destroy;
begin
  FListeners.Free;

  inherited Destroy;
end;

procedure TBeepServer.DoConnectionAccept(aConnection: TBeepConnectionListener;
  var Accept: boolean);
begin
  if assigned(FOnConnectionAccept) then
    FOnConnectionAccept(aConnection, Accept);
end;

procedure TBeepServer.DoConnectionClose(aConnection: TBeepConnection);
begin
  if assigned(FOnConnectionClose) then
    FOnConnectionClose(aConnection);
end;

procedure TBeepServer.DoListenerFailed(aListener: TBeepListener;
  const ErrMsg: string);
begin
  if assigned(FOnListenerFailed) then
    FOnListenerFailed(aListener, ErrMsg);
end;

procedure TBeepServer.DoListenerReady(aListener: TBeepListener);
begin
  if assigned(FOnListenerReady) then
    FOnListenerReady(aListener);
end;

procedure TBeepServer.DoListenerStop(aListener: TBeepListener);
begin
  if assigned(FOnListenerStop) then
    FOnListenerStop(aListener);
end;

procedure TBeepServer.ListenConnectionAccept(
  aConnection: TBeepConnectionListener; var Accept: boolean);
begin
  {$IFDEF DBG_SERVER}
  writeln(Format('Beep Server(%s:%s) Connection Accept', [aConnection.Host, aConnection.Port]));
  {$ENDIF}

  //Trigger event
  DoConnectionAccept(aConnection, Accept);
end;

procedure TBeepServer.ListenConnectionClose(aConnection: TBeepConnection);
begin
  {$IFDEF DBG_SERVER}
  writeln(Format('Beep Server(%s:%s) Closing connection', [aConnection.Host, aConnection.Port]));
  {$ENDIF}

  //Trigger event
  DoConnectionClose(aConnection);
end;

procedure TBeepServer.ListenFailed(aListener: TObject; const ErrMsg: string);
begin
  DoListenerFailed( TBeepListener(aListener), ErrMsg);
end;

procedure TBeepServer.ListenReady(aListener: TObject);
begin
  DoListenerReady( TBeepListener(aListener) );
end;

procedure TBeepServer.ListenStop(aListener: TObject);
begin
  DoListenerStop( TBeepListener(aListener) );
end;

procedure TBeepServer.StartAll;
var
  k: Integer;
begin
  for k := 0 to FListeners.Count-1 do
     TBeepListener(FListeners.Items[k]).Start;
end;

procedure TBeepServer.StopAll;
var
  k: Integer;
begin
  for k := 0 to FListeners.Count-1 do
     TBeepListener(FListeners.Items[k]).Stop;
end;

end.

