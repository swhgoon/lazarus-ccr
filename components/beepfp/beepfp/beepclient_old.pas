(*
 *  BeepClient.pas: BEEP protocol client
 *  Copyright (C) 2009,  Wimpie Nortje <wimpienortje@gmail.com>
 *
 *  This file is part of LazBEEP.
 *
 *  LazBEEP is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  LazBEEP is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with LazBEEP.  If not, see <http://www.gnu.org/licenses/>.}
 *)

unit BeepClient;

{TODO: add file desc header}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  axl, Vortex, BeepPeer, BeepProfile;

type
  { Enums }
  TConnectStates = (csDisconnected, csConnecting, csConnected);

  { TBeepClient }

  TBeepClient = class(TBeepPeer)
  private
    FConnected: TConnectStates;
    FHost: string;
    FPort: string;
    FOnConnected: TNotifyEvent;
    FOnConnectionFailed: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;

    Con : PVortexConnection;  //BEEP connection
    Channels: TStringList;  //Open channels

    procedure SetPort(const AValue: string);

    //Event triggers
    procedure DoConnectionFailed(Reason: string);
    procedure DoConnected;
    procedure DoDisconnected;

    //Event handlers
    procedure BeepConnectionNew(Connection:PVortexConnection);
  protected
    procedure BeepConnectionClose(Connection:PVortexConnection); override;
  public
    //Events
    property OnConnectionFailed: TNotifyEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;

    //Properties
    property Connected: TConnectStates read FConnected;
    property Host: string read FHost write FHost;
    property Port: string read FPort write SetPort;

    constructor Create; override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    function OpenChannel(ChanNum: longint; Profile: string):boolean;
    procedure CloseChannel(ChanNum: longint);

    function SendMSG(ChanNum: longint; Msg: PByte; MsgLen: integer; var MsgID: integer):boolean;
    function SendRPY(ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: integer):boolean;
    function SendANS(ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: integer):boolean;
    function SendANSFinal(ChanNum: longint; MsgID: longint):boolean;
    function SendERR(ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: integer):boolean;

    function CreateChannelPool:boolean;
  end;

  {usage
  - get remote profiles
  - get local profiles
  - create channel pool -> events
  - send file -> events
  - how to use documentation

  }
{ Vortex functionality to implement:
  - 1st level message frame handlers, ie Receive and close handlers passed to
    OpenChannel
  - OpenChannel -> OnCreated, OnFailed
  - Channel -> OnClosed
  - Example: Send file
  - Example: Send/Receive
}
implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//User_data points to connection's TBeepClient object (ie owning client)
procedure VortexConnectionNew(Connection:PVortexConnection; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepClient;
begin
  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Owner := TBeepClient(User_data);
    Owner.BeepConnectionNew(Connection);
  end;
end;

{ TBeepClient }

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
procedure TBeepClient.BeepConnectionClose(Connection: PVortexConnection);
begin
  inherited BeepConnectionClose(Connection);
end;

procedure TBeepClient.BeepConnectionNew(Connection: PVortexConnection);
var
  Error: string;
begin
  //Store connection
  Con := Connection;

  //Check new connection
  if (axl_false = vortex_connection_is_ok(Con, axl_false)) then
  begin
    //Get error message
    Error := vortex_connection_get_message(Con);

    //Set state
    FConnected := csDisconnected;

    //Trigger event
    DoConnectionFailed(Error);
  end
  else
  begin
    //Set close handler
    vortex_connection_set_on_close_full(Con, @VortexConnectionClose, self);

    //Set state
    FConnected := csConnected;

    //Trigger event
    DoConnected;
  end;

end;

procedure TBeepClient.CloseChannel(ChanNum: longint);
begin
  ClosePeerChannel(Con, ChanNum);
end;

constructor TBeepClient.Create;
begin
  inherited Create;

  //Init
  FConnected := csDisconnected;
  FHost := 'localhost';
  FPort := '3000';

  //Create objects
  Channels := TStringList.Create;;
end;

destructor TBeepClient.Destroy;
begin
  Disconnect;

  Channels.Free;

  inherited Destroy;
end;

procedure TBeepClient.DoConnected;
begin
  if assigned(FOnConnected) then
    FOnConnected(self);
end;

procedure TBeepClient.DoConnectionFailed(Reason: string);
begin
  if assigned(FOnConnectionFailed) then
    FOnConnectionFailed(self);
end;

procedure TBeepClient.DoDisconnected;
begin
  if assigned(FOnDisconnected) then
    FOnDisconnected(self);
end;

function TBeepClient.OpenChannel(ChanNum: longint; Profile: string):boolean;
begin
  Result := OpenPeerChannel(Con, ChanNum, Profile);
end;

function TBeepClient.SendANS(ChanNum: longint; Msg: PByte; MsgLen: integer;
  MsgID: integer):boolean;
begin
  Result := SendPeerChannelANS(Con, ChanNum, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendANSFinal(ChanNum: longint; MsgID: longint): boolean;
begin
  Result := SendPeerChannelANSFinal(Con, ChanNum, MsgID);
end;

function TBeepClient.SendERR(ChanNum: longint; Msg: PByte; MsgLen: integer;
  MsgID: integer):boolean;
begin
  Result := SendPeerChannelERR(Con, ChanNum, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendMSG(ChanNum: longint; Msg: PByte; MsgLen: integer;
  var MsgID: integer):boolean;
begin
  Result := SendPeerChannelMSG(Con, ChanNum, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendRPY(ChanNum: longint; Msg: PByte; MsgLen: integer;
  MsgID: integer):boolean;
begin
  Result := SendPeerChannelRPY(Con, ChanNum, Msg, MsgLen, MsgID);
end;

procedure TBeepClient.SetPort(const AValue: string);
var
  Val: integer;
begin
  if AValue <> FPort then
  begin
    //Check string value
    Val := StrToInt(AValue);
    if (Val < 0) or (MAX_PORT < Val) then
      raise EBeepInvalidPort.Create(AValue + ' is an invalid port number. Valid range is from 0 to 65536.');

    //Valid
    FPort := AValue;
  end;
end;

procedure TBeepClient.Connect;
begin
  if FConnected = csDisconnected then
  begin
    //Create connection
    vortex_connection_new(Ctx, PChar(FHost), PChar(FPort), @VortexConnectionNew, self);

    //Set state
    FConnected := csConnecting;
  end;
end;

procedure TBeepClient.Disconnect;
begin
  if FConnected = csConnected then
  begin
    //Close connection
    vortex_connection_close(Con);

    //Set state
    FConnected := csDisconnected;

    //Trigger event
    DoDisconnected;
  end;
end;

end.

