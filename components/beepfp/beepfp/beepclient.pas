(*******************************************************************************
 *  BeepClient.pas: BEEP protocol client
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
 *  TBeepClient implements a BEEP protocol client using the TBeepXXXXX classes.
 *  The client can make only 1 connection but have multiple channels to the peer.
 ******************************************************************************)
unit BeepClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  BeepUtils, BeepPeer, BeepConnection, BeepChannelPool;

type

  { TBeepClient }

  TBeepClient = class(TBeepPeer)
  private
    //FConnected: TConnectStates;
    FConnection: TBeepConnectionInitiator; //BEEP connection
    FHost: string;    //Host to connect to
    FOnConnected: TConnectionEvent;   //Connected to listener
    FOnConnectionFailed: TConnectionErrorEvent; //Could not connect to listener
    FOnDisconnected: TConnectionEvent;  //Disconnected from listener
    FPort: string;   //Port to connect to

    procedure SetPort(const AValue: string);

    //Event triggers
    procedure DoConnectionFailed(Sender: TBeepConnection; Reason: string);
    procedure DoConnected(Sender: TBeepConnection);
    procedure DoDisconnected(Sender: TBeepConnection);

    //Event handlers
    procedure ConnectionConnected(Sender: TObject);
    procedure ConnectionConnectionFailed(Sender: TObject; Reason: string);
    procedure ConnectionDisconnected(Sender: TObject);
  public
    //Events
    property OnConnectionFailed: TConnectionErrorEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnConnected: TConnectionEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TConnectionEvent read FOnDisconnected write FOnDisconnected;

    //Properties
    property Connection: TBeepConnectionInitiator read FConnection;
    property Host: string read FHost write FHost;
    property Port: string read FPort write SetPort;

    constructor Create; override;
    destructor Destroy; override;

    //Connection
    procedure Connect;
    procedure Disconnect;

    //Channels
    procedure OpenChannel(ChanNum: longint); reintroduce;
    procedure CloseChannel(ChanNum: longint); reintroduce;

    //Channel pools
    function  AddChannelPool(aProfileName: string; aChannelCount: integer; var PoolIdx: integer): TBeepChannelPool; reintroduce;

    //Messages
    function SendMSG(aNumber: integer; Msg: PByte; MsgLen: integer; var MsgID: longint): boolean; reintroduce;
    function SendRPY(aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: integer):boolean; reintroduce;
    function SendANS(aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: integer):boolean; reintroduce;
    function SendANSFinal(aNumber: integer; MsgID: longint):boolean; reintroduce;
    function SendERR(aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: integer):boolean; reintroduce;
  end;

implementation

{ TBeepClient }

function TBeepClient.AddChannelPool(aProfileName: string;
  aChannelCount: integer; var PoolIdx: integer): TBeepChannelPool;
begin
  Result := inherited AddChannelPool(Connection, aProfileName, aChannelCount, PoolIdx);
end;

procedure TBeepClient.CloseChannel(ChanNum: longint);
begin
  inherited CloseChannel(Connection, ChanNum);
end;

procedure TBeepClient.Connect;
begin

  //Create connection object
  if FConnection = nil then
  begin
    FConnection := TBeepConnectionInitiator.Create(Context, Host, Port);
    FConnection.OnConnected        := @ConnectionConnected;
    FConnection.OnConnectionFailed := @ConnectionConnectionFailed;
    FConnection.OnDisconnected     := @ConnectionDisconnected;
  end;

  //DEBUG.remove this line******
  writeln('state='+inttostr(ord(FConnection.Connected )));

  //Connect
  if FConnection.Connected = csDisconnected then
    FConnection.Connect;
end;

procedure TBeepClient.ConnectionConnected(Sender: TObject);
begin
  DoConnected(TBeepConnectionInitiator(Sender));
end;

procedure TBeepClient.ConnectionConnectionFailed(Sender: TObject;
  Reason: string);
begin
  DoConnectionFailed(TBeepConnection(Sender), Reason);
end;

procedure TBeepClient.ConnectionDisconnected(Sender: TObject);
begin
  //Disconnect;
  CloseAllChannels;

  DoDisconnected(TBeepConnection(Sender));
end;

constructor TBeepClient.Create;
begin
  inherited Create;

  //Init
  FConnection := nil;
  FHost := 'localhost';
  FPort := '3000';

  //FConnected := csDisconnected;


end;

destructor TBeepClient.Destroy;
begin
  //Gracefull disconnect, channel close
  Disconnect;

  //Free objects
  if assigned(FConnection) then
    FConnection.Free;

  inherited Destroy;
end;

procedure TBeepClient.Disconnect;
begin
  //Close channels
  CloseAllChannels;

  //Close connection
  if assigned(FConnection) then
    FConnection.Disconnect;
end;

procedure TBeepClient.DoConnected(Sender: TBeepConnection);
begin
  if assigned(FOnConnected) then
    FOnConnected(Sender);
end;

procedure TBeepClient.DoConnectionFailed(Sender: TBeepConnection;
  Reason: string);
begin
  if assigned(FOnConnectionFailed) then
    FOnConnectionFailed(Sender, Reason);
end;

procedure TBeepClient.DoDisconnected(Sender: TBeepConnection);
begin
  if assigned(FOnDisconnected) then
    FOnDisconnected(Sender);
end;

procedure TBeepClient.OpenChannel(ChanNum: longint);
begin
  inherited OpenChannel(Connection, ChanNum);
end;

function TBeepClient.SendANS(aNumber: integer; Msg: PByte; MsgLen: integer;
  MsgID: integer): boolean;
begin
  Result := inherited SendANS(Connection, aNumber, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendANSFinal(aNumber: integer; MsgID: longint): boolean;
begin
  Result := inherited SendANSFinal(Connection, aNumber, MsgID);
end;

function TBeepClient.SendERR(aNumber: integer; Msg: PByte; MsgLen: integer;
  MsgID: integer): boolean;
begin
  Result := inherited SendERR(Connection, aNumber, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendMSG(aNumber: integer; Msg: PByte; MsgLen: integer;
  var MsgID: longint): boolean;
begin
  Result := inherited SendMSG(Connection, aNumber, Msg, MsgLen, MsgID);
end;

function TBeepClient.SendRPY(aNumber: integer; Msg: PByte; MsgLen: integer;
  MsgID: integer): boolean;
begin
  Result := inherited SendRPY(Connection, aNumber, Msg, MsgLen, MsgID);
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

end.

