(*
 *  BeepPeer.pas: BEEP protocol peer base class
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

unit BeepPeer;

{TODO: add file desc header}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepUtils, BeepProfile, BeepProfileBase;

type
  { Event types}
  TVortexConnClose = procedure (Connection: PVortexConnection) of object;

  { Exceptions }
  EBeepInvalidPort = class(EBeepError);

  { TBeepPeer }

  TBeepPeer = class(TObject)
  private //Managed
    FOnConnectionClose: TVortexConnClose;

    procedure DoConnectionClose(Connection: PVortexConnection);
  protected
    Ctx: PVortexCtx;  //Vortex Context
    Profiles: TStringList;  //All profiles active in context

    function OpenPeerChannel(Connection: PVortexConnection; ChanNum: longint; Profile: string):boolean;
    function ClosePeerChannel(Connection: PVortexConnection; ChanNum: longint): boolean;

    function SendPeerChannelMSG(Connection: PVortexConnection; ChanNum: longint; Msg: PByte; MsgLen: integer; var MsgID: longint): boolean;
    function SendPeerChannelRPY(Connection: PVortexConnection; ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendPeerChannelANS(Connection: PVortexConnection; ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint):boolean;
    function SendPeerChannelANSFinal(Connection: PVortexConnection; ChanNum: longint; MsgID: longint):boolean;
    function SendPeerChannelERR(Connection: PVortexConnection; ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;

    function CreatePeerChannelPool(Connection: PVortexConnection; ChanNum: longint; Profile: string):boolean;

    //Event handlers
    procedure BeepConnectionClose(Connection:PVortexConnection); virtual;

  public
    //Events
    property OnConnectionClose: TVortexConnClose read FOnConnectionClose write FOnConnectionClose; //Listener or client are closing the connection. See   VortexConnectionOnClose

    constructor Create; virtual;
    destructor Destroy; override;



    //Register a profile in the context. See vortex_profiles_register. All profiles are visible/usable by all listeners and all channels in the context
    function AddProfile(Name: string): TBeepProfile;
    procedure DeleteProfile(Name: string);

  end;

{NOTE: this is Vortex Context module. Do all the main funcionts and context functions in here}

const
  MAX_PORT = 65536; //Highest port number allowed

{ Vortex callbacks }

// Connection
procedure VortexConnectionClose(Connection:PVortexConnection; Data:TaxlPointer);cdecl;


implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//Data points to connection's TBeepListener object (ie owning listener)
procedure VortexConnectionClose(Connection:PVortexConnection; Data:TaxlPointer);cdecl;
var
  Owner: TBeepPeer;
begin
  if assigned(Data) then
  begin
    Owner := TBeepPeer(Data);
    Owner.BeepConnectionClose(Connection);
  end;
end;

{ TBeepPeer }

function TBeepPeer.AddProfile(Name: string): TBeepProfile;
var
  Prof: TBeepProfile;
  Index: integer;
begin
  //Check for duplicates
  Index := Profiles.IndexOfName(Name);

  //If not exist
  if Index = -1 then
  begin
    //Create a new profile
    Prof := TBeepProfile.Create(Name);

    //Add new profile
    Profiles.AddObject(Name, Prof);

    //Register profile
    vortex_profiles_register(Ctx                 , PChar(Prof.Name),
                             @VortexStartChannel , TaxlPointer(Prof),
                             @VortexCloseChannel , TaxlPointer(Prof),
                             @VortexFrameReceived, TaxlPointer(Prof));

    //Return a reference
    Result := Prof;
  end
  else  //Exists
  begin
    //Return existing profile
    Result := TBeepProfile(Profiles.Objects[Index]);
  end;
end;

procedure TBeepPeer.BeepConnectionClose(Connection: PVortexConnection);
begin
  //Trigger event
  DoConnectionClose(Connection);
end;

function TBeepPeer.ClosePeerChannel(Connection: PVortexConnection; ChanNum: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default result
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
    //Close channel
    Result := (axl_true = vortex_channel_close(Channel, nil));
end;

constructor TBeepPeer.Create;
var
  VResult : Taxl_bool;
begin
  //Create objects
  Profiles := TStringList.Create;

  //Create the context
	Ctx := vortex_ctx_new ();

	//Init vortex library
  VResult := vortex_init_ctx (Ctx);

  //Init error
  if axl_false = VResult then
    raise EBeepContext.Create('Could not initialise BEEP context. Network communication is not functional');
end;

procedure TBeepPeer.DeleteProfile(Name: string);
begin
  //if registered, unregister
  //if in list, remove, free
  {TODO: unregister profile}
end;

destructor TBeepPeer.Destroy;
var
  k: Integer;
begin
  //Free the objects
  if assigned(Profiles) then
    for k := 0 to Profiles.Count-1 do
      Profiles.Objects[k].Free;
  Profiles.Free;

  //End vortex function, stop all threads
  if assigned(Ctx) then
	  vortex_exit_ctx (Ctx, axl_true);

  inherited Destroy;
end;

procedure TBeepPeer.DoConnectionClose(Connection: PVortexConnection);
begin
  if assigned(FOnConnectionClose) then
    FOnConnectionClose(Connection);
end;

function TBeepPeer.OpenPeerChannel(Connection: PVortexConnection; ChanNum: longint; Profile: string): boolean;
begin
  //Default = success
  Result := false;

  //Check profile support
  if axl_true = vortex_connection_is_profile_supported(Connection, PChar(Profile)) then
  begin
    //Open channel
    Result := (nil <> vortex_channel_new(Connection, ChanNum, PChar(Profile), nil, nil, nil, nil, nil, nil) );
    {TODO: Add 1st level profile handlers}
    {TODO: Give possibilty to have OnCreated handler to prevent blocking}
  end

end;

function TBeepPeer.SendPeerChannelANS(Connection: PVortexConnection;
  ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
  begin
    //Queue the message
    Result := (axl_true = vortex_channel_send_ans_rpy(Channel, Msg, MsgLen, MsgID));
  end;
end;

function TBeepPeer.SendPeerChannelANSFinal(Connection: PVortexConnection;
  ChanNum: longint; MsgID: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
  begin
    //Queue the message
    Result := (axl_true = vortex_channel_finalize_ans_rpy(Channel, MsgID));
  end;
end;

function TBeepPeer.SendPeerChannelERR(Connection: PVortexConnection;
  ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
  begin
    //Queue the message
    Result := (axl_true = vortex_channel_send_err(Channel, Msg, MsgLen, MsgID));
  end;
end;

function TBeepPeer.SendPeerChannelMSG(Connection: PVortexConnection;
  ChanNum: longint; Msg: PByte; MsgLen: integer; var MsgID: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
  begin
    //Queue the message
    Result := (axl_true = vortex_channel_send_msg(Channel, Msg, MsgLen, MsgID));
  end;
end;

function TBeepPeer.SendPeerChannelRPY(Connection: PVortexConnection;
  ChanNum: longint; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Channel: PVortexChannel;
begin
  //Default
  Result := false;

  //Get channel object
  Channel := vortex_connection_get_channel(Connection, ChanNum);

  if Channel <> nil then
  begin
    //Queue the message
    Result := (axl_true = vortex_channel_send_rpy(Channel, Msg, MsgLen, MsgID));
  end;
end;

end.

