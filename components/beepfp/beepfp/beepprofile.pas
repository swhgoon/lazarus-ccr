(*******************************************************************************
 *  BeepProfile.pas: Helper class for BeepPeer
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
 *  TBeepProfile implements all the BEEP profile handling capabilities of the
 *  Vortex library
 ******************************************************************************)
unit BeepProfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepUtils, BeepObject, BeepContext, BeepConnection, BeepFrame, BeepChannel;

type
  { Event types }
  TBeepStartChannelRequest  = procedure (aConnection: TBeepConnection; ChanNum: integer; var Accept: boolean) of object;
  TBeepCreatedChannel       = procedure (aChannel: TBeepChannelListener) of object;
  TBeepFrameReceived        = procedure (aConnection: TBeepConnection; aChannel: TBeepChannel; aFrame: TBeepFrame) of object;
  TBeepCloseChannelRequest  = procedure (aConnection: TBeepConnection; ChanNum: integer; var Accept: boolean) of object;
  TBeepClosingChannel       = procedure (aChannel: TBeepChannel) of object;

  { TBeepProfile }

  TBeepProfile = class(TBEEPObject)
  private
    FContext: TBeepContext;
    FName: string;
    FOnCloseChannelRequest: TBeepCloseChannelRequest;   //User event: can channel close?
    FOnClosingChannel: TBeepClosingChannel; //Owner event: closing channel. User said yes
    FOnCreatedChannel: TBeepCreatedChannel; //Owner event: creating new channel. User said yes
    FOnFrameReceived: TBeepFrameReceived; //User event: received new frame
    FOnStartChannelRequest: TBeepStartChannelRequest;   //User event: can open channel?

    procedure SetName(const AValue: string);

    //Event triggers
    procedure DoCloseChannel(aConnection: TBeepConnection; ChanNum: integer; var Accept: boolean);
    procedure DoClosingChannel(aChannel: TBeepChannel);
    procedure DoCreatedChannel(aChannel: TBeepChannelListener);
    procedure DoFrameReceived(aConnection: TBeepConnection; aChannel: TBeepChannel; aFrame: TBeepFrame);
    procedure DoStartChannel(aConnection: TBeepConnection; ChanNum: integer; var Accept: boolean);

    //Event handlers
    procedure BeepCloseChannel(ChanNum: integer; Connection: PVortexConnection; var Accept: boolean);
    procedure BeepFrameReceived(Channel: PVortexChannel; Connection: PVortexConnection; Frame: PVortexFrame);
    procedure BeepStartChannel(ChanNum: integer; Connection: PVortexConnection; var Accept: boolean);
  public
    //Events
    property OnCloseChannelRequest: TBeepCloseChannelRequest read FOnCloseChannelRequest write FOnCloseChannelRequest;
    property OnFrameReceived: TBeepFrameReceived read FOnFrameReceived write FOnFrameReceived;
    property OnStartChannelRequest: TBeepStartChannelRequest read FOnStartChannelRequest write FOnStartChannelRequest;

    //Properties
    property Context: TBeepContext read FContext;
    property Name: string read FName write SetName;

    constructor Create(aCtx: TBeepContext; aName: string; OnCreatedChannel: TBeepCreatedChannel; OnClosingChannel: TBeepClosingChannel);
    destructor Destroy; override;

    procedure RegisterProfile;
    procedure UnRegisterProfile;
  end;

{ Vortex funtions to implement

function vortex_profiles_set_mime_type

function         vortex_profiles_get_mime_type

function         vortex_profiles_get_transfer_encoding ar;

function  vortex_profiles_register_extended_start ;

function    vortex_profiles_is_defined_start

function    vortex_profiles_is_defined_close

function    vortex_profiles_is_defined_received

function    vortex_profiles_get_actual_list       (ctx : PVortexCtx):PaxlList;

function    vortex_profiles_get_actual_list_ref   (ctx : PVortexCtx):PaxlList;

function    vortex_profiles_registered            (ctx : PVortexCtx):longint;

function vortex_profiles_is_registered

procedure   vortex_profiles_set_automatic_mime

function    vortex_profiles_get_automatic_mime

DONE:
function vortex_profiles_register
function vortex_profiles_unregister

}

implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//User_data points to profile's TBeepProfile object (ie owning profile)
function VortexCloseChannelRequest(Channel_num: longint; Connection: PVortexConnection;
  User_data: TaxlPointer): longint; cdecl;
var
  Prof: TBeepProfile;
  Accept: boolean;
begin
  //Default response.
  Accept := true;

  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Prof := TBeepProfile(User_data);
    Prof.AcquireLock;
    try
      Prof.BeepCloseChannel(Channel_num, Connection, Accept);
    finally
      Prof.ReleaseLock;
    end;
  end;

	//Return axl_false to deny channel closure
  //Return axl_true to allow new channel closure
  Result :=  VortexBool(Accept);
end;

//User_data points to profile's TBeepProfile object (ie owning profile)
procedure VortexFrameReceived (Channel: PVortexChannel; Connection: PVortexConnection;
  Frame: PVortexFrame; User_data: TaxlPointer); cdecl;
var
  Prof: TBeepProfile;
begin
  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Prof := TBeepProfile(User_data);
    Prof.AcquireLock;
    try
      Prof.BeepFrameReceived(Channel, Connection, Frame);
    finally
      Prof.ReleaseLock;
    end;
  end;
end;

//User_data points to profile's TBeepProfile object (ie owning profile)
function VortexStartChannel (Channel_num: longint; Connection: PVortexConnection;
  User_data: TaxlPointer):longint;cdecl;
var
  Prof: TBeepProfile;
  Accept: boolean;
begin
  //Default response.
  Accept := true;

  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Prof := TBeepProfile(user_data);
    Prof.AcquireLock;
    try
      Prof.BeepStartChannel(Channel_num, Connection, Accept);
    finally
      Prof.ReleaseLock;
    end;
  end;

	//Return axl_false to deny channel creation
  //Return axl_true to allow new channel creation
  Result :=  VortexBool(Accept);
end;

{ TBeepProfile }

procedure TBeepProfile.BeepCloseChannel(ChanNum: integer;
  Connection: PVortexConnection; var Accept: boolean);
var
  BeepCon: TBeepConnection;
  VChan: PVortexChannel;
  BeepChan: TBeepChannel;
begin
  //Retrieve the associated objects
  BeepCon := TBeepConnection(vortex_connection_get_data(Connection, KEY_OWNER));

  //Trigger event
  DoCloseChannel(BeepCon, ChanNum, Accept);

  //Inform owner about the closing
  if Accept then
  begin
    //Fetch the closing channel
    VChan := vortex_connection_get_channel(Connection, ChanNum);

    //Fetch the channel object
    BeepChan := TBeepChannel(vortex_channel_get_data(VChan, KEY_OWNER));

    //Trigger event
    DoClosingChannel(BeepChan);
  end;
end;

procedure TBeepProfile.BeepFrameReceived(Channel: PVortexChannel;
  Connection: PVortexConnection; Frame: PVortexFrame);
var
  BeepCon: TBeepConnection;
  BeepChan: TBeepChannel;
  BeepFrame: TBeepFrame;
begin
  //Retrieve the associated objects
  BeepCon   := TBeepConnection(vortex_connection_get_data(Connection, KEY_OWNER));
  BeepChan  := TBeepChannel   (vortex_channel_get_data   (Channel   , KEY_OWNER));

  //Create temporary object
  BeepFrame := TBeepFrame.Create(Frame);

  //Trigger event
  DoFrameReceived(BeepCon, BeepChan, BeepFrame);

  //Free object
  BeepFrame.Free;
end;

procedure TBeepProfile.BeepStartChannel(ChanNum: integer;
  Connection: PVortexConnection; var Accept: boolean);
var
  BeepCon: TBeepConnection;
  VChan: PVortexChannel;
  BeepChan: TBeepChannelListener;
begin
  //Retrieve the associated objects
  BeepCon := TBeepConnection(vortex_connection_get_data(Connection, KEY_OWNER));

  //Trigger event
  DoStartChannel(BeepCon, ChanNum, Accept);

  //Inform the owner about the new channel
  if Accept then
  begin
    //Fetch the semi existent channel
    VChan := vortex_connection_get_channel(Connection, ChanNum);

    //Create a temp channel object
    //BeepChan := TBeepChannelListener.Create(BeepCon, VChan, ChanNum);
    BeepChan := TBeepChannelListener.Create(BeepCon, VChan);

    //Trigger event
    DoCreatedChannel(BeepChan);

    //Free the temp object
    BeepChan.Free;
  end;
end;

constructor TBeepProfile.Create(aCtx: TBeepContext; aName: string;
  OnCreatedChannel: TBeepCreatedChannel; OnClosingChannel: TBeepClosingChannel);
begin
  //Safety checks
  if aCtx = nil then
    raise EBeepInvalidContext.Create('TBeepProfile requires a valid context.');

  if aName = EmptyStr then
    ; //raise
    {TODO: exception}

  inherited Create;

  FContext := aCtx;
  FName    := aName;
  FOnCreatedChannel := OnCreatedChannel; //Private event to owner
  FOnClosingChannel := OnClosingChannel; //Private event to owner
end;

destructor TBeepProfile.Destroy;
begin
  inherited Destroy;
end;

procedure TBeepProfile.DoCloseChannel(aConnection: TBeepConnection;
  ChanNum: integer; var Accept: boolean);
begin
  if assigned(FOnCloseChannelRequest) then
    FOnCloseChannelRequest(aConnection, ChanNum, Accept);
end;

procedure TBeepProfile.DoClosingChannel(aChannel: TBeepChannel);
begin
  if assigned(FOnClosingChannel) then
    FOnClosingChannel(aChannel);
end;

procedure TBeepProfile.DoCreatedChannel(aChannel: TBeepChannelListener);
begin
  if assigned(FOnCreatedChannel) then
    FOnCreatedChannel(aChannel);
end;

procedure TBeepProfile.DoFrameReceived(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
begin
  if assigned(FOnFrameReceived) then
    FOnFrameReceived(aConnection, aChannel, aFrame);
end;

procedure TBeepProfile.DoStartChannel(aConnection: TBeepConnection;
  ChanNum: integer; var Accept: boolean);
begin
  if assigned(FOnStartChannelRequest) then
    FOnStartChannelRequest(aConnection, ChanNum, Accept);
end;

procedure TBeepProfile.RegisterProfile;
begin
  //Only if not registered yet
  if axl_false = vortex_profiles_is_registered(Context.VortexCtx, PChar(Name)) then
    //Register
    if axl_false = vortex_profiles_register(Context.VortexCtx, PChar(Name),
                                            @VortexStartChannel  , self,
                                            @VortexCloseChannelRequest, self,
                                            @VortexFrameReceived , self) then
    raise EProfileNotRegistered.Create('Profile could not be registered:#13#10'+Name);
end;

procedure TBeepProfile.SetName(const AValue: string);
begin
  if FName = AValue then exit;
  FName := AValue;
end;

procedure TBeepProfile.UnRegisterProfile;
begin
  //Unregister
  vortex_profiles_unregister(Context.VortexCtx, PChar(Name));
end;

end.

