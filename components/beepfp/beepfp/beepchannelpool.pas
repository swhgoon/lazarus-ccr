(*******************************************************************************
 *  BeepChannelPool.pas: BEEP protocol channel pool
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
 *  TBeepChannelPool implements the channel pool handling capabilities of the
 *  Vortex library
 ******************************************************************************)
unit BeepChannelPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  axl, Vortex, BeepUtils, BeepObject, BeepConnection, BeepChannel;

type
  { TBeepChannelPool }

  TBeepChannelPool = class(TBeepObject)
  private
    FConnection: TBeepConnection;   //Connection in which this pool exists
    FTargetCount: integer;          //Number of channels to create in pool
    FTargetProfile: string;         //Profile for all channels in pool
    FVortexPool: PVortexChannelPool;//Vortex channel pool
    FPool: TFPObjectList;           //All channels in pool. Match 1:1 with vortex channels
    FOnPoolCreated: TNotifyEvent;

    function GetCount: integer;
    function GetID: integer;
    function FindBeepChannel(VChan: PVortexChannel):TBeepChannelListener;
    procedure FreeBeepChannel(VChan: PVortexChannel);

    //Event triggers
    procedure DoPoolCreated;

    //Event handlers
    procedure BeepCloseChannel(aChanNum: integer; aConnection: PVortexConnection; var Accept: boolean);
    procedure BeepChannelDropped(Channel:PVortexChannel);
    function BeepChannelPoolCreate(aConnection:PVortexConnection;
      channel_num: longint; profile: PChar; on_close: TVortexOnCloseChannel;
      on_close_user_data: TaxlPointer; on_received: TVortexOnFrameReceived;
      on_received_user_data:TaxlPointer): PVortexChannel;
    procedure BeepChannelPoolCreated(aPool:PVortexChannelPool);
  public
    //Events
    property OnPoolCreated: TNotifyEvent read FOnPoolCreated write FOnPoolCreated;

    //Properties
    property Connection: TBeepConnection read FConnection;  //Connection in which pool exists
    property VortexPool: PVortexChannelPool read FVortexPool;
    property Profile: string read FTargetProfile; //Profile for all channels in pool
    property Count: integer read GetCount;        //Actual number of channels in pool
    property ID: integer read GetID;              //Pool's unique ID

    constructor Create(aConnection: TBeepConnection; aProfileName: string; ChanCount: longint);
    destructor Destroy; override;

    procedure Initialise;   //Start up the pool's channels

    procedure AddChannels(aCount: integer); //Add more channels to the pool
    procedure RemoveChannels(aCount: integer); //Close some of the pool's channels

    function  GetNextReady(AutoInc: boolean = true):TBeepChannel; //Get a channel to use
    procedure ReleaseChannel(aChannel: TBeepChannel); //Return the channel back to the pool

    //attach
    //deattach
  end;

{Vortex functions to implement
procedure             vortex_channel_pool_attach

procedure             vortex_channel_pool_deattach

DONE
function              vortex_channel_pool_new
function              vortex_channel_pool_new_full
function              vortex_channel_pool_get_num
function              vortex_channel_pool_get_id
function              vortex_channel_pool_get_connection
procedure             vortex_channel_pool_add
procedure             vortex_channel_pool_add_full
procedure             vortex_channel_pool_remove
function              vortex_channel_pool_get_next_ready
function              vortex_channel_pool_get_next_ready_full
procedure             vortex_channel_pool_release_channel
}

implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//Request to close channel.
//User_data points to channel pool's TBeepChannelPool object (ie owning pool)
function VortexCloseChannelRequest(Channel_num: longint; Connection: PVortexConnection;
  User_data: TaxlPointer): longint; cdecl;
var
  Pool: TBeepChannelPool;
  Accept: boolean;
begin
  //Default response.
  Accept := true;

  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Pool := TBeepChannelPool(User_data);
    Pool.AcquireLock;
    try
      Pool.BeepCloseChannel(Channel_num, Connection, Accept);
    finally
      Pool.ReleaseLock;
    end;
  end;

	//Return axl_false to deny channel closure
  //Return axl_true to allow new channel closure
  Result :=  VortexBool(Accept);
end;

//Channel improperly closed, ie connection dropped
//User_data points to channel's TBeepChannelPool object (ie owning pool)
procedure VortexChannelClosed(Channel:PVortexChannel; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepChannelPool;
begin
  if assigned(User_data) then
  begin
    Owner := TBeepChannelPool(User_data);
    Owner.AcquireLock;
    try
      Owner.BeepChannelDropped(Channel);
    finally
      Owner.ReleaseLock;
    end;
  end;
end;

//Create a new channel for the channel pool
//User_data points to channel pool's TBeepChannelPool object (ie owning pool)
function VortexChannelPoolCreate(connection:PVortexConnection;
    channel_num: longint; profile: PChar; on_close: TVortexOnCloseChannel;
    on_close_user_data: TaxlPointer; on_received: TVortexOnFrameReceived;
    on_received_user_data:TaxlPointer; User_data: TaxlPointer
    ): PVortexChannel;cdecl;
var
  Pool: TBeepChannelPool;
begin
  if assigned(User_data) then
  begin
    Pool := TBeepChannelPool(User_data);
    Pool.AcquireLock;
    try
      try
        Result := Pool.BeepChannelPoolCreate(connection, channel_num, profile,
                      on_close, on_close_user_data, on_received,
                      on_received_user_data);
      except
        Result := nil;
      end;
    finally
      Pool.ReleaseLock;
    end;
  end;
end;

//Creation of channel pool is completed
//User_data points to channel pool's TBeepChannelPool object (ie owning pool)
procedure VortexChannelPoolCreated(aPool:PVortexChannelPool; User_data:TaxlPointer);cdecl;
var
  Pool: TBeepChannelPool;
begin
  if assigned(User_data) then
  begin
    Pool := TBeepChannelPool(User_data);
    Pool.AcquireLock;
    try
      Pool.BeepChannelPoolCreated(aPool);
    finally
      Pool.ReleaseLock;
    end;
  end;
end;

{ TBeepChannelPool }

procedure TBeepChannelPool.AddChannels(aCount: integer);
begin
  //Add more channels
  if aCount > 0 then
    vortex_channel_pool_add_full(FVortexPool, aCount, self);
end;

procedure TBeepChannelPool.BeepChannelDropped(Channel: PVortexChannel);
begin
  //Remove the beep channel from the list
  FreeBeepChannel(Channel);
end;

function TBeepChannelPool.BeepChannelPoolCreate(aConnection: PVortexConnection;
  channel_num: longint; profile: PChar; on_close: TVortexOnCloseChannel;
  on_close_user_data: TaxlPointer; on_received: TVortexOnFrameReceived;
  on_received_user_data: TaxlPointer): PVortexChannel;
var
  VChan: PVortexChannel;
  BeepChan: TBeepChannelListener;
begin
  //Create the new Vortex channel
  VChan := vortex_channel_new(aConnection, channel_num, profile, on_close,
              on_close_user_data, on_received, on_received_user_data, nil, nil);

  //Create the Beep channel objects
  BeepChan := TBeepChannelListener.Create(Connection, VChan);

  //Set event handler.
  //NOTE: this is a hack this class should not use the vortex_channel_xxxxx
  //functions directly. The TBeepChannel class must provide this functionality
  vortex_channel_set_closed_handler(VChan, @VortexChannelClosed, self);

  //Add new object to channel list
  FPool.Add(BeepChan);

  //Return new channel
  Result := VChan;
end;

procedure TBeepChannelPool.BeepChannelPoolCreated(aPool: PVortexChannelPool);
begin
  //Store reference
  FVortexPool := aPool;

  //Trigger event
  DoPoolCreated;
end;

procedure TBeepChannelPool.BeepCloseChannel(aChanNum: integer;
  aConnection: PVortexConnection; var Accept: boolean);
var
  VChan: PVortexChannel;
begin
  //Fetch the vortex channel
  VChan := vortex_connection_get_channel(aConnection, aChanNum);

  //Remove the beep channel from the list
  FreeBeepChannel(VChan);
end;

constructor TBeepChannelPool.Create(aConnection: TBeepConnection;
  aProfileName: string; ChanCount: longint);
begin
  //Safety checks
  if aConnection = nil then
    raise EBeepInvalidConnection.Create('A valid BeepConnection must be supplied to create a channel pool.');

  if aProfileName = EmptyStr then
    raise EBeepInvalidProfile.Create('A valid Profile name must be supplied to create a channel pool');

  inherited Create;

  //Init
  FConnection    := aConnection;
  FTargetProfile := aProfileName;
  FTargetCount   := ChanCount;

  //Create objects
  FPool := TFPObjectList.Create(true);
end;

destructor TBeepChannelPool.Destroy;
begin
  FPool.Free;

  inherited Destroy;
end;

procedure TBeepChannelPool.DoPoolCreated;
begin
  if assigned(FOnPoolCreated) then
    FOnPoolCreated(self);
end;

function TBeepChannelPool.FindBeepChannel(
  VChan: PVortexChannel): TBeepChannelListener;
var
  k: integer = 0;
  Found : boolean = false;
  PoolChan: PVortexChannel;
begin
  Result := nil;

  while (k < FPool.Count) and (not Found) do
  begin
    //Get vortex channel from list
    PoolChan := TBeepChannelListener(FPool.Items[k]).VortexChannel;

    //Compare
    if axl_true = vortex_channel_are_equal(VChan, PoolChan) then
    begin
      Found := true;

      Result := TBeepChannelListener(FPool.Items[k]);
    end;

    //Inc
    Inc(k);
  end;
end;

procedure TBeepChannelPool.FreeBeepChannel(VChan: PVortexChannel);
var
  BeepChan: TBeepChannel;
  Index: integer;
begin
  //Find matching channel in list
  BeepChan := FindBeepChannel(VChan);

  //Remove the channel from the list. Free it
  Index := FPool.Remove(BeepChan);

  writeln('Removed item '+inttostr(Index));
  //////////////

end;

function TBeepChannelPool.GetCount: integer;
begin
  Result := vortex_channel_pool_get_num(FVortexPool);
end;

function TBeepChannelPool.GetID: integer;
begin
  Result := vortex_channel_pool_get_id(FVortexPool);
end;

function TBeepChannelPool.GetNextReady(AutoInc: boolean): TBeepChannel;
var
  VChan: PVortexChannel;
begin
  //Fetch the channel from the pool
  VChan := vortex_channel_pool_get_next_ready_full(FVortexPool, VortexBool(AutoInc), self);

  //Find matching channel in list
  Result := FindBeepChannel(VChan);
end;

procedure TBeepChannelPool.Initialise;
var
  HandleClose: TVortexOnCloseChannel = nil;
  HandleCreated: TVortexOnChannelPoolCreated = nil;
begin
  //Blocking mode?
  if assigned(FOnPoolCreated) then
    HandleCreated := @VortexChannelPoolCreated;

  //Create the pool
  vortex_channel_pool_new_full(Connection.VortexConnection,
    PChar(FTargetProfile), FTargetCount,
    @VortexChannelPoolCreate, self,
    HandleClose             , self,
    nil                     , self, {<--- OnFrameRcd 2nd level callbakc}
    HandleCreated           , self);
end;

procedure TBeepChannelPool.ReleaseChannel(aChannel: TBeepChannel);
begin
  //Some safety
  if aChannel <> nil then
  begin
    //Release channel back to pool
    vortex_channel_pool_release_channel(FVortexPool, aChannel.VortexChannel);
  end;
end;

procedure TBeepChannelPool.RemoveChannels(aCount: integer);
begin
  if aCount > 0 then
    vortex_channel_pool_remove(FVortexPool, aCount);
end;

end.

