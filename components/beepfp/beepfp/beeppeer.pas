(*******************************************************************************
 *  BeepPeer.pas: BEEP protocol peer base class
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
 *  TBeepPeer implements the common features found in a BEEP client and a BEEP
 *  listener using the TBeepXXXXX classes.
 *  TBeepPeer does not have any connections and thus cannot function without a
 *  higher level class.
 ******************************************************************************)
unit BeepPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  BeepUtils, BeepObject, BeepContext, BeepConnection, BeepProfile, BeepChannel,
  BeepChannelPool;

type
  { Exceptions }
  EDuplicateChannel = class(EBeepError);

  { Event types}
  TConnectionEvent  = procedure (Sender: TBeepConnection) of object;
  TConnectionErrorEvent  = procedure (Sender: TBeepConnection; Reason: string) of object;

  { TBeepPeer }

  TBeepPeer = class(TBEEPObject)
  private //Managed
    FChannelsInitiator: TStringList;  //Open channels initiated
    FChannelsListener: TStringList;   //Open channels accepted
    FChannelZombies: TFPObjectList;   //Closed channel objects that must be destroyed
    FChannelPool: TFPObjectList;      //Channel pools
    FContext: TBeepContext;           //Operating context
    FProfiles: TStringList;           //Active profiles


    function CalcChannelID(aConnection: TBeepConnection; aNumber: integer): string;
    function FetchChannel(aConnection: TBeepConnection; aNumber: integer): TBeepChannelInitiator;

    //Event triggers


    //Event handlers
    procedure ChannelDropped(aBeepChannel: TObject);
    procedure ProfileCreatedChannel(aChannel: TBeepChannelListener);
    procedure ProfileClosingChannel(aChannel: TBeepChannel);
  public
    //Events

    //Properties
    property Context: TBeepContext read FContext;   //Operating context

    constructor Create; virtual;
    destructor Destroy; override;

    //Profiles
    function AddProfile(aName: string): TBeepProfile;
    procedure DeleteProfile(aName: string);

    //Channels
    function AddChannel(aConnection: TBeepConnection; aProfileName: string; aNumber: integer): TBeepChannelInitiator;
    //DeleteChannel
    procedure OpenChannel(aConnection: TBeepConnection; aNumber: integer);
    procedure CloseChannel(aConnection: TBeepConnection; aNumber: integer);
    procedure CloseAllChannels;

    //Channel pools
    function  AddChannelPool(aConnection: TBeepConnection; aProfileName: string; aChannelCount: integer; var PoolIdx: integer): TBeepChannelPool;
    procedure InitialisePool(PoolIdx: integer);
    procedure AddPoolChannels(PoolIdx: integer; aCount: integer);
    procedure RemovePoolChannels(PoolIdx: integer; aCount: integer);
    function  GetPoolChannel(PoolIdx: integer; AutoInc: boolean = true):TBeepChannel;
    procedure ReleasePoolChannel(PoolIdx: integer; aChannel: TBeepChannel);

    //Messages
    function SendMSG(aConnection: TBeepConnection; aNumber: integer; Msg: PByte; MsgLen: integer; var MsgID: longint): boolean;
    function SendRPY(aConnection: TBeepConnection; aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendANS(aConnection: TBeepConnection; aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendANSFinal(aConnection: TBeepConnection; aNumber: integer; MsgID: longint): boolean;
    function SendERR(aConnection: TBeepConnection; aNumber: integer; Msg: PByte; MsgLen: integer; MsgID: longint): boolean;

  end;

implementation

{ TBeepPeer }

function TBeepPeer.AddChannel(aConnection: TBeepConnection;
  aProfileName: string; aNumber: integer): TBeepChannelInitiator;
var
  Chan: TBeepChannelInitiator;
  Index: integer;
  ID: string;
begin
  //Channel must be open
  if aConnection = nil then
    raise EBeepInvalidConnection.Create('Connection must be established before adding channels.');

  if aConnection.Connected <> csConnected then
    raise EBeepInvalidConnection.Create('Connection must be established before adding channels.');

  //Don't allow auto channel number selection
  if aNumber < 1 then
    raise EBeepInvalidChannelNumber.Create('Channel number must be > 0.');

  //Convert
  ID := CalcChannelID(aConnection, aNumber);

  //Check for duplicates
  Index := FChannelsInitiator.IndexOf(ID);

  //If not exist
  if Index = -1 then
  begin
    //Create a new channel
    Chan := TBeepChannelInitiator.Create(aConnection, aProfileName, aNumber);

    //Keep track of all channels
    FChannelsInitiator.AddObject(ID, Chan);

    //Return a reference
    Result := Chan;
  end
  else  //Exists
  begin
    //Return existing profile
    Result := TBeepChannelInitiator(FChannelsInitiator.Objects[Index]);
  end;
end;

function TBeepPeer.AddChannelPool(aConnection: TBeepConnection;
  aProfileName: string; aChannelCount: integer;
  var PoolIdx: integer): TBeepChannelPool;
var
  Pool: TBeepChannelPool;
begin
  //Create a pool
  Pool := TBeepChannelPool.Create(aConnection, aProfileName, aChannelCount);

  //Put pool in list
  PoolIdx := FChannelPool.Add(Pool);

  //Return object
  Result := Pool;
end;

procedure TBeepPeer.AddPoolChannels(PoolIdx: integer; aCount: integer);
var
  Pool: TBeepChannelPool;
begin
  //Fetch the pool
  Pool := TBeepChannelPool(FChannelPool.Items[PoolIdx]);

  //Safety
  if Pool <> nil then
    Pool.AddChannels(aCount);
end;

function TBeepPeer.AddProfile(aName: string): TBeepProfile;
var
  Prof: TBeepProfile;
  Index: integer;
begin
  //Check for duplicates
  Index := FProfiles.IndexOfName(aName);

  //If not exist
  if Index = -1 then
  begin
    //Create a new profile
    Prof := TBeepProfile.Create(Context, aName, @ProfileCreatedChannel, @ProfileClosingChannel);
    Prof.RegisterProfile;

    //Keep track of all profiles
    FProfiles.AddObject(aName, Prof);

    //Return a reference
    Result := Prof;
  end
  else  //Exists
  begin
    //Return existing profile
    Result := TBeepProfile(FProfiles.Objects[Index]);
  end;
end;

function TBeepPeer.CalcChannelID(aConnection: TBeepConnection;
  aNumber: integer): string;
begin
  Result := IntToStr(aConnection.ID)+':'+IntToStr(aNumber);
end;

procedure TBeepPeer.ChannelDropped(aBeepChannel: TObject);
var
  aChannel: TBeepChannel;
  ChannelList: TStringList;
  Index: integer;
  ID: string;
begin
  //Cast object
  aChannel := TBeepChannel(aBeepChannel);

  ////////*****
writeln('Peer dropped channel '+inttostr(aChannel.Number));

  //Convert
  ID := CalcChannelID(aChannel.Connection, aChannel.Number);

  //Find index
  ChannelList := FChannelsInitiator;
  Index := FChannelsInitiator.IndexOf(ID);
  if Index = -1 then
  begin
    ChannelList := FChannelsListener;
    Index := FChannelsListener.IndexOf(ID);
  end;

  //Delete item from list.
  ChannelList.Delete(Index);

  //Add to zombies
  FChannelZombies.Add(aBeepChannel);
end;

procedure TBeepPeer.CloseAllChannels;
var
  Chan: TBeepChannel;
  k: Integer;
begin
  if assigned(FChannelsInitiator) then
    for k := 0 to FChannelsInitiator.Count-1 do
    begin
      Chan := TBeepChannel(FChannelsInitiator.Objects[k]);
      Chan.CloseChannel;
      //writeln('Closing ch '+FChannelsInitiator.Strings[k]);
    end;
end;

procedure TBeepPeer.CloseChannel(aConnection: TBeepConnection;
  aNumber: integer);
var
  Chan: TBeepChannel;
begin
writeln('Peer closed channel '+inttostr(aNumber));
  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Open the connection
  if Chan <> nil then
    Chan.CloseChannel;
end;

constructor TBeepPeer.Create;
begin
  inherited Create;

  //Create objects
  FContext  := TBeepContext.Create;
  FProfiles := TStringList.Create;
  FChannelsInitiator := TStringList.Create;
  FChannelsListener  := TStringList.Create;
  FChannelZombies    := TFPObjectList.Create(true);
  FChannelPool       := TFPObjectList.Create(true);
end;

procedure TBeepPeer.DeleteProfile(aName: string);
var
  Prof: TBeepProfile;
  Index: integer;
begin
  //Get index
  Index := FProfiles.IndexOfName(aName);

  //Get profile
  Prof := TBeepProfile(FProfiles.Objects[Index]);

  //Unregister
  Prof.UnRegisterProfile;

  //Delete from local list
  FProfiles.Delete(Index);

  //Free
  Prof.Free;
end;

destructor TBeepPeer.Destroy;
var
  k: Integer;
begin
  //Free objects
  FChannelPool.Free;
  FChannelZombies.Free;

  //Received channels
  if assigned(FChannelsListener) then
    for k := 0 to FChannelsListener.Count-1 do
      FChannelsListener.Objects[k].Free;
  FChannelsListener.Free;

  //Initiated channels
  if assigned(FChannelsInitiator) then
    for k := 0 to FChannelsInitiator.Count-1 do
      FChannelsInitiator.Objects[k].Free;
  FChannelsInitiator.Free;

  //Profiles
  if assigned(FProfiles) then
    for k := 0 to FProfiles.Count-1 do
      FProfiles.Objects[k].Free;
  FProfiles.Free;

  FContext.Free;

  inherited Destroy;
end;

function TBeepPeer.FetchChannel(aConnection: TBeepConnection;
  aNumber: integer): TBeepChannelInitiator;
var
  ID: string;
  Index: integer;
begin
  //Get channel ID
  ID := CalcChannelID(aConnection, aNumber);

  //Get index
  Index := FChannelsInitiator.IndexOf(ID);

  //Get channel object
  if Index >= 0 then
    Result := TBeepChannelInitiator(FChannelsInitiator.Objects[Index])
  else
    Result := nil;
end;

function TBeepPeer.GetPoolChannel(PoolIdx: integer;
  AutoInc: boolean): TBeepChannel;
var
  Pool: TBeepChannelPool;
begin
  //Fetch the pool
  Pool := TBeepChannelPool(FChannelPool.Items[PoolIdx]);

  //Safety
  if Pool <> nil then
    Result := Pool.GetNextReady(AutoInc)
  else
    Result := nil;
end;

procedure TBeepPeer.InitialisePool(PoolIdx: integer);
var
  Pool: TBeepChannelPool;
begin
  //Fetch the pool
  Pool := TBeepChannelPool(FChannelPool.Items[PoolIdx]);

  //Safety
  if Pool <> nil then
    Pool.Initialise;
end;

procedure TBeepPeer.OpenChannel(aConnection: TBeepConnection;
  aNumber: integer);
var
  Chan: TBeepChannelInitiator;
begin
  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Open the connection
  if Chan <> nil then
    Chan.OpenChannel;
end;

procedure TBeepPeer.ProfileClosingChannel(aChannel: TBeepChannel);
var
  Index: integer;
  ID: string;
begin
writeln('Peer closing channel '+inttostr(aChannel.Number));
  //Convert
  ID := CalcChannelID(aChannel.Connection, aChannel.Number);

  //Find index
  Index := FChannelsListener.IndexOf(ID);

  //Delete item from open list.
  FChannelsListener.Delete(Index);

  //Add to zombie list
  FChannelZombies.Add(aChannel);
end;

procedure TBeepPeer.ProfileCreatedChannel(aChannel: TBeepChannelListener);
var
  Chan: TBeepChannelListener;
  Index: integer;
  ID: string;
begin
writeln('Peer created channel '+inttostr(aChannel.Number));
//***********

  //Convert
  ID := CalcChannelID(aChannel.Connection, aChannel.Number);

  //Check for duplicates
  Index := FChannelsListener.IndexOf(ID);

  //If not exist
  if Index = -1 then
  begin
    //Create a new channel
    Chan := TBeepChannelListener.Create(aChannel.Connection, aChannel.VortexChannel);

    //Add handlers
    Chan.OnDropped  := @ChannelDropped;

    //Keep track of all channels
    FChannelsListener.AddObject(ID, Chan);
  end
  else  //Exists
  begin
    //Raise exception
    raise EDuplicateChannel.Create('Duplicate channel. Can''t have identical channels in a single peer.');
  end;
end;

procedure TBeepPeer.ReleasePoolChannel(PoolIdx: integer;
  aChannel: TBeepChannel);
var
  Pool: TBeepChannelPool;
begin
  //Fetch the pool
  Pool := TBeepChannelPool(FChannelPool.Items[PoolIdx]);

  //Safety
  if Pool <> nil then
    Pool.ReleaseChannel(aChannel);
end;

procedure TBeepPeer.RemovePoolChannels(PoolIdx: integer; aCount: integer);
var
  Pool: TBeepChannelPool;
begin
  //Fetch the pool
  Pool := TBeepChannelPool(FChannelPool.Items[PoolIdx]);

  //Safety
  if Pool <> nil then
    Pool.RemoveChannels(aCount);
end;

function TBeepPeer.SendANS(aConnection: TBeepConnection; aNumber: integer;
  Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Chan: TBeepChannel;
begin
  //Default
  Result := false;

  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Queue the message
  if Chan <> nil then
    Result := Chan.SendANS(Msg, MsgLen, MsgID);
end;

function TBeepPeer.SendANSFinal(aConnection: TBeepConnection; aNumber: integer;
  MsgID: longint): boolean;
var
  Chan: TBeepChannel;
begin
  //Default
  Result := false;

  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Queue the message
  if Chan <> nil then
    Result := Chan.SendANSFinal(MsgID);
end;

function TBeepPeer.SendERR(aConnection: TBeepConnection; aNumber: integer;
  Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Chan: TBeepChannel;
begin
  //Default
  Result := false;

  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Queue the message
  if Chan <> nil then
    Result := Chan.SendERR(Msg, MsgLen, MsgID);
end;

function TBeepPeer.SendMSG(aConnection: TBeepConnection;
  aNumber: integer; Msg: PByte; MsgLen: integer; var MsgID: longint): boolean;
var
  Chan: TBeepChannel;
begin
  //Default
  Result := false;

  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Queue the message
  if Chan <> nil then
    Result := Chan.SendMSG(Msg, MsgLen, MsgID);
end;

function TBeepPeer.SendRPY(aConnection: TBeepConnection; aNumber: integer;
  Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
var
  Chan: TBeepChannel;
begin
  //Default
  Result := false;

  //Fetch the channel object
  Chan := FetchChannel(aConnection, aNumber);

  //Queue the message
  if Chan <> nil then
    Result := Chan.SendRPY(Msg, MsgLen, MsgID);
end;

end.

