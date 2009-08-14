(*******************************************************************************
 *  BeepChannel.pas: BEEP protocol channel
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
 *  TBeepChannel implements the channel handling capabilities of the
 *  Vortex library
 ******************************************************************************)
unit BeepChannel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepUtils, BeepObject, BeepContext, BeepConnection;

type
  { Event types}
  TBeepCloseChannelNotify = procedure(ChanNum: integer) of object;
  TBeepChannelEvent = procedure(ChanNum: integer) of object;
  TBeepDroppedChannelEvent = procedure(aBeepChannel: TObject) of object;

  { Enums }
  TChannelStates = (csInvalid, csBusy, csOK);

  { TBeepChannel }

  TBeepChannel = class(TBEEPObject)
  private
    FOnClosed: TBeepCloseChannelNotify;
    FOnDropped: TBeepDroppedChannelEvent;
    FState: TChannelStates;         //Channel state
    FTargetNumber: integer;         //Target channel number
    FConnection: TBeepConnection;   //Connection in which to operate
    FVortexChannel: PVortexChannel; //The vortex channel object

    function GetContext: TBeepContext;
    function GetNumber: integer;
    function GetProfileName: string;

    procedure SetChannelRef;
    procedure SetClosedHandler;

    //Event triggers
    procedure DoCloseNotify(ChanNum: integer);
    procedure DoDropped;

    //Event handlers
    procedure BeepChannelDropped(Channel:PVortexChannel);
  public
    //Events
    property OnCloseNotify: TBeepCloseChannelNotify read FOnClosed write FOnClosed;
    property OnDropped: TBeepDroppedChannelEvent read FOnDropped write FOnDropped;

    //Propereties
    property VortexChannel: PVortexChannel read FVortexChannel; //Vortex channel object
    property Context: TBeepContext read GetContext;         //Context in which channel exists
    property Connection: TBeepConnection read FConnection;  //Connection in which channel exists
    property ProfileName: string read GetProfileName;       //Profile for this channel
    property Number: integer read GetNumber;                //Channel number
    property State: TChannelStates read FState;             //Channel creation state

    constructor Create(aConnection: TBeepConnection; ChanNum: longint);
    destructor Destroy; override;

    procedure CloseChannel;

    function SendMSG(Msg: PByte; MsgLen: integer; var MsgID: longint): boolean;
    function SendMSG(Msg: string; var MsgID: longint): boolean;

    function SendRPY(Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendRPY(Msg: string; MsgID: longint): boolean;

    function SendANS(Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendANS(Msg: string; MsgID: longint): boolean;

    function SendANSFinal(MsgID: longint): boolean;

    function SendERR(Msg: PByte; MsgLen: integer; MsgID: longint): boolean;
    function SendERR(Msg: string; MsgID: longint): boolean;
  end;

  { TBeepChannelListener }

  TBeepChannelListener = class(TBeepChannel)
  private

  public
    constructor Create(aConnection: TBeepConnection; aVChannel: PVortexChannel); reintroduce;
    destructor Destroy; override;
  end;

  { TBeepChannelInitiator }

  TBeepChannelInitiator = class(TBeepChannel)
  private
    FOnCreated: TBeepChannelEvent;
    FOnFailed: TBeepChannelEvent;
    FTargetProfile: string;   //Associated profile

    //Event triggers
    procedure DoCreated(ChanNum: integer);
    procedure DoFailed(ChanNum: integer);

    //Event handlers
    procedure BeepChannelCreated(ChannelNum:longint; Channel:PVortexChannel; Conn:PVortexConnection);
  public
    //Events
    property OnCreated: TBeepChannelEvent read FOnCreated write FOnCreated;
    property OnFailed: TBeepChannelEvent read FOnFailed write FOnFailed;

    constructor Create(aConnection: TBeepConnection; aProfileName: string; ChanNum: longint); reintroduce;
    destructor Destroy; override;

    procedure OpenChannel;
  end;


{ Vortex functions to implement
function              vortex_channel_new_full

function              vortex_channel_new_fullv

function             vortex_channel_empty_new

procedure            vortex_channel_set_close_handler

procedure            vortex_channel_invoke_closed

procedure            vortex_channel_set_close_notify_handler

procedure            vortex_channel_set_received_handler

procedure            vortex_channel_set_complete_flag

function             vortex_channel_have_previous_frame

function             vortex_channel_get_previous_frame

procedure            vortex_channel_store_previous_frame

function             vortex_channel_build_single_pending_frame

procedure            vortex_channel_update_status

procedure            vortex_channel_update_status_received

function             vortex_channel_get_next_msg_no

procedure            vortex_channel_set_next_seq_no

function             vortex_channel_get_last_msg_no_received     (channel : PVortexChannel):longint;

function             vortex_channel_get_next_seq_no              (channel : PVortexChannel):longword;

function             vortex_channel_get_next_expected_seq_no     (channel : PVortexChannel):longword;

function             vortex_channel_get_next_ans_no              (channel : PVortexChannel):longword;

function             vortex_channel_get_next_expected_ans_no     (channel:PVortexChannel):longword;

function             vortex_channel_get_next_reply_no            (channel:PVortexChannel):longint;

function             vortex_channel_get_next_expected_reply_no   (channel : PVortexChannel):longint;

function             vortex_channel_get_window_size              (channel : PVortexChannel):longint;

procedure            vortex_channel_set_window_size

function             vortex_channel_get_mime_type                (channel : PVortexChannel):Pchar;

function             vortex_channel_get_transfer_encoding        (channel : PVortexChannel):Pchar;

procedure            vortex_channel_set_automatic_mime

function             vortex_channel_get_automatic_mime           (channel : PVortexChannel):longint;

function             vortex_channel_get_max_seq_no_remote_accepted (channel : PVortexChannel):longword;

function             vortex_channel_get_next_frame_size

function                  vortex_channel_set_next_frame_size_handler

procedure            vortex_channel_update_remote_incoming_buffer

function             vortex_channel_seq_no_exceeded_after_update  (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_get_max_seq_no_accepted       (channel : PVortexChannel):longword;

procedure            vortex_channel_set_max_seq_no_accepted

function             vortex_channel_are_equal

function             vortex_channel_update_incoming_buffer

procedure            vortex_channel_queue_pending_message

function             vortex_channel_is_empty_pending_message     (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_next_pending_message         (channel : PVortexChannel):TaxlPointer;

procedure            vortex_channel_remove_pending_message       (channel : PVortexChannel);

function             vortex_channel_is_running_profile

function             vortex_channel_queue_frame

function             vortex_channel_queue_is_empty                  (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_queue_next_msg                  (channel : PVortexChannel):PVortexWriterData;

function             vortex_channel_queue_length                    (channel : PVortexChannel):longint;

procedure            vortex_channel_set_serialize

procedure            vortex_channel_set_data

procedure            vortex_channel_set_data_full

procedure            vortex_channel_delete_data

function             vortex_channel_get_data

function             vortex_channel_ref                             (channel : PVortexChannel):Taxl_bool;

procedure            vortex_channel_unref                           (channel : PVortexChannel);

function             vortex_channel_ref_count                       (channel : PVortexChannel):longint;

function             vortex_channel_send_msgv

function             vortex_channel_send_msg_and_wait

function             vortex_channel_send_msg_and_waitv

function             vortex_channel_send_rpyv

function             vortex_channel_send_ans_rpyv

function             vortex_channel_send_errv

function             vortex_channel_is_opened                      (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_is_being_closed                (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_is_defined_received_handler    (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_is_defined_close_handler       (channel : PVortexChannel):Taxl_bool;

function             vortex_channel_wait_reply

function             vortex_channel_create_wait_reply              :PWaitReplyData;

procedure            vortex_channel_wait_reply_ref                 (wait_reply : PWaitReplyData);

procedure            vortex_channel_free_wait_reply                (wait_reply : PWaitReplyData);

function             vortex_channel_is_ready                       (channel : PVortexChannel):Taxl_bool;

procedure            vortex_channel_queue_reply

function             vortex_channel_get_reply

function             vortex_channel_get_piggyback                  (channel : PVortexChannel):PVortexFrame;

function             vortex_channel_have_piggyback                 (channel : PVortexChannel):Taxl_bool;

procedure            vortex_channel_set_piggyback                  (channel         : PVortexChannel;

procedure            vortex_channel_defer_start                    (channel : PVortexChannel);

function             vortex_channel_validate_err

DONE:
function             vortex_channel_new
function             vortex_channel_get_number
function             vortex_channel_close_full
function             vortex_channel_close
function             vortex_channel_get_ctx
function             vortex_channel_get_connection
function             vortex_channel_send_msg
function             vortex_channel_send_rpy
function             vortex_channel_send_ans_rpy
function             vortex_channel_finalize_ans_rpy
function             vortex_channel_send_err
function             vortex_channel_get_profile
procedure            vortex_channel_set_closed_handler

}


implementation

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//New channel completely created
//User_data points to channel's TBeepChannel object (ie owning channel)
procedure VortexChannelCreated(Channel_num:longint; Channel:PVortexChannel;
    Conn:PVortexConnection; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepChannelInitiator;
begin
  if assigned(User_data) then
  begin
    Owner := TBeepChannelInitiator(User_data);
    Owner.AcquireLock;
    try
      Owner.BeepChannelCreated(Channel_num, Channel, Conn);
    finally
      Owner.ReleaseLock;
    end;
  end;
end;

//Channel improperly closed, ie connection dropped
//User_data points to channel's TBeepChannel object (ie owning channel)
procedure VortexChannelClosed(Channel:PVortexChannel; User_data:TaxlPointer);cdecl;
var
  Owner: TBeepChannel;
begin
  if assigned(User_data) then
  begin
    Owner := TBeepChannel(User_data);
    Owner.AcquireLock;
    try
      Owner.BeepChannelDropped(Channel);
    finally
      Owner.ReleaseLock;
    end;
  end;
end;

{ TBeepChannel }

procedure TBeepChannel.BeepChannelDropped(Channel: PVortexChannel);
begin
  //Set channel state
  FState := csInvalid;

  //Trigger event
  DoDropped;
end;

procedure TBeepChannel.CloseChannel;
begin
  if FState = csOK then
  begin
    //CloseChannel the channel
    vortex_channel_close(FVortexChannel, nil);
    {TODO: what to do when channel closing is denied?}
    // see  vortex_channel_is_being_closed.
    //add enum value csClosing?

    //Set state
    FState := csInvalid;

    //Trigger event
    DoCloseNotify(FTargetNumber);
  end;
end;

constructor TBeepChannel.Create(aConnection: TBeepConnection; ChanNum: longint);
begin
  //Safety checks
  if aConnection = nil then
    raise EBeepInvalidConnection.Create('A valid BeepConnection must be supplied to create a channel.');

  if ChanNum < 0 then
    raise EBeepInvalidChannelNumber.Create('Channel number must be >= 0');

  inherited Create;

  //Init
  FConnection    := aConnection;
  FTargetNumber  := ChanNum;
  FVortexChannel := nil;
  FState         := csInvalid;
end;

destructor TBeepChannel.Destroy;
begin
  inherited Destroy;
end;

procedure TBeepChannel.DoCloseNotify(ChanNum: integer);
begin
  if assigned(FOnClosed) then
    FOnClosed(ChanNum);
end;

procedure TBeepChannel.DoDropped;
begin
  if assigned(FOnDropped) then
    FOnDropped(self);
end;

function TBeepChannel.GetContext: TBeepContext;
begin
  Result := Connection.Context;
end;

function TBeepChannel.GetNumber: integer;
begin
  Result := vortex_channel_get_number(FVortexChannel)
end;

function TBeepChannel.GetProfileName: string;
begin
  if FState = csOK then
    Result := string(vortex_channel_get_profile(FVortexChannel))
  else
    Result := '';
end;

function TBeepChannel.SendANS(Msg: PByte; MsgLen: integer;
  MsgID: longint): boolean;
begin
  if FState = csOK then
    //Queue the message
    Result := (axl_true = vortex_channel_send_ans_rpy(FVortexChannel, Msg, MsgLen, MsgID))
  else
    //Channel is not open
    Result := false;
end;

function TBeepChannel.SendANS(Msg: string; MsgID: longint): boolean;
begin
  Result := SendANS(PByte(PChar(Msg)), length(Msg), MsgID);
end;

function TBeepChannel.SendANSFinal(MsgID: longint): boolean;
begin
  if FState = csOK then
    //Queue the message
    Result := (axl_true = vortex_channel_finalize_ans_rpy(FVortexChannel, MsgID))
  else
    //Channel is not open
    Result := false;   end;

function TBeepChannel.SendERR(Msg: PByte; MsgLen: integer;
  MsgID: longint): boolean;
begin
  if FState = csOK then
    //Queue the message
    Result := (axl_true = vortex_channel_send_err(FVortexChannel, Msg, MsgLen, MsgID))
  else
    //Channel is not open
    Result := false;
end;

function TBeepChannel.SendERR(Msg: string; MsgID: longint): boolean;
begin
  Result := SendERR(PByte(PChar(Msg)), length(Msg), MsgID);
end;

function TBeepChannel.SendMSG(Msg: PByte; MsgLen: integer;
  var MsgID: longint): boolean;
begin
  if FState = csOK then
    //Queue the message
    Result := (axl_true = vortex_channel_send_msg(FVortexChannel, Msg, MsgLen, MsgID))
  else
    //Channel is not open
    Result := false;
end;

function TBeepChannel.SendMSG(Msg: string; var MsgID: longint): boolean;
begin
  Result := SendMSG(PByte(PChar(Msg)), length(Msg), MsgID);
end;

function TBeepChannel.SendRPY(Msg: PByte; MsgLen: integer;
  MsgID: longint): boolean;
begin
  if FState = csOK then
    //Queue the message
    Result := (axl_true = vortex_channel_send_rpy(FVortexChannel, Msg, MsgLen, MsgID))
  else
    //Channel is not open
    Result := false;
end;

function TBeepChannel.SendRPY(Msg: string; MsgID: longint): boolean;
begin
  Result := SendRPY(PByte(PChar(Msg)), length(Msg), MsgID);
end;

procedure TBeepChannel.SetChannelRef;
begin
  //Store object ref
  vortex_channel_set_data(FVortexChannel, KEY_OWNER, self);
end;

procedure TBeepChannel.SetClosedHandler;
begin
  //Store failed connection handler
  vortex_channel_set_closed_handler(FVortexChannel, @VortexChannelClosed, self);
end;

{ TBeepChannelListener }

constructor TBeepChannelListener.Create(aConnection: TBeepConnection;
  aVChannel: PVortexChannel);
var
  ChanNum: longint;
begin
  //Safety checks
  if aVChannel = nil then
    raise EBeepInvalidChannel.Create('A valid Vortex channel must be supplied to create a listener channel.');

  //Get channel number
  ChanNum := vortex_channel_get_number(aVChannel);

  //Create base
  inherited Create(aConnection, ChanNum);

  //Init
  FVortexChannel := aVChannel;  //override default
  FState         := csOK;       //override default

  //Store object ref
  SetChannelRef;

  //Set closed handler
  SetClosedHandler;
end;

destructor TBeepChannelListener.Destroy;
begin
  if FState = csOK then
    {close}; {TODO channel close}
    //don't close for channel pool

  inherited Destroy;
end;

{ TBeepChannelInitiator }

procedure TBeepChannelInitiator.BeepChannelCreated(ChannelNum: longint;
  Channel: PVortexChannel; Conn: PVortexConnection);
begin
  //Check for valid channel
  if nil <> Channel then
  begin
    //Store new channel
    FVortexChannel := Channel;

    //Update local value in case target was channel 0 (auto select)
    FTargetNumber := ChannelNum;

    //Set channel state
    FState := csOK;

    //Store object ref
    SetChannelRef;

    //Set closed handler
    SetClosedHandler;

    //Trigger event
    DoCreated(ChannelNum);
  end
  else
  begin
    //Set channel state
    FState := csInvalid;

    //Trigger event
    DoFailed(ChannelNum);
  end;
end;

constructor TBeepChannelInitiator.Create(aConnection: TBeepConnection;
  aProfileName: string; ChanNum: longint);
begin
  //Safety checks
  if aProfileName = EmptyStr then
    raise EBeepInvalidProfile.Create('A valid BeepProfile must be supplied to create a channel.');

  //Create base
  inherited Create(aConnection, ChanNum);

  //Init
  FTargetProfile  := aProfileName;
end;

destructor TBeepChannelInitiator.Destroy;
begin
  CloseChannel;

  inherited Destroy;
end;

procedure TBeepChannelInitiator.DoCreated(ChanNum: integer);
begin
  if assigned(FOnCreated) then
    FOnCreated(ChanNum);
end;

procedure TBeepChannelInitiator.DoFailed(ChanNum: integer);
begin
  if assigned(FOnFailed) then
    FOnFailed(ChanNum);
end;

procedure TBeepChannelInitiator.OpenChannel;
begin
  //Check if ProfileName is supported on this connection
  if axl_false = vortex_connection_is_profile_supported(Connection.VortexConnection, PChar(FTargetProfile)) then
    raise EProfileNotSupported.Create('Profile'+#13#10+FTargetProfile+#13#10+
        ' is not supported on the current connection');

  //Only do this once
  if FState = csInvalid then
  begin
    {TODO: add support for 2nd level ProfileName functions}
    //ie if assigned(fonreceived)
    {TODO: implement blocking mode channel creation}

    vortex_channel_new(FConnection.VortexConnection, FTargetNumber, PChar(FTargetProfile),
      nil,nil,nil,nil,  //<---- 2nd level handlers
      @VortexChannelCreated, self);

    //Set state
    FState := csBusy;
  end;
end;

end.

