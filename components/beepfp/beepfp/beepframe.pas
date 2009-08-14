(*******************************************************************************
 *  BeepFrame.pas: Beep frames
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
 *  TBeepFrame implements all the frame handling capabilities of the Vortex
 *  library
 ******************************************************************************)
unit BeepFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  Vortex, BeepUtils, BeepObject;

type

  { TBeepFrame }

  TBeepFrame = class(TBEEPObject)
  private
    FVortexFrame: PVortexFrame;
    function GetMsgNum: integer;
    function GetPayload: PByte;
    function GetPayloadAsByteArray: TByteDynArray;
    function GetPayloadAsString: string;
    function GetPayloadSize: integer;

  public
    //Properties
    property VortexFrame: PVortexFrame read FVortexFrame;
    property Payload: PByte read GetPayload;
    property PayloadSize: integer read GetPayloadSize;
    property PayloadAsByteArray: TByteDynArray read GetPayloadAsByteArray;
    property PayloadAsString: string read GetPayloadAsString;
    property MsgNum: integer read GetMsgNum;

    //Overload create to create a frame from other initial conditions
    constructor Create(aFrame: PVortexFrame);
    destructor Destroy; override;
  end;


{ Vortex functions to implement
function         vortex_frame_build_up_from_params

function         vortex_frame_build_up_from_params_s

function  vortex_frame_build_up_from_params_s_buffer


function         vortex_frame_seq_build_up_from_params

function  vortex_frame_seq_build_up_from_params_buffer

function       vortex_frame_create

function        vortex_frame_create_full

function        vortex_frame_create_full_ref

function        vortex_frame_copy                 (frame : PVortexFrame):PVortexFrame;

function        vortex_frame_get_raw_frame         (frame : PVortexFrame):Pchar;

function        vortex_frame_get_next              (connection : PVortexConnection):PVortexFrame;

function        vortex_frame_send_raw              (connection :PVortexConnection;

function        vortex_frame_receive_raw           (connection : PVortexConnection;

function        vortex_frame_readline              (connection : PVortexConnection;

function        vortex_frame_ref                   (frame : PVortexFrame):Taxl_bool;

procedure       vortex_frame_unref                 (frame : PVortexFrame);

function        vortex_frame_ref_count             (frame : PVortexFrame):longint;

procedure       vortex_frame_free                  (frame : PVortexFrame);

function        vortex_frame_join                  (a : PVortexFrame;

function        vortex_frame_join_extending       (a : PVortexFrame;

function        vortex_frame_are_joinable          (a : PVortexFrame;

function        vortex_frame_are_equal             (a : PVortexFrame;

function        vortex_frame_get_id                (frame : PVortexFrame):longint;

function          vortex_frame_get_type            (frame:PVortexFrame):TVortexFrameType;

function        vortex_frame_get_content_type      (frame : PVortexFrame):Pchar;

function        vortex_frame_get_transfer_encoding (frame : PVortexFrame):Pchar;

function        vortex_frame_get_mime_header_size  (frame : PVortexFrame):longint;

function        vortex_frame_get_channel           (frame : PVortexFrame):longint;

function          vortex_frame_get_channel_ref     (frame : PVortexFrame):PVortexChannel;

procedure       vortex_frame_set_channel_ref       (frame   : PVortexFrame;

function        vortex_frame_get_msgno             (frame : PVortexFrame):longint;

function        vortex_frame_get_more_flag         (frame : PVortexFrame):Taxl_bool;

function        vortex_frame_get_seqno             (frame:PVortexFrame):longword;

function        vortex_frame_get_ansno             (frame:PVortexFrame):longint;

function       vortex_frame_get_ok_message          :Pchar;

function        vortex_frame_get_ctx               (frame : PVortexFrame):PVortexCtx;

function        vortex_frame_get_content_size      (frame : PVortexFrame):longint;

function        vortex_frame_get_content           (frame:PVortexFrame):Pchar;

function        vortex_frame_get_error_message     (code          : Pchar;

function        vortex_frame_is_error_message      (frame    : PVortexFrame;

function        vortex_frame_get_start_message     (channel_num          : longint;

function        vortex_frame_get_start_rpy_message (profile         : Pchar;

function        vortex_frame_get_close_message     (number        : longint;

function        vortex_frame_is_mime_message       (frame : PVortexFrame):Taxl_bool;

function        vortex_frame_mime_process          (frame : PVortexFrame):Taxl_bool;

procedure       vortex_frame_set_mime_header       (frame               : PVortexFrame;

function              vortex_frame_get_mime_header       (frame       : PVortexFrame;

function             vortex_frame_mime_header_name       (header : PVortexMimeHeader):Pchar;

function             vortex_frame_mime_header_content    (header : PVortexMimeHeader):Pchar;

function             vortex_frame_mime_header_next       (header : PVortexMimeHeader):PVortexMimeHeader;

function             vortex_frame_mime_header_count      (header : PVortexMimeHeader):longint;

function             vortex_frame_mime_status_is_available  (frame : PVortexFrame):Taxl_bool;

DONE
function        vortex_frame_get_payload_size
function        vortex_frame_get_payload
}

implementation


{ TBeepFrame }

constructor TBeepFrame.Create(aFrame: PVortexFrame);
begin
  FVortexFrame := aFrame;
end;

destructor TBeepFrame.Destroy;
begin
  inherited Destroy;
end;

function TBeepFrame.GetMsgNum: integer;
begin
  Result := vortex_frame_get_msgno(FVortexFrame);
end;

function TBeepFrame.GetPayload: PByte;
begin
  Result := PByte(vortex_frame_get_payload(FVortexFrame));
end;

function TBeepFrame.GetPayloadAsByteArray: TByteDynArray;
var
  FramePayloadSize: integer;
  FramePayload: PByte;
  Data: TByteDynArray;
  k: Integer;
begin
  //Read info from frame
  FramePayloadSize := PayloadSize;
  FramePayload     := Payload;

  //Allocate memory
  Setlength(Data, FramePayloadSize);

  //Copy the data into the array
  for k := 0 to FramePayloadSize-1 do
    Data[k] := FramePayload[k];

  Result := Data;
end;

function TBeepFrame.GetPayloadAsString: string;
var
  FramePayloadSize: integer;
  FramePayload: PByte;
  Data: string;
  k: Integer;
begin
  //Read info from frame
  FramePayloadSize := PayloadSize;
  FramePayload     := Payload;

  //Allocate memory
  Setlength(Data, FramePayloadSize);

  //Copy the data into the array
  for k := 0 to FramePayloadSize-1 do
    Data[k+1] := Char(FramePayload[k]);

  Result := Data;
end;

function TBeepFrame.GetPayloadSize: integer;
begin
  Result := vortex_frame_get_payload_size(FVortexFrame);
end;

end.

