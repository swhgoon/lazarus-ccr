{ Access Wii-Remote devices.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit WiiMoteTools;

{$mode objfpc}{$H+}

{$linklib bluetooth}

interface

uses
  Classes, SysUtils, Bluetooth, ctypes, Sockets;

//------------------------------------------------------------------------------
// mini libc
type
   __time_t = longint;
   __suseconds_t = longint;
   Ptimeval = ^timeval;
   timeval = record
        tv_sec : __time_t;
        tv_usec : __suseconds_t;
     end;
   __fd_mask = dWord;

const
  __FD_SETSIZE = 1024;
  __NFDBITS       = 8 * sizeof(__fd_mask);

type
  __fd_set = record
     fds_bits: packed array[0..(__FD_SETSIZE div __NFDBITS)-1] of __fd_mask;
  end;
  TFdSet = __fd_set;
  PFdSet = ^TFdSet;
Type
  Pfd_set = ^_fd_set;
  _fd_set = __fd_set;

const
  FD_SETSIZE = __FD_SETSIZE;

Type
  Pfd_mask = ^fd_mask;
  fd_mask = __fd_mask;

const
  NFDBITS = __NFDBITS;

Function __FDELT(d: longint): Integer;
Function __FDMASK(d: longint): __fd_mask;
procedure FD_ZERO(out fdset: _fd_set);
procedure FD_SET(fd: longint; var fdset: _fd_Set);
procedure FD_CLR(fd: longint; var fdset: _fd_set);
function FD_ISSET(fd: longint; const fdset: _fd_set): Boolean;
function select(__nfds:longint; __readfds:Pfd_set; __writefds:Pfd_set; __exceptfds:Pfd_set; __timeout:Ptimeval):longint;cdecl;external 'c' name 'select';

//------------------------------------------------------------------------------
const
  WM_OUTPUT_CHANNEL    =       $11;
  WM_INPUT_CHANNEL     =       $13;

  // led bit masks
  WIIMOTE_LED_NONE     =           $00;
  WIIMOTE_LED_1        =           $10;
  WIIMOTE_LED_2        =           $20;
  WIIMOTE_LED_3        =           $40;
  WIIMOTE_LED_4        =           $80;
  WIIMOTE_LED_ALL      =           $F0;

  WM_SET_REPORT        =       $50;

// commands
  WM_CMD_LED                  =$11;
  WM_CMD_REPORT_TYPE          =$12;
  WM_CMD_RUMBLE               =$13;
  WM_CMD_IR                   =$13;
  WM_CMD_CTRL_STATUS          =$15;
  WM_CMD_WRITE_DATA           =$16;
  WM_CMD_READ_DATA            =$17;
  WM_CMD_IR_2                 =$1A;

// input report ids
  WM_RPT_CTRL_STATUS          =$20;
  WM_RPT_READ                 =$21;
  WM_RPT_WRITE                =$22;
  WM_RPT_BTN                  =$30;
  WM_RPT_BTN_ACC              =WM_RPT_BTN+1;
  WM_RPT_BTN_IR               =WM_RPT_BTN+2;
  WM_RPT_BTN_EXP              =WM_RPT_BTN+4;

  WM_BT_INPUT                 =$01;
  WM_BT_OUTPUT                =$02;

// Identify the wiimote device by its class
  WM_DEV_CLASS_0              =$04;
  WM_DEV_CLASS_1              =$25;
  WM_DEV_CLASS_2              =$00;
  WM_VENDOR_ID                =$057E;
  WM_PRODUCT_ID               =$0306;

// controller status stuff
  WM_MAX_BATTERY_CODE         =$C8;

// offsets in wiimote memory
  WM_MEM_OFFSET_CALIBRATION   =$16;
  WM_EXP_MEM_BASE             =$04A40000;
  WM_EXP_MEM_ENABLE           =$04A40040;
  WM_EXP_MEM_CALIBR           =$04A40020;

  WM_REG_IR                   =$04B00030;
  WM_REG_IR_BLOCK1            =$04B00000;
  WM_REG_IR_BLOCK2            =$04B0001A;
  WM_REG_IR_MODENUM           =$04B00033;

// ir block data
 // WM_IR_BLOCK1_CLIFF          "\x02\x00\x00\x71\x01\x00\xAA\x00\x64"
 // WM_IR_BLOCK2_CLIFF          "\x63\x03"

  WM_IR_TYPE_BASIC            =$01;
  WM_IR_TYPE_EXTENDED         =$03;

// controller status flags for the first message byte
// bit 1 is unknown
  WM_CTRL_STATUS_BYTE1_ATTACHMENT         =$02;
  WM_CTRL_STATUS_BYTE1_SPEAKER_ENABLED    =$04;
  WM_CTRL_STATUS_BYTE1_IR_ENABLED         =$08;
  WM_CTRL_STATUS_BYTE1_LED_1              =$10;
  WM_CTRL_STATUS_BYTE1_LED_2              =$20;
  WM_CTRL_STATUS_BYTE1_LED_3              =$40;
  WM_CTRL_STATUS_BYTE1_LED_4              =$80;

// aspect ratio
  WM_ASPECT_16_9_X    =660;
  WM_ASPECT_16_9_Y    =370;
  WM_ASPECT_4_3_X     =560;
  WM_ASPECT_4_3_Y     =420;


//  Expansion stuff

// encrypted expansion id codes (located at =$04A400FC)
  EXP_ID_CODE_NUNCHUK                 =$9A1EFEFE;
  EXP_ID_CODE_CLASSIC_CONTROLLER      =$9A1EFDFD;
  EXP_ID_CODE_GUITAR                  =$9A1EFDFB;

  EXP_HANDSHAKE_LEN                   =224;

// button codes
  WIIMOTE_BUTTON_TWO               = $0001;
  WIIMOTE_BUTTON_ONE               = $0002;
  WIIMOTE_BUTTON_B                 = $0004;
  WIIMOTE_BUTTON_A                 = $0008;
  WIIMOTE_BUTTON_MINUS             = $0010;
  WIIMOTE_BUTTON_ZACCEL_BIT6       = $0020;
  WIIMOTE_BUTTON_ZACCEL_BIT7       = $0040;
  WIIMOTE_BUTTON_HOME              = $0080;
  WIIMOTE_BUTTON_LEFT              = $0100;
  WIIMOTE_BUTTON_RIGHT             = $0200;
  WIIMOTE_BUTTON_DOWN              = $0400;
  WIIMOTE_BUTTON_UP                = $0800;
  WIIMOTE_BUTTON_PLUS              = $1000;
  WIIMOTE_BUTTON_ZACCEL_BIT4       = $2000;
  WIIMOTE_BUTTON_ZACCEL_BIT5       = $4000;
  WIIMOTE_BUTTON_UNKNOWN           = $8000;
  WIIMOTE_BUTTON_ALL               = $1F9F;

// nunchul button codes
  NUNCHUK_BUTTON_Z                 = $01;
  NUNCHUK_BUTTON_C                 = $02;
  NUNCHUK_BUTTON_ALL               = $03;

// classic controller button codes
  CLASSIC_CTRL_BUTTON_UP           = $0001;
  CLASSIC_CTRL_BUTTON_LEFT         = $0002;
  CLASSIC_CTRL_BUTTON_ZR           = $0004;
  CLASSIC_CTRL_BUTTON_X            = $0008;
  CLASSIC_CTRL_BUTTON_A            = $0010;
  CLASSIC_CTRL_BUTTON_Y            = $0020;
  CLASSIC_CTRL_BUTTON_B            = $0040;
  CLASSIC_CTRL_BUTTON_ZL           = $0080;
  CLASSIC_CTRL_BUTTON_FULL_R       = $0200;
  CLASSIC_CTRL_BUTTON_PLUS         = $0400;
  CLASSIC_CTRL_BUTTON_HOME         = $0800;
  CLASSIC_CTRL_BUTTON_MINUS        = $1000;
  CLASSIC_CTRL_BUTTON_FULL_L       = $2000;
  CLASSIC_CTRL_BUTTON_DOWN         = $4000;
  CLASSIC_CTRL_BUTTON_RIGHT        = $8000;
  CLASSIC_CTRL_BUTTON_ALL          = $FEFF;

// guitar hero 3 button codes
  GUITAR_HERO_3_BUTTON_STRUM_UP    = $0001;
  GUITAR_HERO_3_BUTTON_YELLOW      = $0008;
  GUITAR_HERO_3_BUTTON_GREEN       = $0010;
  GUITAR_HERO_3_BUTTON_BLUE        = $0020;
  GUITAR_HERO_3_BUTTON_RED         = $0040;
  GUITAR_HERO_3_BUTTON_ORANGE      = $0080;
  GUITAR_HERO_3_BUTTON_PLUS        = $0400;
  GUITAR_HERO_3_BUTTON_MINUS       = $1000;
  GUITAR_HERO_3_BUTTON_STRUM_DOWN  = $4000;
  GUITAR_HERO_3_BUTTON_ALL         = $FEFF;

// ir block data (cliff is the person who found out and documented it first)
  WM_IR_BLOCK1_CLIFF : string = #$02#$00#$00#$71#$01#$00#$AA#$00#$64;
  WM_IR_BLOCK2_CLIFF : string = #$63#$03;

type
  TWiimoteEventType = (
    WiiMote_NONE,
    WiiMote_EVENT,
    WiiMote_STATUS,
    WiiMote_DISCONNECT,
    WiiMote_NUNCHUK_INSERTED,
    WiiMote_NUNCHUK_REMOVED,
    WiiMote_CLASSIC_CTRL_INSERTED,
    WiiMote_CLASSIC_CTRL_REMOVED,
    WiiMote_GUITAR_HERO_3_CTRL_INSERTED,
    WiiMote_GUITAR_HERO_3_CTRL_REMOVED
    );

type
  TAccelVector = record
    x,y,z: integer;
  end;
  
  TAccelCalibration = record
    cal_zero: TAccelVector;
    cal_g: TAccelVector;
  end;
  
  TWiiMoteReadRequest = class;

  TWiiMoteReadCallback = procedure(Request: TWiiMoteReadRequest) of object;

  { TWiiMoteReadRequest }

  TWiiMoteReadRequest = class
  public
    Callback: TWiiMoteReadCallback;
    Addr: cardinal;
    BufSize: word;
    Buf: PByte;
    Received: word;
    constructor Create(const TheCallback: TWiiMoteReadCallback;
                       TheAddr: cardinal; TheBufSize: word);
    destructor Destroy; override;
  end;

  TWiiMoteDot = record
    X: word;
    Y: word;
    Size: word;
    Visible: boolean;
  end;

  TWiimotes = class;

  { TWiiMote }

  TWiimote = class
  private
    FLEDS: integer;
    FRealizedIR: boolean;
    FReadRequests: TFPList;// list of TWiiMoteReadRequest
    function SendCmd(ReportType: byte; Msg: PByte; Count: integer): PtrInt;
    function SendData(Addr: cuint; Data: Pointer; Count: byte): PtrInt;
    function RequestRead(const Callback: TWiiMoteReadCallback;
                          Addr: cuint; BufSize: cushort): TWiiMoteReadRequest;
    procedure SendNextReadRequest;
    procedure OnHandShake(Request: TWiiMoteReadRequest);
    procedure HandleEvents;
    procedure HandleRead;
  public
    WiiMotes: TWiiMotes;
    ID: integer;
    Name: string;
    bdaddr: bdaddr_t;
    Found: boolean;
    Connected: boolean;
    OutSocket: cint;
    InSocket: cint;
    Handshaking: boolean;
    HandshakeComplete: boolean;
    Event: TWiimoteEventType;
    EventBuf: array[0..31] of byte;
    
    // properties
    LEDs: integer;
    Rumble: boolean;
    Continuous: boolean;
    ReportMotion: boolean;
    ReportIR: boolean;
    ReportExpansion: boolean;
    Buttons: word; // current button state
    Accel: TAccelVector; // current accelerator state
    AccelCalibration: TAccelCalibration;
    Dots: array[0..3] of TWiiMoteDot;

    constructor Create;
    destructor Destroy; override;
    procedure SetLEDs(const AValue: integer);
    function Connect: boolean;
    procedure Disconnect;
    procedure EnableHandshake;
    function RealizeReportType: boolean;
    procedure RealizeIR;
  end;
  
  { TWiiMotes }

  TWiimotes = class
  private
    FItems: TFPList;
    function GetItems(Index: integer): TWiimote;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TWiimote): integer;
    function Count: integer;
    property Items[Index: integer]: TWiimote read GetItems; default;
    procedure FindWiiMotes(timeout: integer);
    function Connect: integer;
    procedure Disconnect;
    function HandleEvents: boolean;
  end;


function c_close(fd: cint): cint; external name 'close';


implementation

function __FDELT(d: longint): Integer;
begin
  Result:=d div __NFDBITS;
end;

function __FDMASK(d: longint): __fd_mask;
begin
  Result:=1 shl (d mod __NFDBITS);
end;

procedure FD_ZERO(out fdset: _fd_set);
var
  I: Integer;
begin
  with fdset do
    for I:=Low(fds_bits) to High(fds_bits) do
      fds_bits[I]:=0;
end;

procedure FD_SET(fd: longint; var fdset: _fd_Set);
begin
  fdset.fds_bits[__FDELT(fd)]:=fdset.fds_bits[__FDELT(fd)] or __FDMASK(fd);
end;

procedure FD_CLR(fd: longint; var fdset: _fd_set);
begin
  fdset.fds_bits[__FDELT(fd)]:=fdset.fds_bits[__FDELT(fd)] and (not __FDMASK(fd));
end;

function FD_ISSET(fd: longint; const fdset: _fd_set): Boolean;
begin
  Result:=(fdset.fds_bits[__FDELT(fd)] and __FDMASK(fd))<>0;
end;


procedure TWiimotes.FindWiiMotes(timeout: integer);
var
  device_id, device_sock: cint;
  scan_info: array[0..127] of inquiry_info;
  scan_info_ptr: Pinquiry_info;
  found_devices: cint;
  DevName: PCChar;
  CurWiiMote: TWiimote;
  i: Integer;
begin
  // get the id of the first bluetooth device.
  device_id := hci_get_route(nil);
  if (device_id < 0) then
    raise Exception.Create('FindWiiMotes: hci_get_route');

  // create a socket to the device
  device_sock := hci_open_dev(device_id);
  if (device_sock < 0) then
    raise Exception.Create('hci_open_dev');

  // scan for bluetooth devices for 'timeout' seconds
  scan_info_ptr:=@scan_info[0];
  FillByte(scan_info[0],SizeOf(inquiry_info)*128,0);
  found_devices := hci_inquiry_1(device_id, timeout, 128, nil,
                                 @scan_info_ptr, IREQ_CACHE_FLUSH);
  if (found_devices < 0) then
    raise Exception.Create('hci_inquiry');
  writeln('found_devices=',found_devices);
  
  // display discovered devices
  DevName:=nil;
  GetMem(DevName,20);
  for i:=0 to found_devices-1 do begin
    if ((scan_info[i].dev_class[0] = WM_DEV_CLASS_0) and
        (scan_info[i].dev_class[1] = WM_DEV_CLASS_1) and
        (scan_info[i].dev_class[2] = WM_DEV_CLASS_2)) then
    begin
      CurWiiMote:=TWiimote.Create;
      Add(CurWiiMote);
      // found a device
      ba2str(@scan_info[i].bdaddr, DevName);
      CurWiiMote.Name:=PChar(DevName);
      CurWiiMote.bdaddr:=scan_info[i].bdaddr;
      CurWiiMote.Found:=true;

      writeln(i,' Device=',CurWiiMote.Name);
    end;
  end;
  FreeMem(DevName);

  c_close(device_sock);
end;

function TWiimotes.Connect: integer;
var
  CurWiiMote: TWiimote;
  i: Integer;
begin
  Result:=0;
  for i:=0 to Count-1 do begin
    CurWiiMote:=Items[i];
    if not CurWiiMote.Found then
      // if the device address is not set, skip it
      continue;

    if CurWiiMote.Connect then
      inc(Result);
  end;
end;

procedure TWiimotes.Disconnect;
var
  i: Integer;
begin
  for i:=0 to Count-1 do Items[i].Disconnect;
end;

function TWiimotes.HandleEvents: boolean;
var
  fds: _fd_set;
  highest_fd: integer;
  tv: timeval;
  i: integer;
  r: PtrInt;
begin
  highest_fd:=0;
  Result:=false;

  // block select() for 1/2000th of a second
  tv.tv_sec := 0;
  tv.tv_usec := 500;

  FD_ZERO(fds);

  for i:=0 to Count-1 do begin
    // only poll it if it is connected
    if Items[i].Connected then begin
      FD_SET(Items[i].InSocket, fds);

      // find the highest fd of the connected wiimotes
      if (Items[i].InSocket > highest_fd) then
        highest_fd := Items[i].InSocket;
    end;

    Items[i].Event:= WiiMote_NONE;
  end;

  if (select(highest_fd + 1, @fds, nil, nil, @tv) = -1) then
    raise Exception.Create('Unable to select() the wiimote interrupt socket(s)');

  // check each socket for an event
  for i:=0 to Count-1 do begin
    // if this wiimote is not connected, skip it
    if not Items[i].Connected then
      continue;

    if (FD_ISSET(Items[i].InSocket, fds)) then begin
      // clear out the event buffer
      FillByte(Items[i].EventBuf[0],32,0);

      // read the pending message into the buffer
      r := fprecv(Items[i].InSocket,@Items[i].EventBuf[0], 32,0);
      if (r = -1) then begin
        // error reading data
        writeln('Receiving wiimote data '+IntToStr(Items[i].ID));
        // remote disconnect
        Items[i].Disconnect;
        Result:=true;
        continue;
      end;

      // propagate the event
      if Items[i].EventBuf[1]<>0 then begin
        Items[i].HandleEvents;
        Result:=true;
      end;
    end else begin
      //idle_cycle(wm[i]);
    end;
  end;
end;

{ TWiimotes }

function TWiimotes.GetItems(Index: integer): TWiimote;
begin
  Result:=TWiimote(FItems[Index]);
end;

constructor TWiimotes.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TWiimotes.Destroy;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TWiimotes.Add(Item: TWiimote): integer;
begin
  Item.ID:=Count;
  Item.WiiMotes:=Self;
  Result:=FItems.Add(Item);
end;

function TWiimotes.Count: integer;
begin
  Result:=FItems.Count;
end;

{ TWiimote }

procedure TWiimote.SetLEDs(const AValue: integer);
var
  buf: byte;
begin
  // remove the lower 4 bits because they control rumble
  FLEDs:=AValue and $F0;
  // make sure if the rumble is on that we keep it on
  if Rumble then
    FLEDs := FLEDs or 1;

  if Connected then begin
    buf := FLEDs;
    SendCmd(WM_CMD_LED, @buf, 1);
  end;
end;

function TWiimote.Connect: boolean;
var
  Addr: sockaddr_l2;
begin
  Addr.l2_family:=AF_BLUETOOTH;
  Addr.l2_bdaddr:=bdaddr;

  // OUTPUT CHANNEL
  OutSocket:=fpsocket(AF_BLUETOOTH, SOCK_SEQPACKET, BTPROTO_L2CAP);
  if (OutSocket = -1) then
    exit(false);

  {$IFDEF BIG_ENDIAN}
  {$ERROR ToDo BIG_ENDIAN}
  {$ENDIF}
  Addr.l2_psm := WM_OUTPUT_CHANNEL; // htobs

  // connect to wiimote
  if (fpconnect(OutSocket, psockaddr(@addr), SizeOf(addr)) < 0) then
    raise Exception.Create('fpconnect output');

  // INPUT CHANNEL
  InSocket:=fpsocket(AF_BLUETOOTH, SOCK_SEQPACKET, BTPROTO_L2CAP);
  if (InSocket = -1) then begin
    CloseSocket(OutSocket);
    OutSocket := -1;
    exit(false);
  end;

  Addr.l2_psm := WM_INPUT_CHANNEL; // htobs

  // connect to wiimote
  if (fpconnect(InSocket, psockaddr(@addr), SizeOf(addr)) < 0) then
  begin
    CloseSocket(OutSocket);
    OutSocket := -1;
    raise Exception.Create('fpconnect input');
  end;

  writeln('Connected to wiimote ',ID);

  // do the handshake
  Connected:=true;

  EnableHandshake;
  RealizeReportType;

  Result:=true;
end;

procedure TWiimote.Disconnect;
begin
  if not Connected then exit;

  CloseSocket(OutSocket);
  CloseSocket(InSocket);

  OutSocket:=-1;
  InSocket:=-1;

  Connected:=false;
  Handshaking:=false;
  HandshakeComplete:=false;
  FRealizedIR:=false;
end;

procedure TWiimote.EnableHandshake;
begin
  // send request to wiimote for accelerometer calibration

  Handshaking:=true;
  SetLEDs(WIIMOTE_LED_ALL);
  RequestRead(@OnHandShake,WM_MEM_OFFSET_CALIBRATION, 7);

  //WiiMote_read_data(wm, WiiMote_handshake, buf, WM_MEM_OFFSET_CALIBRATION, 7);
  //inc(WiiMote.Handshake_State);

  //WiiMote_set_leds(wm, WIIMOTE_LED_NONE);
end;

function TWiimote.RealizeReportType: boolean;
var
  buf: array[0..1] of byte;
begin
  if not Connected then exit(false);

  if Continuous then
    buf[0] := 4 // set to 0x04 for continuous reporting
  else
    buf[0] :=0;
  buf[1] := 0;

  // if rumble is enabled, make sure we keep it
  if Rumble then
    buf[0] := buf[0] or 1;

  if ReportMotion    then buf[1] := buf[1] or WM_RPT_BTN_ACC;
  if ReportExpansion then buf[1] := buf[1] or WM_RPT_BTN_EXP;
  if ReportIR        then buf[1] := buf[1] or WM_RPT_BTN_IR;
  writeln('TWiiMote.RealizeReportType ',buf[1]);

  if SendCmd(WM_CMD_REPORT_TYPE,@buf[0],2)<=0 then begin
    writeln('TWiiMote.RealizeReportType FAILED');
    exit(false);
  end;

  Result:=true;
end;

procedure TWiimote.RealizeIR;
var
  buf: byte;
begin
  if not HandshakeComplete then begin
    writeln('TWiiMote.EnableIR still handshaking');
    exit;
  end;
  
  if ReportIR=FRealizedIR then exit;

  writeln('TWiiMote.RealizeIR ReportIR=',ReportIR);
  
  // set camera 1 and 2
  if ReportIR then
    buf:=4
  else
    buf:=0;
  SendCmd(WM_CMD_IR,@buf,1);
  SendCmd(WM_CMD_IR_2,@buf,1);

  if ReportIR then begin
    // enable IR, set sensitivity
    buf:=8;
    SendData(WM_REG_IR,@buf,1);

    // wait for the wiimote to catch up
    Sleep(50);

    // write sensitivity blocks
    SendData(WM_REG_IR_BLOCK1, Pointer(WM_IR_BLOCK1_CLIFF), length(WM_IR_BLOCK1_CLIFF));
    SendData(WM_REG_IR_BLOCK2, Pointer(WM_IR_BLOCK2_CLIFF), length(WM_IR_BLOCK2_CLIFF));

    // set the IR mode
    if ReportExpansion then
      buf := WM_IR_TYPE_BASIC
    else
      buf := WM_IR_TYPE_EXTENDED;
    SendData(WM_REG_IR_MODENUM,@buf,1);

    // wait for the wiimote to catch up
    Sleep(50);

    // set the wiimote report type
    RealizeReportType;
    writeln('TWiiMote.RealizeIR IR enabled');
  end;
end;

procedure TWiimote.HandleEvents;
var
  Data: PByte;
  i: Integer;
begin
  // don't know what WiiMote.EventBuf[0] is
  case EventBuf[1] of

  WM_RPT_CTRL_STATUS:
    begin
      writeln('TWiiMote.HandleEvent WM_RPT_CTRL_STATUS');
    end;

  WM_RPT_READ:
    begin
      writeln('TWiiMote.HandleEvent WM_RPT_READ');
      HandleRead;
    end;

  WM_RPT_WRITE:
    begin
      writeln('TWiiMote.HandleEvent WM_RPT_WRITE');
    end;

  WM_RPT_BTN:
    begin
      Buttons:=PWord(@EventBuf[2])^ and WIIMOTE_BUTTON_ALL;
      writeln('TWiiMote.HandleEvent Button ',Buttons);
    end;

  WM_RPT_BTN_ACC or WM_RPT_BTN_IR:
    begin
      Buttons:=PWord(@EventBuf[2])^ and WIIMOTE_BUTTON_ALL;
      Accel.x:=EventBuf[4];
      Accel.y:=EventBuf[5];
      Accel.z:=EventBuf[6];
      // 7,8,9,10
      Data:=PByte(@EventBuf[7]);
      for i := 0 to 3 do begin
        Dots[i].x := 1023 - (data[3*i] or ((data[(3*i)+2] and $30) shl 4));
        Dots[i].y := data[(3*i)+1] or ((data[(3*i)+2] and $C0) shl 2);

        Dots[i].size := data[(3*i)+2] and $0F;

        // if in range set to visible
        Dots[i].visible := (Dots[i].y <> 1023);
        //if dot[i].visible then write(' ',dot[i].rx,' ',dot[i].ry,' ',dot[i].size,' ',dot[i].visible);
      end;
      //for i:=0 to 20 do write(IntToHex(ord(EventBuf[i]),2),' ');
      //writeln(' TWiiMotes.HandleEvent x=',Accel.x,' y=',Accel.y,' z=',Accel.z);
      //Sleep(300);
    end;

  else
    writeln('TWiiMotes.HandleEvent other decimal=',EventBuf[1],' hex=',IntToHex(EventBuf[1],2));
  end;
end;

procedure TWiimote.HandleRead;
var
  Error: byte;
  len: byte;
  offset: word;
  Request: TWiiMoteReadRequest;
begin
  // always assume the packet received is from the most recent request

  Buttons:=PWord(@EventBuf[2])^ and WIIMOTE_BUTTON_ALL;

  // if we don't have a request out then we didn't ask for this packet
  if FReadRequests.Count=0 then begin
    writeln('TWiiMote.HandleRead Received data packet whithout request');
    exit;
  end;

  Error := EventBuf[4] and $0F;

  if (Error = $08) then
    writeln('Unable to read data - address does not exist.')
  else if (Error = $07) then
    writeln('Unable to read data - address is for write-only registers.')
  else if (Error>0) then
    writeln('Unable to read data - unknown error code ',Error);

  if (Error>0) then begin
    // skip this request
    TObject(FReadRequests[0]).Free;
    FReadRequests.Delete(0);

    // send next request to wiimote
    if (FReadRequests.Count>0) then
      SendNextReadRequest;
    exit;
  end;

  len := ((EventBuf[4] and $F0) shr 4) + 1;
  offset := BEtoN(PWord(@EventBuf[5])^);
  Request:=TWiiMoteReadRequest(FReadRequests[0]);
  inc(Request.Received,len);

  if (Request.Received>Request.BufSize) then begin
    // this should never happen
    writeln('WARNING: TWiiMote.HandleRead Request.Received>Request.BufSize');
    Request.Received:=Request.BufSize;
  end;

  writeln('Received read packet:');
  writeln('  Packet read offset:   ',offset,' bytes');
  writeln('  Request read offset:  ',Request.Addr,' bytes');
  writeln('  Read offset into buf: ',offset-Request.Addr,' bytes');
  writeln('  Read data size:       ',len,' bytes');
  writeln('  Still need:           ',Request.BufSize-Request.Received,' bytes');

  // reconstruct this part of the data
  System.Move(EventBuf[7],(Request.Buf+offset-Request.Addr)^,len);

  // if all data has been received, execute the read event callback
  if Request.Received=Request.BufSize then begin
    Request.Callback(Request);
    if TWiiMoteReadRequest(FReadRequests[0])<>Request then
      raise Exception.Create('TWiiMote.HandleRead inconsistency');

    // skip this request
    Request.Free;
    FReadRequests.Delete(0);

    // send next request to wiimote
    if (FReadRequests.Count>0) then
      SendNextReadRequest;
  end;
end;

function TWiimote.SendCmd(ReportType: byte; Msg: PByte; Count: integer): PtrInt;
var
  Buf: array[0..31] of byte;
  CurRumble: boolean;
begin
  writeln('TWiiMote.SendCmd ReportType=',ReportType,' Count=',Count);
  buf[0] := WM_SET_REPORT or WM_BT_OUTPUT;
  buf[1] := ReportType;

  CurRumble:=false;
  case ReportType of
  WM_CMD_LED,
  WM_CMD_RUMBLE,
  WM_CMD_CTRL_STATUS:
    begin
      // Rumble flag for: =$11, =$13, =$14, =$15, =$19 or =$1a
      if Rumble then
        CurRumble := true;
    end;
  end;

  System.Move(Msg^,Buf[2],Count);
  if CurRumble then
    buf[2] := buf[2] or 1;
  Result:=fpsend(OutSocket,@Buf[0],Count+2,0);
end;

function TWiimote.SendData(Addr: cuint; Data: Pointer; Count: byte): PtrInt;
var
  Buf: array[0..20] of byte;
begin
  writeln('TWiiMote.SendData Addr=',IntToHex(Addr,8),' Count=',Count);
  if Count>16 then
    raise Exception.Create('TWiiMote.SendData too many bytes');
  Buf[0]:=0;
  FillByte(Buf[0],21,0);
  PDWord(@Buf[0])^:=NtoBE(cardinal(Addr));// Addr as big endian
  Buf[4]:=Count;
  System.Move(Data^,Buf[5],Count);
  Result:=SendCmd(WM_CMD_WRITE_DATA,@Buf[0],21);
end;

function TWiimote.RequestRead(const Callback: TWiiMoteReadCallback;
  Addr: cuint; BufSize: cushort): TWiiMoteReadRequest;
begin
  Result:=nil;
  if not Connected then exit;
  if (BufSize=0) or (not Assigned(Callback)) then exit;
  Result:=TWiiMoteReadRequest.Create(Callback,Addr,BufSize);
  FReadRequests.Add(Result);
  if FReadRequests.Count=1 then begin
    // this is the only request => send immediately
    SendNextReadRequest;
  end;
end;

procedure TWiimote.SendNextReadRequest;
var
  Request: TWiiMoteReadRequest;
  buf: array[1..6] of byte;
begin
  if (FReadRequests.Count=0) or (not Connected) then exit;
  Request:=TWiiMoteReadRequest(FReadRequests.First);
  // addr and bufsize in big endian
  PDWord(@buf[1])^:=NtoBE(cardinal(Request.Addr));
  PWord(@buf[5])^:=NtoBE(word(Request.BufSize));
  writeln('TWiiMote.SendNextReadRequest Addr=',Request.Addr,' BufSize=',Request.BufSize);
  SendCmd(WM_CMD_READ_DATA, @buf[1], 6);
end;

procedure TWiimote.OnHandShake(Request: TWiiMoteReadRequest);
begin
  writeln('TWiiMote.OnHandShake ');

  // received read data
  AccelCalibration.cal_zero.x := Request.buf[0];
  AccelCalibration.cal_zero.y := Request.buf[1];
  AccelCalibration.cal_zero.z := Request.buf[2];

  AccelCalibration.cal_g.x := Request.buf[4] - AccelCalibration.cal_zero.x;
  AccelCalibration.cal_g.y := Request.buf[5] - AccelCalibration.cal_zero.y;
  AccelCalibration.cal_g.z := Request.buf[6] - AccelCalibration.cal_zero.z;

  // handshake complete
  writeln('TWiiMote.OnHandShake Calibration: Idle: ',
          ' x=',AccelCalibration.cal_zero.x,
          ' y=',AccelCalibration.cal_zero.y,
          ' z=',AccelCalibration.cal_zero.z,
          ' +1g: ',
          ' x=',AccelCalibration.cal_g.x,
          ' y=',AccelCalibration.cal_g.y,
          ' z=',AccelCalibration.cal_g.z
          );

  // request the status of the wiimote to see if there is an expansion
  // ToDo

  Handshaking:=false;
  HandshakeComplete:=true;

  // now enable IR
  if ReportIR then begin
    RealizeIR;
  end;
end;

constructor TWiimote.Create;
begin
  InSocket:=-1;
  OutSocket:=-1;
  FReadRequests:=TFPList.Create;
end;

destructor TWiimote.Destroy;
var
  i: Integer;
begin
  for i:=0 to FReadRequests.Count-1 do TObject(FReadRequests[i]).Free;
  FreeAndNil(FReadRequests);
  inherited Destroy;
end;

{ TWiiMoteReadRequest }

constructor TWiiMoteReadRequest.Create(const TheCallback: TWiiMoteReadCallback;
  TheAddr: cardinal; TheBufSize: word);
begin
  Callback:=TheCallback;
  Addr:=TheAddr;
  BufSize:=TheBufSize;
  ReAllocMem(Buf,BufSize);
end;

destructor TWiiMoteReadRequest.Destroy;
begin
  ReAllocMem(Buf,0);
  inherited Destroy;
end;

end.

