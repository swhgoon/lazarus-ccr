(*
 *  BeepProfileBase.pas: Base class for Beep profiles
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
 *)

unit BeepProfileBase;

{TODO: add file desc header}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepFrame;

type
  { Event types}
  TVortexCloseChannel  = procedure (ChanNum: integer; Connection: PVortexConnection; var Accept: boolean) of object;
  //TVortexFrameReceived = procedure (Channel: PVortexChannel; Connection: PVortexConnection; Frame: PVortexFrame) of object;
  TVortexFrameReceived = procedure (Frame: TBeepFrame) of object;

  { TBeepProfileBase }

  TBeepProfileBase = class(TObject)
  private
    FOnCloseChannel: TVortexCloseChannel;
    FOnFrameReceived: TVortexFrameReceived;

    //Event triggers
    procedure DoCloseChannel(ChanNum: integer; Connection: PVortexConnection; var Accept: boolean);
    //procedure DoFrameReceived(Channel: PVortexChannel; Connection: PVortexConnection; Frame: PVortexFrame);
    procedure DoFrameReceived(aFrame: TBeepFrame);

    //Event handlers
    procedure BeepCloseChannel(ChanNum: integer; Connection: PVortexConnection; var Accept: boolean);cdecl;
    procedure BeepFrameReceived(Channel: PVortexChannel; Connection: PVortexConnection; Frame: PVortexFrame);
  public
    //Events
    property OnCloseChannel: TVortexCloseChannel read FOnCloseChannel write FOnCloseChannel;
    property OnFrameReceived: TVortexFrameReceived read FOnFrameReceived write FOnFrameReceived;
  end;

{ Vortex Callbacks }

function VortexCloseChannel(Channel_num: longint; Connection: PVortexConnection;
  User_data: TaxlPointer): longint; cdecl;

procedure VortexFrameReceived (Channel: PVortexChannel; Connection: PVortexConnection;
  Frame: PVortexFrame; User_data: TaxlPointer); cdecl;


implementation

uses
  BeepUtils;

{ Vortex Callbacks }

//* Implementation note:
//* Vortex library can't accept methods as callback functions. To allow multiple
//* instantiations of the class, global functions are used as callbacks. The
//* object references are passed to the global functions to allow the correct
//* event handlers to be called.

//User_data points to profile's TBeepProfile object (ie owning profile)
function VortexCloseChannel(Channel_num: longint; Connection: PVortexConnection;
  User_data: TaxlPointer): longint; cdecl;
var
  Prof: TBeepProfileBase;
  Accept: boolean;
begin
  //Default response.
  Accept := true;

  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Prof := TBeepProfileBase(User_data);
    Prof.BeepCloseChannel(Channel_num, Connection, Accept);
  end;

	//Return axl_false to deny channel closure
  //Return axl_true to allow new channel closure
  Result :=  VortexBool(Accept);
end;

//User_data points to profile's TBeepProfile object (ie owning profile)
procedure VortexFrameReceived (Channel: PVortexChannel; Connection: PVortexConnection;
  Frame: PVortexFrame; User_data: TaxlPointer); cdecl;
var
  Prof: TBeepProfileBase;
begin
  //Execute user assigned event handler
  if assigned(User_data) then
  begin
    Prof := TBeepProfileBase(User_data);
    Prof.BeepFrameReceived(Channel, Connection, Frame);
  end;
end;

{ TBeepProfileBase }

procedure TBeepProfileBase.BeepCloseChannel(ChanNum: integer;
  Connection: PVortexConnection; var Accept: boolean);        cdecl;
begin
  //Trigger event
  DoCloseChannel(ChanNum, Connection, Accept);
end;

procedure TBeepProfileBase.BeepFrameReceived(Channel: PVortexChannel;
  Connection: PVortexConnection; Frame: PVortexFrame);
begin
  //Trigger event
  //DoFrameReceived(Channel, Connection, Frame);
  //DoFrameReceived();
end;

procedure TBeepProfileBase.DoCloseChannel(ChanNum: integer;
  Connection: PVortexConnection; var Accept: boolean);
begin
  if assigned(FOnCloseChannel) then
    FOnCloseChannel(ChanNum, Connection, Accept);
end;

procedure TBeepProfileBase.DoFrameReceived(aFrame: TBeepFrame);
begin
  if assigned(FOnFrameReceived) then
    FOnFrameReceived(aFrame);
end;

end.

