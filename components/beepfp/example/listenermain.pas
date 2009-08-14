(*******************************************************************************
 *  ListenerMain.pas: Listener test application for BeepFp
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
 *  This is an example of using the TBeepServer class
 ******************************************************************************)
 unit ListenerMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs,
  BeepProfile, BeepListener, BeepConnection, BeepChannel,
  BeepServer, BeepFrame;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBoxActive: TCheckBox;
    EditOpen: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    MemoLog: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Prof1CloseChannelRequest(aConnection: TBeepConnection; ChanNum: integer;
      var Accept: boolean);
    procedure Prof1FrameReceive(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
    procedure Prof1StartChannelRequest(aConnection: TBeepConnection; ChanNum: integer;
      var Accept: boolean);
    procedure Prof2FrameReceive(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
    procedure ServerConnectionAccept(aConnection: TBeepConnectionListener;
      var Accept: boolean);
    procedure ServerConnectionClose(aConnection: TBeepConnection);
    procedure ServerListenerFailed(aListener: TBeepListener;
      const ErrMsg: string);
    procedure ServerListenerReady(aListener: TBeepListener);
    procedure ServerListenerStop(aListener: TBeepListener);
  private
    Server: TBeepServer;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

const
  PLAIN_PROFILE = 'http://fact.aspl.es/profiles/plain_profile';

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  Server.StartAll;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Server.StopAll;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  NewProf: TBeepProfile;
begin
  { Steps to prepare a server:
    * Create server object

    * Add profile(s)
    * Set for profile (required): OnFrameReceived
    * Set for profile (optional): OnStartChannel, OnCloseChannel

    * Add port listener
    * Set (optional): OnListenerReady, OnListenerFailed, OnListenerStop
    * Set (optional): OnConnectionAccept, OnConnectionClose
    * Start server
  }

  //Create server
  Server := TBeepServer.Create;

  //Add profile 1
  NewProf := Server.AddProfile('http://lazarus/beep/Plain_Profile');

  //Set required handlers for profile 1
  NewProf.OnFrameReceived  := @Prof1FrameReceive;

  //Set optional handlers for profile 1
  NewProf.OnStartChannelRequest  := @Prof1StartChannelRequest;
  NewProf.OnCloseChannelRequest  := @Prof1CloseChannelRequest;

  //Add profile 2
  NewProf := Server.AddProfile(PLAIN_PROFILE);

  //Set required handlers for profile 2
  NewProf.OnFrameReceived  := @Prof2FrameReceive;

  //Add port listener
  Server.AddListener('localhost', '44000');

  //Set optional handlers
  Server.OnListenerReady    := @ServerListenerReady;
  Server.OnListenerFailed   := @ServerListenerFailed;
  Server.OnListenerStop     := @ServerListenerStop;
  Server.OnConnectionAccept := @ServerConnectionAccept;
  Server.OnConnectionClose  := @ServerConnectionClose;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
end;

procedure TForm1.Prof1CloseChannelRequest(aConnection: TBeepConnection;
  ChanNum: integer; var Accept: boolean);
begin
  { Implement profile requirements to check whether the request to close the
    channel should be allowed }

  //MemoLog.Lines.Add
  writeln
  (format('Closing channel %s on connection %s:%s',
                      		 [IntToStr(ChanNum),
                           aConnection.Host,
                           aConnection.Port]));

  { To close channel, set Accept := true, := false to deny
    Default behaviour when this function is not implemented, is to allow all
    closure requests }
  Accept := true;
end;

procedure TForm1.Prof1FrameReceive(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
var
  Reply: string;
begin
  { Implement the profile here.
    This function must respond to message frames using
    - aChannel.SendRPY
    - aChannel.SendERR
    - aChannel.SendANS
    - aChannel.SendANSFinal

    For meaning and usage of these functions, see:
    http://www.aspl.es/fact/files/af-arch/vortex-1.1/html/starting_to_program.html#vortex_manual_sending_frames
  }

  //MemoLog.Lines.Add
  writeln
  (format('A frame received on channel: %d', [aChannel.Number] ) );

  MemoLog.Lines.Add
  //writeln
  (format('Data received: "%s"', [aFrame.PayloadAsString] ) );

	{ reply the peer client with the same content }
  Reply := Format('Received Ok: %s', [aFrame.PayloadAsString]);
  aChannel.SendRPY(PByte(PChar(Reply)), length(Reply), aFrame.MsgNum);
end;

procedure TForm1.Prof1StartChannelRequest(aConnection: TBeepConnection;
  ChanNum: integer; var Accept: boolean);
begin
  { Implement profile requirements to check whether the request for a new
    channel should be allowed }

  //MemoLog.Lines.Add
  writeln
  (format('Starting channel %s on connection %s:%s',
                      		 [IntToStr(ChanNum),
                           aConnection.Host,
                           aConnection.Port]));

  { To allow channel, set Accept := true, := false to deny
    Default behaviour when this function is not implemented, is to allow all
    channels }
  Accept := true;
end;

procedure TForm1.Prof2FrameReceive(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
var
  Reply: string;
begin
  //MemoLog.Lines.Add
  writeln
  (format('A frame received on channel: %d', [aChannel.Number] ) );

  //MemoLog.Lines.Add
  writeln
  (format('Data received: "%s"', [aFrame.PayloadAsString] ) );

	{ reply the peer client with the same content }
  Reply := Format('Received Ok: %s', [aFrame.PayloadAsString]);
  aChannel.SendRPY(PByte(PChar(Reply)), length(Reply), aFrame.MsgNum);
end;

procedure TForm1.ServerConnectionAccept(aConnection: TBeepConnectionListener;
  var Accept: boolean);
begin
  { Implement requirements to check whether the connection should be accepted.}

  //EditOpen.Text :=  IntToStr( StrToInt(EditOpen.Text)+1);
  //MemoLog.Lines.Add
  writeln
  (format('New connection accepted from: %s:%s',
            [aConnection.Host, aConnection.Port]));

  { To accept the connection, set Action:=true. Otherwise action:=false.
    Default behaviour when this function is not implemented is to allow all
    connection requests}
  Accept := true;
end;

procedure TForm1.ServerConnectionClose(aConnection: TBeepConnection);
begin
  { This function is for information purpose only. It gets called when either
    the listener or the client actively closes a connection (and all contained
    channels) and when the listener realises that the connection was broken for
    another reason.}

  //EditOpen.Text :=  IntToStr( StrToInt(EditOpen.Text)-1);
  //MemoLog.Lines.Add
  writeln
  (format('Connection closed: %s:%s', [aConnection.Host, aConnection.Port]));
end;

procedure TForm1.ServerListenerFailed(aListener: TBeepListener;
  const ErrMsg: string);
begin
  CheckBoxActive.Checked := false;
  writeln('Listener creation failed for '+aListener.Host+':'+aListener.Port);
  writeln('Vortex reason: '+ ErrMsg);
end;

procedure TForm1.ServerListenerReady(aListener: TBeepListener);
begin
  CheckBoxActive.Checked := true;
  writeln('Listener ready on '+aListener.Host+':'+aListener.Port);
end;

procedure TForm1.ServerListenerStop(aListener: TBeepListener);
begin
  CheckBoxActive.Checked := false;
  writeln('Listener stopping on '+aListener.Host+':'+aListener.Port);
end;

initialization
  {$I listenermain.lrs}

end.

