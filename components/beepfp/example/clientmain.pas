(*******************************************************************************
 *  ClientMain.pas: Client test application for BeepFp
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
 *  This is an example of using TBeepClient class
 ******************************************************************************)
unit clientmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Spin, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs,
  BeepClient, BeepProfile, BeepConnection, BeepChannel, BeepFrame,
  BeepChannelPool;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonClose: TButton;
    ButtonOpen: TButton;
    ButtonAddPool: TButton;
    ButtonSend: TButton;
    ButtonDisconnect: TButton;
    Button1Connect: TButton;
    ButtonSendPool: TButton;
    CheckBoxPool: TCheckBox;
    CheckBoxCon: TCheckBox;
    EditChannel: TEdit;
    EditPool: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEditCh: TSpinEdit;
    procedure Button1ConnectClick(Sender: TObject);
    procedure ButtonAddPoolClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonSendPoolClick(Sender: TObject);
    procedure Chan1Closed(ChanNum: integer);
    procedure Chan1Created(ChanNum: integer);
    procedure Chan1Failed(ChanNum: integer);
    procedure ChanDropped(aBeepChannel: TObject);
    procedure ClientConnected(Sender: TBeepConnection);
    procedure ClientConnectionFailed(Sender: TBeepConnection; Reason: string);
    procedure ClientDisconnected(Sender: TBeepConnection);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PoolCreated(Sender: TObject);
    procedure ProfPlainCloseChannel(aConnection: TBeepConnection;
      ChanNum: integer; var Accept: boolean);
    procedure ProfPlainFrameReceive(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
    procedure ProfPlainStartChannel(aConnection: TBeepConnection;
      ChanNum: integer; var Accept: boolean);
    procedure ProfPoolFrameReceive(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
  private
    Client: TBeepClient;

  public
    { public declarations }
  end; 

const
  PLAIN_PROFILE = 'http://fact.aspl.es/profiles/plain_profile';
  POOL_PROFILE  = 'http://lazarus/beep/Plain_Profile';
var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1ConnectClick(Sender: TObject);
begin
  Client.Connect;
end;

procedure TForm1.ButtonAddPoolClick(Sender: TObject);
var
  Pool: TBeepChannelPool;
  Index: integer = 0;
begin
  CheckBoxPool.Checked := false;

  //Add a pool
  Pool := Client.AddChannelPool(POOL_PROFILE, 3, Index);

  //Add event handlers
  Pool.OnPoolCreated  := @PoolCreated;

  //Initialise
  Pool.Initialise;
end;

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
  Client.CloseChannel(1);
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  Client.Disconnect;
end;

procedure TForm1.ButtonOpenClick(Sender: TObject);
var
  Chan: integer;
  NewChan: TBeepChannelInitiator;
begin
  Chan := SpinEditCh.Value;

  //Add channel
  NewChan := Client.AddChannel(Client.Connection, PLAIN_PROFILE, Chan);

  //Add optional event handlers
  NewChan.OnCreated := @Chan1Created;
  NewChan.OnFailed  := @Chan1Failed;
  NewChan.OnCloseNotify  := @Chan1Closed;
  NewChan.OnDropped  := @ChanDropped;

  Client.OpenChannel(Chan);
end;

procedure TForm1.ButtonSendClick(Sender: TObject);
var
  ID: integer = 0;
begin
  Client.SendMSG(SpinEditCh.Value, PByte(PChar(EditChannel.Text)), length(EditChannel.Text), ID);
end;

procedure TForm1.ButtonSendPoolClick(Sender: TObject);
var
  Channel: TBeepChannel;
  ID: integer = 0;
begin
  //Get next available channel from pool
  Channel := Client.GetPoolChannel(0, false);

  //Send the message
  Channel.SendMSG(EditPool.Text, ID);
end;

procedure TForm1.Chan1Closed(ChanNum: integer);
begin
  writeln('Closed channel ' + IntToStr(ChanNum));
end;

procedure TForm1.Chan1Created(ChanNum: integer);
begin
  writeln('Opened channel ' + IntToStr(ChanNum));
end;

procedure TForm1.Chan1Failed(ChanNum: integer);
begin
  writeln('Failed to open channel ' + IntToStr(ChanNum));
end;

procedure TForm1.ChanDropped(aBeepChannel: TObject);
begin
  //
end;

procedure TForm1.ClientConnected(Sender: TBeepConnection);
begin
  CheckBoxCon.Checked := true;
  writeln('Connected to '+Sender.Host+':'+Sender.Port);
end;

procedure TForm1.ClientConnectionFailed(Sender: TBeepConnection;
  Reason: string);
begin
  CheckBoxCon.Checked := false;
  writeln('Connection failed to '+Sender.Host+':'+Sender.Port);
end;

procedure TForm1.ClientDisconnected(Sender: TBeepConnection);
begin
  CheckBoxCon.Checked := false;
  writeln('Disconnected from '+Sender.Host+':'+Sender.Port);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  NewProf: TBeepProfile;
begin
  { Steps to prepare a client:
    * Create client
    * Set host, port
    * Set (optional) OnConnected, OnDisconnected, OnConnectionFailed
    * Add a profile
    * Set for profile (required): OnFrameReceived
    * Set for profile (optional): OnStartChannel, OnCloseChannel
    * Connect

    With open connection:
    * Add channel(s)
    * Set (optional) OnCreated, OnFailed, OnClosed
    * Open channel(s)
    * Send MSG, RPY, ANS, ERR

    * Add channel pool(s)
    * Set (optional) OnPoolCreated
    * Initialise pool(s)
    * Get a channel
    * Send Msg
    * Release channel
  }

  //Create a client
  Client := TBeepClient.Create;

  //Set host, port
  Client.Host := 'localhost';
  Client.Port := '44000';

  //Add optional event handlers
  Client.OnConnected        := @ClientConnected;
  Client.OnDisconnected     := @ClientDisconnected;
  Client.OnConnectionFailed := @ClientConnectionFailed;

  //Add a profile
  NewProf := Client.AddProfile(PLAIN_PROFILE);

  //Add required event handler
  NewProf.OnFrameReceived    := @ProfPlainFrameReceive;

  //Add optional event handlers
  NewProf.OnStartChannelRequest := @ProfPlainStartChannel;
  NewProf.OnCloseChannelRequest := @ProfPlainCloseChannel;

  //Add a profile
  NewProf := Client.AddProfile(POOL_PROFILE);

  //Add required event handler
  NewProf.OnFrameReceived     := @ProfPoolFrameReceive;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
end;

procedure TForm1.PoolCreated(Sender: TObject);
begin
  CheckBoxPool.Checked := true;
  writeln('A new channel pool is ready for use');
end;

procedure TForm1.ProfPlainCloseChannel(aConnection: TBeepConnection;
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

procedure TForm1.ProfPlainFrameReceive(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
begin
  { Implement the profile here.
    This function must respond to message frames using
    - SendRPy
    - SendANS
    - SendERR
  }

  //MemoLog.Lines.Add
  writeln
  (format('A frame received on channel: %d', [aChannel.Number] ) );

  //MemoLog.Lines.Add
  writeln
  (format('Data received: "%s"', [aFrame.PayloadAsString] ) );

end;

procedure TForm1.ProfPlainStartChannel(aConnection: TBeepConnection;
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

procedure TForm1.ProfPoolFrameReceive(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
begin
  {Profile used for the pool channels}

  //MemoLog.Lines.Add
  writeln
  (format('A frame received on pool channel: %d', [aChannel.Number] ) );

  //MemoLog.Lines.Add
  writeln
  (format('Data received: "%s"', [aFrame.PayloadAsString] ) );

  {Release the channel back to the pool}
  Client.ReleasePoolChannel(0, aChannel);
end;

initialization
  {$I clientmain.lrs}

end.

