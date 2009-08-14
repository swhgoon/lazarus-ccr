(*******************************************************************************
 *  TestListenMain.pas: Program to test BeepFp classes
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
 *  TestListenMain is an application used to test the BeepFp classes, mainly
 *  used during development. To see how to use the classes in a real application,
 *  see BEEP_Client and BEEP_Listen
 ******************************************************************************)
 unit TestListenMain;

{$mode objfpc}{$H+}

{TODO: list of remaining library problems}
{
  - Valgrind report reachable memory. Find out where that is, maybe in LCL.
  - Reachable memory because threading doesn't clear all at program stop.

}
interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs,
  BeepContext, BeepListener, BeepConnection, BeepProfile,
  BeepChannel, BeepFrame;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonSendMsg: TButton;
    ButtonChanClose: TButton;
    ButtonChanNew: TButton;
    ButtonPoolNew: TButton;
    ButtonPoolClose: TButton;
    ButtonStart: TButton;
    ButtonStop: TButton;
    CheckBoxListen: TCheckBox;
    Edit1: TEdit;
    EditPoolSize: TEdit;
    procedure ButtonChanCloseClick(Sender: TObject);
    procedure ButtonChanNewClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonPoolNewClick(Sender: TObject);
    procedure ButtonSendMsgClick(Sender: TObject);
    procedure ChannelClose(ChanNum: integer);
    procedure ChannelCreated(ChanNum: integer);
    procedure ChannelFailed(ChanNum: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListenAcceptConnection(aConnection: TBeepConnectionListener;
      var Accept: boolean);
    procedure ListenConnectionClose(aConnection: TBeepConnection);
    procedure ListenFailed(aListener: TObject; const ErrMsg: string);
    procedure ListenReady(aListener: TObject);
    procedure ListenStop(aListener: TObject);
    procedure PoolPoolCreated(Sender: TObject);
    procedure ProfileFrameReceived(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
  private
    Ctx: TBeepContext;
    Listen: TBeepListener;
    Connection: TBeepConnection;
    Profile: TBeepProfile;
    //Pool: TBeepChannelPool;
    Channel: TBeepChannelInitiator;
  public
    { public declarations }
  end; 

const
  PLAIN_PROFILE = 'http://fact.aspl.es/profiles/plain_profile';
var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ButtonChanCloseClick(Sender: TObject);
begin
  Channel.Free;
end;

procedure TForm1.ButtonChanNewClick(Sender: TObject);
begin
  Channel := TBeepChannelInitiator.Create(Connection, Profile.Name, 1);
  Channel.OnCreated := @ChannelCreated;
  Channel.OnFailed  := @ChannelFailed;
  Channel.OnCloseNotify  := @ChannelClose;
  Channel.OpenChannel;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  Listen.Start;
  //Connection.Connect;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  Listen.Stop;
end;

procedure TForm1.ButtonPoolNewClick(Sender: TObject);
begin
  EditPoolSize.Text := '';
  //Pool.Initialise;
end;

procedure TForm1.ButtonSendMsgClick(Sender: TObject);
var
  MsgID: integer = 0;
begin
  Channel.SendMSG(PByte(PChar(Edit1.Text)), length(Edit1.Text), MsgID);
end;

procedure TForm1.ChannelClose(ChanNum: integer);
begin
  writeln('Closed channel '+IntToStr(ChanNum));
end;

procedure TForm1.ChannelCreated(ChanNum: integer);
begin
  writeln('Created channel '+inttostr(ChanNum));
end;

procedure TForm1.ChannelFailed(ChanNum: integer);
begin
  writeln('Failed to create channel '+IntToStr(ChanNum));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Ctx := TBeepContext.Create;

  Listen := TBeepListener.Create(Ctx);
  Listen.Port := '3000';
  Listen.OnReady  := @ListenReady;
  Listen.OnFailed := @ListenFailed;
  Listen.OnStop   := @ListenStop;
  Listen.OnConnectionAccept  := @ListenAcceptConnection;
  Listen.OnConnectionClose  := @ListenConnectionClose;

//  Connection := TBeepConnection.Create(Ctx, 'localhost', '3000');
//  Connection.OnConnected  := @ConnectionConnected;
////  Connection.OnConnectionFailed  := @ConnectionConnectionFailed;
//  Connection.OnDisconnected  := @ConnectionDisconnected;

  //Profile := TBeepProfile.Create(Ctx, PLAIN_PROFILE);
  //Profile.OnFrameReceived    := @ProfileFrameReceived;
  //Profile.RegisterProfile;

  //Pool := TBeepChannelPool.Create(Connection, Profile, 3);
  //Pool.OnPoolCreated    := @PoolPoolCreated;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //Pool.Free;
  //Profile.Free;
  //Connection.Free;
  Listen.Free;
  Ctx.Free;
end;

procedure TForm1.ListenAcceptConnection(aConnection: TBeepConnectionListener;
  var Accept: boolean);
begin
  //writeln('Accepted connection from: '+aConnection.Host+':'+aConnection.Port);
end;

procedure TForm1.ListenConnectionClose(aConnection: TBeepConnection);
begin
  //writeln('Disconnected connection from: '+aConnection.Host+':'+aConnection.Port);
end;

procedure TForm1.ListenFailed(aListener: TObject; const ErrMsg: string);
begin
  //writeln('Listener creation failed. Reason: '+ErrMsg);
  CheckBoxListen.Checked := false;
end;

procedure TForm1.ListenReady(aListener: TObject);
//var
//  Listener: TBeepListener;
begin
  //Listener := TBeepListener(aListener);
  //writeln('Listener ready on '+Listener.Host+':'+Listener.Port);
  CheckBoxListen.Checked := true;
end;

procedure TForm1.ListenStop(aListener: TObject);
//var
//  Listener: TBeepListener;
begin
  //Listener := TBeepListener(aListener);
  //writeln('Listener stopped on '+Listener.Host+':'+Listener.Port);
  CheckBoxListen.Checked := false;
end;

procedure TForm1.PoolPoolCreated(Sender: TObject);
begin
  //EditPoolSize.Text := IntToStr(Pool.Count);
end;

procedure TForm1.ProfileFrameReceived(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
begin
  writeln(aConnection.Host+':'+aConnection.Port+
    ' Channel '+IntToStr( aChannel.Number)+
    ' Message: ' + string(PCHar(aFrame.PayloadAsByteArray)) );
end;


initialization
  {$I testlistenmain.lrs}

end.

