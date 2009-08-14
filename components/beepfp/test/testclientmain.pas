(*******************************************************************************
 *  TestClientMain.pas: Program to test BeepFp classes
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
 *  TestClientMain is an application used to test the BeepFp classes, mainly
 *  used during development. To see how to use the classes in a real application,
 *  see BEEP_Client and BEEP_Listen
 ******************************************************************************)
 unit TestClientMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs,
  Vortex, BeepContext, BeepConnection, BeepProfile,
  BeepChannel, BeepFrame, BeepChannelPool;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonDelCh: TButton;
    ButtonPoolCount: TButton;
    ButtonPoolAdd: TButton;
    ButtonSendMsg: TButton;
    ButtonChanClose: TButton;
    ButtonChanNew: TButton;
    ButtonPoolNew: TButton;
    ButtonCon: TButton;
    ButtonDiscon: TButton;
    ButtonSendMsgPool: TButton;
    CheckBoxCon: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    EditPoolSize: TEdit;
    procedure ButtonChanCloseClick(Sender: TObject);
    procedure ButtonChanNewClick(Sender: TObject);
    procedure ButtonConClick(Sender: TObject);
    procedure ButtonDelChClick(Sender: TObject);
    procedure ButtonDisconClick(Sender: TObject);
    procedure ButtonPoolAddClick(Sender: TObject);
    procedure ButtonPoolCountClick(Sender: TObject);
    procedure ButtonPoolNewClick(Sender: TObject);
    procedure ButtonSendMsgClick(Sender: TObject);
    procedure ButtonSendMsgPoolClick(Sender: TObject);
    procedure ChannelClose(ChanNum: integer);
    procedure ChannelCreated(ChanNum: integer);
    procedure ChannelFailed(ChanNum: integer);
    procedure ConnectionConnected(Sender: TObject);
    procedure ConnectionConnectionFailed(Sender: TObject);
    procedure ConnectionDisconnected(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PoolPoolCreated(Sender: TObject);
    procedure ProfileFrameReceived(aConnection: TBeepConnection;
      aChannel: TBeepChannel; aFrame: TBeepFrame);
  private
    Ctx: TBeepContext;
    Connection: TBeepConnectionInitiator;
    Profile: TBeepProfile;
    Pool: TBeepChannelPool;
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
  Channel.OnCloseNotify := @ChannelCreated;
  Channel.OnFailed    := @ChannelFailed;
  Channel.OnCloseNotify  := @ChannelClose;
  Channel.OpenChannel;
end;

procedure TForm1.ButtonConClick(Sender: TObject);
begin
  Connection.Connect;
end;

procedure TForm1.ButtonDelChClick(Sender: TObject);
begin
  Pool.RemoveChannels(1);
end;

procedure TForm1.ButtonDisconClick(Sender: TObject);
begin
  Connection.Disconnect;
end;

procedure TForm1.ButtonPoolAddClick(Sender: TObject);
begin
  Pool.AddChannels(2);
end;

procedure TForm1.ButtonPoolCountClick(Sender: TObject);
begin
  EditPoolSize.Text := IntToStr(Pool.Count);
end;

procedure TForm1.ButtonPoolNewClick(Sender: TObject);
begin
  EditPoolSize.Text := '';
  Pool.Initialise;
end;

procedure TForm1.ButtonSendMsgClick(Sender: TObject);
var
  MsgID: integer = 0;
begin
  Channel.SendMSG(PByte(PChar(Edit1.Text)), length(Edit1.Text), MsgID);
end;

procedure TForm1.ButtonSendMsgPoolClick(Sender: TObject);
var
  Chan: TBeepChannel;
  ID: integer = 0;
begin
  Chan := Pool.GetNextReady(false);
  writeln(Format('Got channel %d from pool', [Chan.Number]));

  Chan.SendMSG(Edit2.Text, ID);

  Pool.ReleaseChannel(Chan);
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

procedure TForm1.ConnectionConnected(Sender: TObject);
begin
  CheckBoxCon.Checked := true;
end;

procedure TForm1.ConnectionConnectionFailed(Sender: TObject);
begin
  CheckBoxCon.Checked := false;
end;

procedure TForm1.ConnectionDisconnected(Sender: TObject);
begin
  CheckBoxCon.Checked := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Ctx := TBeepContext.Create;

  Connection := TBeepConnectionInitiator.Create(Ctx, 'localhost', '44000');
  Connection.OnConnected  := @ConnectionConnected;
//  Connection.OnConnectionFailed  := @ConnectionConnectionFailed;
  Connection.OnDisconnected  := @ConnectionDisconnected;

  Profile := TBeepProfile.Create(Ctx, PLAIN_PROFILE, nil, nil);
  Profile.OnFrameReceived    := @ProfileFrameReceived;
  Profile.RegisterProfile;

  Pool := TBeepChannelPool.Create(Connection, Profile.Name, 3);
  Pool.OnPoolCreated    := @PoolPoolCreated;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Pool.Free;
  Profile.Free;
  Connection.Free;
  Ctx.Free;
end;

procedure TForm1.PoolPoolCreated(Sender: TObject);
begin
  EditPoolSize.Text := IntToStr(Pool.Count);
end;

procedure TForm1.ProfileFrameReceived(aConnection: TBeepConnection;
  aChannel: TBeepChannel; aFrame: TBeepFrame);
begin
  writeln(aConnection.Host+':'+aConnection.Port+
    ' Channel '+IntToStr( aChannel.Number)+
    ' Message: ' + string(PCHar(aFrame.PayloadAsByteArray)) );
end;


initialization
  {$I testclientmain.lrs}

end.

