{ Demonstrating reading the infrared sensors of a wii-remote.

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
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, FPimage, IntfGraphics, WiiMoteTools, StdCtrls;

type
  THeadTrackingDot = record
    X, Y: integer;
    Size: integer;// negative or 0 means not visible
  end;

  { TForm1 }

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    WorldPaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WorldPaintBoxPaint(Sender: TObject);
  private
    procedure ConnectWiiMotes;
    procedure DisconnectWiiMotes;
    procedure UpdateHeadtracking;
  public
    WiiMotes: TWiiMotes;
    Dots: array[1..4] of THeadTrackingDot;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConnectWiiMotes;
  Application.AddOnIdleHandler(@OnIdle);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DisconnectWiiMotes;
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var
  i: Integer;
begin
  if WiiMotes<>nil then begin
    for i:=1 to 100 do begin
      if not WiiMotes.HandleEvents then break;
      UpdateHeadtracking;
    end;
    Done:=false;
  end;
end;

procedure TForm1.WorldPaintBoxPaint(Sender: TObject);
var
  i: Integer;
  Size: Integer;
  X: Integer;
  Y: Integer;
begin
  //DebugLn(['TForm1.WorldPaintBoxPaint ']);
  if WiiMotes=nil then begin
    WorldPaintBox.Canvas.TextOut(10,200,'not connected');
  end else begin
    for i:=1 to 4 do begin
      if Dots[i].Size>0 then begin
        case i of
        1: Canvas.Brush.Color:=clYellow;
        2: Canvas.Brush.Color:=clRed;
        3: Canvas.Brush.Color:=clGreen;
        4: Canvas.Brush.Color:=clBlue;
        end;
        X:=(WorldPaintBox.Width*Dots[i].X) div 1024;
        Y:=(WorldPaintBox.Height*Dots[i].Y) div 768;
        Size:=Dots[i].Size;
        Canvas.Ellipse(X-Size,Y-Size,X+Size,Y+Size);
      end;
    end;
  end;
end;

procedure TForm1.ConnectWiiMotes;
var
  connected: LongInt;
  i: Integer;
begin
  WiiMotes:=TWiiMotes.Create;
  try
    WiiMotes.FindWiiMotes(5);
    connected := WiiMotes.Connect;
    if connected>0 then
      writeln('Connected to ',connected,' of ',WiiMotes.Count,' wiimotes.')
    else
      raise Exception.Create('Failed to connect to any wiimote.');

    // Now set the LEDs for a second so it's easy
    // to tell which wiimotes are connected
    for i:=0 to 3 do begin
      if i<WiiMotes.Count then
        case i of
        0: WiiMotes[i].SetLEDs(WIIMOTE_LED_1);
        1: WiiMotes[i].SetLEDs(WIIMOTE_LED_2);
        2: WiiMotes[i].SetLEDs(WIIMOTE_LED_3);
        3: WiiMotes[i].SetLEDs(WIIMOTE_LED_4);
        end;
    end;
    WiiMotes[0].ReportIR:=true;
    WiiMotes[0].ReportMotion:=true;
    WiiMotes[0].RealizeReportType;
    // already done in handshake, not needed here: WiiMotes[0].RealizeIR;
  except
    on E: Exception do begin
      DebugLn(['TForm1.ConnectWiiMotes ERROR: ',E.Message]);
      MessageDlg('Connection failed: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TForm1.DisconnectWiiMotes;
begin
  WiiMotes.Disconnect;
  FreeAndNil(WiiMotes);
end;

procedure TForm1.UpdateHeadtracking;
var
  WiiMote: TWiiMote;
  NewDots: array[1..4] of THeadTrackingDot;
  i: Integer;
  DotsChanged: Boolean;
begin
  NewDots[1].x:=0;
  FillByte(NewDots[1],SizeOf(THeadTrackingDot)*4,0);
  if (WiiMotes<>nil) and (WiiMotes.Count>0) and (WiiMotes[0].Connected) then
  begin
    WiiMote:=WiiMotes[0];
    for i:=1 to 4 do begin
      NewDots[i].x:=WiiMote.Dots[i-1].x;
      NewDots[i].y:=WiiMote.Dots[i-1].y;
      NewDots[i].Size:=WiiMote.Dots[i-1].Size;
      if not WiiMote.Dots[i-1].Visible then
        NewDots[i].Size:=0;
    end;
  end;
  // check for changes
  DotsChanged:=false;
  for i:=1 to 4 do begin
    if (Dots[i].X<>NewDots[i].X)
    or (Dots[i].Y<>NewDots[i].Y)
    or (Dots[i].Size<>NewDots[i].Size)
    then
      DotsChanged:=true;
    Dots[i]:=NewDots[i];
  end;
  //DebugLn(['TForm1.UpdateHeadtracking X=',Dots[1].X,' Y=',Dots[1].Y,' Size=',Dots[1].Size]);
  if DotsChanged then
    WorldPaintBox.Invalidate;
end;

initialization
  {$I mainunit.lrs}

end.

