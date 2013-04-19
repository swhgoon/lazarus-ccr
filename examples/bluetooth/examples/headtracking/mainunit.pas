{ Demonstrating VR Headtracking with a wii-remote.

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
  Classes, SysUtils, Math, LResources, LCLProc, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, FPimage, IntfGraphics, StdCtrls, LCLType,
  WiiMoteTools,
  OpenGLContext, Vectors, Asmoday, AsmTypes, AsmShaders;

type

  { THeadtrackingCamera }

  THeadtrackingCamera = class(TRotationCamera)
  public
    procedure RotateAboutView(Angle: single);
    procedure SetRoll(Angle: single);
  end;

  THeadTrackingDot = record
    X, Y: integer;
    Size: integer;// negative or 0 means not visible
  end;
  
  THeadTrackingDots = array[1..4] of THeadTrackingDot;
  
  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
  private
    fInitialized: Boolean;
    procedure ConnectWiiMotes;
    procedure DisconnectWiiMotes;
    procedure UpdateHeadtracking;
    procedure UpdateSceneHeadTracking;// set camera
    procedure Init;
  public
    EnableRotation: boolean;
  
    WiiMotes: TWiimotes;
    Dots: array[1..5] of THeadTrackingDots;

    Camera: THeadtrackingCamera;
    CameraAngleRot: single;
    CameraAngleX: single;
    CameraAngleY: single;
    Scene: TScene;

    // Die letzten Kameraeinstellungen ueber denen gemittelt wird:
    OldAngleX: array[0..4] of single;
    OldAngleY: array[0..4] of single;
    OldCamDist: array[0..4] of single;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl1:=TOpenGLControl.Create(Self);
  with OpenGLControl1 do begin
    Name:='OpenGLControl1';
    Align:=alClient;
    Parent:=Self;
    OnPaint:=@OpenGLControl1Paint;
    OnResize:=@OpenGLControl1Resize;
    OnKeyDown:=@OpenGLControl1KeyDown;
  end;

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

procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_ESCAPE: Close;
  VK_R:
    begin
      EnableRotation:=not EnableRotation;
      OpenGLControl1.Invalidate;
    end;
  end;
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  // Initialize the Scene if not already done
  if not fInitialized then Init;
  // update head tracking position
  UpdateSceneHeadTracking;
  // Render the scene
  Scene.RenderScene;
end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
  // on a resize we want to update our viewport and redraw it
  if fInitialized then begin
    Scene.UpdateViewport;
    OpenGLControl1.Invalidate;
  end;
end;

procedure TForm1.ConnectWiiMotes;
var
  connected: LongInt;
  i: Integer;
begin
  Wiimotes:=TWiimotes.Create;
  try
    Wiimotes.FindWiiMotes(5);
    connected := WiiMotes.Connect;
    if connected>0 then
      writeln('Connected to ',connected,' of ',WiiMotes.Count,' wiimotes.')
    else
      raise Exception.Create('Failed to connect to any wiimote.');

    // Now set the LEDs for a second so it's easy
    // to tell which wiimotes are connected
    for i:=0 to 3 do begin
      if i<Wiimotes.Count then
        case i of
        0: Wiimotes[i].SetLEDs(WIIMOTE_LED_1);
        1: Wiimotes[i].SetLEDs(WIIMOTE_LED_2);
        2: Wiimotes[i].SetLEDs(WIIMOTE_LED_3);
        3: Wiimotes[i].SetLEDs(WIIMOTE_LED_4);
        end;
    end;
    Wiimotes[0].ReportIR:=true;
    Wiimotes[0].ReportMotion:=true;
    Wiimotes[0].RealizeReportType;
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
    if (Dots[5][i].X<>NewDots[i].X)
    or (Dots[5][i].Y<>NewDots[i].Y)
    or (Dots[5][i].Size<>NewDots[i].Size)
    then
      DotsChanged:=true;
    Dots[5][i]:=NewDots[i];
  end;
  //DebugLn(['TForm1.UpdateHeadtracking ']);
  if DotsChanged then
    OpenGLControl1.Invalidate;
end;

procedure TForm1.UpdateSceneHeadTracking;
var
  Distance: single;
  CamDist: single;
  dx: Integer;
  dy: Integer;
  Angle: single;
  CenterX: Integer;
  CenterY: Integer;
  AngleX: single;
  AngleY: single;
  SmoothDots: THeadTrackingDots;
  TimeID: Integer;
  DotID: Integer;
  i: Integer;
  NewAngleX: single;
  NewAngleY: single;
  NewCamDist: single;
begin
  SmoothDots:=Dots[5];
  if (SmoothDots[1].Size>0) and (SmoothDots[2].Size>0) then begin
    // last point valid

    // compute average of last 5 states
    for DotID:=1 to 4 do begin
      for TimeID:=1 to 4 do begin
        inc(SmoothDots[DotID].X,Dots[TimeID][DotID].X);
        inc(SmoothDots[DotID].Y,Dots[TimeID][DotID].Y);
      end;
      SmoothDots[DotID].X:=SmoothDots[DotID].X div 5;
      SmoothDots[DotID].Y:=SmoothDots[DotID].Y div 5;
    end;
  
    // use the two best dots
    // x: 0-1023
    // y: 0-768
    
    dx:=SmoothDots[2].x-SmoothDots[1].x;
    dy:=SmoothDots[2].y-SmoothDots[1].y;

    // Distance: 170-800 map to 4-0.1
    Distance:=Max(1,Sqrt(Sqr(dx)+Sqr(dy)));
    CamDist:=Power(500/Distance,1.5);

    // Position
    // WiiMote has 45degree field of view. Map it to 90degree so the user can see more.
    CenterX:=(SmoothDots[1].x+SmoothDots[2].x) div 2;
    CenterY:=(SmoothDots[1].y+SmoothDots[2].y) div 2;
    CenterX:=CenterX-(1024 div 2);
    CenterY:=CenterY-(768 div 2);
    AngleX:=CenterX*90/1024;
    AngleY:=CenterY*90/768;

    // Angle
    if dx=0 then
      Angle:=90
    else
      Angle:=-radtodeg(arctan(dy/dx));

    //DebugLn(['TForm1.UpdateSceneHeadTracking AngleX=',AngleX,' AngleY=',AngleY,' AngleRot=',Angle,' CamDist=',CamDist,' dx=',dx,' dy=',dy,' Size1=',SmoothDots[1].Size,' Size2=',SmoothDots[2].Size]);
    
    NewAngleX:=0;
    NewAngleY:=0;
    NewCamDist:=0;
    
    for i := 0 to 4 do begin
      NewAngleX:=NewAngleX+OldAngleX[i];
      NewAngleY:=NewAngleY+OldAngleY[i];
      NewCamDist:=NewCamDist+OldCamDist[i];
    end;
    
    NewAngleX:=NewAngleX/5;
    NewAngleY:=NewAngleY/5;
    NewCamDist:=NewCamDist/5;
    
    // NewAngel = SmoothedValues
    // Angle = original Values
    Camera.Alpha:=NewAngleX;
    Camera.Beta:=90-NewAngleY;
    Camera.Radius:=NewCamDist;
    if EnableRotation then
      Camera.SetRoll(Angle)
    else
      Camera.SetRoll(0);

    for i := 1 to 4 do begin
      OldAngleX[i-1]:=OldAngleX[i];
      OldAngleY[i-1]:=OldAngleY[i];
      OldCamDist[i-1]:=OldCamDist[i];
    end;
    
    OldAngleX[4]:=AngleX;
    OldAngleY[4]:=AngleY;
    OldCamDist[4]:=CamDist;
    // move old values down
    for i:=1 to 4 do
      Dots[i]:=Dots[i+1];

  end else begin
    // not enough data
  end;
end;

procedure TForm1.Init;
var
  CubeMesh: TMesh;
  i: Integer;
  k: Integer;
  Model: TAsmObject;
  Light: TDirectionalLight;
begin
  // Create and intialize the scene
  Scene := TScene.Create(OpenGLControl1);
  Scene.Init;
  writeln('GL Version: ', Scene.Version);
  writeln('Max Indicies: ', Scene.MaxIndex);
  writeln('Max Verticies: ', Scene.MaxVertex);

  // Set up our camera at (0, 0, 3), let it look at the origin as there our
  // cube will be placed. The field of view is 45 degree and the near and far plane
  // are at 1 unit in front of our camera and 100 units respectively (this means
  // only objects between 1 and 100 units in front of our camera are visible)
  Camera := THeadtrackingCamera.Create(3, 0,  0, 0, 0, 45, 0.1, 100);
  //Camera := T6DOFCamera.Create(0,0,3,45,0.1,100);
  Scene.ActiveCamera := Camera;
  
  Scene.SetSkybox('neg_z.bmp','pos_z.bmp','neg_x.bmp','pos_x.bmp','pos_y.bmp','neg_y.bmp');

  // load obj
  // We need a mesh that holds the geometry information
  CubeMesh := TMesh.Create;
  // Let Asmoday create a unitcube for us
  CubeMesh.LoadMeshFromObjFile('bunny.obj');

  // Now lets set up our object
  for i:=1 to 3 do begin
    for k:=1 to 3 do begin
      Model := TAsmObject.Create;
      // Tell it where to find the geometry
      Model.Mesh := CubeMesh;
      // Color it gray
      Model.SetColor(150, 150, 150, 255);
      // Every object needs a shader, so that Asmoday knows how to render it
      // Our models should be rendered with lighting
      Model.Shader := ShaderLighting;
      // Make it visible
      Model.Visible := true;

      Model.SetScale(5,5,5);
      Model.RotateAboutLocalX(90);
      Model.SetPosition(i-2,0,k-2);
      // Add the object to the scene
      Scene.Objectlist.Add(Model);
    end;
  end;

  // Now we need a light
  Light := TDirectionalLight.Create;
  // Directional lights don't really have a position, they just emit parallel
  // light rays. SetPosition is used to set the direction of this rays. The
  // direction is the vector from the position to the origin.
  Light.SetPosition(-0.5, 5, 1);
  // Enable the light
  Light.Enabled := true;
  // set ambient
  Light.SetAmbientColor(55,55,55,255);
  // And add it to the scene
  Scene.Lightlist.Add(Light);

  Finitialized := true;
end;

{ THeadtrackingCamera }

procedure THeadtrackingCamera.RotateAboutView(Angle: single);
var
  View: TVector3;
  Right: TVector3;
begin
  UpdatePosition;
  View:=fCoV-fPosition;
  Normalize(View);
  Right:=Normalized(CrossProduct(View,Up));
  Rotate(Right,View,Angle);
  Normalize(Right);
  // create orthogonal Up
  fUp:=CrossProduct(Right,View);
  Normalize(fUp);
end;

procedure THeadtrackingCamera.SetRoll(Angle: single);
var
  View: TVector3;
  Right: TVector3;
begin
  UpdatePosition;
  View:=fCoV-fPosition;
  Normalize(View);
  Right:=Normalized(CrossProduct(View,UnitVectorY));
  Rotate(Right,View,Angle);
  Normalize(Right);
  // create orthogonal Up
  fUp:=CrossProduct(Right,View);
  Normalize(fUp);
end;

initialization
  {$I mainunit.lrs}

end.

