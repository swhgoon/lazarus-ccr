{
LazEyes

Fun application which follows the mouse wherever it goes.

The window has the form of the eyes, which are ellipses.
}
program lazeyes;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Classes, Math, SysUtils,
  Graphics, Forms, LResources, StdCtrls, ExtCtrls,
  Controls, LCLType, LCLIntf;

const
  // Sizes for the basic eye ellipse
  INT_EYE_WIDTH = 60;
  INT_EYE_HALFWIDTH = (INT_EYE_WIDTH div 2);
  INT_EYE_HALFWIDTH_SQR = INT_EYE_HALFWIDTH *
    INT_EYE_HALFWIDTH;
  INT_EYE_HEIGHT = 60;
  INT_EYE_HALFHEIGHT = (INT_EYE_HEIGHT div 2);
  INT_EYE_HALFHEIGHT_SQR = INT_EYE_HALFHEIGHT *
    INT_EYE_HALFHEIGHT;

  // This extra border width is the size of the pen,
  // and is also used to make the outer ellipse larger
  // then the puppil ellipse, to create a better
  // visual effect
  INT_BORDER_WIDTH = 15;
  INT_OUTER_EYE_WIDTH = INT_EYE_WIDTH + 2*INT_BORDER_WIDTH;
  INT_OUTER_EYE_HEIGHT = INT_EYE_HEIGHT+2*INT_BORDER_WIDTH;

  INT_INTEREYE_SPACE = 20;

  INT_WINDOW_WIDTH = 2*INT_OUTER_EYE_WIDTH
    + INT_OUTER_EYE_HEIGHT;

type

  { TMainForm }

  TMainForm = class(TForm)
  public
    MyTimer: TTimer;
    MousePos, LeftEyePos, RightEyePos: TPoint;
    WindowDragMousePos, WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalculateEyePosition(
      EyeXc, EyeYc: Integer): TPoint;
    procedure HandleOnTimer(ASender: TObject);
    procedure HandleOnPaint(ASender: TObject);
    procedure HandleOnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure HandleOnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure HandleOnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
 end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
var
  Rgn1, Rgn2, TotalRgn: HRGN;
begin
  inherited Create(AOwner);

  // basic initial data
  MousePos := Mouse.CursorPos;

  // create child objects
  MyTimer := TTimer.Create(Self);
  MyTimer.Interval := 1000 div 60;
  MyTimer.OnTimer := @HandleOnTimer;
  MyTimer.Enabled := True;

  // set events
  OnPaint := @HandleOnPaint;
  OnMouseMove := @HandleOnMouseMove;
  OnMouseDown := @HandleOnMouseDown;
  OnMouseUp := @HandleOnMouseUp;

  // set window properties
  BorderStyle := bsNone;
  Position := poScreenCenter;
  DoubleBuffered := True;

  // set window transparency
  Rgn1 := CreateEllipticRgn(
    0, 0,
    INT_OUTER_EYE_WIDTH, INT_OUTER_EYE_HEIGHT);
  Rgn2 := CreateEllipticRgn(
    INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE, 0,
    2*INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE,
    INT_OUTER_EYE_HEIGHT);
  // The dest region needs to exist before calling
  // CombineRgn, so we create it with dummy values
  TotalRgn := CreateEllipticRgn(0, 0, 10, 10);
  LCLIntf.CombineRgn(TotalRgn, Rgn1, Rgn2, RGN_OR);
  LCLIntf.SetWindowRgn(Handle, TotalRgn, True);
end;

destructor TMainForm.Destroy;
begin
  // free child object
  MyTimer.Free;

  inherited Destroy;
end;

{ Calculates where the eye should be painted
  EyeXc, EyeYc is the center of the eye ellipse
  The size of the ellipse is given by the constants
  INT_EYE_WIDTH and INT_EYE_HEIGHT }
function TMainForm.CalculateEyePosition(
  EyeXc, EyeYc: Integer): TPoint;
var
  RelMousePos: TPoint;
  MousePosAngle: Double;
  DeltaX, DeltaY: Double;
  AbsEyeXc, AbsEyeYc: Integer;
begin
  // Get the mouse position relative to the window
  RelMousePos.X := MousePos.X - Left;
  RelMousePos.Y := MousePos.Y - Top;

  // Get the eye center absolute position in the screen
  AbsEyeXc := EyeXc + Left;
  AbsEyeYc := EyeYc + Top;

  // First check if the cursor is inside the eye, in a
  // position that it will fall right over the eye pupil
  // eye pupil elipse area equation:
  // (X - Xc)^2 / A^2 + (Y - Yc)^2 / B^2 <= 1
  // (Xc, Yc) is the center of the elipse
  // A and B are the half axis of the elipse
  if (Sqr(RelMousePos.X - EyeXc) / INT_EYE_HALFWIDTH_SQR)
   + (Sqr(RelMousePos.Y - EyeYc) / INT_EYE_HALFHEIGHT_SQR)
   <= 1 then
  begin
    Result.X := RelMousePos.X;
    Result.Y := RelMousePos.Y;
    Exit;
  end;

  // Calculate the position of the eye, by calculating how
  // many grads the cursor is forming with the center of
  // the eye. The polar equation of the elipse is:
  // X = Xc + A * cos(t)
  // Y = Yc + B * sen(t)
  if MousePos.X - AbsEyeXc = 0 then
    MousePosAngle := Pi / 2
  else
    MousePosAngle := arctan(Abs(MousePos.Y - AbsEyeYc)
     / Abs(MousePos.X - AbsEyeXc));

  DeltaX := INT_EYE_HALFWIDTH * Cos(MousePosAngle);
  DeltaY := INT_EYE_HALFHEIGHT * Sin(MousePosAngle);

  // 1st quadrant
  if (MousePos.X >= AbsEyeXc) and
     (MousePos.Y <= AbsEyeYc) then
  begin
    Result.X := Round(EyeXc + DeltaX);
    Result.Y := Round(EyeYc - DeltaY);
  end
  // 2nd quadrant
  else if (MousePos.X >= AbsEyeXc) and
          (MousePos.Y >= AbsEyeYc) then
  begin
    Result.X := Round(EyeXc + DeltaX);
    Result.Y := Round(EyeYc + DeltaY);
  end
  // 3rd quadrant
  else if (MousePos.X <= AbsEyeXc) and
          (MousePos.Y >= AbsEyeYc) then
  begin
    Result.X := Round(EyeXc - DeltaX);
    Result.Y := Round(EyeYc + DeltaY);
  end
  // 4th quadrant
  else
  begin
    Result.X := Round(EyeXc - DeltaX);
    Result.Y := Round(EyeYc - DeltaY);
  end;
end;

{ Timer event - Updates the eyes if the mouse moved }
procedure TMainForm.HandleOnTimer(ASender: TObject);
begin
  // Check if mouse position changed
  if (MousePos.X = Mouse.CursorPos.X) and
     (MousePos.Y = Mouse.CursorPos.Y) then Exit;

  MousePos := Mouse.CursorPos;

  // Calculate the position of the eyes
  LeftEyePos := CalculateEyePosition(
    INT_EYE_HALFWIDTH + INT_BORDER_WIDTH,
    INT_EYE_HALFHEIGHT + INT_BORDER_WIDTH);
  RightEyePos := CalculateEyePosition(
    INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE +
    INT_EYE_HALFWIDTH + INT_BORDER_WIDTH,
    INT_EYE_HALFHEIGHT + INT_BORDER_WIDTH);

  // Redraw the eye
  Invalidate;
end;

{ MouseDown - Code to drag the main window using the mouse}
procedure TMainForm.HandleOnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  WindowDragStarted := True;
  WindowDragMousePos := Mouse.CursorPos;
  WindowDragTopLeft.X := Left;
  WindowDragTopLeft.Y := Top;
end;

{ MouseMove - Code to drag the main window using the mouse}
procedure TMainForm.HandleOnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if WindowDragStarted then
  begin
    Left := WindowDragTopLeft.X +
      (Mouse.CursorPos.X - WindowDragMousePos.X);
    Top := WindowDragTopLeft.Y +
      (Mouse.CursorPos.Y - WindowDragMousePos.Y);
  end;
end;

{ Painting code, activated on OnPaint event }
procedure TMainForm.HandleOnPaint(ASender: TObject);
begin
  // Background
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.FillRect(0, 0, Width, Height);

  // Eyes form
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 5;
  Canvas.Ellipse(
    0,
    0,
    INT_OUTER_EYE_WIDTH,
    INT_OUTER_EYE_HEIGHT);
  Canvas.Ellipse(
    INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE,
    0,
    2* INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE,
    INT_OUTER_EYE_HEIGHT);

  // Eyes pupils
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Width := 10;
  Canvas.Ellipse(
    LeftEyePos.X - 2,
    LeftEyePos.Y - 2,
    LeftEyePos.X + 2,
    LeftEyePos.Y + 2);
  Canvas.Ellipse(
    RightEyePos.X - 2,
    RightEyePos.Y - 2,
    RightEyePos.X + 2,
    RightEyePos.Y + 2);
end;

{ MouseUp - Code to drag the main window using the mouse }
procedure TMainForm.HandleOnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  WindowDragStarted := False;
end;

var
  MainForm: TMainForm;

{$IFDEF WINDOWS}{$R lazeyes.rc}{$ENDIF}

begin
  {$I lazeyes.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

