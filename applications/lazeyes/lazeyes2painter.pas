unit lazeyes2painter;

{$mode objfpc}{$H+}

interface

uses
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

  { TLazEye2Painter }

  TLazEye2Painter = class(TCustomControl)
  public
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

var
  MousePos, LeftEyePos, RightEyePos: TPoint;

implementation

procedure TLazEye2Painter.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TLazEye2Painter.Paint;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    DrawToCanvas(Bitmap.Canvas);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

  inherited Paint;
end;

procedure TLazEye2Painter.DrawToCanvas(ACanvas: TCanvas);
begin
  // Background
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clWhite;
  ACanvas.FillRect(0, 0, Width, Height);

  // Eyes form
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 5;
  ACanvas.Ellipse(
    0,
    0,
    INT_OUTER_EYE_WIDTH,
    INT_OUTER_EYE_HEIGHT);
  ACanvas.Ellipse(
    INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE,
    0,
    2* INT_OUTER_EYE_WIDTH + INT_INTEREYE_SPACE,
    INT_OUTER_EYE_HEIGHT);

  // Eyes pupils
  ACanvas.Brush.Color := clBlack;
  ACanvas.Pen.Width := 10;
  ACanvas.Ellipse(
    LeftEyePos.X - 2,
    LeftEyePos.Y - 2,
    LeftEyePos.X + 2,
    LeftEyePos.Y + 2);
  ACanvas.Ellipse(
    RightEyePos.X - 2,
    RightEyePos.Y - 2,
    RightEyePos.X + 2,
    RightEyePos.Y + 2);
end;

end.

