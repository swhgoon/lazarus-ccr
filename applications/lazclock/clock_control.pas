unit clock_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, DateUtils;

type

  { TLazClockControl }

  TLazClockControl = class(TCustomControl)
  public
    BackgroundImage: TPortableNetworkGraphic;
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

implementation

constructor TLazClockControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BackgroundImage := TPortableNetworkGraphic.Create;
  BackgroundImage.LoadFromFile('skins\wallclock1.PNG');
end;

procedure TLazClockControl.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TLazClockControl.Paint;
var
  lCurTime: TDateTime;
  lHours, lMinutes, lSeconds, lMilliseconds: word;
  lPointerAngleMajor, lPointerAngleMinor: Double;
  Lmajor, Lminor: Integer; // Size of the pointer, bigger and smaller parts, counting from the clock center
  ClockCenter, MajorPos, MinorPos: TPoint;
begin
  Canvas.Draw(0, 0, BackgroundImage);
  lCurTime := Now();
  SysUtils.DecodeTime(lCurTime, lHours, lMinutes, lSeconds, lMilliseconds);
  ClockCenter := Point(Width div 2, Height div 2);

  // Seconds indicator
  lPointerAngleMajor := - 2 * Pi * (lSeconds / 60);
  Lmajor := 150;
  Lminor := 50;
  MinorPos.X := Round(ClockCenter.X + Lminor * Sin(lPointerAngleMajor));
  MinorPos.Y := Round(ClockCenter.Y + Lminor * Cos(lPointerAngleMajor));
  MajorPos.X := Round(ClockCenter.X - Lmajor * Sin(lPointerAngleMajor));
  MajorPos.Y := Round(ClockCenter.Y - Lmajor * Cos(lPointerAngleMajor));
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 3;
  Canvas.Line(MinorPos, MajorPos);

  // Minutes indicator
  lPointerAngleMajor := - 2 * Pi * (lMinutes / 60);
  Lmajor := 120;
  Lminor := 30;
  MinorPos.X := Round(ClockCenter.X + Lminor * Sin(lPointerAngleMajor));
  MinorPos.Y := Round(ClockCenter.Y + Lminor * Cos(lPointerAngleMajor));
  MajorPos.X := Round(ClockCenter.X - Lmajor * Sin(lPointerAngleMajor));
  MajorPos.Y := Round(ClockCenter.Y - Lmajor * Cos(lPointerAngleMajor));
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 5;
  Canvas.Line(MinorPos, MajorPos);

  // Hours indicator
  if lHours > 12 then lHours := lHours - 12;
  lPointerAngleMajor := - 2 * Pi * (lHours / 12);
  Lmajor := 80;
  Lminor := 20;
  MinorPos.X := Round(ClockCenter.X + Lminor * Sin(lPointerAngleMajor));
  MinorPos.Y := Round(ClockCenter.Y + Lminor * Cos(lPointerAngleMajor));
  MajorPos.X := Round(ClockCenter.X - Lmajor * Sin(lPointerAngleMajor));
  MajorPos.Y := Round(ClockCenter.Y - Lmajor * Cos(lPointerAngleMajor));
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 7;
  Canvas.Line(MinorPos, MajorPos);
end;

end.

