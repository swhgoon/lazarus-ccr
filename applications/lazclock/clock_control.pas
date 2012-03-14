unit clock_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, DateUtils, Types;

type

  { TLazClockControl }

  TLazClockControl = class(TCustomControl)
  public
    BackgroundImage: TPortableNetworkGraphic;
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetResourcesDir: string;
  end;

implementation

{$ifdef Darwin}
uses
  MacOSAll;
{$endif}

constructor TLazClockControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BackgroundImage := TPortableNetworkGraphic.Create;
  BackgroundImage.LoadFromFile(GetResourcesDir() + 'skins' + PathDelim + 'wallclock1.PNG');
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
  ClockCenter := Types.Point(Width div 2, Height div 2);

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

function TLazClockControl.GetResourcesDir: string;
{$ifdef Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + '/Contents/Resources/';
{$else}
  Result := '/usr/share/lazclock/';
{$endif}
{$endif}

{$ifdef Windows}
  Result := ExtractFilePath(Application.EXEName);
{$endif}
end;

end.

