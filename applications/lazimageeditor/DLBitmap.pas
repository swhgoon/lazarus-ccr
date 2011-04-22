unit DLBitmap;  

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, LMessages, LCLProc, Controls, Graphics,
  Forms, Types, IntfGraphics, FPImage, Math, FPImgCanv, FPCanvas, ClipBrd;

type
  tagRGBATRIPLE = record
    rgbtBlue: byte;
    rgbtGreen: byte;
    rgbtRed: byte;
    rgbtAlpha: byte;
  end;
  PRGBATriple = ^TRGBATriple;
  {$ifdef MSWINDOWS}
  TRGBATriple = tagRGBTRIPLE;
  {$else}
  TRGBATriple = tagRGBATRIPLE;
  {$endif}
  PRGBATripleArray = ^TRGBATripleArray;
  TRGBATripleArray = array[word] of TRGBATriple;

  TDLBitmap = class(TBitmap)
  private
    FIntfImgA: TLazIntfImage;
    FFillColor: TColor;
    FOutlineColor: TColor;
    FPaperColor: TColor;
    function GetScanline(Row: integer): pRGBATriple;
    function GetFillColor: TColor;
    function GetOutlineColor: TColor;
    function GetPaperColor: TColor;
    procedure SetFillColor(const AValue: TColor);
    procedure SetOutlineColor(const AValue: TColor);
    procedure SetPaperColor(const AValue: TColor);
  protected
    procedure SetWidth(Value: integer); override;
    procedure SetHeight(Value: integer); override;
    procedure Changed(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ResetScanLine;
    procedure InvalidateScanLine;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure ClearWhite; virtual;
    procedure Invert; virtual;
    procedure Grayscale; virtual;
    procedure FlipHorz; virtual;
    procedure FlipVert; virtual;
    procedure Rotate90; virtual;
    procedure Rotate180; virtual;
    procedure Rotate270; virtual;
    procedure RGBDelta(RedChange, GreenChange, BlueChange: integer);
    procedure Brightness(ValueChange: integer);
    procedure Contrast(ValueChange: integer);
    property ScanLine[Row: integer]: pRGBATriple read GetScanLine;
    procedure FillEllipse(X1, Y1, X2, Y2: integer); virtual;
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure Delete; virtual;
    procedure FloodFill(x, y: integer);
    procedure Spray(x, y, radian: integer; PColor: TColor);
    property FillColor: TColor read GetFillColor write SetFillColor;
    property OutlineColor: TColor read GetOutlineColor write SetOutlineColor;
    property PaperColor: TColor read GetPaperColor write SetPaperColor;
  end;

procedure LazBMPRotate90(const aBitmap: TDLBitmap; IsTurnRight: boolean);
procedure BMPRotate90(const Bitmap: TDLBitmap);
procedure DrawSamePixel(ABitmap: TDLBitmap; Value: integer);
procedure BMPRotate180(const Bitmap: TDLBitmap);
procedure BMPRotate270(const Bitmap: TDLBitmap);
function RotateBitmap(Bitmap: TDLBitmap; Angle: integer; BackColor: TColor): TDLBitmap;
function BitmapFlip(const Vertical: boolean; const Horizontal: boolean;
  var BitmapIn: TDLBitmap; out BitmapOut: TDLBitmap): boolean;
procedure InvertBitmap(aBitmap: TDLBitmap);
procedure ChangeRGB(SrcBmp: TDLBitmap; RedChange, GreenChange, BlueChange: integer);
procedure ChangeBrightness(SrcBmp: TDLBitmap; ValueChange: integer);
procedure ChangeContrast(SrcBmp: TDLBitmap; ValueChange: integer);
procedure SprayPoints(DLBmp: TDLBitmap; X, Y: integer; Radians: integer; PColor: TColor);
function GetRColor(const Color: TColor): Byte;
function GetGColor(const Color: TColor): Byte;
function GetBColor(const Color: TColor): Byte;
procedure SprayPoints(aCanvas: TCanvas; X, Y: integer; Radians: Integer; PColor: TColor);

implementation

{$I DLBmpUtils.inc}

constructor TDLBitmap.Create;
begin
  inherited;
  PixelFormat := pf24bit;
  FIntfImgA := TLazIntfImage.Create(0, 0);
end;

destructor TDLBitmap.Destroy;
begin
  FIntfImgA.Free;
  inherited;
end;

function TDLBitmap.GetScanLine(Row: integer): pRGBATriple;
begin
  if FIntfImgA <> nil then
    Result := FIntfImgA.GetDataLineStart(Row);
end;

procedure TDLBitmap.ResetScanLine;
begin
  FIntfImgA.LoadFromBitmap(Handle, MaskHandle);
end;

procedure TDLBitmap.InvalidateScanLine;
var
  TmpBmp: TDLBitmap;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  TmpBmp := TDLBitmap.Create;
  FIntfImgA.CreateBitmaps(ImgHandle, ImgMaskHandle, True);
  TmpBmp.Handle := ImgHandle;
  TmpBmp.MaskHandle := ImgMaskHandle;
  Empty;
  Width := TmpBmp.Width;
  Height := TmpBmp.Height;
  Canvas.Draw(0, 0, TmpBmp);
  TmpBmp.Free;
end;

procedure TDLBitmap.CutToClipboard;
begin
  CopyToClipboard;
  Delete;
end;

procedure TDLBitmap.CopyToClipboard;
begin
  ClipBoard.Assign(Self);
end;

procedure TDLBitmap.PasteFromClipboard;
var
  oBmp: TBitmap;
begin
  oBmp := TBitmap.Create;
  try
    oBmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));
    Canvas.Draw(0, 0, oBmp);
  finally
    oBmp.Free;
  end;
end;

procedure TDLBitmap.Delete;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := PaperColor;
  Canvas.FillRect(0, 0, Width, Height);
end;

procedure TDLBitmap.Assign(Source: TPersistent);
begin
  inherited;
end;

function TDLBitmap.GetFillColor: TColor;
begin
  Result := FFillColor;
end;

function TDLBitmap.GetOutlineColor: TColor;
begin
  Result := FOutlineColor;
end;

function TDLBitmap.GetPaperColor: TColor;
begin
  Result := FPaperColor;
end;

procedure TDLBitmap.SetFillColor(const AValue: TColor);
begin
  FFillColor := AValue;
end;

procedure TDLBitmap.SetOutlineColor(const AValue: TColor);
begin
  FOutlineColor := AValue;
end;

procedure TDLBitmap.SetPaperColor(const AValue: TColor);
begin
  FPaperColor := AValue;
end;

procedure TDLBitmap.SetWidth(Value: integer);
begin
  inherited;
end;

procedure TDLBitmap.SetHeight(Value: integer);
begin
  inherited;
end;

procedure TDLBitmap.Changed(Sender: TObject);
begin
  inherited;
  ResetScanLine;
end;

procedure TDLBitmap.Clear;
begin

end;

procedure TDLBitmap.ClearWhite;
begin

end;

procedure TDLBitmap.Invert;
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  InvertBitmap(Tmp);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.Grayscale;
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  ConvertBitmapToGrayScale(Tmp);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.FlipHorz;
var
  tmp, tmp2: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp2 := TDLBitmap.Create;
  tmp2.Width := Width;
  tmp2.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BitmapFlip(False, True, tmp, tmp2);
  Canvas.Draw(0, 0, tmp2);
  tmp.Free;
  tmp2.Free;
end;

procedure TDLBitmap.FlipVert;
var
  tmp, tmp2: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp2 := TDLBitmap.Create;
  tmp2.Width := Width;
  tmp2.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BitmapFlip(True, False, tmp, tmp2);
  Canvas.Draw(0, 0, tmp2);
  tmp.Free;
  tmp2.Free;
end;

procedure TDLBitmap.Rotate90;
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate90(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.Rotate180;
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate180(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.Rotate270;
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate270(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.RGBDelta(RedChange, GreenChange, BlueChange: integer);
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  ChangeRGB(Tmp, RedChange, GreenChange, BlueChange);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.Brightness(ValueChange: integer);
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  ChangeBrightness(Tmp, ValueChange);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.Contrast(ValueChange: integer);
var
  tmp: TDLBitmap;
begin
  tmp := TDLBitmap.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  ChangeContrast(Tmp, ValueChange);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TDLBitmap.FloodFill(x, y: integer);
begin
  Canvas.Brush.Color := FFillColor;
  Canvas.FloodFill(x, y, Canvas.Pixels[x, y], fsSurface);
end;

procedure TDLBitmap.Spray(x, y, radian: integer; PColor: TColor);
begin
  SprayPoints(Self.Canvas, x, y, radian, PColor);
end;

procedure TDLBitmap.FillEllipse(X1, Y1, X2, Y2: integer);
begin

end;

end.

