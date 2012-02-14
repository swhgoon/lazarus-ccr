{
  Authors: Felipe Monteiro de Carvalho, Yang JiXian

  License: The same modifying LGPL with static linking exception as the LCL

  This unit implements the TDLBitmap class which has similar property "ScanLine"
  of Delphi TBitmap. With this property we can reuse some classic code of delphi
  to yield our platform independent bitmap class. We hope it simple and powerful.

  Also some useful image process function has been added into the class.
}

unit DLBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, LMessages, LCLProc, Controls, Graphics,
  Forms, Types, IntfGraphics, FPImage, Math, FPImgCanv, FPCanvas, StdCtrls,
  ClipBrd, ExtCtrls;

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
    function GetScanLinePixel(X, Y: Integer): TColor;
    procedure SetScanLinePixel(X, Y: Integer; Value: TColor);
  protected
    ImgHandle, ImgMaskHandle: HBitmap;
    procedure SetWidth(Value: integer); override;
    procedure SetHeight(Value: integer); override;
    procedure Changed(Sender: TObject); override;
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
    procedure FinalizeWriter(AWriter: TFPCustomImageWriter); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ResetScanLine;
    procedure InvalidateScanLine;
    procedure InvalidateScanLineRect(aRect: TRect);
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
    procedure RegularPolygon(Center, ThePoint: TPoint; Count: integer);
    procedure ColorReplace(ColorFrom, ColorTo: TColor);
    procedure ReplaceRectColor(ColorFrom, ColorTo: TColor; aRect: TRect; R: integer; Shape: TShapeType);
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
    property Pixels[X, Y: Integer]: TColor read GetScanLinePixel write SetScanLinePixel;
  end;

  TTextEditor = class;

  TTextEdit = class(TCustomEdit)
  private
    FCanvas: TCanvas;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    Editor: TTextEditor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;

  TTextEditor = class(TCustomControl)
  private
    FEdit: TTextEdit;
    FTimer: TIdleTimer;
    flashnum: integer;
  protected
    procedure Paint; override;
    procedure DrawFlashLine(Sender: TObject);
    procedure Changing(Sender: TObject);
  public
    IMGCanvas: TCanvas;
    PositionIndex, StartX, StartY, TextX, TextY: integer;
    procedure StartEdit(ContainerX, ContainerY, IMGX, IMGY: integer);
    procedure StopEdit;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    property Editor: TTextEdit read FEdit write FEdit;
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
function GetRColor(const Color: TColor): byte;
function GetGColor(const Color: TColor): byte;
function GetBColor(const Color: TColor): byte;
procedure SprayPoints(aCanvas: TCanvas; X, Y: integer; Radians: integer; PColor: TColor);
procedure DLBMPColorReplace(aBitmap: TDLBitmap; ColorFrom, ColorTo: TColor);
operator + (const A, B: TRGBATriple): TRGBATriple;
operator - (const A, B: TRGBATriple): TRGBATriple;
operator * (const A, B: TRGBATriple): TRGBATriple;
operator = (A, B: TRGBATriple): Boolean;
operator div (const A, B: TRGBATriple): TRGBATriple;
function DWordTrans(SrcRow: TRGBATriple): DWORD;
function DWordToTriple(SrcRow: DWORD): TRGBATriple;
procedure StretchLinear(Dest, Src: TDLBitmap);
procedure StretchDLBMP(ACanvas: TCanvas; Src: TDLBitmap; NewLeft, NewTop, NewWidth, NewHeight: integer);
procedure StretchDLBMPEx(ACanvas: TCanvas; Src: TDLBitmap;
  NewLeft, NewTop, NewWidth, NewHeight: integer; Posx, Posy, aWidth, aHeight: integer);
procedure DrawRegularPolygon(aCanvas: TCanvas; Center, ThePoint: TPoint; Count: integer);

implementation

{$I DLBmpUtils.inc}

operator + (const A, B: TRGBATriple): TRGBATriple;
begin
  Result.rgbtBlue := A.rgbtBlue + B.rgbtBlue;
  Result.rgbtRed := A.rgbtRed + B.rgbtRed;
  Result.rgbtGreen := A.rgbtBlue + B.rgbtGreen;
end;

operator - (const A, B: TRGBATriple): TRGBATriple;
begin
  Result.rgbtBlue := A.rgbtBlue - B.rgbtBlue;
  Result.rgbtRed := A.rgbtRed - B.rgbtRed;
  Result.rgbtGreen := A.rgbtBlue - B.rgbtGreen;
end;

operator * (const A, B: TRGBATriple): TRGBATriple;
begin
  Result.rgbtBlue := A.rgbtBlue * B.rgbtBlue;
  Result.rgbtRed := A.rgbtRed * B.rgbtRed;
  Result.rgbtGreen := A.rgbtBlue * B.rgbtGreen;
end;

operator div (const A, B: TRGBATriple): TRGBATriple;
begin
  Result.rgbtBlue := A.rgbtBlue div B.rgbtBlue;
  Result.rgbtRed := A.rgbtRed div B.rgbtRed;
  Result.rgbtGreen := A.rgbtBlue div B.rgbtGreen;
end;

operator = (A, B: TRGBATriple): Boolean;
begin
  A.rgbtBlue := B.rgbtBlue;
  A.rgbtRed := B.rgbtRed;
  A.rgbtBlue := B.rgbtGreen;
  Result := True;
end;

function DWordTrans(SrcRow: TRGBATriple): DWORD;
var RR, GG, BB: integer;
begin
  RR := SrcRow.rgbtRed;
  GG := SrcRow.rgbtGreen;
  BB := SrcRow.rgbtBlue;
  Result := RR + (GG shl 8) and $FF00 + (BB shl 16) and $FF0000;
end;

function DWordToTriple(SrcRow: DWORD): TRGBATriple;
begin
  Result.rgbtBlue := (SrcRow shr 16) and $FF0000;
  Result.rgbtGreen := (SrcRow shr 8) and $FF00;
  Result.rgbtRed := SrcRow and $FF;
end;

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
begin
  FIntfImgA.CreateBitmaps(ImgHandle, ImgMaskHandle, True);
  Handle := ImgHandle;
  MaskHandle := ImgMaskHandle;
end;

procedure TDLBitmap.InvalidateScanLineRect(aRect: TRect);
var
  TmpBmp: TDLBitmap;
begin
  TmpBmp := TDLBitmap.Create;
  FIntfImgA.CreateBitmaps(ImgHandle, ImgMaskHandle, True);
  TmpBmp.Handle := ImgHandle;
  TmpBmp.MaskHandle := ImgMaskHandle;
  Empty;
  Width := TmpBmp.Width;
  Height := TmpBmp.Height;
  Canvas.CopyRect(aRect, TmpBmp.Canvas, aRect);
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
  oBmp: TPicture;
begin
  oBmp := TPicture.Create;
  try
    oBmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));
    Width := oBmp.Width;
    Height := oBmp.Height;
    Canvas.Draw(0, 0, oBmp.Graphic);
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

procedure TDLBitmap.RegularPolygon(Center, ThePoint: TPoint; Count: integer);
begin
  DrawRegularPolygon(Canvas, Center, ThePoint, Count);
end;

procedure TDLBitmap.FloodFill(x, y: integer);
begin
  Canvas.Brush.Color := FFillColor;
  Canvas.FloodFill(x, y, Canvas.Pixels[x, y], fsSurface);
end;

function TDLBitmap.GetScanLinePixel(X, Y: Integer): TColor;
var SrcRow: pRGBATriple; RR, GG, BB: integer;
begin
  if (x >= 0) and (x < Width) and (y >= 0) and (y < Height) then
  begin
    SrcRow := ScanLine[y];
    RR := SrcRow[x].rgbtRed;
    GG := SrcRow[x].rgbtGreen;
    BB := SrcRow[x].rgbtBlue;
  end;
  Result := RR + (GG shl 8) and $FF00 + (BB shl 16) and $FF0000;
end;

procedure TDLBitmap.SetScanLinePixel(X, Y: Integer; Value: TColor);
var SrcRow: pRGBATriple;
begin
  if (x >= 0) and (x < Width) and (y >= 0) and (y < Height) then
  begin
    SrcRow := ScanLine[y];
    SrcRow[x].rgbtRed:=GetRColor(Value);
    SrcRow[x].rgbtGreen:=GetGColor(Value);
    SrcRow[x].rgbtBlue:=GetBColor(Value);
  end;
end;

procedure TDLBitmap.Spray(x, y, radian: integer; PColor: TColor);
begin
  SprayPoints(Self, x, y, radian, PColor);
end;

procedure TDLBitmap.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
begin
  inherited;
  FIntfImgA := AImage;
end;

procedure TDLBitmap.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
begin
  inherited;
end;

procedure TDLBitmap.FinalizeWriter(AWriter: TFPCustomImageWriter);
begin
  inherited;
end;

procedure TDLBitmap.FillEllipse(X1, Y1, X2, Y2: integer);
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TDLBitmap.ColorReplace(ColorFrom, ColorTo: TColor);
begin
  DLBMPColorReplace(Self, ColorFrom, ColorTo);
end;

procedure TDLBitmap.ReplaceRectColor(ColorFrom, ColorTo: TColor; aRect: TRect; R: integer; Shape: TShapeType);
var maskbmp: TDLBitmap; i, j: integer;
begin
  maskbmp := TDLBitmap.Create;
  maskbmp.Width := aRect.Right - aRect.Left;
  maskbmp.Height := aRect.Bottom - aRect.Top;
  maskbmp.Canvas.Brush.Color := clWhite;
  maskbmp.Canvas.Brush.Style := bsSolid;
  maskbmp.Canvas.Pen.Color := clWhite;
  maskbmp.Canvas.Rectangle(0, 0, Width, Height);
  maskbmp.Canvas.Brush.Color := clBlack;
  maskbmp.Canvas.Pen.Color := clBlack;
  case Shape of
   stRectangle: maskbmp.Canvas.Rectangle(0, 0, maskbmp.Width, maskbmp.Height);
   stRoundRect: maskbmp.Canvas.RoundRect(0, 0, maskbmp.Width, maskbmp.Height, R, R);
   stEllipse  : maskbmp.Canvas.Ellipse(0, 0, maskbmp.Width, maskbmp.Height);
  end;
  for i := 0 to maskbmp.Width do
    for j := 0 to maskbmp.Height do
      if maskbmp.Pixels[i, j] = clBlack then
        if Pixels[aRect.Left + i, aRect.Top + j] = ColorFrom then
          Pixels[aRect.Left + i, aRect.Top + j] := ColorTo;
  InvalidateScanline;
  maskbmp.Free;
end;

constructor TTextEdit.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  Width := 1;
  Parent := TWinControl(AOwner);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := GetDC(Self.Handle);
  FCanvas.Brush.Style := bsClear;
end;

destructor TTextEdit.Destroy;
begin
  inherited;
  FCanvas.Free;
end;

procedure TTextEdit.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);

end;

procedure TTextEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Editor.PositionIndex <> SelStart then
  begin
    Editor.PositionIndex := Length(Text); //SelStart;
    Editor.DrawFlashLine(nil);
  end;
end;

procedure TTextEdit.DoEnter;
begin
  inherited;
end;

procedure TTextEdit.DoExit;
begin
  inherited;
  Editor.StopEdit;
end;

constructor TTextEditor.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  Width := 1;
  Height := 16;
  Parent := TWinControl(AOwner);
  FEdit := TTextEdit.Create(AOwner);
  FEdit.Parent := Self;
  FEdit.Left := 3;
  FEdit.Top := 0;
  FEdit.Editor := Self;
  FEdit.Show;
  FTimer := TIdleTimer.Create(AOwner);
  FTimer.OnTimer := @DrawFlashLine;
  FTimer.Interval := 500;
  FTimer.Enabled := False;
  FEdit.OnChange := @Changing;
  Visible := False;
end;

destructor TTextEditor.Destroy;
begin
  inherited;
end;

procedure TTextEditor.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);

end;

procedure TTextEditor.Paint;
begin
  inherited Paint;
end;

procedure TTextEditor.DrawFlashLine(Sender: TObject);
var FlashLeft: integer; LeftText: string;
begin
  flashnum := flashnum + 1;
  if flashnum > 1000 then
    flashnum := 0;
  if flashnum mod 2 = 0 then
    Canvas.Pen.Color := clWhite
  else
    Canvas.Pen.Color := clBlack;
  FEdit.Font.Assign(IMGCanvas.Font);
  Height := FEdit.Height - 3;
  LeftText := Copy(FEdit.Text, 1, PositionIndex);
  FlashLeft := StartX + IMGCanvas.TextWidth(LeftText);
  Left := FlashLeft;
  Top := StartY;
  Canvas.Line(0, 0, 0, Height);
end;

procedure TTextEditor.StartEdit(ContainerX, ContainerY, IMGX, IMGY: integer);
begin
  if IMGCanvas = nil then
    Exit;
  Left := ContainerX;
  Top := ContainerY;
  StartX := ContainerX;
  StartY := ContainerY;
  TextX := IMGX;
  TextY := IMGY;
  FEdit.Text := '';
  Show;
  FTimer.Enabled := True;
  FEdit.SetFocus;
end;

procedure TTextEditor.StopEdit;
begin
  FTimer.Enabled := False;
  Hide;
end;

procedure TTextEditor.Changing(Sender: TObject);
var TextLeft: integer; LeftText, RightText: string;
begin
  LeftText := Copy(FEdit.Text, 1, PositionIndex);
  RightText := Copy(FEdit.Text, PositionIndex + 1, Length(FEdit.text));
  TextLeft := TextX + IMGCanvas.TextWidth(LeftText);
  if IMGCanvas = nil then
    Exit;
  IMGCanvas.Brush.Style := bsClear;
  IMGCanvas.TextOut(TextLeft, TextY, RightText);
  PositionIndex := Length(FEdit.Text);
end;

end.

