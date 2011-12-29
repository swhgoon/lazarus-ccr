{
 /***************************************************************************
                                  RGBTypes.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    TRGB32Pixel - TRGB32Bitmap picture element, contains red, green and blue
      component and is platform dependent!
    TRGBBitmapCore - universal RGB bitmap core.
    TRGB32BitmapCore - 32-bit core of TRGB32Bitmap.
}
unit BmpRGBTypes;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

{$ifdef LCLwin32}
 {$define RGB}
{$endif}
{$ifdef LCLqt}
 {$define RGB}
{$endif}

interface

uses
  Classes, SysUtils, FPImage, IntfGraphics, Graphics, Math, LCLProc,
  BmpRGBUtils, DLBitmap;

const
  MAXRANDOMDENSITY = $FFFF;

type
  TRandomDensity = word;
  TDrawMode = (dmFillAndOutline, dmOutline, dmFill);
  TEraseMode = (ermNone, ermErase, ermReplace);
  TDrawPixelProcedure = procedure(X, Y: integer) of object;

  TPixelDifference = word;

const
  MAXDIFFERENCE = 255 + 255 + 255;

type
  // integral float with 1/256 precision
  TIntensityFloat = integer;
  TIntensityFloatTable = array [0..255] of TIntensityFloat;

  { TRGBBitmapCore }

  TRGBBitmapCore = class(TDLBitmap)
  private
    FSizeOfPixel: integer;
    FRowPixelStride: integer;
    FDataOwner: boolean;
    FDrawMode: TDrawMode;
    FEraseMode: TEraseMode;
    FFillColor: TColor;
    FFloodFillTolerance: TPixelDifference;
    FOutlineColor: TColor;
    FPaperColor: TColor;
    FRandomDensity: TRandomDensity;
    FRandomEnabled: boolean;
    FRectangleRoundness: integer;
    function GetSize: integer;
  protected
    function PixelMasked(X, Y: integer): boolean;

    procedure DrawOutlinePixel(X, Y: integer);
    procedure DrawFillPixel(X, Y: integer);
    procedure DrawPaperPixel(X, Y: integer);

    procedure DrawReplacePixel(X, Y: integer);

    procedure DrawRandomOutlinePixel(X, Y: integer);
    procedure DrawRandomFillPixel(X, Y: integer);
    procedure DrawRandomPaperPixel(X, Y: integer);

    procedure DrawEmptyPixel(X, Y: integer);

    function GetOutlineProcedure: TDrawPixelProcedure; virtual;
    function GetFillProcedure: TDrawPixelProcedure; virtual;
  public
    constructor Create(AWidth, AHeight: integer; ASizeOfPixel: integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore; ASizeOfPixel: integer); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: integer;
      ASizeOfPixel: integer; ADataOwner: boolean = False); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); virtual;
  public
    procedure Clear; override;
    procedure ClearWhite; override;
    procedure Invert; override;

    procedure FlipHorz; override;
    procedure FlipVert; override;
    procedure Rotate90; override;
    procedure Rotate180; override;
    procedure Rotate270; override;
  public
    procedure SetColor(X, Y: integer; Value: TColor);
    function GetColor(X, Y: integer): TColor;

    procedure Fill(Color: TColor);
    procedure FillEllipse(X1, Y1, X2, Y2: integer); override;
    procedure MaskFloodFill(X, Y: integer);
    // Alpha drawing methods
    procedure AlphaRectangle(X1, Y1, X2, Y2, AAlpha: integer);
    // Effect drawing methods
    procedure FuzzyRectangle(X1, Y1, X2, Y2: integer);
  public
    procedure DrawTo(ACanvas: TCanvas; X, Y: integer);
    procedure StretchDrawTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight, HPos, VPos, aWidth, aHeight: integer);

    property EraseMode: TEraseMode read FEraseMode write FEraseMode;
    property DrawMode: TDrawMode read FDrawMode write FDrawMode;
    property FloodFillTolerance: TPixelDifference
      read FFloodFillTolerance write FFloodFillTolerance;

    property Width;
    property Height;

    property RandomEnabled: boolean read FRandomEnabled write FRandomEnabled;
    property RandomDensity: TRandomDensity read FRandomDensity write FRandomDensity;

    property RectangleRoundness: integer read FRectangleRoundness
      write FRectangleRoundness;
    property DataOwner: boolean read FDataOwner;
    property Size: integer read GetSize;
    property SizeOfPixel: integer read FSizeOfPixel;
  end;

  { TRGB8BitmapCore }

  TRGB8BitmapCore = class(TRGBBitmapCore)
  public
    constructor Create(AWidth, AHeight: integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: integer;
      ADataOwner: boolean = False); virtual;

    procedure LoadFromLazIntfImageAlpha(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImageAlpha(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImageAlpha(AImage: TLazIntfImage;
      const ARect: TRect); virtual;

    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
  public
  end;

  { TRGB32BitmapCore }

  TRGB32BitmapCore = class(TRGBBitmapCore)
  private
  public
    constructor Create(AWidth, AHeight: integer); virtual;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); virtual;
    constructor CreateFromLazIntfImage(AImage: TLazIntfImage); virtual;
    constructor CreateFromData(AData: Pointer; AWidth, AHeight: integer;
      ADataOwner: boolean = False); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage); virtual;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect); virtual;
  public
  published
  end;

procedure FlipHorzRGBBitmap(Bitmap: TRGBBitmapCore);
procedure FlipVertRGBBitmap(Bitmap: TRGBBitmapCore);

// intensity tables
function GetIntensityFloatTable(A, B: single): TIntensityFloatTable;

// rotate clockwise
procedure Rotate90CWRGBBitmap(Bitmap: TRGBBitmapCore);
procedure Rotate180CWRGBBitmap(Bitmap: TRGBBitmapCore);
procedure Rotate270CWRGBBitmap(Bitmap: TRGBBitmapCore);

procedure InvertRGBBitmap(Bitmap: TRGBBitmapCore);
procedure GrayscaleRGB32Bitmap(Bitmap: TRGB32BitmapCore);
procedure DisableRGB32Bitmap(Bitmap: TRGB32BitmapCore);

implementation

uses BmpRGBGraph;

function AbsByte(Src: integer): byte; inline;
begin
  if Src >= 0 then
    Result := Src
  else
    Result := -Src;
end;

function RoundIntensityFloatInline(V: TIntensityFloat): byte; inline;
begin
  Result := Max(0, Min(255, (V + 128) shr 8));
end;

(*
  Creates look-up table T[I = 0..255] = A + I * B.
*)

function GetIntensityFloatTable(A, B: single): TIntensityFloatTable;
var
  I: integer;
  C: single;
begin
  C := A;
  for I := 0 to High(Result) do
  begin
    Result[I] := Round(C * 256);
    C := C + B;
  end;
end;

procedure FlipHorzRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure FlipVertRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure Rotate90CWRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure Rotate180CWRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure Rotate270CWRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure InvertRGBBitmap(Bitmap: TRGBBitmapCore);
begin

end;

procedure GrayscaleRGB32Bitmap(Bitmap: TRGB32BitmapCore);
begin

end;

procedure DisableRGB32Bitmap(Bitmap: TRGB32BitmapCore);
begin

end;

{ TRGBBitmapCore }

function TRGBBitmapCore.GetSize: integer;
begin

end;

constructor TRGBBitmapCore.Create(AWidth, AHeight: integer; ASizeOfPixel: integer);
begin
  inherited Create;

  Width := AWidth;
  Height := AHeight;
  // TODO: check on 64-bit arch.
  // 32-bit alignment
  FRowPixelStride := (((AWidth * ASizeOfPixel + 3) shr 2) shl 2) div ASizeOfPixel;
  FSizeOfPixel := ASizeOfPixel;

  FDataOwner := True;

  FRandomDensity := MAXRANDOMDENSITY;
  FFloodFillTolerance := 0;
  FRectangleRoundness := 0;
end;

constructor TRGBBitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore; ASizeOfPixel: integer);
begin
  inherited Create;

  Width := ABitmap.Width;
  Height := ABitmap.Height;
  FSizeOfPixel := ASizeOfPixel;

  FDataOwner := True;
end;

constructor TRGBBitmapCore.CreateFromData(AData: Pointer; AWidth, AHeight: integer;
  ASizeOfPixel: integer; ADataOwner: boolean);
begin
  inherited Create;

  Width := AWidth;
  Height := AHeight;
  // TODO: check on 64-bit arch.
  // 32-bit alignment
  FRowPixelStride := (((AWidth * ASizeOfPixel + 3) shr 2) shl 2) div ASizeOfPixel;
  FSizeOfPixel := ASizeOfPixel;
  FDataOwner := ADataOwner;
end;

destructor TRGBBitmapCore.Destroy;
begin
  inherited;
end;

procedure TRGBBitmapCore.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;
  if Source = Self then
    Exit;
  if Source is TRGBBitmapCore then
  begin
    Width := (Source as TRGBBitmapCore).Width;
    Height := (Source as TRGBBitmapCore).Height;
    FSizeOfPixel := (Source as TRGBBitmapCore).SizeOfPixel;
  end
  else
    inherited Assign(Source);
end;

procedure TRGBBitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  if ABitmap = nil then
    Exit;
  SwapInt(Width, ABitmap.Width);
  SwapInt(Height, ABitmap.Height);
  SwapInt(FRowPixelStride, ABitmap.FRowPixelStride);
  SwapInt(FSizeOfPixel, ABitmap.FSizeOfPixel);
end;

procedure TRGBBitmapCore.Clear;
begin
  inherited;
end;

procedure TRGBBitmapCore.ClearWhite;
begin
  inherited;
end;

procedure TRGBBitmapCore.Invert;
begin
  inherited;
end;

procedure TRGBBitmapCore.FlipHorz;
begin
  inherited;
end;

procedure TRGBBitmapCore.FlipVert;
begin
  inherited;
end;

procedure TRGBBitmapCore.Rotate90;
begin
  inherited;
end;

procedure TRGBBitmapCore.Rotate180;
begin
  inherited;
end;

procedure TRGBBitmapCore.Rotate270;
begin
  inherited;
end;

procedure TRGBBitmapCore.SetColor(X, Y: integer; Value: TColor);
begin
  Canvas.Pixels[X, Y] := Value;
end;

function TRGBBitmapCore.GetColor(X, Y: integer): TColor;
begin
  if (X > 0) and (X < Width) and (Y > 0) and (Y < Height) then
    Result := Canvas.Pixels[X, Y]
  else
    Result := clNone;
end;

function TRGBBitmapCore.PixelMasked(X, Y: integer): boolean;
begin

end;

procedure TRGBBitmapCore.DrawOutlinePixel(X, Y: integer);
begin

end;

procedure TRGBBitmapCore.DrawFillPixel(X, Y: integer);
begin

end;

procedure TRGBBitmapCore.DrawPaperPixel(X, Y: integer);
begin

end;

procedure TRGBBitmapCore.DrawReplacePixel(X, Y: integer);
begin

end;

procedure TRGBBitmapCore.DrawRandomOutlinePixel(X, Y: integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then;

end;

procedure TRGBBitmapCore.DrawRandomFillPixel(X, Y: integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then;

end;

procedure TRGBBitmapCore.DrawRandomPaperPixel(X, Y: integer);
begin
  if PixelMasked(X, Y) and (Random(MAXRANDOMDENSITY) < FRandomDensity) then;

end;

procedure TRGBBitmapCore.DrawEmptyPixel(X, Y: integer);
begin

end;

function TRGBBitmapCore.GetOutlineProcedure: TDrawPixelProcedure;
begin
  if not FRandomEnabled then
  begin
    case DrawMode of
      dmFillAndOutline, dmOutline:
      begin
        case EraseMode of
          ermNone: Result := @DrawOutlinePixel;
          ermErase: Result := @DrawPaperPixel;
          ermReplace: Result := @DrawReplacePixel;
        end;
      end;
      else
        Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
      ermNone: Result := @DrawRandomFillPixel;
      ermErase: Result := @DrawRandomPaperPixel;
      ermReplace: Result := @DrawRandomFillPixel;
    end;
  end;
end;

function TRGBBitmapCore.GetFillProcedure: TDrawPixelProcedure;
begin
  if not FRandomEnabled then
  begin
    case DrawMode of
      dmFillAndOutline, dmFill:
      begin
        case EraseMode of
          ermNone: Result := @DrawFillPixel;
          ermErase: Result := @DrawPaperPixel;
          ermReplace: Result := @DrawReplacePixel;
        end;
      end;
      else
        Result := @DrawEmptyPixel;
    end;
  end
  else
  begin
    case EraseMode of
      ermNone: Result := @DrawRandomFillPixel;
      ermErase: Result := @DrawRandomPaperPixel;
      ermReplace: Result := @DrawRandomFillPixel;
    end;
  end;
end;

procedure TRGBBitmapCore.Fill(Color: TColor);
var
  I, J: integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TRGBBitmapCore.FillEllipse(X1, Y1, X2, Y2: integer);
begin
  inherited;
end;

procedure TRGBBitmapCore.MaskFloodFill(X, Y: integer);
begin
  Canvas.Brush.Color := clBlack;
//  Canvas.FillRect(Rect(0, 0, Width, Height));
  FloodFill(X, Y);
end;

// AAlpha is the alpha of the rectangle, ranging from
// 0 - fully transparent to 100 - fully opaque
procedure TRGBBitmapCore.AlphaRectangle(X1, Y1, X2, Y2, AAlpha: integer);
var
  OldColor, NewColor: integer;
  X, Y: longint;
  OldR, OldG, OldB, NewR, NewG, NewB: byte;
begin
  // If the rectangle is fully opaque this is the same as a normal rectangle
  if AAlpha = 100 then
  begin
    Canvas.Rectangle(X1, Y1, X2, Y2);
    Exit;
  end;

  // if it is fully transparent there is nothing to draw
  if AAlpha = 0 then
    Exit;

  // A partially transparent rectangle
  for Y := Y1 to Y2 do
    for X := X1 to X2 do
    begin
      OldColor := GetColor(X, Y);
      RedGreenBlue(OldColor, OldR, OldG, OldB);

      NewR := ((100 - AAlpha) * OldR + AAlpha * Red(FillColor)) div 100;
      NewG := ((100 - AAlpha) * OldG + AAlpha * Green(FillColor)) div 100;
      NewB := ((100 - AAlpha) * OldB + AAlpha * Blue(FillColor)) div 100;

      SetColor(X, Y, RGBToColor(NewR, NewG, NewB));
    end;
end;

procedure TRGBBitmapCore.FuzzyRectangle(X1, Y1, X2, Y2: integer);
var
  X, Y: longint;
  deltaX, deltaY: integer;
begin
  for Y := Y1 to Y2 do
    for X := X1 to X2 do
    begin
      // This computation has a good effect of making text illegible,
      // but keeping the overal image with the same colors
      deltaX := X mod 5;
      deltaY := deltaX;

      // Makes sure we won't get any invalid pixel positions
      if X < 5 then
        deltaX := -deltaX;
      if Y < 5 then
        deltaY := -deltaY;

      // Change the color
      SetColor(X, Y, GetColor(X - deltaX, Y - deltaY));
    end;
end;

procedure TRGBBitmapCore.DrawTo(ACanvas: TCanvas; X, Y: integer);
begin
  if ACanvas <> nil then
    ACanvas.Draw(X, Y, Self);
end;

procedure TRGBBitmapCore.StretchDrawTo(ACanvas: TCanvas;
  DstX, DstY, DstWidth, DstHeight, HPos, VPos, aWidth, aHeight: integer);
begin
 // if ACanvas <> nil then
  //  ACanvas.StretchDraw(Rect(DstX, DstY, DstWidth, DstHeight), Self);
 //   StretchDLBMP(Self, 3, 3);
  StretchDLBMPEx(ACanvas, Self, DstX, DstY, DstWidth, DstHeight, HPos, VPos, aWidth, aHeight);
  {begin
    ACanvas.AntialiasingMode:=amOff;
    ACanvas.CopyMode:=cmSrcPaint;
    ACanvas.CopyRect(Rect(DstX, DstY, DstWidth, DstHeight), Self.Canvas, Rect(0,0,Width,Height));
  end;     }
end;

{ TRGB32BitmapCore }

constructor TRGB32BitmapCore.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight, 1);
end;

constructor TRGB32BitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = 0 then
    inherited CreateAsCopy(ABitmap, 1);
end;

constructor TRGB32BitmapCore.CreateFromLazIntfImage(AImage: TLazIntfImage);
var
  I, J: integer;
begin
  Create(AImage.Width, AImage.Height);

  for J := 0 to Pred(Height) do
  begin

  end;
end;

constructor TRGB32BitmapCore.CreateFromData(AData: Pointer;
  AWidth, AHeight: integer; ADataOwner: boolean);
begin
  inherited CreateFromData(AData, AWidth, AHeight, 0, ADataOwner);
end;

procedure TRGB32BitmapCore.Assign(Source: TPersistent);
begin
  if (Source is TRGBBitmapCore) and ((Source as TRGBBitmapCore).SizeOfPixel = 0) then
    inherited Assign(Source);
end;

procedure TRGB32BitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = 0 then
    inherited SwapWith(ABitmap);
end;

procedure TRGB32BitmapCore.SaveToLazIntfImage(AImage: TLazIntfImage);
begin
  SaveToLazIntfImage(AImage, Bounds(0, 0, Width, Height));
end;

procedure TRGB32BitmapCore.SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect);
var
  I, J: integer;
  W, H: integer;
begin
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  AImage.SetSize(W, H);
  try
    for J := 0 to Pred(H) do
    begin

      for I := 0 to Pred(W) do
      begin

      end;
    end;
  except
    AImage.Free;
  end;
end;

destructor TRGB32BitmapCore.Destroy;
begin
  inherited;
end;

{ TRGB8BitmapCore }

constructor TRGB8BitmapCore.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight, 1);
end;

constructor TRGB8BitmapCore.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  if ABitmap.SizeOfPixel = 0 then
    inherited CreateAsCopy(ABitmap, 1);
end;

constructor TRGB8BitmapCore.CreateFromData(AData: Pointer;
  AWidth, AHeight: integer; ADataOwner: boolean);
begin
  inherited CreateFromData(AData, AWidth, AHeight, 0, ADataOwner);
end;

procedure TRGB8BitmapCore.LoadFromLazIntfImageAlpha(AImage: TLazIntfImage);
var
  I, J: integer;
begin
  for J := 0 to Pred(Height) do
  begin
    for I := 0 to Pred(Width) do
    begin

    end;
  end;
end;

procedure TRGB8BitmapCore.SaveToLazIntfImageAlpha(AImage: TLazIntfImage);
begin
  SaveToLazIntfImageAlpha(AImage, Bounds(0, 0, Width, Height));
end;

procedure TRGB8BitmapCore.SaveToLazIntfImageAlpha(AImage: TLazIntfImage;
  const ARect: TRect);
var
  I, J: integer;
  F: TFPColor;
begin
  for J := 0 to Pred(AImage.Height) do
  begin
    for I := 0 to Pred(AImage.Width) do
    begin
      F := AImage.Colors[I, J];
      //   F.alpha := P^ shl 8;
      AImage.Colors[I, J] := F;

    end;
  end;
end;

procedure TRGB8BitmapCore.Assign(Source: TPersistent);
begin
  if (Source is TRGBBitmapCore) and ((Source as TRGBBitmapCore).SizeOfPixel = 0) then
    inherited Assign(Source);
end;

procedure TRGB8BitmapCore.SwapWith(ABitmap: TRGBBitmapCore);
begin
  inherited SwapWith(ABitmap);
end;

end.

