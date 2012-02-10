unit FastBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  TFastBitmapPixel = integer;
  (*TFastBitmapPixel = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;*)
  PFastBitmapPixel = ^TFastBitmapPixel;

  TFastBitmapPixelComponents = packed record
    B, G, R, A: byte;
  end;

const
  FastPixelSize = SizeOf(TFastBitmapPixel);

type
  { TFastBitmap }

  TFastBitmap = class
  private
    FPixelsData: PByte;
    FSize: TPoint;
    function GetPixel(X, Y: integer): TFastBitmapPixel; inline;
    procedure SetPixel(X, Y: integer; const AValue: TFastBitmapPixel); inline;
    procedure SetSize(const AValue: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RandomImage;
    property Size: TPoint read FSize write SetSize;
    property Pixels[X, Y: integer]: TFastBitmapPixel read GetPixel write SetPixel;
    property PixelsData: PByte read FPixelsData;
  end;

function RGBA(red, green, blue, alpha: byte): integer;
function SwapBRComponent(Value: integer): integer; inline;
function NoSwapBRComponent(Value: integer): integer; inline;

implementation

function RGBA(red, green, blue, alpha: byte): integer;
var
  tmp: TFastBitmapPixelComponents;
begin
  tmp.R := red;
  tmp.G := green;
  tmp.B := blue;
  tmp.A := alpha;
  Result := TFastBitmapPixel(tmp);
end;

function SwapBRComponent(Value: integer): integer;
begin
  //  Result := (Value and $00ff00) or ((Value shr 16) and $ff) or ((Value and $ff) shl 16);
  Result := Value;
  TFastBitmapPixelComponents(Result).R := TFastBitmapPixelComponents(Value).B;
  TFastBitmapPixelComponents(Result).B := TFastBitmapPixelComponents(Value).R;
end;

function NoSwapBRComponent(Value: integer): integer;
begin
  //  Result := (Value and $00ff00) or ((Value shr 16) and $ff) or ((Value and $ff) shl 16);
  Result := Value;
  TFastBitmapPixelComponents(Result).B := TFastBitmapPixelComponents(Value).B;
  TFastBitmapPixelComponents(Result).R := TFastBitmapPixelComponents(Value).R;
end;

{ TFastBitmap }

function TFastBitmap.GetPixel(X, Y: integer): TFastBitmapPixel;
begin
  Result := PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) * FastPixelSize)^;
end;

procedure TFastBitmap.SetPixel(X, Y: integer; const AValue: TFastBitmapPixel);
begin
  PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) * FastPixelSize)^ := AValue;
end;

procedure TFastBitmap.SetSize(const AValue: TPoint);
begin
  if (FSize.X = AValue.X) and (FSize.Y = AValue.X) then
    Exit;
  FSize := AValue;
  FPixelsData := ReAllocMem(FPixelsData, FSize.X * FSize.Y * FastPixelSize);
end;

constructor TFastBitmap.Create;
begin
  Size := Point(0, 0);
end;

destructor TFastBitmap.Destroy;
begin
  FreeMem(FPixelsData);
  inherited Destroy;
end;

procedure TFastBitmap.RandomImage;
var
  I, X, Y: integer;
begin
  for I := 0 to 2 do
    for Y := 0 to (Size.Y div 2) - 1 do
      for X := 0 to (Size.X div 3) - 1 do
        Pixels[X + (I * (Size.X div 3)), Y] := 255 shl (I * 8);

  for Y := (Size.Y div 2) to Size.Y - 1 do
    for X := 0 to Size.X - 1 do
      Pixels[X, Y] := Random(256) or (Random(256) shl 16) or (Random(256) shl 8);
end;


end.
