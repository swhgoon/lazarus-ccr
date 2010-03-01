unit uRotateBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Buttons, LCLType, IntfGraphics, Types;

type

  TRotateDirection = (rdNormal, rdRight, rdLeft);

  { TCustomRotatedBitmap }

  TCustomRotatedBitmap = class
  private
    FActiveBitmap: TBitmap;
    FDirection: TRotateDirection;
    FNormalBitmap: TBitmap;
    FRotatedBitmap: TBitmap;
    FTransparent: Boolean;
    FActiveBitmapNeedsUpdate: Boolean;
    function GetBitmap : TBitmap;
    function GetEmpty: Boolean;
    procedure NormalBitmapChanged(Sender: TObject);
    procedure SetBitmap(const AValue: TBitmap);
    procedure SetDirection(const AValue: TRotateDirection);
    procedure SetTransparent(const AValue: Boolean);
    procedure UpdateActiveBitmap; virtual;
  protected
    procedure NotifyBitmapChange; virtual;
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Transparent: Boolean read FTransparent write SetTransparent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y: Integer); virtual;
    function IsBitmapStored : Boolean;
    property Direction: TRotateDirection read FDirection write SetDirection;
    property Empty: Boolean read GetEmpty;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { TRotatedBitmap }

  TRotatedBitmap = class (TCustomRotatedBitmap)
  public
    property Bitmap;
    property Transparent;
  end;

  { TRotatedGlyph }

  TRotatedGlyph = class (TCustomRotatedBitmap)
  private
    FGlyph : TButtonGlyph;
    FButtonState : TButtonState;
    FOnChange: TNotifyEvent;
    procedure SetButtonState(Value: TButtonState);
    procedure UpdateActiveBitmap; override;
  protected
    procedure NotifyBitmapChange; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y: Integer); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property State: TButtonState read FButtonState write SetButtonState;
    property Bitmap;
    property Transparent;
  end;

function CreateRotatedBitmap(SrcImage: TRasterImage; Direction: TRotateDirection): TBitmap;

procedure DrawRotatedText(Canvas: TCanvas; X, Y, TextWidth, TextHeight: Integer;
  const Text: String; Direction: TRotateDirection);

implementation

uses
  LCLProc;

function CreateRotatedBitmap(SrcImage: TRasterImage; Direction: TRotateDirection): TBitmap;
var
  px, py, nx, ny : Integer;
  RotateImg, NormalImg: TLazIntfImage;
begin
  Result := TBitmap.Create;
  if (SrcImage.Width = 0) or (SrcImage.Height = 0) then
  begin
    Exit;
  end;

  NormalImg := SrcImage.CreateIntfImage;
  RotateImg := TLazIntfImage.Create(NormalImg.Height, NormalImg.Width);
  RotateImg.DataDescription := NormalImg.DataDescription;
  RotateImg.SetSize(NormalImg.Height, NormalImg.Width);
  RotateImg.FillPixels(TColorToFPColor(clBlack));

  for px := 0 to NormalImg.Width - 1 do
    for py := 0 to NormalImg.Height - 1 do
    begin
      if Direction = rdRight then
      begin
        nx := RotateImg.Width - 1 - py;
        ny := px;
      end else begin
        nx := py;
        ny := RotateImg.Height - 1 - px;
      end;

      RotateImg.Colors[nx,ny] := NormalImg.Colors[px,py];
    end;

  Result.LoadFromIntfImage(RotateImg);

  if SrcImage.Masked then
    Result.TransparentColor := SrcImage.TransparentColor;
  Result.Transparent := SrcImage.Transparent;

  RotateImg.Free;
  NormalImg.Free;
end;

procedure DrawRotatedText(Canvas: TCanvas; X, Y, TextWidth, TextHeight: Integer;
  const Text: String; Direction: TRotateDirection);
begin
  case Direction of
    rdNormal:
    begin
      Canvas.Font.Orientation := 0;
      Canvas.TextOut(X, Y, Text);
    end;
    rdLeft:
    begin
      Canvas.Font.Orientation := 900;
      Canvas.TextOut(X, Y + TextHeight, Text);
    end;
    rdRight:
    begin
      Canvas.Font.Orientation := -900;
      Canvas.TextOut(X + TextWidth, Y, Text);
    end;
  end;
end;

{ TCustomRotatedBitmap }

function TCustomRotatedBitmap.GetBitmap: TBitmap;
begin
  Result := FNormalBitmap;
end;

function TCustomRotatedBitmap.GetEmpty: Boolean;
begin
  Result := (FNormalBitmap.Width = 0) or (FNormalBitmap.Height = 0);
end;

procedure TCustomRotatedBitmap.NormalBitmapChanged(Sender: TObject);
begin
  FActiveBitmapNeedsUpdate := True;
  NotifyBitmapChange;
end;

procedure TCustomRotatedBitmap.SetBitmap(const AValue: TBitmap);
begin
  FNormalBitmap.Assign(AValue);
  FActiveBitmapNeedsUpdate := True;
end;

procedure TCustomRotatedBitmap.SetDirection(const AValue: TRotateDirection);
begin
  if FDirection = AValue then
    Exit;
  FDirection := AValue;
  FActiveBitmapNeedsUpdate := True;
end;

procedure TCustomRotatedBitmap.SetTransparent(const AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  FActiveBitmap.Transparent := FTransparent;
end;

procedure TCustomRotatedBitmap.UpdateActiveBitmap;
begin
  FreeAndNil(FRotatedBitmap);
  if FDirection = rdNormal then
    FActiveBitmap := FNormalBitmap
  else
  begin
    FRotatedBitmap := CreateRotatedBitmap(FNormalBitmap, FDirection);
    FActiveBitmap := FRotatedBitmap;
  end;
  FActiveBitmapNeedsUpdate := False;
end;

procedure TCustomRotatedBitmap.NotifyBitmapChange;
begin

end;

function TCustomRotatedBitmap.GetWidth: Integer;
begin
  if FActiveBitmapNeedsUpdate then
    UpdateActiveBitmap;
  Result := FActiveBitmap.Width;
end;

function TCustomRotatedBitmap.GetHeight: Integer;
begin
  if FActiveBitmapNeedsUpdate then
    UpdateActiveBitmap;
  Result := FActiveBitmap.Height;
end;

constructor TCustomRotatedBitmap.Create;
begin
  FDirection := rdNormal;
  FNormalBitmap := TBitmap.Create;
  FNormalBitmap.OnChange := @NormalBitmapChanged;
  FActiveBitmap := FNormalBitmap;
end;

destructor TCustomRotatedBitmap.Destroy;
begin
  FNormalBitmap.Destroy;
  FRotatedBitmap.Free;
end;

procedure TCustomRotatedBitmap.Draw(Canvas: TCanvas; X, Y: Integer);
begin
  if FActiveBitmapNeedsUpdate then
    UpdateActiveBitmap;
  Canvas.Draw(X, Y, FActiveBitmap);
end;

function TCustomRotatedBitmap.IsBitmapStored : Boolean;
begin
  Result := (not FActiveBitmap.Empty)
            and (FActiveBitmap.Width>0) and (FActiveBitmap.Height>0);
end;

{ TRotatedGlyph }

procedure TRotatedGlyph.SetButtonState(Value: TButtonState);
begin
  FButtonState := Value;
end;

procedure TRotatedGlyph.UpdateActiveBitmap;
begin
  inherited UpdateActiveBitmap;
  FGlyph.Glyph := FActiveBitmap;
end;

procedure TRotatedGlyph.NotifyBitmapChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TRotatedGlyph.Create;
begin
  inherited Create;
  FGlyph := TButtonGlyph.Create;
end;

destructor TRotatedGlyph.Destroy;
begin
  FGlyph.Destroy;
  inherited Destroy;
end;

procedure TRotatedGlyph.Draw(Canvas: TCanvas; X, Y: Integer);
var
  R: TRect;
  P: TPoint;
begin
  if FActiveBitmapNeedsUpdate then
    UpdateActiveBitmap;
  R := Rect(0, 0, FActiveBitmap.Width, FActiveBitmap.Height);
  P := Point(X, Y);
  //DebugLn(DbgS(R));
  //DebugLn(DbgS(P));
  //DebugLn('Transparent: '+BoolToStr(Transparent, true));
  FGlyph.Draw(Canvas, R, P, FButtonState, Transparent, 0);
end;

end.

