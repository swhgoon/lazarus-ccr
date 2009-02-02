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

  { TRotatedText }

  TRotatedText = class (TCustomRotatedBitmap)
  private
    FText : String;
    procedure SetFont(const AValue: TFont);
    procedure SetText(const Value: String);
    procedure UpdateText;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
  public
    constructor Create; override;
    property Font: TFont write SetFont;
    property Text: String read FText write SetText;
  end;

function CreateRotatedBitmap(SrcImage: TRasterImage; Direction: TRotateDirection): TBitmap;

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
  FGlyph.Draw(Canvas, R, P, FButtonState, Transparent, 0);
end;

{ TRotatedText }

procedure TRotatedText.SetText(const Value: String);
begin
  if Value = FText then
    Exit;
  FText := Value;
  UpdateText;
end;

procedure TRotatedText.SetFont(const AValue: TFont);
begin
  FNormalBitmap.Canvas.Font := AValue;
  FNormalBitmap.Canvas.Font.Quality := fqNonAntialiased;

  UpdateText;
end;

procedure TRotatedText.UpdateText;
var
  TextSize : TSize;
  TransColor: TColor;
begin
  FNormalBitmap.Canvas.Font.Quality := fqNonAntialiased;

  with FNormalBitmap, Canvas do
  begin
    TextSize := TextExtent(FText);
    {$if defined(LCLWin32) or defined (LCLQt)}
    //win32 and Qt does not comput correct text extent when Italic style is set.
    //gtk1/2 does not support Italic at all
    if fsItalic in Font.Style then
      Inc(TextSize.cx, 4);
    {$endif}
    SetSize(TextSize.cx, TextSize.cy);
    if Font.Color <> clFuchsia then
      TransColor := clFuchsia
    else
      TransColor := clWhite;
    Brush.Color := TransColor;
    FillRect(0, 0, FNormalBitmap.Width, FNormalBitmap.Height);
    TextOut(0, 0, FText);
    Mask(TransColor);
  end;
end;

function TRotatedText.GetWidth: Integer;
begin
  if FText <> '' then
    Result := inherited GetWidth
  else
    Result := 0;
end;

function TRotatedText.GetHeight: Integer;
begin
  if FText <> '' then
    Result := inherited GetHeight
  else
    Result := 0;
end;

constructor TRotatedText.Create;
begin
  inherited Create;
  Transparent := True;
end;

end.

