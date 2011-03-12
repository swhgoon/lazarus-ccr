unit rgbdrawutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, LMessages, LCLProc, Controls, Graphics,
  Forms, Types, IntfGraphics, FPImage, Math, FPImgCanv, FPCanvas;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..32767] of TRGBTriple;

  TCustomRGBBitmapCore = class(TBitmap)
  private
    FIntfImg: TLazIntfImage;
    FFillColor: TColor;
    FOutlineColor: TColor;
    FPaperColor: TColor;
    function GetScanline(Row: integer): Pointer;
    function GetFillColor: TColor;
    function GetOutlineColor: TColor;
    function GetPaperColor: TColor;
    procedure SetFillColor(const AValue: TColor);
    procedure SetOutlineColor(const AValue: TColor);
    procedure SetPaperColor(const AValue: TColor);
  protected
    procedure SetWidth(Value: integer); override;
    procedure SetHeight(Value: integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyScanLine;
    procedure Clear; virtual;
    procedure ClearWhite; virtual;
    procedure Invert; virtual;
    procedure FlipHorz; virtual;
    procedure FlipVert; virtual;
    procedure Rotate90; virtual;
    procedure Rotate180; virtual;
    procedure Rotate270; virtual;
    procedure FillEllipse(X1, Y1, X2, Y2: integer); virtual;
    property FillColor: TColor read GetFillColor write SetFillColor;
    property OutlineColor: TColor read GetOutlineColor write SetOutlineColor;
    property PaperColor: TColor read GetPaperColor write SetPaperColor;
    property ScanLine[Row: integer]: Pointer read GetScanLine;
  end;

implementation

procedure BMPRotate90(const Bitmap: TCustomRGBBitmapCore);
var
  i, j: integer;
  rowIn, rowOut: PRGBTripleArray;
  Bmp: TCustomRGBBitmapCore;
  Width, Height: integer;
begin
  Bmp := TCustomRGBBitmapCore.Create;
  Bmp.Width := Bitmap.Height;
  Bmp.Height := Bitmap.Width;
  Bmp.PixelFormat := pf24bit;
  Width := Bitmap.Width - 1;
  Height := Bitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := Bitmap.ScanLine[j];
    for i := 0 to Width do
    begin
      rowOut := Bmp.ScanLine[i];
   //   Inc(rowOut, Height - j);
      rowOut^[i * (Height - j)] := rowIn^[i];
   //   Inc(rowIn);
    end;
  end;
  Bmp.ApplyScanLine;
  Bitmap.Assign(Bmp);
end;

procedure BMPRotate180(const Bitmap: TCustomRGBBitmapCore);
var
  i, j: integer;
  rowIn, rowOut: pRGBTriple;
  Bmp: TCustomRGBBitmapCore;
  Width, Height: integer;
begin
  Bmp := TCustomRGBBitmapCore.Create;
  Bmp.Width := Bitmap.Width;
  Bmp.Height := Bitmap.Height;
  Bmp.PixelFormat := pf24bit;
  Width := Bitmap.Width - 1;
  Height := Bitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := Bitmap.ScanLine[j];
    for i := 0 to Width do
    begin
      rowOut := Bmp.ScanLine[Height - j];
      Inc(rowOut, Width - i);
      rowOut^ := rowIn^;
      Inc(rowIn);
    end;
  end;
  Bmp.ApplyScanLine;
  Bitmap.Assign(Bmp);
end;

procedure BMPRotate270(const Bitmap: TCustomRGBBitmapCore);
var
  i, j: integer;
  rowIn, rowOut: pRGBTriple;
  Bmp: TCustomRGBBitmapCore;
  Width, Height: integer;
begin
  Bmp := TCustomRGBBitmapCore.Create;
  Bmp.Width := Bitmap.Height;
  Bmp.Height := Bitmap.Width;
  Bmp.PixelFormat := pf24bit;
  Width := Bitmap.Width - 1;
  Height := Bitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := Bitmap.ScanLine[j];
    for i := 0 to Width do
    begin
      rowOut := Bmp.ScanLine[Width - i];
      Inc(rowOut, j);
      rowOut^ := rowIn^;
      Inc(rowIn);
    end;
  end;
  Bmp.ApplyScanLine;
  Bitmap.Assign(Bmp);
end;

function RotateBitmap(Bitmap: TCustomRGBBitmapCore; Angle: integer;
  BackColor: TColor): TCustomRGBBitmapCore;
var
  i, j, iOriginal, jOriginal, CosPoint, SinPoint: integer;
  RowOriginal, RowRotated: pRGBTriple;
  SinTheta, CosTheta: extended;
  AngleAdd: integer;
begin
  Result := TCustomRGBBitmapCore.Create;
  Result.PixelFormat := pf24bit;
  Result.Canvas.Brush.Color := BackColor;
  Angle := Angle mod 360;
  if Angle < 0 then
    Angle := 360 - Abs(Angle);
  if Angle = 0 then
    Result.Assign(Bitmap)
  else if Angle = 90 then
  begin
    Result.Assign(Bitmap);
    BMPRotate90(Result);
  end
  else if (Angle > 90) and (Angle < 180) then
  begin
    AngleAdd := 90;
    Angle := Angle - AngleAdd;
  end
  else if Angle = 180 then
  begin
    Result.Assign(Bitmap);
    BMPRotate180(Result);
  end
  else if (Angle > 180) and (Angle < 270) then
  begin
    AngleAdd := 180;
    Angle := Angle - AngleAdd;
  end
  else if Angle = 270 then
  begin
    Result.Assign(Bitmap);
    BMPRotate270(Result);
  end
  else if (Angle > 270) and (Angle < 360) then
  begin
    AngleAdd := 270;
    Angle := Angle - AngleAdd;
  end
  else
    AngleAdd := 0;
  if (Angle > 0) and (Angle < 90) then
  begin
    SinCos((Angle + AngleAdd) * Pi / 180, SinTheta, CosTheta);
    if (SinTheta * CosTheta) < 0 then
    begin
      Result.Width := Round(Abs(Bitmap.Width * CosTheta - Bitmap.Height * SinTheta));
      Result.Height := Round(Abs(Bitmap.Width * SinTheta - Bitmap.Height * CosTheta));
    end
    else
    begin
      Result.Width := Round(Abs(Bitmap.Width * CosTheta + Bitmap.Height * SinTheta));
      Result.Height := Round(Abs(Bitmap.Width * SinTheta + Bitmap.Height * CosTheta));
    end;
    CosTheta := Abs(CosTheta);
    SinTheta := Abs(SinTheta);
    if (AngleAdd = 0) or (AngleAdd = 180) then
    begin
      CosPoint := Round(Bitmap.Height * CosTheta);
      SinPoint := Round(Bitmap.Height * SinTheta);
    end
    else
    begin
      SinPoint := Round(Bitmap.Width * CosTheta);
      CosPoint := Round(Bitmap.Width * SinTheta);
    end;
    for j := 0 to Result.Height - 1 do
    begin
      RowRotated := Result.Scanline[j];
      for i := 0 to Result.Width - 1 do
      begin
        case AngleAdd of
          0:
          begin
            jOriginal := Round((j + 1) * CosTheta - (i + 1 - SinPoint) * SinTheta) - 1;
            iOriginal := Round((i + 1) * CosTheta - (CosPoint - j - 1) * SinTheta) - 1;
          end;
          90:
          begin
            iOriginal := Round((j + 1) * SinTheta - (i + 1 - SinPoint) * CosTheta) - 1;
            jOriginal := Bitmap.Height - Round((i + 1) * SinTheta -
              (CosPoint - j - 1) * CosTheta);
          end;
          180:
          begin
            jOriginal := Bitmap.Height - Round((j + 1) * CosTheta -
              (i + 1 - SinPoint) * SinTheta);
            iOriginal := Bitmap.Width - Round((i + 1) * CosTheta -
              (CosPoint - j - 1) * SinTheta);
          end;
          270:
          begin
            iOriginal := Bitmap.Width - Round((j + 1) * SinTheta -
              (i + 1 - SinPoint) * CosTheta);
            jOriginal := Round((i + 1) * SinTheta - (CosPoint - j - 1) * CosTheta) - 1;
          end;
        end;
        if (iOriginal >= 0) and (iOriginal <= Bitmap.Width - 1) and
          (jOriginal >= 0) and (jOriginal <= Bitmap.Height - 1) then
        begin
          RowOriginal := Bitmap.Scanline[jOriginal];
          Inc(RowOriginal, iOriginal);
          RowRotated^ := RowOriginal^;
          Inc(RowRotated);
        end
        else
        begin
          Inc(RowRotated);
        end;
      end;
    end;
  end;
  Result.ApplyScanLine;
end;

procedure DrawSamePixel(ABitmap: TCustomRGBBitmapCore; Value: integer);
var
  LNew: TRGBTriple;
  LMinusRatio: real;
  LScan: PRGBTripleArray;
  i, j: integer;
begin
  for i := 0 to ABitmap.Height - 1 do
  begin
    LScan := ABitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      LNew.rgbtBlue := Value; //LNew.rgbtBlue - Value;
      LNew.rgbtGreen := Value; //LNew.rgbtGreen - Value;
      LNew.rgbtRed := Value; //LNew.rgbtRed - Value;
      LScan^[j] := LNew;
    end;
  end;
  ABitmap.ApplyScanLine;
end;

constructor TCustomRGBBitmapCore.Create;
begin
  inherited;
  PixelFormat := pf24bit;
  FIntfImg := TLazIntfImage.Create(0, 0);
  FIntfImg.LoadFromBitmap(Handle, MaskHandle);
end;

destructor TCustomRGBBitmapCore.Destroy;
begin
  FIntfImg.Free;
  inherited;
end;

function TCustomRGBBitmapCore.GetScanLine(Row: integer): Pointer;
var
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  Result := FIntfImg.GetDataLineStart(Row);
end;

procedure TCustomRGBBitmapCore.ApplyScanLine;
var
  TmpBmp: TBitmap;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  TmpBmp := TBitmap.Create;
  TmpBmp.PixelFormat := pf24bit;
  FIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  TmpBmp.Handle := ImgHandle;
  TmpBmp.MaskHandle := ImgMaskHandle;
  Empty;
  Canvas.Draw(0, 0, TmpBmp);
  TmpBmp.Free;
  FIntfImg.LoadFromBitmap(Handle, MaskHandle);
end;

function TCustomRGBBitmapCore.GetFillColor: TColor;
begin
  Result := FFillColor;
end;

function TCustomRGBBitmapCore.GetOutlineColor: TColor;
begin
  Result := FOutlineColor;
end;

function TCustomRGBBitmapCore.GetPaperColor: TColor;
begin
  Result := FPaperColor;
end;

procedure TCustomRGBBitmapCore.SetFillColor(const AValue: TColor);
begin
  FFillColor := AValue;
end;

procedure TCustomRGBBitmapCore.SetOutlineColor(const AValue: TColor);
begin
  FOutlineColor := AValue;
end;

procedure TCustomRGBBitmapCore.SetPaperColor(const AValue: TColor);
begin
  FPaperColor := AValue;
end;

procedure TCustomRGBBitmapCore.SetWidth(Value: integer);
begin
  inherited;
  FIntfImg.LoadFromBitmap(Handle, MaskHandle);
end;

procedure TCustomRGBBitmapCore.SetHeight(Value: integer);
begin
  inherited;
  FIntfImg.LoadFromBitmap(Handle, MaskHandle);
end;

procedure TCustomRGBBitmapCore.Clear;
begin

end;

procedure TCustomRGBBitmapCore.ClearWhite;
begin

end;

procedure TCustomRGBBitmapCore.Invert;
begin

end;

procedure TCustomRGBBitmapCore.FlipHorz;
begin

end;

procedure TCustomRGBBitmapCore.FlipVert;
begin

end;

procedure TCustomRGBBitmapCore.Rotate90;
begin
  //DrawSamePixel(Self, 240);
  BMPRotate90(Self);
end;

procedure TCustomRGBBitmapCore.Rotate180;
begin
  RotateBitmap(Self, 180, clWhite);
end;

procedure TCustomRGBBitmapCore.Rotate270;
begin
  RotateBitmap(Self, 270, clWhite);
end;

procedure TCustomRGBBitmapCore.FillEllipse(X1, Y1, X2, Y2: integer);
begin

end;

end.

