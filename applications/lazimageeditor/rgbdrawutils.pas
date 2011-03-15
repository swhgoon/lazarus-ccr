unit RGBDrawUtils;

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
    FIntfImgA: TLazIntfImage;
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
    procedure OpenScanLine;
    procedure CloseScanLine;
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
    property ScanLine[Row: integer]: Pointer read GetScanLine;
    procedure FillEllipse(X1, Y1, X2, Y2: integer); virtual;
    property FillColor: TColor read GetFillColor write SetFillColor;
    property OutlineColor: TColor read GetOutlineColor write SetOutlineColor;
    property PaperColor: TColor read GetPaperColor write SetPaperColor;
  end;

procedure LazBMPRotate90(const aBitmap: TCustomRGBBitmapCore; IsTurnRight: boolean);
procedure BMPRotate90(const Bitmap: TCustomRGBBitmapCore);
procedure DrawSamePixel(ABitmap: TCustomRGBBitmapCore; Value: integer);
procedure BMPRotate180(const Bitmap: TCustomRGBBitmapCore);
procedure BMPRotate270(const Bitmap: TCustomRGBBitmapCore);
function RotateBitmap(Bitmap: TCustomRGBBitmapCore; Angle: integer;
  BackColor: TColor): TCustomRGBBitmapCore;
function BitmapFlip(const Vertical: boolean; const Horizontal: boolean;
  var BitmapIn: TCustomRGBBitmapCore; out BitmapOut: TCustomRGBBitmapCore): boolean;
procedure InvertBitmap(aBitmap: TCustomRGBBitmapCore);

implementation

procedure LazBMPRotate90(const aBitmap: TCustomRGBBitmapCore; IsTurnRight: boolean);
var
  i, j: integer;
  rowIn, rowOut: PRGBTripleArray;
  Bmp: TCustomRGBBitmapCore;
  Width, Height: integer;
  IntfImg1, IntfImg2: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  Bmp := TCustomRGBBitmapCore.Create;
  Bmp.Width := aBitmap.Height;
  Bmp.Height := aBitmap.Width;
  Bmp.PixelFormat := pf24bit;
  IntfImg1 := TLazIntfImage.Create(0, 0);
  IntfImg1.LoadFromBitmap(Bmp.Handle, Bmp.MaskHandle);
  IntfImg2 := TLazIntfImage.Create(0, 0);
  IntfImg2.LoadFromBitmap(aBitmap.Handle, aBitmap.MaskHandle);
  Width := aBitmap.Width - 1;
  Height := aBitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := IntfImg2.GetDataLineStart(j);
    for i := 0 to Width do
    begin
      rowOut := IntfImg1.GetDataLineStart(i);
      if IsTurnRight then
        rowOut^[Height - j] := rowIn^[i]
      else
        rowOut^[j] := rowIn^[Width - i];
    end;
  end;
  IntfImg1.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Bmp.Handle := ImgHandle;
  Bmp.MaskHandle := ImgMaskHandle;
  IntfImg1.Free;
  IntfImg2.Free;
  aBitmap.Assign(Bmp);
  Bmp.Free;
end;

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
  Bmp.OpenScanLine;
  Bitmap.OpenScanLine;
  Width := Bitmap.Width - 1;
  Height := Bitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := Bitmap.ScanLine[j];
    for i := 0 to Width do
    begin
      rowOut := Bmp.ScanLine[i];
      rowOut^[Height - j] := rowIn^[i];
    end;
  end;
  Bmp.CloseScanLine;
  Bitmap.CloseScanLine;
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
  Bmp.OpenScanLine;
  Bitmap.OpenScanLine;
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
  Bmp.CloseScanLine;
  Bitmap.CloseScanLine;
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
  Bmp.OpenScanLine;
  Bitmap.OpenScanLine;
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
  Bmp.CloseScanLine;
  Bitmap.CloseScanLine;
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
  Result.OpenScanLine;
  Bitmap.OpenScanLine;
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
  Result.CloseScanLine;
  Bitmap.CloseScanLine;
end;

procedure DrawSamePixel(ABitmap: TCustomRGBBitmapCore; Value: integer);
var
  LNew: TRGBTriple;
  LMinusRatio: real;
  LScan: PRGBTripleArray;
  i, j: integer;
begin
  aBitmap.OpenScanLine;
  for i := 0 to ABitmap.Height - 1 do
  begin
    LScan := ABitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      LNew := LScan^[j];
      LScan^[j].rgbtBlue := LScan^[j].rgbtBlue * Value div 100; //Value; //LNew.rgbtBlue;
      LScan^[j].rgbtGreen := LScan^[j].rgbtGreen * Value div 100; //LNew.rgbtGreen;
      LScan^[j].rgbtRed := LScan^[j].rgbtRed * Value div 100; //LNew.rgbtRed;
      //LScan^[j] := LNew;
    end;
  end;
  ABitmap.CloseScanLine;
end;

function BitmapFlip(const Vertical: boolean; const Horizontal: boolean;
  var BitmapIn: TCustomRGBBitmapCore; out BitmapOut: TCustomRGBBitmapCore): boolean;
var
  DataIn: PRGBTripleArray;
  DataOut: PRGBTripleArray;
  inRow: integer;
  inCol: integer;
begin
  Result := False;
  try
    BitmapIn.OpenScanLine;
    BitmapOut.OpenScanLine;
    if BitmapIn.PixelFormat <> pf24bit then
      Exit;
    with BitmapOut do
    begin
      Width := BitmapIn.Width;
      Height := BitmapIn.Height;
      PixelFormat := BitmapIn.PixelFormat;
    end;
    for inRow := 0 to BitmapIn.Height - 1 do
    begin
      DataIn := BitmapIn.Scanline[inRow];
      if Vertical then
      begin
        DataOut := BitmapOut.ScanLine[BitmapIn.Height - 1 - inRow];
      end
      else
      begin
        DataOut := BitmapOut.ScanLine[inRow];
      end;
      if Horizontal then
      begin
        for inCol := 0 to BitmapIn.Width - 1 do
          DataOut^[inCol] := DataIn^[BitmapIn.Width - 1 - inCol];
      end
      else
      begin
        for inCol := 0 to BitmapIn.Width - 1 do
          DataOut^[inCol] := DataIn^[inCol];
      end;
    end;
    Result := True;
    BitmapIn.CloseScanLine;
    BitmapOut.CloseScanLine;
  except
  end;
end;

procedure InvertBitmap(aBitmap: TCustomRGBBitmapCore);
var
  LNew: TRGBTriple;
  LMinusRatio: real;
  LScan: PRGBTripleArray;
  i, j: integer;
begin
  aBitmap.OpenScanLine;
  for i := 0 to ABitmap.Height - 1 do
  begin
    LScan := ABitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      LNew := LScan^[j];
      LScan^[j].rgbtBlue := not LScan^[j].rgbtBlue;
      LScan^[j].rgbtGreen := not LScan^[j].rgbtGreen;
      LScan^[j].rgbtRed := not LScan^[j].rgbtRed;
    end;
  end;
  ABitmap.CloseScanLine;
end;

procedure ConvertBitmapToGrayScale(const Bitmap: TCustomRGBBitmapCore);
var
  X: integer;
  Y: integer;
  P: PRGBTripleArray;
  Gray: byte;
begin
  Bitmap.OpenScanLine;
  for Y := 0 to (Bitmap.Height - 1) do
  begin
    P := Bitmap.ScanLine[Y];
    for X := 0 to (Bitmap.Width - 1) do
    begin
      Gray := Round(0.30 * P^[X].rgbtBlue + 0.59 * P^[X].rgbtGreen + 0.11 * P^[X].rgbtRed);
      P^[X].rgbtRed := Gray;
      P^[X].rgbtGreen := Gray;
      P^[X].rgbtBlue := Gray;
    end;
  end;
  Bitmap.CloseScanLine;
end;

constructor TCustomRGBBitmapCore.Create;
begin
  inherited;
  PixelFormat := pf24bit;
end;

destructor TCustomRGBBitmapCore.Destroy;
begin
  inherited;
end;

function TCustomRGBBitmapCore.GetScanLine(Row: integer): Pointer;
begin
  if FIntfImgA <> nil then
    Result := FIntfImgA.GetDataLineStart(Row);
end;

procedure TCustomRGBBitmapCore.OpenScanLine;
begin
  FIntfImgA := TLazIntfImage.Create(0, 0);
  FIntfImgA.LoadFromBitmap(Handle, MaskHandle);
end;

procedure TCustomRGBBitmapCore.CloseScanLine;
var
  TmpBmp: TBitmap;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  TmpBmp := TBitmap.Create;
  TmpBmp.PixelFormat := pf24bit;
  FIntfImgA.CreateBitmaps(ImgHandle, ImgMaskHandle, True);
  TmpBmp.Handle := ImgHandle;
  TmpBmp.MaskHandle := ImgMaskHandle;
  Empty;
  Width := TmpBmp.Width;
  Height := TmpBmp.Height;
  Canvas.Draw(0, 0, TmpBmp);
  TmpBmp.Free;
  FIntfImgA.Free;
end;

procedure TCustomRGBBitmapCore.Assign(Source: TPersistent);
begin
  inherited;
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

end;

procedure TCustomRGBBitmapCore.SetHeight(Value: integer);
begin
  inherited;

end;

procedure TCustomRGBBitmapCore.Clear;
begin

end;

procedure TCustomRGBBitmapCore.ClearWhite;
begin

end;

procedure TCustomRGBBitmapCore.Invert;
var
  tmp: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  InvertBitmap(Tmp);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;


procedure TCustomRGBBitmapCore.Grayscale;
var
  tmp: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  ConvertBitmapToGrayScale(Tmp);
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TCustomRGBBitmapCore.FlipHorz;
var
  tmp, tmp2: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp2 := TCustomRGBBitmapCore.Create;
  tmp2.Width := Width;
  tmp2.Height := Height;
  tmp.PixelFormat := pf24bit;
  tmp2.PixelFormat := pf24bit;
  tmp.Canvas.Draw(0, 0, Self);
  BitmapFlip(False, True, tmp, tmp2);
  Canvas.Draw(0, 0, tmp2);
  tmp.Free;
  tmp2.Free;
end;

procedure TCustomRGBBitmapCore.FlipVert;
var
  tmp, tmp2: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp2 := TCustomRGBBitmapCore.Create;
  tmp2.Width := Width;
  tmp2.Height := Height;
  tmp.PixelFormat := pf24bit;
  tmp2.PixelFormat := pf24bit;
  tmp.Canvas.Draw(0, 0, Self);
  BitmapFlip(True, False, tmp, tmp2);
  Canvas.Draw(0, 0, tmp2);
  tmp.Free;
  tmp2.Free;
end;

procedure TCustomRGBBitmapCore.Rotate90;
var
  tmp: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate90(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TCustomRGBBitmapCore.Rotate180;
var
  tmp: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate180(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TCustomRGBBitmapCore.Rotate270;
var
  tmp: TCustomRGBBitmapCore;
begin
  tmp := TCustomRGBBitmapCore.Create;
  tmp.Width := Width;
  tmp.Height := Height;
  tmp.Canvas.Draw(0, 0, Self);
  BMPRotate270(Tmp);
  Self.Width := tmp.Width;
  Self.Height := tmp.Height;
  Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

procedure TCustomRGBBitmapCore.FillEllipse(X1, Y1, X2, Y2: integer);
begin

end;

end.

