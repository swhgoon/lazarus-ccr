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
  TRGBATriple = tagRGBATRIPLE;
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
    property ScanLine[Row: integer]: pRGBATriple read GetScanLine;
    procedure FillEllipse(X1, Y1, X2, Y2: integer); virtual;
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure Delete; virtual;
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

implementation

procedure LazBMPRotate90(const aBitmap: TDLBitmap; IsTurnRight: boolean);
var
  i, j: integer;
  rowIn, rowOut: PRGBATripleArray;
  Bmp: TDLBitmap;
  Width, Height: integer;
  IntfImg1, IntfImg2: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  Bmp := TDLBitmap.Create;
  Bmp.Width := aBitmap.Height;
  Bmp.Height := aBitmap.Width;
  {$ifdef MSWINDOWS}
  Bmp.PixelFormat := pf32bit;
  {$else}
  Bmp.PixelFormat := pf24bit;
  {$endif}
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

procedure BMPRotate90(const Bitmap: TDLBitmap);
var
  i, j: integer;
  rowIn, rowOut: pRGBATriple;
  Bmp: TDLBitmap;
  Width, Height: integer;
begin
  Bmp := TDLBitmap.Create;
  Bmp.Width := Bitmap.Height;
  Bmp.Height := Bitmap.Width;
  Width := Bitmap.Width - 1;
  Height := Bitmap.Height - 1;
  for  j := 0 to Height do
  begin
    rowIn := Bitmap.ScanLine[j];
    for i := 0 to Width do
    begin
      rowOut := Bmp.ScanLine[i];
      rowOut[Height - j] := rowIn[i];
    end;
  end;
  Bmp.InvalidateScanLine;
  Bitmap.Assign(Bmp);
end;

procedure BMPRotate180(const Bitmap: TDLBitmap);
var
  i, j: integer;
  rowIn, rowOut: pRGBATriple;
  Bmp: TDLBitmap;
  Width, Height: integer;
begin
  Bmp := TDLBitmap.Create;
  Bmp.Width := Bitmap.Width;
  Bmp.Height := Bitmap.Height;
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
  Bmp.InvalidateScanLine;
  Bitmap.InvalidateScanLine;
  Bitmap.Assign(Bmp);
end;

procedure BMPRotate270(const Bitmap: TDLBitmap);
var
  i, j: integer;
  rowIn, rowOut: pRGBATriple;
  Bmp: TDLBitmap;
  Width, Height: integer;
begin
  Bmp := TDLBitmap.Create;
  Bmp.Width := Bitmap.Height;
  Bmp.Height := Bitmap.Width;
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
  Bmp.InvalidateScanLine;
  Bitmap.Assign(Bmp);
end;

function RotateBitmap(Bitmap: TDLBitmap; Angle: integer; BackColor: TColor): TDLBitmap;
var
  i, j, iOriginal, jOriginal, CosPoint, SinPoint: integer;
  RowOriginal, RowRotated: pRGBATriple;
  SinTheta, CosTheta: extended;
  AngleAdd: integer;
begin
  Result := TDLBitmap.Create;
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
  Result.InvalidateScanLine;
  Bitmap.InvalidateScanLine;
end;

procedure DrawSamePixel(ABitmap: TDLBitmap; Value: integer);
var
  LNew: TRGBATriple;
  LMinusRatio: real;
  LScan: pRGBATriple;
  i, j: integer;
begin
  for i := 0 to ABitmap.Height - 1 do
  begin
    LScan := ABitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      LNew := LScan[j];
      LScan[j].rgbtBlue := LScan[j].rgbtBlue * Value div 100; //Value; //LNew.rgbtBlue;
      LScan[j].rgbtGreen := LScan[j].rgbtGreen * Value div 100; //LNew.rgbtGreen;
      LScan[j].rgbtRed := LScan[j].rgbtRed * Value div 100; //LNew.rgbtRed;
    end;
  end;
  ABitmap.InvalidateScanLine;
end;

function BitmapFlip(const Vertical: boolean; const Horizontal: boolean;
  var BitmapIn: TDLBitmap; out BitmapOut: TDLBitmap): boolean;
var
  DataIn: pRGBATriple;
  DataOut: pRGBATriple;
  inRow: integer;
  inCol: integer;
begin
  Result := False;
  try
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
          DataOut[inCol] := DataIn[BitmapIn.Width - 1 - inCol];
      end
      else
      begin
        for inCol := 0 to BitmapIn.Width - 1 do
          DataOut[inCol] := DataIn[inCol];
      end;
    end;
    Result := True;
    BitmapOut.InvalidateScanLine;
  except
  end;
end;

procedure InvertBitmap(aBitmap: TDLBitmap);
var
  LNew: TRGBATriple;
  LMinusRatio: real;
  LScan: pRGBATriple;
  i, j: integer;
begin
  for i := 0 to ABitmap.Height - 1 do
  begin
    LScan := ABitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      LNew := LScan[j];
      LScan[j].rgbtBlue := not LScan[j].rgbtBlue;
      LScan[j].rgbtGreen := not LScan[j].rgbtGreen;
      LScan[j].rgbtRed := not LScan[j].rgbtRed;
    end;
  end;
  ABitmap.InvalidateScanLine;
end;

procedure ConvertBitmapToGrayScale(const Bitmap: TDLBitmap);
var
  X: integer;
  Y: integer;
  P: pRGBATriple;
  Gray: byte;
begin
  for Y := 0 to (Bitmap.Height - 1) do
  begin
    P := Bitmap.ScanLine[Y];
    for X := 0 to (Bitmap.Width - 1) do
    begin
      Gray := Round(0.30 * P[X].rgbtBlue + 0.59 * P[X].rgbtGreen +
        0.11 * P[X].rgbtRed);
      P[X].rgbtRed := Gray;
      P[X].rgbtGreen := Gray;
      P[X].rgbtBlue := Gray;
    end;
  end;
  Bitmap.InvalidateScanLine;
end;

constructor TDLBitmap.Create;
begin
  inherited;
  {$ifdef MSWINDOWS}
  PixelFormat := pf32bit;
  {$else}
  PixelFormat := pf24bit;
  {$endif}
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

procedure TDLBitmap.FillEllipse(X1, Y1, X2, Y2: integer);
begin

end;

end.

