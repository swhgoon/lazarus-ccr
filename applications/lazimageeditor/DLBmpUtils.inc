{
  Authors: Felipe Monteiro de Carvalho, Yang JiXian

  License: The same modifying LGPL with static linking exception as the LCL

  Those are the TDLBitmap process functions.
}

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
      LScan[j].rgbtBlue := LScan[j].rgbtBlue * Value div 100;
      LScan[j].rgbtGreen := LScan[j].rgbtGreen * Value div 100;
      LScan[j].rgbtRed := LScan[j].rgbtRed * Value div 100;
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

procedure ChangeRGB(SrcBmp: TDLBitmap; RedChange, GreenChange, BlueChange: integer);
var
  SrcRow, DestRow: pRGBATriple;
  i, j: integer; DestBmp: TDLBitmap;
begin
  DestBmp := TDLBitmap.Create;
  DestBmp.Width := SrcBmp.Width;
  DestBmp.Height := SrcBmp.Height;
  for  i := 0 to SrcBmp.Height - 1 do
  begin
    SrcRow := SrcBmp.ScanLine[i];
    DestRow := DestBmp.ScanLine[i];
    for  j := 0 to SrcBmp.Width - 1 do
    begin
      if RedChange > 0 then
        DestRow[j].rgbtRed :=
          Min(255, SrcRow[j].rgbtRed + RedChange)
      else
        DestRow[j].rgbtRed :=
          Max(0, SrcRow[j].rgbtRed + RedChange);
      if GreenChange > 0 then
        DestRow[j].rgbtGreen :=
          Min(255, SrcRow[j].rgbtGreen + GreenChange)
      else
        DestRow[j].rgbtGreen :=
          Max(0, SrcRow[j].rgbtGreen + GreenChange);
      if BlueChange > 0 then
        DestRow[j].rgbtBlue :=
          Min(255, SrcRow[j].rgbtBlue + BlueChange)
      else
        DestRow[j].rgbtBlue :=
          Max(0, SrcRow[j].rgbtBlue + BlueChange);
    end;
  end;
  DestBmp.InvalidateScanLine;
  SrcBmp.Assign(DestBmp);
  DestBmp.Free;
end;

procedure ChangeBrightness(SrcBmp: TDLBitmap; ValueChange: integer);
var
  i, j: integer; DestBmp: TDLBitmap;
  SrcRow, DestRow: pRGBATriple;
begin
  DestBmp := TDLBitmap.Create;
  DestBmp.Width := SrcBmp.Width;
  DestBmp.Height := SrcBmp.Height;
  for  i := 0 to SrcBmp.Height - 1 do
  begin
    SrcRow := SrcBmp.ScanLine[i];
    DestRow := DestBmp.ScanLine[i];
    for  j := 0 to SrcBmp.Width - 1 do
    begin
      if ValueChange > 0 then
      begin
        DestRow[j].rgbtRed :=
          Min(255, SrcRow[j].rgbtRed + ValueChange);
        DestRow[j].rgbtGreen :=
          Min(255, SrcRow[j].rgbtGreen + ValueChange);
        DestRow[j].rgbtBlue :=
          Min(255, SrcRow[j].rgbtBlue + ValueChange);
      end
      else
      begin
        DestRow[j].rgbtRed := Max(0, SrcRow[j].rgbtRed + ValueChange);
        DestRow[j].rgbtGreen :=
          Max(0, SrcRow[j].rgbtGreen + ValueChange);
        DestRow[j].rgbtBlue :=
          Max(0, SrcRow[j].rgbtBlue + ValueChange);
      end;
    end;
  end;
  DestBmp.InvalidateScanLine;
  SrcBmp.Assign(DestBmp);
  DestBmp.Free;
end;

procedure ChangeContrast(SrcBmp: TDLBitmap; ValueChange: integer);
var
  X, Y: integer; DestBmp: TDLBitmap;
  SrcRow, DestRow: pRGBATriple;
begin
  DestBmp := TDLBitmap.Create;
  DestBmp.Width := SrcBmp.Width;
  DestBmp.Height := SrcBmp.Height;
  if valuechange >= 0 then
  begin
    for  Y := 0 to SrcBmp.Height - 1 do
    begin
      SrcRow := SrcBmp.ScanLine[Y];
      DestRow := DestBmp.ScanLine[y];
      for  X := 0 to SrcBmp.Width - 1 do
      begin
        if SrcRow[x].rgbtRed >= 128 then
          DestRow[x].rgbtRed := min(255, SrcRow[x].rgbtRed + ValueChange)
        else
          DestRow[x].rgbtRed := max(0, SrcRow[x].rgbtRed - ValueChange);
        if SrcRow[x].rgbtGreen >= 128 then
          DestRow[x].rgbtGreen := min(255, SrcRow[x].rgbtGreen + ValueChange)
        else
          DestRow[x].rgbtGreen :=
            max(0, SrcRow[x].rgbtGreen - ValueChange);
        if SrcRow[x].rgbtBlue >= 128 then
          DestRow[x].rgbtBlue := min(255, SrcRow[x].rgbtBlue + ValueChange)
        else
          DestRow[x].rgbtBlue :=
            max(0, SrcRow[x].rgbtBlue - ValueChange);
      end;
    end;
  end
  else
  begin
    for  Y := 0 to SrcBmp.Height - 1 do
    begin
      SrcRow := SrcBmp.ScanLine[Y];
      DestRow := DestBmp.ScanLine[y];
      for  X := 0 to SrcBmp.Width - 1 do
      begin
        DestRow[x].rgbtRed := min(128, SrcRow[x].rgbtRed - ValueChange);
        DestRow[x].rgbtGreen :=
          min(128, SrcRow[x].rgbtGreen - ValueChange);
        DestRow[x].rgbtBlue := min(128, SrcRow[x].rgbtBlue - ValueChange);
      end;
    end;
  end;
  DestBmp.InvalidateScanLine;
  SrcBmp.Assign(DestBmp);
  DestBmp.Free;
end;

procedure SetDLBMPPixel(DLBmp: TDLBitmap; x, y: integer; aColor: TColor);
var SrcRow: pRGBATriple;
begin
  if (x >= 0) and (x < DLBmp.Width) and (y >= 0) and (y < DLBmp.Height) then
  begin
    SrcRow := DLBmp.ScanLine[y];
    SrcRow[x].rgbtRed:=GetRColor(aColor);
    SrcRow[x].rgbtGreen:=GetGColor(aColor);
    SrcRow[x].rgbtBlue:=GetBColor(aColor);
  end;
end;

procedure SprayPoints(DLBmp: TDLBitmap; X, Y: integer; Radians: Integer; PColor: TColor);
var
   i, a, b, temp, ci, center, Radian2, Radian3: Integer;
begin
  if DLBmp = nil then
    Exit;
  Randomize;
  for i := 0 to Radians do
  begin
    temp := Random(100);
    a := Random(Round(Radians * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radians * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radians * 0.65))) then
      DLBmp.Pixels[X + a, Y + b] := PColor;
    Radian2 := Radians div 3;
    temp := Random(100);
    a := Random(Round(Radian2 * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radian2 * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radian2 * 0.65))) then
      DLBmp.Pixels[X + a, Y + b] := PColor;
    Radian3 := Radians * 2 div 3;
    temp := Random(100);
    a := Random(Round(Radian3 * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radian3 * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radian3 * 0.65))) then
      DLBmp.Pixels[X + a, Y + b] := PColor;
  end;
  DLBmp.InvalidateScanLine;//Rect(Rect(X - Radians, Y - Radians, X + Radians, Y + Radians));
end;

function GetRColor(const Color: TColor): Byte;
begin
  Result := Color and $FF;
end;

function GetGColor(const Color: TColor): Byte;
begin
  Result := (Color and $FF00) shr 8;
end;

function GetBColor(const Color: TColor): Byte;
begin
  Result := (Color and $FF0000) shr 16;
end;

procedure SprayPoints(aCanvas: TCanvas; X, Y: integer; Radians: Integer; PColor: TColor);
var
   i, a, b, temp, ci, center, Radian2, Radian3: Integer;
begin
  if aCanvas = nil then
    Exit;
  Randomize;
  for i := 0 to Radians do
  begin
    temp := Random(100);
    a := Random(Round(Radians * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radians * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radians * 0.65))) then
      aCanvas.Pixels[X + a, Y + b] := PColor;
    Radian2 := Radians div 3;
    temp := Random(100);
    a := Random(Round(Radian2 * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radian2 * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radian2 * 0.65))) then
      aCanvas.Pixels[X + a, Y + b] := PColor;
    Radian3 := Radians * 2 div 3;
    temp := Random(100);
    a := Random(Round(Radian3 * 0.65));
    if (temp < 50) then a := 0 - a;
    temp := Random(100);
    b := Random(Round(Radian3 * 0.65));
    if (temp < 50) then b := 0 - b;
    if (a * a + b * b < Sqr(Round(Radian3 * 0.65))) then
      aCanvas.Pixels[X + a, Y + b] := PColor;
  end;
end;

procedure DLBMPColorReplace(aBitmap: TDLBitmap; ColorFrom, ColorTo: TColor);
var
  LScan: pRGBATriple;
  i, j: integer;
begin
  for i := 0 to aBitmap.Height - 1 do
  begin
    LScan := aBitmap.Scanline[i];
    for j := 0 to ABitmap.Width - 1 do
    begin
      if (LScan[j].rgbtBlue = GetBColor(ColorFrom)) and
        (LScan[j].rgbtGreen = GetGColor(ColorFrom)) and
        (LScan[j].rgbtRed = GetRColor(ColorFrom)) then
      begin
        LScan[j].rgbtBlue := GetBColor(ColorTo);
        LScan[j].rgbtGreen := GetGColor(ColorTo);
        LScan[j].rgbtRed := GetRColor(ColorTo);
      end;
    end;
  end;
  aBitmap.InvalidateScanLine;
end;

procedure StretchLinear(Dest, Src: TDLBitmap);
var
  sw, sh, dw, dh, B, N, x, y, i, j, k, nPixelSize: DWord;
  pLinePrev, pLineNext, pDest, pA, pB, pC, pD: pRGBATriple;
begin
  sw := Src.Width - 1;
  sh := Src.Height - 1;
  dw := Dest.Width - 1;
  dh := Dest.Height - 1;
  nPixelSize := 3;
  for  i := 0 to dh do
  begin
    pDest := Dest.ScanLine[i];
    y := i * sh div dh;
    N := dh - i * sh mod dh;
    pLinePrev := Src.ScanLine[y];
    Inc(y);
    if N = dh then
    begin
      pLineNext := pLinePrev;
    end
    else
    begin
      pLineNext := Src.ScanLine[y];
    end;
    for  j := 0 to dw do
    begin
      x := j * sw div dw * nPixelSize;
      B := dw - j * sw mod dw;
      pA := pLinePrev;
      Inc(pA, x);
      pB := pA;
      Inc(pB, nPixelSize);
      pC := pLineNext;
      Inc(pC, x);
      pD := pC;
      Inc(pD, nPixelSize);
      if B = dw then
      begin
        pB := pA;
        pD := pC;
      end;
      for  k := 0 to nPixelSize - 1 do
      begin
        pDest^ := DWordToTriple(
          (B * N * DWordTrans(pA^ - pB^ - pC^ + pD^) +
          dw * N * DWordTrans(pB^) +
          dh * B * DWordTrans(pC^) + (dw * dh - dh * B - dw * N) *
          DWordTrans(pD^) +
          dw * dh div 2) div (dw * dh));
        Inc(pDest);
        Inc(pA);
        Inc(pB);
        Inc(pC);
        Inc(pD);
      end;
    end;
  end;
  Dest.InvalidateScanLine;
end;

procedure StretchDLBMP(ACanvas: TCanvas; Src: TDLBitmap; NewLeft, NewTop, NewWidth, NewHeight: integer);
var
  dw, dh, i, j, temph: DWord; Dest: TDLBitmap; sw, sh: Float;
begin
  Dest := TDLBitmap.Create;
  dw := NewWidth - NewLeft;
  dh := NewHeight - NewTop;
  sw := dw / Src.Width;
  sh := dh / Src.Height;
  if sw = 0 then
    sw := 1;
  if sh = 0 then
    sh := 1;
  Dest.Width := dw;
  Dest.Height := dh;
  for  i := 0 to dh - 1 do
  begin
    temph := Trunc(i / sh);
    for  j := 0 to dw - 1 do
      Dest.Pixels[j, i] := Src.Pixels[Trunc(j / sw), temph];
  end;
  Dest.InvalidateScanLine;
  ACanvas.Draw(NewLeft, NewTop, Dest);
  Dest.Free;
end;

function RotatePointEx_0(Center, ThePoint: TPoint; Angle: Double): TPoint;
var
  cosRadians: Double;
  inX: Integer;
  inXOriginal: Integer;
  inXPrime: Integer;
  inXPrimeRotated: Integer;
  inY: Integer;
  inYOriginal: Integer;
  inYPrime: Integer;
  inYPrimeRotated: Integer;
  Radians: Double;
  sinRadians: Double;
begin
  Radians := -(Angle) * PI / 180;
  sinRadians := Sin(Radians);
  cosRadians := Cos(Radians);
  inX := ThePoint.X;
  inY := ThePoint.Y;
  inXPrime := 2 * (inX - Center.y) + 1;
  inYPrime := 2 * (inY - Center.x) + 1;
  inYPrimeRotated := Round(inYPrime * CosRadians - inXPrime * sinRadians);
  inXPrimeRotated := Round(inYPrime * sinRadians + inXPrime * cosRadians);
  inYOriginal := (inYPrimeRotated - 1) div 2 + Center.x;
  inXOriginal := (inXPrimeRotated - 1) div 2 + Center.y;
  Result := Point(inXOriginal, inYOriginal);
end;

function RotatePointEx(Center, ThePoint: TPoint; Angle: Double): TPoint;
var
  cosRadians: Double;
  inX: Integer;
  inXOriginal: Integer;
  inXPrime: Integer;
  inXPrimeRotated: Integer;
  inY: Integer;
  inYOriginal: Integer;
  inYPrime: Integer;
  inYPrimeRotated: Integer;
  Radians: Double;
  sinRadians: Double;
begin
  Radians := -(Angle) * PI / 180;
  sinRadians := Sin(Radians);
  cosRadians := Cos(Radians);
  inX := ThePoint.X;
  inY := ThePoint.Y;
  inXPrime := 2 * (inX - Center.x) + 1;
  inYPrime := 2 * (inY - Center.y) + 1;
  inYPrimeRotated := Round(inYPrime * CosRadians - inXPrime * sinRadians);
  inXPrimeRotated := Round(inYPrime * sinRadians + inXPrime * cosRadians);
  inYOriginal := (inYPrimeRotated - 1) div 2 + Center.y;
  inXOriginal := (inXPrimeRotated - 1) div 2 + Center.x;
  Result := Point(inXOriginal, inYOriginal);
end;

procedure DrawRegularPolygon(aCanvas: TCanvas; Center, ThePoint: TPoint; Count: integer);
var Angle: Double; ptempaddr: array of TPoint; i: integer;
begin
  SetLength(ptempaddr, Count);
  //这是个五角星  Angle := Round(1080 / Count);
  Angle := Round(360 / Count);
  ptempaddr[0] := ThePoint;
  for i := 1 to Count - 1 do
    ptempaddr[i] := RotatePointEx(Center, ptempaddr[i - 1], Angle);
  aCanvas.Polygon(ptempaddr);
end;

procedure StretchDLBMPEx(ACanvas: TCanvas; Src: TDLBitmap;
  NewLeft, NewTop, NewWidth, NewHeight: integer; Posx, Posy, aWidth, aHeight: integer);
var
  iw, ih, th, dw, dh, i, j, k, m, n, ypos, xpos, ylen, xlen: DWord; Dest: TDLBitmap; sw, sh: Float; LScan1, LScan2: pRGBATriple;
begin
  Dest := TDLBitmap.Create;
  dw := NewWidth - NewLeft;
  dh := NewHeight - NewTop;
  sw := dw / Src.Width;
  sh := dh / Src.Height;
  iw := dw div Src.Width;
  ih := dh div Src.Height;
  if sw = 0 then
    sw := 1;
  if sh = 0 then
    sh := 1;
  if iw = 0 then
    iw := 1;
  if ih = 0 then
    ih := 1;
  if dw > aWidth then
  begin
    xlen := Min(aWidth + Posx, dw);
    xpos := Posx;
  end
  else
  begin
    xlen := dw;
    xpos := 0
  end;
  if dh > aHeight then
  begin
    ylen := Min(aHeight + Posy, dh);
    ypos := Posy;
  end
  else
  begin
    ylen := dh;
    ypos := 0
  end;
  if dw > aWidth then
    dw := aWidth;
  if dh > aHeight then
    dh := aHeight;
  Dest.Width := dw;
  Dest.Height := dh;
{  m := Trunc(ypos / ih);
  n := Trunc(xpos / iw);
  for  i := ypos to ylen - 1 do
  begin
    LScan1 := Dest.Scanline[i - ypos];
    LScan2 := Src.Scanline[m];
    if (i mod ih) = 0 then
      m := m + 1;
    k := n;
    for  j := xpos to xlen - 1 do
    begin
      LScan1[j - xpos] := LScan2[k];
      if (j mod iw) = 0 then
        k := k + 1;
    end;
  end;     }
  for  i := ypos to ylen - 1 do
  begin
    LScan1 := Dest.Scanline[i - ypos];
    LScan2 := Src.Scanline[Trunc(i / sh)];
    for  j := xpos to xlen - 1 do
    begin
      k := Trunc(j / sw);
      LScan1[j - xpos] := LScan2[k];
    end;
  end;
  Dest.InvalidateScanLine;
  ACanvas.Draw(NewLeft + posx, NewTop + posy, Dest);
  Dest.Free;
end;

{
procedure StretchDLBMPEx(ACanvas: TCanvas; Src: TDLBitmap;
  NewLeft, NewTop, NewWidth, NewHeight: integer; Posx, Posy, aWidth, aHeight: integer);
var
  iw, ih, th, dw, dh, i, j, k, m, n, q, r, ypos, xpos, ylen, xlen: DWord; Dest: TDLBitmap; sw, sh: Float; LScan1, LScan2: pRGBATriple;
begin
  Dest := TDLBitmap.Create;
  dw := NewWidth - NewLeft;
  dh := NewHeight - NewTop;
  sw := dw / Src.Width;
  sh := dh / Src.Height;
  iw := dw div Src.Width;
  ih := dh div Src.Height;
  if sw = 0 then
    sw := 1;
  if sh = 0 then
    sh := 1;
  if iw = 0 then
    iw := 1;
  if ih = 0 then
    ih := 1;
  Dest.Width := dw;
  Dest.Height := dh;
  if dw > aWidth then
  begin
    xlen := Min(aWidth + Posx, dw);
    xpos := Posx;
  end
  else
  begin
    xlen := dw;
    xpos := 0
  end;
  if dh > aHeight then
  begin
    ylen := Min(aHeight + Posy, dh);
    ypos := Posy;
  end
  else
  begin
    ylen := dh;
    ypos := 0
  end;
  m := ypos div ih;
  n := xpos div iw;
  r := 1;
  q := 1;
  for  i := ypos to ylen - 1 do
  begin
    LScan1 := Dest.Scanline[i];
    LScan2 := Src.Scanline[m];
    if r = ih then
    begin
      m := m + 1;
      r := 0;
    end;
    r := r + 1;
    k := n;
    q := 0;
    for  j := xpos to xlen - 1 do
    begin
      LScan1[j] := LScan2[k];
      if q = iw then
      begin
        k := k + 1;
        q := 0;
      end;
      q := q + 1;
    end;
  end;
  Dest.InvalidateScanLine;
  ACanvas.Draw(NewLeft, NewTop, Dest);
  Dest.Free;
end;    }




