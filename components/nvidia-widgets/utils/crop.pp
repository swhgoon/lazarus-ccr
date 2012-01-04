program crop;

{$mode objfpc}{$h+}

uses
  //readers
  FPReadBMP, fpreadjpeg, FPReadPNG, fpreadpnm, fpreadtga, FPReadXPM,
  //writers
  FPWriteBMP, fpwritejpeg, FPWritePNG, fpwritepnm, fpwritetga, FPWriteXPM,
  FPImage, SysUtils;

var
  img: TFPMemoryImage;
  img2: TFPMemoryImage;
  x1, y1, x2, y2: integer;
  x: integer;
  y: integer;

begin
  img := TFPMemoryImage.Create(0, 0);
  //img.UsePalette := False;
  img.LoadFromFile(ParamStr(1));

  x1 := StrToInt(ParamStr(2));
  y1 := StrToInt(ParamStr(3));
  x2 := StrToInt(ParamStr(4));
  y2 := StrToInt(ParamStr(5));

  img2 := TFPMemoryImage.Create(x2 - x1, y2 - y1);
  //img2.UsePalette := False;

  for x := x1 to x2 - 1 do
    for y := y1 to y2 - 1 do
      img2.Colors[x - x1, y - y1] := img.Colors[x, y];

  img2.SaveToFile(ParamStr(6));

  img.Free;
  img2.Free;
end.

