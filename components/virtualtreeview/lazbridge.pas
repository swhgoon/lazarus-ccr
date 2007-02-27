unit lazbridge;

{ *************************************************************************** }
{ Copyright (c) 2007 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

{$MODE objfpc}{$H+}

{$DEFINE VER_VTV} //Version for VTV.

interface

uses Classes, SysUtils, Graphics, GraphType, InterfaceBase, LCLType,
  IntfGraphics, FPimage, LCLIntf, ExtDlgs, FileUtil, ExtCtrls,
  opbitmap {$IFNDEF VER_VTV} , opbitmapformats  {$ENDIF};


type

{ TMyIntfImage }

  TMyIntfImage = class(TLazIntfImage)
  public
    procedure CreateBitmapLateMask(var Bitmap, MaskBitmap: HBitmap;
      AlwaysCreateMask: boolean; const RawImage: TRawImage);
  end;

  { TOPOpenDialog }

  {$IFNDEF VER_VTV}
  TOPOpenDialog = class(TOpenPictureDialog)
  private
    FPreviewFilename: string;
  protected
    procedure UpdatePreview; override;
    function Execute: boolean; override;
  end;
  
  
  { TLazOPPicture }

  TLazOPPicture=class(TOPPicture)
  private
   fImage:TImage;
   fUpdateImageSize:Boolean;
  public
   constructor Create(Image:TImage);
   procedure DrawImage;
   property  UpdateImageSize:Boolean read fUpdateImageSize write fUpdateImageSize;
  end;
  {$ENDIF}

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap; PreserveFormat: boolean = true);
procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);

implementation

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
var int: TLazIntfImage;
  i: integer;
  x, y: integer;
begin
  int := Bitmap.CreateIntfImage;
  OpBitmap.Width := int.Width;
  OpBitmap.Height := int.Height;
  OpBitmap.Pixelformat := PixelFormatFromBPP(Int.DataDescription.BitsPerPixel);
  for y := 0 to OpBitmap.Height - 1 do
    for x := 0 to OpBitmap.Width - 1 do
      OpBitmap.Pixels[X, Y] := Int.TColors[X, Y];
  if Bitmap.Transparent then
    OpBitmap.TransparentColor := Bitmap.TransparentColor else OPBitmap.Transparent:=false;
end;


procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap; PreserveFormat: boolean = true);
var int: TMyIntfImage;
var bmph, mbmph: HBitmap;
  x, y: integer;
  pmask: PByte;
  rawi: TRawImage;
  OPBitmap: TOpBitmap;
begin
  if PreserveFormat then
  begin
    OpBitmap := TOPBitmap.create;
    OpBitmap.Assign(SourceBitmap);
  end else OpBitmap := SourceBitmap;

  Int := TMyIntfImage.Create(0, 0);
  Int.AutoCreateMask := false;
  Int.GetDescriptionFromDevice(0);
  Int.Width := OpBitmap.Width;
  Int.Height := OpBitmap.Height;
  OpBitmap.Pixelformat := PixelFormatFromBPP(Int.DataDescription.BitsPerPixel);
  for y := 0 to OpBitmap.Height - 1 do
    for x := 0 to OpBitmap.Width - 1 do
      Int.TColors[X, Y] := OpBitmap.Pixels[X, Y];

  if OPBitmap.Transparent then
  begin
    int.GetRawImage(Rawi);
    rawi.MaskSize := OpBitmap.GetTransparentMask(0, pmask,
      Rawi.Description.AlphaBitOrder = riboReversedBits,
      TOPRawImageLineEnd(Rawi.Description.AlphaLineEnd));
    rawi.Mask := pmask;
(*    writeln(RawImageDescriptionAsString(@Rawi));
    writeln('bwid: ',OpBitmap.Width, ' bhei: ',OpBitmap.Height,' rmsiz:',Rawi.MaskSize); *)
    Int.CreateBitmapLateMask(bmph, mbmph, false, rawi);
  end else
  begin
    Int.CreateBitmap(bmph, mbmph, false);
  end;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.Handle := bmph;
  Bitmap.MaskHandle := mbmph;
  Int.free;
  if PreserveFormat then OPBitmap.free;
end;

procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.create;
  AssignOpBitmapToBitmap(OpBitmap, Bmp);
  aCanvas.Draw(X, Y, bmp);
  Bmp.free;
end;


{$IFNDEF VER_VTV}

{ TOPOpenDialog }

procedure TOPOpenDialog.UpdatePreview;
var
  CurFilename: string;
  FileIsValid: boolean;
  OP: TOPPicture;
  LBPP: Integer;
begin
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExists(FPreviewFilename)
    and (not DirPathExists(FPreviewFilename))
    and FileIsReadable(FPreviewFilename);
  if FileIsValid then
  try
    OP := TOPPicture.create;
    try
      OP.LoadFromFile(FPreviewFilename);
      LBPP := OP.Bitmap.BPP;
      OP.Bitmap.Transparent := false;
      AssignOpBitmapToBitmap(Op.Bitmap, ImageCtrl.Picture.Bitmap, false);
      PictureGroupBox.Caption := Format('(%dx%d BPP:%d)',
        [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height, LBPP]);
    finally
      OP.free;
    end;
  except
    FileIsValid := False;
  end;
  if not FileIsValid then
    ClearPreview;
end;

function TOPOpenDialog.Execute: boolean;
begin
  Filter := OPGLoadFilters;
  result := inherited Execute;
end;

{$ENDIF}


{ TMyIntfImage }

procedure TMyIntfImage.CreateBitmapLateMask(var Bitmap, MaskBitmap: HBitmap;
  AlwaysCreateMask: boolean; const RawImage: TRawImage);
var
  ARawImage: TRawImage;
begin
  GetRawImage(ARawImage);
  ARawImage.Mask := RawImage.Mask;
  ARawImage.MaskSize := RawImage.MaskSize;
  if not CreateBitmapFromRawImage(ARawImage, Bitmap, MaskBitmap, AlwaysCreateMask)
    then
    raise FPImageException.Create('Failed to create bitmaps');
end;


{$IFNDEF VER_VTV}

{ TLazOPPicture }

constructor TLazOPPicture.Create(Image: TImage);
begin
  inherited Create;
  fImage:=Image;
  fUpdateImageSize:=true;
end;

procedure TLazOPPicture.DrawImage;
begin
  if fImage<>nil then
  begin
  if fUpdateImageSize then fImage.SetBounds(0,0,Bitmap.Width,Bitmap.Height);
  AssignOpBitmapToBitmap(Bitmap, fImage.Picture.Bitmap);
  fImage.invalidate;
  end;
end;

{$ENDIF}

end.
unit lazbridge;

{ *************************************************************************** }
{ Copyright (c) 2007 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

{$MODE objfpc}{$H+}

{$DEFINE VER_VTV} //Version for VTV.

interface

uses Classes, SysUtils, Graphics, GraphType, InterfaceBase, LCLType,
  IntfGraphics, FPimage, LCLIntf, ExtDlgs, FileUtil, ExtCtrls,
  opbitmap {$IFNDEF VER_VTV} , opbitmapformats  {$ENDIF};


type

{ TMyIntfImage }

  TMyIntfImage = class(TLazIntfImage)
  public
    procedure CreateBitmapLateMask(var Bitmap, MaskBitmap: HBitmap;
      AlwaysCreateMask: boolean; const RawImage: TRawImage);
  end;

  { TOPOpenDialog }

  {$IFNDEF VER_VTV}
  TOPOpenDialog = class(TOpenPictureDialog)
  private
    FPreviewFilename: string;
  protected
    procedure UpdatePreview; override;
    function Execute: boolean; override;
  end;
  
  
  { TLazOPPicture }

  TLazOPPicture=class(TOPPicture)
  private
   fImage:TImage;
   fUpdateImageSize:Boolean;
  public
   constructor Create(Image:TImage);
   procedure DrawImage;
   property  UpdateImageSize:Boolean read fUpdateImageSize write fUpdateImageSize;
  end;
  {$ENDIF}

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap; PreserveFormat: boolean = true);
procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);

implementation

procedure AssignBitmapToOpBitmap(Bitmap: TBitmap; OpBitmap: TOpBitmap);
var int: TLazIntfImage;
  i: integer;
  x, y: integer;
begin
  int := Bitmap.CreateIntfImage;
  OpBitmap.Width := int.Width;
  OpBitmap.Height := int.Height;
  OpBitmap.Pixelformat := PixelFormatFromBPP(Int.DataDescription.BitsPerPixel);
  for y := 0 to OpBitmap.Height - 1 do
    for x := 0 to OpBitmap.Width - 1 do
      OpBitmap.Pixels[X, Y] := Int.TColors[X, Y];
  if Bitmap.Transparent then
    OpBitmap.TransparentColor := Bitmap.TransparentColor;
end;


procedure AssignOpBitmapToBitmap(SourceBitmap: TOpBitmap; Bitmap: TBitmap; PreserveFormat: boolean = true);
var int: TMyIntfImage;
var bmph, mbmph: HBitmap;
  x, y: integer;
  pmask: PByte;
  rawi: TRawImage;
  OPBitmap: TOpBitmap;
begin
  if PreserveFormat then
  begin
    OpBitmap := TOPBitmap.create;
    OpBitmap.Assign(SourceBitmap);
  end else OpBitmap := SourceBitmap;

  Int := TMyIntfImage.Create(0, 0);
  Int.AutoCreateMask := false;
  Int.GetDescriptionFromDevice(0);
  Int.Width := OpBitmap.Width;
  Int.Height := OpBitmap.Height;
  OpBitmap.Pixelformat := PixelFormatFromBPP(Int.DataDescription.BitsPerPixel);
  for y := 0 to OpBitmap.Height - 1 do
    for x := 0 to OpBitmap.Width - 1 do
      Int.TColors[X, Y] := OpBitmap.Pixels[X, Y];

  if OPBitmap.Transparent then
  begin
    int.GetRawImage(Rawi);
    rawi.MaskSize := OpBitmap.GetTransparentMask(0, pmask,
      Rawi.Description.AlphaBitOrder = riboReversedBits,
      Rawi.Description.AlphaLineEnd = rileWordBoundary);
    rawi.Mask := pmask;
{    writeln(RawImageDescriptionAsString(@Rawi));
    writeln('bwid: ',OpBitmap.Width, ' bhei: ',OpBitmap.Height,' rmsiz:',Rawi.MaskSize); }
    Int.CreateBitmapLateMask(bmph, mbmph, false, rawi);
  end else
  begin
    Int.CreateBitmap(bmph, mbmph, false);
  end;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.Handle := bmph;
  Bitmap.MaskHandle := mbmph;
  Int.free;
  if PreserveFormat then OPBitmap.free;
end;

procedure AssignOpBitmapToCanvas(OpBitmap: TOpBitmap; aCanvas: Graphics.TCanvas; X, Y: integer);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.create;
  AssignOpBitmapToBitmap(OpBitmap, Bmp);
  aCanvas.Draw(X, Y, bmp);
  Bmp.free;
end;


{$IFNDEF VER_VTV}

{ TOPOpenDialog }

procedure TOPOpenDialog.UpdatePreview;
var
  CurFilename: string;
  FileIsValid: boolean;
  OP: TOPPicture;
  LBPP: Integer;
begin
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExists(FPreviewFilename)
    and (not DirPathExists(FPreviewFilename))
    and FileIsReadable(FPreviewFilename);
  if FileIsValid then
  try
    OP := TOPPicture.create;
    try
      OP.LoadFromFile(FPreviewFilename);
      LBPP := OP.Bitmap.BPP;
      OP.Bitmap.Transparent := false;
      AssignOpBitmapToBitmap(Op.Bitmap, ImageCtrl.Picture.Bitmap, false);
      PictureGroupBox.Caption := Format('(%dx%d BPP:%d)',
        [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height, LBPP]);
    finally
      OP.free;
    end;
  except
    FileIsValid := False;
  end;
  if not FileIsValid then
    ClearPreview;
end;

function TOPOpenDialog.Execute: boolean;
begin
  Filter := OPGLoadFilters;
  result := inherited Execute;
end;

{$ENDIF}


{ TMyIntfImage }

procedure TMyIntfImage.CreateBitmapLateMask(var Bitmap, MaskBitmap: HBitmap;
  AlwaysCreateMask: boolean; const RawImage: TRawImage);
var
  ARawImage: TRawImage;
begin
  GetRawImage(ARawImage);
  ARawImage.Mask := RawImage.Mask;
  ARawImage.MaskSize := RawImage.MaskSize;
  if not CreateBitmapFromRawImage(ARawImage, Bitmap, MaskBitmap, AlwaysCreateMask)
    then
    raise FPImageException.Create('Failed to create bitmaps');
end;


{$IFNDEF VER_VTV}

{ TLazOPPicture }

constructor TLazOPPicture.Create(Image: TImage);
begin
  inherited Create;
  fImage:=Image;
  fUpdateImageSize:=true;
end;

procedure TLazOPPicture.DrawImage;
begin
  if fImage<>nil then
  begin
  if fUpdateImageSize then fImage.SetBounds(0,0,Bitmap.Width,Bitmap.Height);
  AssignOpBitmapToBitmap(Bitmap, fImage.Picture.Bitmap);
  fImage.invalidate;
  end;
end;

{$ENDIF}

end.
