{
 /***************************************************************************
                                  RGBGraphics.pas


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
    TRGB32Bitmap is a memory image which allows fast pixel manipulations.
    TExtCanvas is a TRGB32Bitmap canvas for drawing primitives and
      drawing bitmap image into TCanvas.
}
unit BmpRGBGraph;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LCLIntf, FPWriteBMP, LCLType, LCLProc, FPImage, LResources,
  IntfGraphics, GraphType, Graphics, Forms, Math, Clipbrd, BmpRGBTypes, BmpRGBUtils;
  
type

  TMaskFillMode = (mfAdd, mfRemove, mfXOR);

  { TRGBMask }

  TRGBMask = class(TRGB8BitmapCore)
  private
    FBGPen: TPen;
    FFGPen: TPen;
    FFillMode: TMaskFillMode;
    FMaskedPixels: Integer;
  protected
    procedure CreatePens; virtual;
  public
    constructor Create(AWidth, AHeight: Integer); override;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); override;
    destructor Destroy; override;
    
    procedure LoadFromLazIntfImageAlpha(AImage: TLazIntfImage); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateMaskedPixels;
    
    procedure Draw(X, Y: Integer; AMask: TRGBMask);
  
    procedure DrawShapeTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchDrawShapeTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer);
    procedure StretchDrawShapePortionTo(ACanvas: TCanvas; DstX, DstY, DstWidth, DstHeight: Integer;
      DX, DY, DW, DH: Integer);
      
    procedure DrawTo(ACanvas: TCanvas; X, Y: Integer);
    procedure StretchTrunc(AWidth, AHeight: Integer); virtual;
      
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    
    procedure Clear; override;
    procedure ClearWhite; override;
    procedure Invert; override;
  public
    function GetFillProcedure: TDrawPixelProcedure; virtual;
    function IsEmpty: Boolean;
    property BackgroundPen: TPen read FBGPen;
    property ForegroundPen: TPen read FFGPen;
    property FillMode: TMaskFillMode read FFillMode write FFillMode;
  end;

  TSmoothMethod = (smAreaPixel, smBilinear, smBicubic);

  { TRGB32Bitmap }

  TRGB32Bitmap = class(TRGB32BitmapCore)
  private
    FMask: TRGBMask;
  protected
    function CreateDefaultLazIntfImage: TLazIntfImage;
  public
    constructor Create(AWidth, AHeight: Integer); override;
    constructor CreateAsCopy(ABitmap: TRGBBitmapCore); override;
    constructor CreateFromLazIntfImage(AImage: TLazIntfImage); override;
    
    constructor CreateFromFile(const FileName: String); virtual;
    constructor CreateFromBitmap(ABitmap: TRasterImage); virtual;
    destructor Destroy; override;
    
    procedure Assign(Source: TPersistent); override;
    procedure SwapWith(ABitmap: TRGBBitmapCore); override;
    procedure SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect); override;

    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream; AWriterClass: TFPCustomImageWriterClass); virtual;
    procedure SaveToStream(Stream: TStream; const ARect: TRect;
      AWriterClass: TFPCustomImageWriterClass); virtual;

    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToLazarusResource(const FileName, Name: String); virtual;
  public
    procedure Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
    
    procedure StretchTrunc(AWidth, AHeight: Integer); virtual;
    procedure StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod); virtual;

    procedure Grayscale; override;
    procedure Disable; virtual;
    
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure Delete; override;
    
    procedure FlipHorz; override;
    procedure FlipVert; override;
    procedure Rotate90; override;
    procedure Rotate180; override;
    procedure Rotate270; override;
  public
    property Mask: TRGBMask read FMask write FMask;
  end;

implementation

{ TRGB32Bitmap }

function TRGB32Bitmap.CreateDefaultLazIntfImage: TLazIntfImage;
var
  RID: TRawImageDescription;
  DC: HDC;
begin
  DC := GetDC(0);
  try
    RawImage_DescriptionFromDevice(DC, RID);
  finally
    ReleaseDC(0, DC);
  end;

  Result := TLazIntfImage.Create(0, 0);
  Result.DataDescription := RID;
end;

constructor TRGB32Bitmap.Create(AWidth, AHeight: Integer);
begin
  inherited;
  FMask := TRGBMask.Create(AWidth, AHeight);
end;

constructor TRGB32Bitmap.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  inherited;
  if ABitmap is TRGB32Bitmap then
    FMask := TRGBMask.CreateAsCopy((ABitmap as TRGB32Bitmap).Mask)
  else
    FMask := TRGBMask.Create(ABitmap.Width, ABitmap.Height);
end;

constructor TRGB32Bitmap.CreateFromLazIntfImage(AImage: TLazIntfImage);
begin
  inherited CreateFromLazIntfImage(AImage);
  FMask.LoadFromLazIntfImageAlpha(AImage);
end;

constructor TRGB32Bitmap.CreateFromFile(const FileName: String);
var
  Image: TLazIntfImage;
begin
  Image := CreateDefaultLazIntfImage;
  try
    Image.LoadFromFile(FileName);
    CreateFromLazIntfImage(Image);
  finally
    Image.Free;
  end;
end;

constructor TRGB32Bitmap.CreateFromBitmap(ABitmap: TRasterImage);
{var
  Image: TLazIntfImage;
begin
  Image := ABitmap.CreateIntfImage;
  try
    CreateFromLazIntfImage(Image);
  finally
    Image.Free;
  end;
end;          }
begin
  Create(ABitmap.Width, ABitmap.Height);
  Canvas.Draw(0,0,ABitmap);
end;

destructor TRGB32Bitmap.Destroy;
begin
  FMask.Free;
  inherited;
end;

procedure TRGB32Bitmap.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRGB32Bitmap then
  begin
    Mask.Assign((Source as TRGB32Bitmap).Mask);
  end;
end;

procedure TRGB32Bitmap.SwapWith(ABitmap: TRGBBitmapCore);
begin
  inherited SwapWith(ABitmap);
  if ABitmap is TRGB32Bitmap then
  begin
    Mask.SwapWith((ABitmap as TRGB32Bitmap).Mask);
  end;
end;

procedure TRGB32Bitmap.SaveToLazIntfImage(AImage: TLazIntfImage; const ARect: TRect);
begin
  inherited SaveToLazIntfImage(AImage, ARect);
  if not Mask.IsEmpty then FMask.SaveToLazIntfImageAlpha(AImage, ARect);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, Bounds(0, 0, Width, Height), TLazWriterXPM);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream;
  AWriterClass: TFPCustomImageWriterClass);
begin
  SaveToStream(Stream, Bounds(0, 0, Width, Height), AWriterClass);
end;

procedure TRGB32Bitmap.SaveToStream(Stream: TStream; const ARect: TRect;
  AWriterClass: TFPCustomImageWriterClass);
var
  Image: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  Image := CreateDefaultLazIntfImage;
  Writer := AWriterClass.Create;
  try
    SaveToLazIntfImage(Image, ARect);
    Image.SaveToStream(Stream, Writer);
  finally
    Writer.Free;
    Image.Free;
  end;
end;

procedure TRGB32Bitmap.SaveToFile(const FileName: String);
var
  Image: TLazIntfImage;
begin
  Image := CreateDefaultLazIntfImage;
  try
    inherited SaveToLazIntfImage(Image);
    Image.SaveToFile(FileName);
  finally
    Image.Free;
  end;
end;

procedure TRGB32Bitmap.SaveToLazarusResource(const FileName, Name: String);
var
  PixmapStream, ResourceStream: TMemoryStream;
  FileStream: TFileStream;
begin
  PixmapStream := TMemoryStream.Create;
  ResourceStream := TMemoryStream.Create;
  try
    SaveToStream(PixmapStream);
    PixmapStream.Position := 0;
    
    BinaryToLazarusResourceCode(PixmapStream, ResourceStream, Name, 'XPM');
    
    ResourceStream.Position := 0;
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.CopyFrom(ResourceStream, ResourceStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    PixmapStream.Free;
    ResourceStream.Free;
  end;
end;

procedure TRGB32Bitmap.Draw(X, Y: Integer; ABitmap: TRGB32Bitmap);
begin
  Canvas.Draw(X, Y, ABitmap);
end;

procedure TRGB32Bitmap.StretchTrunc(AWidth, AHeight: Integer);
var
  Result: TRGB32Bitmap;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;
  Result := TRGB32Bitmap.Create(AWidth, AHeight);
  try
//    StretchRGB32BitmapTrunc(Result, Self);
    inherited SwapWith(Result);
    Mask.StretchTrunc(AWidth, AHeight);
  finally
    FreeAndNil(Result);
  end;
end;

procedure TRGB32Bitmap.StretchSmooth(AWidth, AHeight: Integer; Method: TSmoothMethod);
begin
  //
end;

procedure TRGB32Bitmap.Grayscale;
begin
  inherited;
end;

procedure TRGB32Bitmap.Disable;
begin
  DisableRGB32Bitmap(Self);
end;

procedure TRGB32Bitmap.CutToClipboard;
begin
  CopyToClipboard;
  Delete;
end;

procedure TRGB32Bitmap.CopyToClipboard;
{var
  PixmapStream, BitmapStream: TMemoryStream;
  PixmapWriter, BitmapWriter: TFPCustomImageWriter;
  Image: TLazIntfImage;
  R: TRect;
begin
  PixmapStream := TMemoryStream.Create;
  BitmapStream := TMemoryStream.Create;
  Image := CreateDefaultLazIntfImage;
  PixmapWriter := TLazWriterXPM.Create;
  BitmapWriter := TFPWriterBMP.Create;
  try
    Clipboard.Open;
    try
      Clipboard.Clear;
      
      Image.SaveToStream(PixmapStream, PixmapWriter);
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfPixmap), PixmapStream);

      Image.SaveToStream(BitmapStream, BitmapWriter);
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfBitmap), BitmapStream);
    finally
      Clipboard.Close;
    end;
  finally
    PixmapStream.Free;
    BitmapStream.Free;
    Image.Free;
    PixmapWriter.Free;
    BitmapWriter.Free;
  end;
end;
}
begin
  inherited;
end;

procedure TRGB32Bitmap.Delete;
begin
  //Fill(PaperColor);
  inherited;
end;

procedure TRGB32Bitmap.FlipHorz;
begin
  inherited;
  Mask.FlipHorz;
end;

procedure TRGB32Bitmap.FlipVert;
begin
  inherited;
  Mask.FlipVert;
end;

procedure TRGB32Bitmap.Rotate90;
begin
  inherited;
  Mask.Rotate90;
end;

procedure TRGB32Bitmap.Rotate180;
begin
  inherited;
  Mask.Rotate180;
end;

procedure TRGB32Bitmap.Rotate270;
begin
  inherited;
  Mask.Rotate270;
end;

{ TRGBMask }

procedure TRGBMask.CreatePens;
begin
  FBGPen := TPen.Create;
  FBGPen.Color := clYellow;
  
  FFGPen := TPen.Create;
  FFGPen.Color := clBlue;
  //FFGPen.Style := psDot;
end;

function TRGBMask.GetFillProcedure: TDrawPixelProcedure;
begin
end;

constructor TRGBMask.Create(AWidth, AHeight: Integer);
begin
  inherited Create(AWidth, AHeight);
  Clear;
  
  CreatePens;
end;

constructor TRGBMask.CreateAsCopy(ABitmap: TRGBBitmapCore);
begin
  inherited CreateAsCopy(ABitmap);
  UpdateMaskedPixels;
  CreatePens;
end;

procedure TRGBMask.SwapWith(ABitmap: TRGBBitmapCore);
begin
  inherited SwapWith(ABitmap);
  UpdateMaskedPixels;
end;

procedure TRGBMask.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  UpdateMaskedPixels;
end;

procedure TRGBMask.UpdateMaskedPixels;
begin

end;

procedure TRGBMask.Draw(X, Y: Integer; AMask: TRGBMask);
begin
//  DrawRGB8Bitmap(Self, X, Y, AMask);
  UpdateMaskedPixels;
end;

destructor TRGBMask.Destroy;
begin
  FBGPen.Free;
  FFGPen.Free;

  inherited Destroy;
end;

procedure TRGBMask.LoadFromLazIntfImageAlpha(AImage: TLazIntfImage);
begin
  inherited LoadFromLazIntfImageAlpha(AImage);
  UpdateMaskedPixels;
end;

procedure TRGBMask.DrawShapeTo(ACanvas: TCanvas; X, Y: Integer);
begin
  StretchDrawShapeTo(ACanvas, X, Y, Width, Height);
end;

procedure TRGBMask.StretchDrawShapeTo(ACanvas: TCanvas; DstX, DstY, DstWidth,
  DstHeight: Integer);
begin
  StretchDrawShapePortionTo(ACanvas, DstX, DstY, DstWidth, DstHeight,
    0, 0, Width, Height);
end;

procedure TRGBMask.StretchDrawShapePortionTo(ACanvas: TCanvas; DstX, DstY,
  DstWidth, DstHeight: Integer; DX, DY, DW, DH: Integer);
begin
  if ACanvas <> nil then
//    StretchDrawRGBMaskShapePortion(ACanvas.Handle, DstX, DstY, DstWidth, DstHeight,
//      Self, DX, DY, DW, DH, FBGPen.Reference.Handle, FFGPen.Reference.Handle);
end;

procedure TRGBMask.DrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  if ACanvas <> nil then
  //  DrawRGB8Bitmap(ACanvas.Handle, X, Y, 0, 0, Width, Height, Self);
end;

procedure TRGBMask.StretchTrunc(AWidth, AHeight: Integer);
var
  Result: TRGBMask;
begin
  if (AWidth = Width) and (AHeight = Height) then Exit;
  Result := TRGBMask.Create(AWidth, AHeight);
  try
//    StretchRGB8BitmapTrunc(Result, Self);
    SwapWith(Result);
  finally
    FreeAndNil(Result);
  end;
end;

procedure TRGBMask.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  //FillPixelRect(X1, Y1, X2, Y2, GetFillProcedure);
end;

procedure TRGBMask.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  //EllipticRectangle(X1, Y1, X2, Y2, 0, 0, GetFillProcedure, GetFillProcedure);
end;

procedure TRGBMask.Clear;
begin
  inherited Clear;
  FMaskedPixels := 0;
end;

procedure TRGBMask.ClearWhite;
begin
  inherited ClearWhite;
  
  FMaskedPixels := Width * Height;
end;

procedure TRGBMask.Invert;
begin
  inherited Invert;
  
  FMaskedPixels := Width * Height - FMaskedPixels;
end;

function TRGBMask.IsEmpty: Boolean;
begin
  Result := FMaskedPixels = 0;
end;

end.

