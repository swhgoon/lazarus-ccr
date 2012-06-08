{ Graphic functions for pyramidtiff.

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
program pyramidtiff;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, math, LazFileUtils, CustApp,
  FPimage, FPReadJPEG, FPReadPNG, FPReadBMP, FPImgCanv,
  PyTiGraphics, FPReadTiff, FPTiffCmn, FPWriteTiff;

const
  Version = '1.0';
type

  { TPyramidTiffer }

  TPyramidTiffer = class(TCustomApplication)
  private
    FMinSize: Word;
    FQuiet: boolean;
    //FSkipCheck: boolean;
    FTileHeight: Word;
    FTileWidth: Word;
    FVerbose: boolean;
    procedure LoadTiff(out Img: TPTMemImgBase;
      Reader: TFPReaderTiff; InStream: TMemoryStream;
      var ErrorMsg: string);
    procedure LoadOther(out Img: TPTMemImgBase;
      Reader: TFPCustomImageReader; InStream: TMemoryStream);
    function ShrinkImage(LastImg: TPTMemImgBase): TPTMemImgBase;
    procedure TiffReaderCreateImage(Sender: TFPReaderTiff; IFD: TTiffIFD);
  protected
    procedure DoRun; override;
    procedure ParamError(const Msg: string);
    procedure ReadConfig;
    function CheckIfFileIsPyramidTiled(Filename: string; out ErrorMsg: string): boolean;
    function CheckIfStreamIsPyramidTiled(s: TStream; out ErrorMsg: string): boolean;
    function Convert(InputFilename, OutputFilename: string; out ErrorMsg: string): boolean;
    function Convert(Img: TPTMemImgBase; OutputFilename: string; out ErrorMsg: string): boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(WithHeader: boolean); virtual;
    property TileWidth: Word read FTileWidth write FTileWidth;
    property TileHeight: Word read FTileHeight write FTileHeight;
    property MinSize: Word read FMinSize write FMinSize;
    //property SkipCheck: boolean read FSkipCheck write FSkipCheck;
    property Verbose: boolean read FVerbose write FVerbose;
    property Quiet: boolean read FQuiet write FQuiet;
  end;

function CompareIFDForSize(i1, i2: Pointer): integer;
var
  IFD1: TTiffIFD absolute i1;
  IFD2: TTiffIFD absolute i2;
  Size1: Int64;
  Size2: Int64;
begin
  Size1:=int64(IFD1.ImageWidth)*IFD1.ImageHeight;
  Size2:=int64(IFD2.ImageWidth)*IFD2.ImageHeight;
  if Size1>Size2 then Result:=1
  else if Size1<Size2 then Result:=-1
  else Result:=0;
end;

function StringToList(const LongOpts: string): TStrings;
const
  SepChars = ' '#10#13#9;
var
  L : TStringList;
  Len,I,J : Integer;
begin
  l:=TStringList.Create;
  I:=1;
  Len:=Length(LongOpts);
  while I<=Len do begin
    while Isdelimiter(SepChars,LongOpts,I) do
      Inc(I);
    J:=I;
    while (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
      Inc(J);
    if (I<=J) then
      L.Add(Copy(LongOpts,I,(J-I)));
    I:=J+1;
  end;
  Result:=l;
end;

{ TMyApplication }

procedure TPyramidTiffer.TiffReaderCreateImage(Sender: TFPReaderTiff;
  IFD: TTiffIFD);
var
  Desc: TPTMemImgDesc;
begin
  // free old image
  FreeAndNil(IFD.Img);

  Desc.HasAlpha:=IFD.AlphaBits>0;
  Desc.Gray:=IFD.PhotoMetricInterpretation in [0,1];
  Desc.Depth:=Max(Max(Max(IFD.RedBits,
                          IFD.GreenBits),
                          IFD.BlueBits),
                          IFD.GrayBits);
  IFD.Img:=CreateQVMemImg(Desc,IFD.ImageWidth,IFD.ImageHeight);
end;

function TPyramidTiffer.ShrinkImage(LastImg: TPTMemImgBase): TPTMemImgBase;

  function Half(i: integer): integer;
  begin
    Result:=(i+1) div 2;
    if Result<1 then Result:=1;
  end;

var
  ImgCanvas: TFPImageCanvas;
begin
  Result:=TPTMemImgBase(CreateQVMemImg(LastImg.Desc, Half(LastImg.Width), Half(
    LastImg.Height)));
  ImgCanvas:=TFPImageCanvas.create(Result);
  ImgCanvas.Interpolation:=TLinearInterpolation.Create;
  ImgCanvas.StretchDraw(0, 0, Result.Width, Result.Height, LastImg);
  ImgCanvas.Interpolation.Free;
  ImgCanvas.Free;
end;

procedure TPyramidTiffer.LoadTiff(out Img: TPTMemImgBase;
  Reader: TFPReaderTiff; InStream: TMemoryStream; var ErrorMsg: string);
begin
  Reader.OnCreateImage:=@TiffReaderCreateImage;
  Reader.LoadFromStream(InStream);
  if Reader.ImageCount=0 then begin
    ErrorMsg:='tiff has no image';
    exit;
  end;
  Img:=Reader.GetBiggestImage.Img as TPTMemImgBase;
end;

procedure TPyramidTiffer.LoadOther(out Img: TPTMemImgBase;
  Reader: TFPCustomImageReader; InStream: TMemoryStream);
begin
  Img:=TPTMemImgRGBA8Bit.Create(0, 0);
  Reader.ImageRead(InStream, Img);
  Img:=GetMinimumQVMemImg(Img, true) as TPTMemImgBase;
end;

procedure TPyramidTiffer.DoRun;
var
  InputFilename: String;
  OutputFilename: String;
  ErrorMsg: string;
begin
  ReadConfig;

  if HasOption('min-size') then begin
    MinSize:=StrToInt(GetOptionValue('min-size'));
    if (MinSize<4) or (MinSize>32768) then
      ParamError('min-size out of range (4..32768): '+IntToStr(MinSize));
  end;
  if HasOption('width') then begin
    TileWidth:=StrToInt(GetOptionValue('width'));
    if (TileWidth<4) or (TileWidth>32768) then
      ParamError('width out of range (4..32768): '+IntToStr(TileWidth));
  end;
  if HasOption('height') then begin
    TileHeight:=StrToInt(GetOptionValue('height'));
    if (TileHeight<4) or (TileHeight>32768) then
      ParamError('height out of range (4..32768): '+IntToStr(TileHeight));
  end;

  if HasOption('c') then begin
    // only check
    if HasOption('i') then
      ParamError('can not combine option -c and -i');
    if HasOption('o') then
      ParamError('can not combine option -c and -o');
    InputFilename:=CleanAndExpandFilename(GetOptionValue('c'));
    if not FileExistsUTF8(InputFilename) then
      ParamError('check file not found: '+InputFilename);
    if CheckIfFileIsPyramidTiled(InputFilename,ErrorMsg) then begin
      if not Quiet then
        writeln('ok');
    end else begin
      if not Quiet then
        writeln('not ok: ',ErrorMsg);
      ExitCode:=1;
    end;
  end else begin
    // convert
    if not HasOption('i') then
      ParamError('missing parameter -i');
    if not HasOption('o') then
      ParamError('missing parameter -o');

    InputFilename:=CleanAndExpandFilename(GetOptionValue('i'));
    if not FileExistsUTF8(InputFilename) then
      ParamError('input file not found: '+InputFilename);
    OutputFilename:=CleanAndExpandFilename(GetOptionValue('o'));
    if not DirectoryExistsUTF8(ExtractFilePath(OutputFilename)) then
      ParamError('output directory not found: '+ExtractFilePath(OutputFilename));

    if not Convert(InputFilename,OutputFilename,ErrorMsg) then begin
      if not Quiet then
        writeln('ERROR: ',ErrorMsg);
      ExitCode:=1;
    end;
  end;

  // stop program loop
  Terminate;
end;

procedure TPyramidTiffer.ParamError(const Msg: string);
begin
  writeln('Error: ',Msg);
  writeln;
  WriteHelp(false);
  Halt(2);
end;

procedure TPyramidTiffer.ReadConfig;
const
  ShortOpts = 'hc:i:o:qvV';
  LongOpts = 'help width height min-size quiet verbose version';
var
  LongOptions: TStrings;

  procedure CheckOpts;
  var
    Opts,NonOpts: TStrings;
    ErrorMsg: String;
    i: Integer;
  begin
    Opts:=TStringList.Create;
    NonOpts:=TStringList.Create;
    try
      ErrorMsg:=CheckOptions(ShortOpts,LongOptions,Opts,NonOpts);
      if ErrorMsg<>'' then begin
        ShowException(Exception.Create(ErrorMsg));
        Halt;
      end;
      for i:=0 to NonOpts.Count-1 do
        if NonOpts[i]<>'' then
          ParamError('invalid parameter "'+NonOpts[i]+'"');
    finally
      Opts.Free;
      NonOpts.Free;
    end;
    Verbose:=HasOption('v','verbose');
    Quiet:=HasOption('q','quiet');
  end;

begin
  LongOptions:=StringToList(LongOpts);
  try
    CheckOpts;

    // parse parameters
    if HasOption('h','help') then begin
      WriteHelp(true);
      Halt;
    end;

    // parse parameters
    if HasOption('V','version') then begin
      writeln(Version);
      Halt;
    end;
  finally
    LongOptions.Free;
  end;
end;

function TPyramidTiffer.CheckIfFileIsPyramidTiled(Filename: string; out
  ErrorMsg: string): boolean;
var
  ms: TMemoryStream;
begin
  Result:=false;
  ErrorMsg:='';
  try
    if Verbose then
      writeln('Checking file "',Filename,'"');
    ms:=TMemoryStream.Create;
    try
      ms.LoadFromFile(Filename);
      ms.Position:=0;
      Result:=CheckIfStreamIsPyramidTiled(ms,ErrorMsg);
    finally
      ms.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

function TPyramidTiffer.CheckIfStreamIsPyramidTiled(s: TStream; out
  ErrorMsg: string): boolean;
var
  Reader: TFPReaderTiff;
  i: Integer;
  Img: TTiffIFD;
  SmallerImg: TTiffIFD;
begin
  Result:=false;
  ErrorMsg:='';
  try
    Reader:=TFPReaderTiff.Create;
    try
      ErrorMsg:='this is not a tiff file: ';
      Reader.LoadHeaderFromStream(s);
      ErrorMsg:='error in tiff file: ';
      Reader.LoadIFDsFromStream;
      if Reader.ImageCount<1 then begin
        ErrorMsg:='no images found in tif';
        exit;
      end;
      // sort ascending
      Reader.ImageList.Sort(@CompareIFDForSize);
      SmallerImg:=nil;
      for i:=0 to Reader.ImageCount-1 do begin
        Img:=Reader.Images[i];
        if Verbose then
          writeln('  ',i,'/',Reader.ImageCount,' ',Img.ImageWidth,'x',Img.ImageHeight);
        if (Img.TileWidth<1) or (Img.TileLength<1) then begin
          ErrorMsg:='image '+IntToStr(i)+' is not tiled';
          exit;
        end;
        if SmallerImg=nil then begin
          // this is the smallest image
          if (Img.ImageWidth>DWord(MinSize)*2) or (Img.ImageHeight>DWord(MinSize)*2) then begin
            ErrorMsg:='missing small scale step. min-size='+IntToStr(MinSize)+'.'
              +' Smallest image: '+IntToStr(Img.ImageWidth)+'x'+IntToStr(Img.ImageHeight);
            exit;
          end;
        end else begin
          if (SmallerImg.ImageWidth*2+1)<Img.ImageWidth then begin
            ErrorMsg:='missing scale step between ImageWidth='
              +IntToStr(SmallerImg.ImageWidth)+' and '+IntToStr(Img.ImageWidth);
            exit;
          end;
          if (SmallerImg.ImageHeight*2+1)<Img.ImageHeight then begin
            ErrorMsg:='missing scale step between ImageHeight='
              +IntToStr(SmallerImg.ImageHeight)+' and '+IntToStr(Img.ImageHeight);
            exit;
          end;
        end;
        SmallerImg:=Img;
      end;
      Result:=true;
    finally
      Reader.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=ErrorMsg+E.Message;
    end;
  end;
end;

function TPyramidTiffer.Convert(InputFilename, OutputFilename: string; out
  ErrorMsg: string): boolean;
var
  InStream: TMemoryStream;
  Ext: String;
  ReaderClass: TFPCustomImageReaderClass;
  Reader: TFPCustomImageReader;
  Img: TPTMemImgBase;
  i: Integer;
begin
  Result:=false;
  ErrorMsg:='';
  try
    if Verbose then
      writeln('Reading file "',InputFilename,'"');
    InStream:=TMemoryStream.Create;
    Reader:=nil;
    Img:=nil;
    try
      // load file
      InStream.LoadFromFile(InputFilename);
      InStream.Position:=0;

      // get the right image type reader
      Ext:=lowercase(ExtractFileExt(InputFilename));
      Delete(Ext,1,1); // delete '.'
      if (Ext='tif') or (Ext='tiff') then
        ReaderClass:=TFPReaderTiff
      else begin
        for i:=0 to ImageHandlers.Count-1 do begin
          if Pos(Ext,ImageHandlers.Extentions[ImageHandlers.TypeNames[i]])<1
          then continue;
          ReaderClass:=ImageHandlers.ImageReader[ImageHandlers.TypeNames[i]];
          if Verbose then
            writeln('reading ',ImageHandlers.TypeNames[i]);
        end;
      end;
      if ReaderClass=nil then begin
        ErrorMsg:='unknown file extension "'+Ext+'"';
        exit;
      end;
      Reader:=ReaderClass.Create;

      // parse image
      if Reader is TFPReaderTiff then begin
        LoadTiff(Img, TFPReaderTiff(Reader), InStream, ErrorMsg);
      end else begin
        LoadOther(Img, Reader, InStream);
      end;
      // free memory early
      FreeAndNil(InStream);
      FreeAndNil(Reader);

      // convert
      Result:=Convert(Img,OutputFilename,ErrorMsg);
    finally
      InStream.Free;
      Reader.Free;
      Img.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

function TPyramidTiffer.Convert(Img: TPTMemImgBase; OutputFilename: string; out
  ErrorMsg: string): boolean;
var
  OutStream: TMemoryStream;
  Writer: TFPWriterTiff;
  Size: Int64;
  Count: Integer;
  Index: Integer;
  LastImg: TPTMemImgBase;
  NewImg: TPTMemImgBase;
begin
  Result:=false;
  try
    // compute the number of images
    Count:=1;
    Size:=Int64(Img.Width)*Img.Height;
    while Size>4096 do begin
      Size:=Size div 4;
      inc(Count);
    end;

    // create images
    OutStream:=TMemoryStream.Create;
    Writer:=nil;
    LastImg:=nil;
    NewImg:=nil;
    try
      Writer:=TFPWriterTiff.Create;
      Index:=0;
      Img.Extra[TiffPageNumber]:=IntToStr(Index);
      Img.Extra[TiffPageCount]:=IntToStr(Count);
      Img.Extra[TiffTileWidth]:=IntToStr(TileWidth);
      Img.Extra[TiffTileLength]:=IntToStr(TileHeight);
      SetFPImgExtraTiff(Img.Desc,Img,false);
      Img.Extra[TiffCompression]:=IntToStr(TiffCompressionDeflateZLib);
      Writer.AddImage(Img);

      // add smaller images
      LastImg:=Img;
      while Index+1<Count do begin
        Index+=1;
        // create next image with half the width and height
        NewImg:=ShrinkImage(LastImg);
        // set tiff page number and count
        NewImg.Extra[TiffPageNumber]:=IntToStr(Index);
        NewImg.Extra[TiffPageCount]:=IntToStr(Count);
        NewImg.Extra[TiffTileWidth]:=IntToStr(TileWidth);
        NewImg.Extra[TiffTileLength]:=IntToStr(TileHeight);
        SetFPImgExtraTiff(NewImg.Desc,NewImg,false);
        Img.Extra[TiffCompression]:=IntToStr(TiffCompressionDeflateZLib);
        // add image to tiff
        if Verbose then
          writeln('  adding image ',Index,'/',Count,', size=',NewImg.Width,'x',NewImg.Height);
        Writer.AddImage(NewImg);
        // free last step
        if LastImg<>Img then
          FreeAndNil(LastImg);
        LastImg:=NewImg;
        NewImg:=nil;
      end;
      // free memory early
      FreeAndNil(LastImg);

      // create stream
      Writer.SaveToStream(OutStream);
      OutStream.Position:=0;

      // save to file
      OutStream.SaveToFile(OutputFilename);
      Result:=true;
    finally
      if LastImg<>Img then
        LastImg.Free;
      NewImg.Free;
      Writer.Free;
      OutStream.Free;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
    end;
  end;
end;

constructor TPyramidTiffer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  TileWidth:=256;
  TileHeight:=256;
  MinSize:=32;
end;

destructor TPyramidTiffer.Destroy;
begin
  inherited Destroy;
end;

procedure TPyramidTiffer.WriteHelp(WithHeader: boolean);
var
  ImgType: String;
  i: Integer;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  if WithHeader then begin
    writeln('Version ',Version);
    writeln;
    writeln('pyramidtiff creates a tiff containing the original image, the image');
    writeln('with half the width and half the height (rounded up), the image with');
    writeln('quartered width/height (rounded up), ... and so forth.');
    writeln;
  end;
  writeln('-c <input file>');
  writeln('   Check if file is a pyramid, tiled tif. 0 = yes, 1 = no.');
  writeln('-i <input file>');
  write('   Input image file can be a:');
  for i:=0 to ImageHandlers.Count-1 do begin
    ImgType:=ImageHandlers.TypeNames[i];
    write(' ',ImageHandlers.Extentions[ImgType]);
  end;
  writeln;
  writeln('-o <output file>');
  writeln('   Output image file. It will always be a tif file, no matter what extension it has.');
  writeln('--width=<tilewidth>');
  writeln('   In pixel. Default=',TileWidth);
  writeln('--height=<tileheight>');
  writeln('   In pixel. Default=',TileHeight);
  writeln('--min-size=<min size>');
  writeln('   Create no images with a smaller width or height than this value in pixel.');
  writeln('   Default=',MinSize);
  //writeln('--skip-check');
  //writeln('   Skip check if output file is already a pyramid tiled tif.');
  writeln('-h or --help');
  writeln('   Write this help');
  writeln('-q or --quiet');
  writeln('   Be less verbose');
  writeln('-v or --verbose');
  writeln('   Be more verbose');
  writeln('-V or --version');
  writeln('   Write version.');
  writeln;
  writeln('Examples:');
  writeln('  Convert input.jpg into output.tif:');
  writeln('  ',ExeName,' -i input.jpg -o output.tif');
  writeln;
  writeln('  Check if file.tif is already a pyramid, tiled tif:');
  writeln('  ',ExeName,' -c file.tif');
  writeln;
end;

var
  Application: TPyramidTiffer;
begin
  Application:=TPyramidTiffer.Create(nil);
  Application.Run;
  Application.Free;
end.

