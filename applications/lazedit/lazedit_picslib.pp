unit LazEdit_PicsLib;

{ $DEFINE DebugPicsLib}

interface

uses SysUtils, Classes, Math, LazUtf8Classes;

type
  TImageFormat = (ifUnknown, ifBmp, ifPng, ifGif, ifJpg);

function GetImageSize(const Fn: String; out Width, Height: dword): Boolean;
function GetImageSizeAndFormat(const Fn: String; out Width, Height: dword): TImageFormat;



implementation



type

   TBitmapFileHeader = Packed Record
                         ID: word;
                         FileSize: dword;
                         Reserved: dword;
                         BitmapDataOffset: dword;
                       end;
   TBitmapInfo        = Packed Record
                          BitmapHeaderSize: dword;
                          Width: dword;
                          Height: dword;
                          Planes: word;
                          BitsPerPixel: word;
                          Compression: dword;
                          BitmapDataSize: dword;
                          XpelsPerMeter: dword;
                          YPelsPerMeter: dword;
                          ColorsUsed: dword;
                          ColorsImportant: dword;
                        end;

   TGIFHeader        = Packed Record
                         ID: array[0..5] of char;
                         Width, Height: Word;
                       end;

   TPNGHeader        = Packed Record
                         ID: array[0..7] of Char;
                         ChunkLength: dword;
                         ChunkType: array[0..3] of Char;
                         Width: dword;
                         Height: dword;
                         BitsPerPixel: byte;
                         ColorType: byte;
                         Compression: byte;
                         FilterMethod: byte;
                         CRC: dword;
                       end;

   TJPGHeader        = array[0..1] of Byte; //FFD8 = StartOfImage (SOI)

   TSOFHeader        = Packed record
                         Len: word;
                         DataPrecision: byte;
                         Height, Width: word;
                         NrComponents: byte;
                       end;

function ExtToImageFormat(const Ext: String): TImageFormat;
begin
  if AnsiUpperCase(Ext) = '.BMP' then
  begin
    Result := ifBmp;
  end
  else if AnsiUpperCase(Ext) = '.GIF' then
  begin
    Result := ifGif;
  end
  else if (AnsiUpperCase(Ext) = '.JPG')
    or (AnsiUpperCase(Ext) = '.JPEG') then
  begin
    Result := ifJpg;
  end
  else if AnsiUpperCase(Ext) = '.PNG' then
  begin
    Result := ifPng;
  end
  else
  begin
    Result := ifUnknown;
  end;
end;




function MaybeBmp(St: TStream; ChunkSize: Integer; out Width, Height: DWord): TImageFormat;
var
  BFH: TBitmapFileHeader;
  BInfo: TBitmapInfo;
  IDStr: String;
begin
  Result := ifUnknown;
  if (ChunkSize < (SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfo))) then Exit;
  St.Position := 0;
  St.ReadBuffer(BFH, Sizeof(TBitmapFileHeader));
  St.ReadBuffer(BInfo, SizeOf(TBitmapInfo));
  BFH.ID := LeToN(BFH.ID);
  IDStr := Char(Lo(BFH.ID)) + Char(Hi(BFH.ID));
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('IDStr = ',idstr);
  if IsConsole then writeln('BInfo.BitmapHeaderSize = $',IntToHex(BInfo.BitmapHeaderSize,2));
  if IsConsole then writeln('BInfo.BitsPerPixel = ',BInfo.BitsPerPixel);
  {$endif}
  BInfo.BitmapHeaderSize := LeToN(BInfo.BitmapHeaderSize);
  BInfo.BitsPerPixel := LeToN(BInfo.BitsPerPixel);
  if ((IDStr = 'BM') or (IDStr = 'BA')) and
     (BInfo.BitmapHeaderSize in [$28,$0c,$f0]) and
     (BInfo.BitsPerPixel in [1,4,8,16,24,32])then
  begin
    Width := LeToN(BInfo.Width);
    Height := LeToN(BInfo.Height);
    Result := ifBmp;
  end;
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('MaybeBmp: Result = ',Result);
  {$endif}
end;


function MaybePng(St: TStream; ChunkSize: Integer; out Width, Height: DWord): TImageFormat;
var
  PngHeader: TPngHeader;
begin
  Result := ifUnknown;
  if (ChunkSize < SizeOf(TPngHeader)) then Exit;
  St.Position := 0;
  St.ReadBuffer(PngHeader, SizeOf(TPngHeader));
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('PNGHeader.ID= ',PNGHeader.ID);
  if IsConsole then writeln('PNGHeader.ChunkType = ',PNGHeader.ChunkType);
  {$endif}
  if (AnsiUpperCase(PNGHeader.ID) = #137'PNG'#13#10#26#10) or
     (AnsiUpperCase(PNGHeader.ChunkType) = 'IHDR') then
  begin
    //Vaues are in BigEndian format
    Width := BeToN(PNGHeader.Width);
    Height := BeToN(PNGHeader.Height);
    Result := ifPng;
  end;
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('MaybePng: Result = ',Result);
  {$endif}
end;

function MaybeGif(St: TStream; ChunkSize: Integer; out Width, Height: DWord): TImageFormat;
var
  GifHeader: TGifHeader;
begin
  Result := ifUnknown;
  if (ChunkSize < SizeOf(TGifHeader)) then Exit;
  St.Position := 0;
  St.ReadBuffer(GifHeader, SizeOf(TGifHeader));
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('GifHeader.ID = ',GifHeader.ID);
  {$endif}
  if  ((AnsiUpperCase(GifHeader.ID) = 'GIF87A') or (AnsiUpperCase(GifHeader.ID) = 'GIF89A')) then
  begin
    Width := LeToN(GifHeader.Width);
    Height := LeToN(GifHeader.Height);
    Result := ifGif;
  end;
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('MaybeGif: Result = ',Result);
  {$endif}
end;

function MaybeJpg(St: TStream; ChunkSize: Integer; out Width, Height: DWord): TImageFormat;
const
  Parameterless = [$01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7];
var
  JPGHeader: TJPGHeader;
  SOFHeader: TSOFHeader;
  B, SegType: byte;
  SegSize: Word; //Thumbnail Size
  SOF_Found: boolean;
  Dummy: array[0..65532] of byte; //Max segment length
begin
  Result := ifUnknown;
  if (ChunkSize < SizeOf(TJpgHeader)) then Exit;
  St.Position := 0;

  St.ReadBuffer(JPGHeader, SizeOf(TJPGHeader));
  if (JPGHeader[0] <> $FF) or (JPGHeader[1] <> $D8) then
  begin
    Exit;
  end;
  {$ifdef DebugPicsLib}
  if IsConsole then writeln('StartOfImage Found');
  {$endif}
  SOF_Found := False;

  //Find JFIFF and StartOfFrame (SOF) segment
  St.ReadBuffer(B,1);
  While (not SOF_Found) and (St.Position < St.Size) and (B = $FF) do //All segments start with $FF
  begin
    St.ReadBuffer(SegType,1);
    {$ifdef DebugPicsLib}
    if IsConsole then write('Segment Type: '+IntToHex(SegType,2)+' ');
    {$endif}
    case SegType of
      $c0,$c1,$c2 {,$c3,$c5,$c6,$c7,$c9,$ca,$cb,$cd,$ce,$cf ???}:
      begin//StartOfFrame
        {$ifdef DebugPicsLib}
        if IsConsole then write('  Found SOF');
        {$endif}
        St.ReadBuffer(SOFHeader,SizeOf(TSOFHeader));
        //Values are in BigEndian
        SOFHeader.Len := BeToN(SOFHeader.Len);
        {$ifdef DebugPicsLib}
        if IsConsole then write('  Segment Length: '+IntToStr(SOFHeader.Len),' (StartOfFrame)');
        {$endif}
        //Values are in BigEndian
        SOFHeader.Height := BeToN(SOFHeader.Height);
        SOFHeader.Width := BeTon(SOFHeader.Width);

        St.ReadBuffer(Dummy,SOFHeader.NrComponents*3);
        Width := SOFHeader.Width;
        Height := SOFHeader.Height;
        SOF_Found := true;
      end;

      $01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7:
      begin//Parameterless segment
        {$ifdef DebugPicsLib}
        if IsConsole then write('  Parameterloos');
        {$endif}
        // Ignore
      end;
      $d9:
      begin//EndOfImage
        {$ifdef DebugPicsLib}
        if IsConsole then write('  EndOfImage');
        {$endif}
        Break;
      end;
      $da:
      begin//Start Of Scan: JPG Data
        {$ifdef DebugPicsLib}
        if IsConsole then write('  StartOfScan');
        {$endif}
        Break;
      end;
      else
      begin//Read segment into Dummy and ignore
        //The first 2 bytes represent the length of the segment
        //including the 2 length-bytes
        //Length bytes are in BigEndian format
        St.ReadBuffer(SegSize,SizeOf(SegSize));
        //SegSize := Swap(SegSize);
        SegSize := BeTon(SegSize);
        {$ifdef DebugPicsLib}
        if IsConsole then write('  Segment Length: '+IntToStr(SegSize));
        {$endif}
        if SegSize > 2 then
        begin//Read until end of segemt
          SegSize := SegSize - 2;
          St.ReadBuffer(Dummy,SegSize);
        end;
      end;
    end;//case
    //Lees volgense segmentbegin, B moet nu $FF zijn ...
    St.ReadBuffer(B,1);
    {$ifdef DebugPicsLib}
    if IsConsole then writeln;
    {$endif}
  end;//While
  //Found all info.
  if SOF_Found then Result := ifJpg;
  {$ifdef DebugPicsLib}
  if IsConsole then begin writeln; writeln('  End of Search for markers'); writeln; end;
  if IsConsole then writeln('MaybeJpg: Result = ',Result);
  {$endif}
end;


type
  TMaybeFormatFunc = function(St: TStream; ChunkSize: Integer; out Width, Height: DWord): TImageFormat;
  TMaybeFormatFuncs = array[TImageFormat] of TMaybeFormatFunc;

const
  MaybeFormatFuncs: TMaybeFormatFuncs = (nil, @MaybeBmp, @MaybePng, @MaybeGif, @MaybeJpg);

function GetImageFormatAndDimensions(const St: TStream; const TryFirst: TImageFormat; out Width, Height: DWord): TImageFormat;
var
  ChunkSize: Integer;
  Buf: PByte;
  ImgFormat: TImageFormat;
begin
  ChunkSize := Max(SizeOf(TBitMapFileHeader) + SizeOf(TBitmapInfo),
                  Max(SizeOf(TPngHeader),
                  Max(SizeOf(TGifHeader), SizeOf(TJpgHeader))));
  {$ifdef DebugPicsLib}
  //if IsConsole then writeln('bmp: ', SizeOf(TBitMapFileHeader));
  //if IsConsole then writeln('gif: ', SizeOf(TGifHeader));
  //if IsConsole then writeln('png: ', SizeOf(TPngHeader));
  //if IsConsole then writeln('jpg: ', SizeOf(TJpgHeader));
  if IsConsole then writeln('chunksize ',chunksize);
  if IsConsole then writeln('TryFirst = ',TryFirst);
  {$endif}

  ChunkSize := Max(ChunkSize, St.Size);
  Result := ifUnknown;
  if (TryFirst <> ifUnknown) then Result := MaybeFormatFuncs[TryFirst](St, ChunkSize, Width, Height);
  if (Result = ifUnknown) then
  begin
    for ImgFormat := Succ(Low(TImageFormat)) to High(TImageFormat) do
    begin
      if (ImgFormat <> TryFirst) then Result := MaybeFormatFuncs[ImgFormat](St, ChunkSize, Width, Height);
      if (Result <> ifUnknown) then Break;
    end;
  end;
end;

function GetImageSizeAndFormat(const Fn: String; out Width, Height: dword): TImageFormat;
var
  ImgStream: TFileStreamUtf8;
  ImgFormat: TImageFormat;
begin
  Width := 0;
  Height := 0;
  try
    ImgStream := TFileStreamUtf8.Create(Fn,fmOpenRead or fmShareDenyNone);
    try
      ImgStream.Position := 0;
      ImgFormat := GetImageFormatAndDimensions(ImgStream, ExtToImageFormat(ExtractFileExt(Fn)), Width, Height);
      Result := ImgFormat;
    finally
      ImgStream.Free;
    end;
  except
     on EStreamError do Result := ifUnknown;
  end;
end;

function GetImageSize(const Fn: String; out Width, Height: dword): Boolean;
begin
  Result := (GetImageSizeAndFormat(Fn, Width, Height) <> ifUnknown);
end;

end.


