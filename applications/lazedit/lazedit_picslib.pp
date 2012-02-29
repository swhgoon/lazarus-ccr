unit LazEdit_PicsLib;
{ $DEFINE DEBUG}
interface

uses Windows;

function GetBMPSize(const Fn: String; out Width, Height: dword): Boolean;
function GetGIFSize(const Fn: String; out Width, Height: dword): Boolean;
function GetJPGSize(const Fn: String; out Width, Height: dword): Boolean;
function GetPNGSize(const Fn: String; out Width, Height: dword): Boolean;
function GetImageSize(const Fn: String; out Width, Height: dword): Boolean;




implementation

uses SysUtils, Classes;

type TBitmapFileHeader = Packed Record
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


function MotorolaToIntelDW(DW: dword): dword;
var HiWd, LoWd: word;
begin
  HiWd := HiWord(DW);
  LoWd := LoWord(DW);
  HiWd := Swap(HiWd);
  LoWd := Swap(LoWd);
  Result := HiWd + (LoWd shl 16);
end;


function GetImageSize(const Fn: String; out Width, Height: dword): Boolean;
begin
  if AnsiUpperCase(ExtractFileExt(Fn)) = '.BMP' then
  begin
    Result := GetBMPSize(Fn, Width, Height);
  end
  else if AnsiUpperCase(ExtractFileExt(Fn)) = '.GIF' then
  begin
    Result := GetGIFSize(Fn, Width, Height);
  end
  else if (AnsiUpperCase(ExtractFileExt(Fn)) = '.JPG')
    or (AnsiUpperCase(ExtractFileExt(Fn)) = '.JPEG') then
  begin
    Result := GetJPGSize(Fn, Width, Height);
  end
  else if AnsiUpperCase(ExtractFileExt(Fn)) = '.PNG' then
  begin
    Result := GetPNGSize(Fn, Width, Height);
  end
  else
  begin
    Width := 0;
    Height := 0;
    Result := False;
  end;  
end;



function GetBMPSize(const Fn: String; out Width, Height: dword): Boolean;
var BitmapFileHeader: TBitmapFileHeader;
    BitmapInfo: TBitmapInfo;
    F: File;
    bRead: Integer;
    IDStr: String;
begin
  Result := False;
  Width := 0;
  Height := 0;
  Try
    AssignFile(F,Fn);
    FileMode := fmOpenRead or fmShareDenyWrite;
    Reset(F,1);
    BlockRead(F,BitmapFileHeader,SizeOf(TBitmapFileHeader),bRead);
    if bRead <> SizeOf(TBitmapFileHeader) then Raise EInOutError.Create('');
    BlockRead(F,BitmapInfo,SizeOf(TBitmapInfo),bRead);
    if bRead <> SizeOf(TBitmapInfo) then Raise EInOutError.Create('');
    CloseFile(F);
    IDStr := Char(Lo(BitmapFileHeader.ID)) + Char(Hi(BitmapFileHeader.ID));
    //Is it correct file format?
    if (not (IDStr = 'BM') or (IDStr = 'BA')) or
      (not (BitmapInfo.BitmapHeaderSize in [$28,$0c,$f0])) or
      (not (BitmapInfo.BitsPerPixel in [1,4,8,16,24,32])) then Exit;

    Width := BitmapInfo.Width;
    Height := BitmapInfo.Height;
    Result := True;
  Except
    on EInOutError do
    begin
      {$I-}
      CloseFile(F);
      InOutRes := 0; //Ignore IO Errors at this point
      Exit;
    end;
  end;//try...except
end;

function GetGIFSize(const Fn: String; out Width, Height: dword): Boolean;
var GifHeader: TGIFHeader;
    F: File;
    bRead: Integer;
begin
  Result := False;
  Width := 0;
  Height := 0;
  Try
    AssignFile(F,Fn);
    FileMode := fmOpenRead or fmShareDenyWrite;
    Reset(F,1);
    BlockRead(F,GifHeader,SizeOf(TGIFHeader),bRead);
    if bRead <> SizeOf(TGIFHeader) then Raise EInOutError.Create('');
    CloseFile(F);
    //Is correct file format?
    if not ((AnsiUpperCase(GifHeader.ID) = 'GIF87A') or (AnsiUpperCase(GifHeader.ID) = 'GIF89A')) then Exit;
    Width := GifHeader.Width;
    Height := GifHeader.Height;
    Result := True;
  Except
    on EInOutError do
    begin
      {$I-}
      CloseFile(F);
      InOutRes := 0; //Ignore IO Errors at this point
      Exit;
    end;
  end;//try...except
end;


function GetPNGSize(const Fn: String; out Width, Height: dword): Boolean;
var PNGHeader: TPNGHeader;
    F: File;
    bRead: Integer;
begin
  Result := False;
  Width := 0;
  Height := 0;
  Try
    AssignFile(F,Fn);
    FileMode := fmOpenRead or fmShareDenyWrite;
    Reset(F,1);
    BlockRead(F,PNGHeader,SizeOf(TPNGHeader),bRead);
    if bRead <> SizeOf(TPNGHeader) then Raise EInOutError.Create('');
    CloseFile(F);
    //Is correct file format?
    if (AnsiUpperCase(PNGHeader.ID) <> #137'PNG'#13#10#26#10) or
       (AnsiUpperCase(PNGHeader.ChunkType) <> 'IHDR') then exit;
    Width := MotorolaToIntelDW(PNGHeader.Width);
    Height := MotorolaToIntelDW(PNGHeader.Height);
    Result := true;
  Except
    on EInOutError do
    begin
      {$I-}
      CloseFile(F);
      InOutRes := 0; //Ignore IO Errors at this point
      Exit;
    end;
  end;//try...except
end;


function GetJPGSize(const Fn: String; out Width, Height: dword): Boolean;
var F: File;
    bRead: Integer;
    JPGHeader: TJPGHeader;
    SOFHeader: TSOFHeader;
    B, SegType: byte;
    SegSize: Word; //Thumbnail Size
    SOF_Found: boolean;
    Dummy: array[0..65532] of byte; //Max segment length
begin
  Result := False;
  Width := 0;
  Height := 0;
  Try
    AssignFile(F,Fn);
    FileMode := fmOpenRead or fmShareDenyWrite;
    Reset(F,1);
    BlockRead(F,JPGHeader, SizeOf(TJPGHeader),bRead);
    if bRead <> SizeOf(TJPGHeader) then Raise EInOutError.Create('');
    if (JPGHeader[0] <> $FF) or (JPGHeader[1] <> $D8) then
    begin
      CloseFile(F);
      Exit;
    end;
    SOF_Found := False;
    //Find JFIFF and StartOfFrame (SOF) segment
    BlockRead(F,B,1,bRead);
    if bRead <> 1 then Raise EInoutError.Create('');
    While (not EOF(F)) and (B = $FF) and not (SOF_Found) do //Alle segments start with $FF
    begin
      BlockRead(F,SegType,1,bRead);
      if bRead <> 1 then Raise EInoutError.Create('');
      case SegType of
        $c0,$c1,$c2 {,$c3,$c5,$c6,$c7,$c9,$ca,$cb,$cd,$ce,$cf ???}:
        begin//StartOfFrame
          BlockRead(F,SOFHeader,SizeOf(TSOFHeader),bRead);
          if bRead <> SizeOf(TSOFHeader) then Raise EInOutError.Create('');
          //Motorola -> Intel
          SOFHeader.Len := Swap(SOFHeader.Len);
          SOFHeader.Height := Swap(SOFHeader.Height);
          SOFHeader.Width := Swap(SOFHeader.Width);
          BlockRead(F,Dummy,SOFHeader.NrComponents*3,bRead);
          if bRead <> (SOFHeader.NrComponents * 3) then Raise EInOutError.Create('');
          Width := SOFHeader.Width;
          Height := SOFHeader.Height;
          SOF_Found := true;
        end;

        $01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7:
        begin//Parameterless segment
          // Just ignore
        end;
        $d9:
        begin//EndOfImage
          Break;
        end;
        $da:
        begin//Start Of Scan: JPG Data
          Break;
        end;
        else
        begin//Read segment into dummy and skip
          //Firts 2 bytes are lenggth of segment
          //including the 2 length-bytes
          //Lengthbytes are in Motorola format (Hi-Lo)
          BlockRead(F,SegSize,SizeOf(SegSize),bRead);
          if bRead <> SizeOf(SegSize) then Raise EInOutError.Create('');
          SegSize := Swap(SegSize);
          if SegSize > 2 then
          begin//Read to end of segment
            SegSize := SegSize - 2;
            BlockRead(F,Dummy,SegSize,bRead);
            if bRead <> SegSize then Raise EInOutError.Create('');
          end;
        end;
      end;//case
      //Read next segment, B shold be $FF right now ...
      BlockRead(F,B,1,bRead);
      if bRead <> 1 then Raise EInoutError.Create('');
    end;//While
    //Did we find all info, and file format is correct?
    if {JFIF_Found and} SOF_Found then Result := True;
    CloseFile(F);
  Except
    on EInOutError do
    begin
      {$I-}
      CloseFile(F);
      InOutRes := 0; //Ignore IO Errors at this point
      Exit;
    end;
  end;//try...except
end;


end.


