{ zlibar.pas can create and read a file that contains many compressed files
Copyright (C) 2005 Andrew Haines

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
}
{
  See also the file COPYING.modifiedLGPL included with this file
}
unit zlibar;

// This Unit Provides methods to compress multiple files into an archive of one file and retrieve them
// Also you can use CompressStream and ExtractStream to just compress and decompress a tmemorystream

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paszlib, md5;
  
type

  PFileInfo = ^TFileInfo;
  TFileInfo = record
    CompressedSize: Int64;
    Md5Sum: TMD5Digest;
  end;

  { TZlibFilesList }
  TZlibFilesList = class(TObject)
  private
    FFileList: TFpList;
    FPathList: TFpList;
    FFileInfoList: TFpList;
    function GetFileName(AIndex: Integer): String;
    function GetPath(AIndex: Integer): String;
    procedure SetFileName(AIndex: Integer; AFIleName: String);
    procedure SetPath(AIndex: Integer; APath: String);
    function GetFileInfo(AIndex: Integer): TFileInfo;
    procedure SetFileInfo(AIndex: Integer; AFileInfo: TFileInfo);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(AFileName: String): Integer;
    function AddPath(AFileName: String; APath: String): Integer;
    function Insert(AIndex: Integer; AFileName: String): Integer;
    function InsertPath(AIndex: Integer; AFileName: String; APath: String): Integer;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function Count: Integer;
  public
    // this contains the full path to the file name on the current filesystem
    property FileName[Index: Integer]: String read GetFileName write SetFileName;
    // this is the relative path that you would like the file extracted to
    property Path[Index: Integer]: String read GetPath write SetPath;
    // used internally
    property FileInfo[Index: Integer]: TFileInfo read GetFileInfo write SetFileInfo;
  end;
  TZlibArchiveHeader = packed record
    FileType: array [0..3] of char;// = ('Z','A','R', 0);
    MajorVersion,
    MinorVersion,
    MicroVersion: LongInt;
    TOCLength: Int64; // overkill? :)
    TOCMD5Sum: TMD5Digest;
  end;
  
  PTOCEntry = ^TTOCEntry;
  TTOCEntry = packed record
    FileName: String;
    FilePath: String;
    Position: Int64;
    CompressedSize: Int64;
    Md5sum: TMD5Digest;
  end;
  (*
  A TOC Entry will stored like so:
  1.  Write File Name to be in archive
  2   Write Empty Byte.
  3.  Write File Path
  4.  Write Empty Byte
  5.  Write File Position (Int64) 8 bytes
  6.  Write File Compressed Size(Int64)
  7.  Write md5sum (TMD5Digest)
   Repeat 1-4
  *)
  
  TZlibCompressProgProc = procedure (Sender: TObject; FileIndex: Integer; FileSize, FilePos: Int64) of object;
  
  TZlibExtractProgProc = procedure (Sender: TObject; FileSize, FilePos: Int64) of object;
  
  TZlibErrorProc = procedure(Sender: TObject; var ErrorCode: Integer; ErrorStr: String) of object;

  { TZlibArchive }

  { TZlibWriteArchive }

  TZlibWriteArchive = class(TObject)
  private
    fInputFiles: TZlibFilesList;
    fOnCompress: TZlibCompressProgProc;
    fOnError: TZlibErrorProc;
    fStream: TStream;
    fStreamOwner: Boolean;
    procedure WriteHeader(AHeader: TZlibArchiveHeader);
    procedure WriteTOCEntry(Entry: TTOCEntry; TOCStream: TStream);
    procedure CheckStreamAssigned;
    procedure DoError(ErrorCode: Integer; ErrorString: String);
    function InternalCompressStream(FileIndex: Integer; InStream: TStream; OutStream: TStream): Integer;
    procedure SetStream(AValue: TStream);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function CreateArchive: boolean;
    property OnError: TZlibErrorProc read fOnError write fOnError;
    property OnCompress : TZlibCompressProgProc read fOnCompress write fOnCompress;
  published
    property OutStream: TStream read fStream write SetStream;
    property InputFiles: TZlibFilesList read fInputFiles;
  end;
  
  { TZLibReadArchive }

  TZLibReadArchive = class(TObject)
  private
    fTOCList: TFpList;
    fOnError: TZlibErrorProc;
    fOnExtract: TZlibExtractProgProc;
    fHeader: TZlibArchiveHeader;
    fStream: TStream;
    fFileName: String;
    procedure SetStream(AStream: TStream);
    procedure VerifyFile;
    procedure ReadTOC;
    procedure FreeTOC;
    function GetCount: Integer;
    function GetTOCEntry(AIndex: Integer): TTOCEntry;
    procedure DoError(ErrorCode: Integer; ErrorString: String);
    function InternalExtractStream(InStream: TStream;var OutStream: TStream): Integer;
    function FindFilePath(const AFilePath: String): Integer;
  public
    constructor Create;
    constructor Create(InStream: TStream);
    destructor Destroy; override;
  public
    procedure ExtractFileToStream(AIndex: Integer; Stream: TStream);
    procedure ExtractFileToStream(const AFileName: String; Stream: TStream);
    procedure ExtractFileToStream(const AFileName , APath: String; Stream: TStream);
    property Header: TZlibArchiveHeader read fHeader;
    property FilesInArchive[Index: Integer]: TTOCEntry read GetTOCEntry;
    property Count: Integer read GetCount;
    property OnError: TZlibErrorProc read fOnError write fOnError;
    property OnExtract: TZlibExtractProgProc read fOnExtract write fOnExtract;
  published
    property InStream: TStream read fStream write SetStream;
  end;
const
 ZLibFileType:  array [0..3] of char = ('Z','A','R', #0);
 
 ZAR_MAJOR_VERSION  : LongInt = 1;
 ZAR_MINOR_VERSION : LongInt = 0;
 ZAR_MICRO_VERSION  : LongInt = 0;
 
 ERR_CORRUPT_TOC     = 1;
 ERR_CORRUPT_FILE    = 2;
 ERR_CHECKSUM_FAILED = 3;
 ERR_FILENAME_NOT_FOUND = 4;
 ERR_UNKOWN_ERROR    = 6;
 
function CompressStream(InStream: TStream; OutStream: TStream): Integer; //returns size of compressed file
function ExtractStream(InStream: TStream; OutStream: TStream): Integer;
function StreamMD5(Stream: TStream): TMD5Digest;   // creates a md5digest from a tstream
 
implementation

///////////////////////////////////
//      Generic Functions        //
///////////////////////////////////

{*******************************************************************************
*   This performs an md5 sum on the file and returns it as a TMD5Digest        *
*                                                                              *
*******************************************************************************}
function StreamMD5(Stream: TStream): TMD5Digest;
var
  Buf : array [0..1023] of byte;
  Context: TMD5Context;
  Count : Longint;
begin
  Stream.Position := 0;
  MD5Init(Context);
  repeat
    Count := 1024;
    Count := Stream.Read(Buf, Count);
    If (Count>0) then
      MD5Update(Context, Buf, Count);
  until (Count<1024);
  MD5Final(Context, Result);
end;

{*******************************************************************************
*   This decompresses the data from InStream and Writes the decompressed data  *
*   to OutputStream                                                            *
*******************************************************************************}
function ExtractStream(InStream: TStream; OutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;
const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(z, SizeOf(z), 0);

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := inflateInit(z);
  InStream.Position := 0;

  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);
    
    // wouldn't work for files > 2GB
    //z.next_in := TMemoryStream(InStream).Memory;
    //z.avail_in := InStream.Size;
    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_SYNC_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;
      
      err := inflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
      if err = Z_STREAM_END then Break;
      until Z.avail_out > 0;
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;
  err := inflateEnd(z);
end;

{*******************************************************************************
*   This compresses the data from InStream and Writes the compressed data      *
*   to OutputStream                                                            *
*******************************************************************************}
function CompressStream(InStream: TStream; OutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;

const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := deflateInit(z, -1); //default
  InStream.Position := 0;

  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);
    
    // wouldn't work for files > 2 gb :(
    //z.next_in := TMemoryStream(InStream).Memory;
    //z.avail_in := InStream.Size;
    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_NO_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;
      err := deflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
    until Z.avail_out > 0;

    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;

  err := deflateEnd(z);
end;

////////////////////////////////
//        Objects             //
////////////////////////////////

{ TZlibArchive }

procedure TZlibWriteArchive.WriteHeader(AHeader: TZlibArchiveHeader);
var
X: Integer;
CompressedTOCStream: TMemoryStream;
TOCStream: TMemoryStream;
Position: Int64;
TOCEntry: TTOCEntry;
FileInfo:TFileInfo;
begin
  try
  CheckStreamAssigned;
  TOCStream := TMemoryStream.Create;
  Position := 0;
  OutStream.Position := 0;
  for X := 0 to fInputFiles.Count-1 do begin
    TOCEntry.FileName := fInputFiles.FileName[X];
    TOCEntry.FilePath := fInputFiles.Path[X];
    TOCEntry.Position := Position;
    FileInfo := fInputFiles.FileInfo[X];
    TOCEntry.CompressedSize := FileInfo.CompressedSize;
    TocEntry.Md5sum := FileInfo.Md5Sum;
    WriteTOCEntry(TOCEntry, TOCStream);
    Position += TOCEntry.CompressedSize;
  end;
  CompressedTOCStream:= TMemoryStream.Create;
  CompressStream(TOCStream, CompressedTOCStream);
  CompressedTOCStream.Position := 0;
  AHeader.TOCLength := NtoLE(CompressedTOCStream.Size);
  AHeader.TOCMd5Sum := StreamMd5(TOCStream);
  OutStream.Write(AHeader, SizeOf(TZlibArchiveHeader));
  OutStream.CopyFrom(CompressedTOCStream, CompressedTOCStream.Size);
  finally
  TOCStream.Free;
  CompressedTOCStream.Free;
  end;
end;

procedure TZlibWriteArchive.WriteTOCEntry(Entry: TTOCEntry; TOCStream: TStream);
var
Str: array [0..255]of char;
EmptyByte: Byte =0;
begin
   Str := ExtractFileName(Entry.FileName);
   TOCStream.Write(Str, Length(Trim(Str)));
   TOCStream.WriteByte(EmptyByte);
   Str := Entry.FilePath;
   TOCStream.Write(Str, Length(Trim(Str)));
   TOCStream.WriteByte(EmptyByte);
   Entry.Position := NtoLE(Entry.Position);
   TOCStream.Write(Entry.Position, SizeOf(Int64));
   Entry.CompressedSize := NtoLE(Entry.CompressedSize);
   TOCStream.Write(Entry.CompressedSize, SizeOf(Int64));
   TOCStream.Write(Entry.Md5sum, SizeOf(TMD5Digest));

end;


procedure TZlibWriteArchive.CheckStreamAssigned;
begin
  if fStream = nil then begin
    fStream := TMemoryStream.Create;
    fStreamOwner := True;
  end;
end;

procedure TZlibWriteArchive.DoError(ErrorCode: Integer; ErrorString: String);
var
fErrCode: Integer;
begin
  fErrCode := ErrorCode;
  if Assigned(fOnError) then fOnError(Self, fErrCode, ErrorString);
  if fErrCode <> 0 then Raise Exception.Create('ZLibError('+IntToStr(fErrCode)+') '+ErrorString);
end;

function TZlibWriteArchive.InternalCompressStream(FileIndex: Integer;
  InStream: TStream; OutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;

const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(z, 0 , SizeOf(z));

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := deflateInit(z, -1); //default
  InStream.Position := 0;

  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    // wouldn't work for files > 2 gb :(
    //z.next_in := TMemoryStream(InStream).Memory;
    //z.avail_in := InStream.Size;
    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_NO_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;
      err := deflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
    until Z.avail_out > 0;
    if fOnCompress <> nil then fOnCompress(Self, FileIndex, InStream.Size, InStream.Position);
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;

  err := deflateEnd(z);
end;

procedure TZlibWriteArchive.SetStream(AValue: TStream);
begin
  if AValue <> fStream then fStreamOwner := False;
  fStream := AValue;
end;

constructor TZlibWriteArchive.Create;
begin
  fInputFiles := TZlibFilesList.Create;
  fStream := nil;
end;

destructor TZlibWriteArchive.Destroy;
begin
  fInputFiles.Free;
  if fStreamOwner then fStream.Free;
  inherited Destroy;
end;

function TZlibWriteArchive.CreateArchive: boolean;
var
X: Integer;
AHeader: TZlibArchiveHeader;
TmpStream: TMemoryStream; // this holds all the compressed files temporarily
//TmpFile: TMemoryStream; // this holds the current file to be added to TmpStream
TmpFile: TFileStream; // this holds the current file to be added to TmpStream
FileInfo: TFileInfo;
begin
  Result := False;
  try
  CheckStreamAssigned;
  TmpStream := TMemoryStream.Create;

  AHeader.FileType := ZLibFileType;
  AHeader.MajorVersion := NtoLE(ZAR_MAJOR_VERSION);
  AHeader.MinorVersion := NtoLE(ZAR_MINOR_VERSION);
  AHeader.MicroVersion := NtoLE(ZAR_MICRO_VERSION);

  for X := 0 to fInputFiles.Count-1 do begin
    if FileExists(fInputFiles.FileName[X]) then begin
      try
      TmpFile := TFileStream.Create(fInputFiles.FileName[X],fmOpenRead or fmShareDenyNone);
      FileInfo.CompressedSize := InternalCompressStream(X, TmpFile, TmpStream);
      FileInfo.Md5Sum := StreamMD5(TmpFile);
      fInputFiles.FileInfo[X] := FileInfo;//records the compressed length/size
      finally
        TmpFile.Free;
      end;
    end;
  end;
  //Write file header and Table of contents
  WriteHeader(AHeader);
  //WriteFiles
  TmpStream.Position := 0;

  OutStream.CopyFrom(TmpStream, TmpStream.Size);
  Result := True;
  finally
  TmpStream.Free;
  end;
end;

{ TZLibReadArchive }

procedure TZLibReadArchive.SetStream(AStream: TStream);
begin
  fStream := AStream;
  if AStream <> nil then begin
    fStream.Position := 0;
    fStream.Read(fHeader,SizeOf(TZlibArchiveHeader));
    fHeader.MajorVersion := LEtoN(Header.MajorVersion);
    fHeader.MinorVersion := LEtoN(Header.MinorVersion);
    fHeader.MicroVersion := LetoN(Header.MicroVersion);
    fHeader.TOCLength := LEtoN(Header.TOCLength);
    VerifyFile;
    ReadTOC;
  end
  else begin
    FreeTOC;
  end;
end;

procedure TZLibReadArchive.VerifyFile;
begin
  if (fHeader.FileType <> ZLibFileType) then DoError(ERR_CORRUPT_FILE,'corrupt file or not a correct file type');
end;

procedure TZLibReadArchive.ReadTOC;
var
Entry: PTOCEntry;
PositionOffset: Int64;
fChar: Char;
TmpStream: TMemoryStream; // used to temporarily hold the compressed TOC
TOCStream: TMemoryStream; // the TOC will be extracted into this
begin
  TmpStream:= TMemoryStream.Create;
  TOCStream := TMemoryStream.Create;
  try
  fStream.Position := SizeOf(fHeader);
  //Read The Compressed TOC into the TmpStream from the main fStream
  TmpStream.CopyFrom(fStream, fHeader.TOCLength);
  //Decompress TOC into TOCStream
  ExtractStream(TmpStream, TOCStream);
  if MD5Match(fHeader.TOCMD5Sum, StreamMd5(TOCStream)) = False then DoError(ERR_CORRUPT_TOC, 'corrupted table of contents');

  TOCStream.Position := 0;
  PositionOffset := fHeader.TOCLength + SizeOf(fHeader);
  while TOCStream.Position <> TOCStream.Size do begin
    Entry := New(pTocEntry);
    fTOCList.Add(Entry);
    // Read FileName
    fChar := Char(TOCStream.ReadByte);
    while fChar <> #0 do begin
      Entry^.FileName += fChar;
      fChar := Char(TOCStream.ReadByte);
    end;
    //Read FilePath
    fChar := Char(TOCStream.ReadByte);
    while fChar <> #0 do begin
      Entry^.FilePath += fChar;
      fChar := Char(TOCStream.ReadByte);
    end;
    //Read Position
    TOCStream.Read(Entry^.Position, SizeOf(Int64));
    Entry^.Position := LEtoN(Entry^.Position) + PositionOffset;
    //Read Compressed Size
    TOCStream.Read(Entry^.CompressedSize, SizeOf(Int64));
    Entry^.CompressedSize := LEtoN(Entry^.CompressedSize);
    //Read Md5sum
    TOCStream.Read(Entry^.Md5sum, SizeOf(TMD5Digest));
  end;
  finally
  TmpStream.Free;
  end;
end;

procedure TZLibReadArchive.FreeTOC;
var
X: Integer;
begin
  for X := 0 to fTOCList.Count-1 do begin
    Dispose(PTOCEntry(fTocList.Items[X]));
    fTocList.Items[X] := nil;
  end;
  fTOCList.Clear;
end;

function TZLibReadArchive.GetCount: Integer;
begin
  Result := fTOCList.Count;
end;

function TZLibReadArchive.GetTOCEntry(AIndex: Integer): TTOCEntry;
begin
  Result := PTOCEntry(fTOCList.Items[AIndex])^;
end;

procedure TZLibReadArchive.DoError(ErrorCode: Integer; ErrorString: String);
var
fErrCode: Integer;
begin
  fErrCode := ErrorCode;
  if Assigned(fOnError) then fOnError(Self, fErrCode, ErrorString);
  if fErrCode <> 0 then Raise Exception.Create('ZLibError('+IntToStr(fErrCode)+') '+ErrorString);
end;

function TZLibReadArchive.InternalExtractStream(InStream: TStream;
  var OutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;
const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(z, 0 , SizeOf(z));

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := inflateInit(z);
  InStream.Position := 0;
  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    // wouldn't work for files > 2GB
    //z.next_in := TMemoryStream(InStream).Memory;
    //z.avail_in := InStream.Size;
    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_SYNC_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;

      err := inflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
      if err = Z_STREAM_END then Break;
    until Z.avail_out > 0;
    if fOnExtract <> nil then fOnExtract(Self, InStream.Size, InStream.Position);
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;
  err := inflateEnd(z);
end;

function TZLibReadArchive.FindFilePath(const AFilePath: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to fTOCList.Count - 1 do begin
    if AFilePath = PTOCEntry(fTOCList[i])^.FilePath+PTOCEntry(fTOCList[i])^.FileName then
    begin
      Result:=i;
      Break;
    end;
  end;
end;

constructor TZLibReadArchive.Create;
begin
  fStream := TMemoryStream.Create;
  fFileName := '';
  fTOCList := TFpList.Create;
end;

constructor TZLibReadArchive.Create(InStream: TStream);
begin
  Create;
  SetStream(InStream);
end;

destructor TZLibReadArchive.Destroy;
begin
  FreeTOC;
  fTOCList.Free;
  inherited Destroy;
end;

procedure TZLibReadArchive.ExtractFileToStream(AIndex: Integer; Stream: TStream);
var
TmpStream: TMemoryStream;
Md5Sum: TMD5Digest;
begin
  if Stream = nil then Stream := TMemoryStream.Create;
  Stream.Position := 0;
  Stream.Size := 0;
  TmpStream := TMemoryStream.Create;
  try
  // Move to the position of the compressed file in the archive
  fStream.Position := FilesInArchive[AIndex].Position;
  // read the compressed file into a temp stream
  TmpStream.CopyFrom(fStream, FilesInArchive[AIndex].CompressedSize);
  // decompress the tmp stream into the output stream
  InternalExtractStream(TmpStream, Stream);
  //Check Md5 sum
  Md5Sum := StreamMD5(Stream);
  if not MD5Match(Md5Sum, FilesInArchive[AIndex].Md5sum) then begin
    DoError(ERR_CHECKSUM_FAILED, 'Saved=' + MD5Print(FilesInArchive[AIndex].Md5sum) +' Found='+ MD5Print(Md5sum));
  end;
  finally
  TmpStream.Free;
  end;
  
end;

procedure TZLibReadArchive.ExtractFileToStream(const AFileName: String; Stream: TStream);
begin
  ExtractFileToStream(AFileName,'/',Stream);
end;

procedure TZLibReadArchive.ExtractFileToStream(const AFileName, APath: String; Stream: TStream);
var
  i: Integer;
begin
  i:=FindFilePath(APath+AFileName);
  if i <> -1 then
    ExtractFileToStream(i,Stream)
  else
    DoError(ERR_FILENAME_NOT_FOUND,'Could not find '+APath+AFileName+' in '+fFileName);
end;

{ TZlibFilesList }

function TZlibFilesList.GetFileName(AIndex: Integer): String;
begin
   Result := PString(FFileList.Items[AIndex])^;
end;

function TZlibFilesList.GetPath(AIndex: Integer): String;
begin
   Result := PString(FPathList.Items[AIndex])^;
end;

procedure TZlibFilesList.SetFileName(AIndex: Integer; AFIleName: String);
begin
  PString(FFileList.Items[AIndex])^ := AFileName;
end;

procedure TZlibFilesList.SetPath(AIndex: Integer; APath: String);
begin
  PString(FPathList.Items[AIndex])^ := APath;
end;

function TZlibFilesList.GetFileInfo(AIndex: Integer): TFileInfo;
begin
  Result := PFileInfo(FFileInfoList.Items[AIndex])^;
end;

procedure TZlibFilesList.SetFileInfo(AIndex: Integer; AFileInfo: TFileInfo);
begin
    PFileInfo(FFileInfoList.Items[AIndex])^ := AFileInfo;
end;

constructor TZlibFilesList.Create;
begin
  FFileList := TFpList.Create;
  FPathList := TFpList.Create;
  FFileInfoList := TFpList.Create;
end;

destructor TZlibFilesList.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TZlibFilesList.Add(AFileName: String): Integer;
begin
  Result := InsertPath(Count, AFileName,'/');
end;

function TZlibFilesList.AddPath(AFileName: String; APath: String): Integer;
begin
  Result := InsertPath(Count, AFileName, APath);
end;

function TZlibFilesList.Insert(AIndex: Integer; AFileName: String): Integer;
begin
  Result := InsertPath(AIndex, AFileName, '/');
end;

function TZlibFilesList.InsertPath(AIndex: Integer; AFileName: String;
  APath: String): Integer;
var
FFile,
FPath: PString;
FFileInfo: PFileInfo;
begin
  Result := 0;
  if AIndex > Count then Result := Count
  else Result := AIndex;

  FFile := New(PString);
  FPath := New(PString);
  FFileInfo := New(PFileInfo);
  
  FFile^ := AFileName;
  FPath^ := APAth;
  
  FFileList.Insert(AIndex, FFile);
  FPathList.Insert(AIndex, FPath);
  FFileInfoList.Insert(AIndex, FFileInfo);
end;

procedure TZlibFilesList.Delete(AIndex: Integer);
begin
  Dispose(PString(FFileList.Items[AIndex]));
  Dispose(PString(FPathList.Items[AIndex]));
  Dispose(PFileInfo(FFileInfoList.Items[AIndex]));
  
  FFileList.Delete(AIndex);
  FPathList.Delete(AIndex);
  FFileInfoList.Delete(AIndex);
end;

procedure TZlibFilesList.Clear;
begin
  While Count > 0 do
    Delete(Count-1);
end;

function TZlibFilesList.Count: Integer;
begin
  Result := FFileList.Count;
end;

end.


