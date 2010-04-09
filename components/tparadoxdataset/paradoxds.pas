unit paradoxds;

{ TParadoxdataSet
  Christian Ulrich christian@ullihome.de
  License: LGPL
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Forms, Objects, LclProc;


const
    { Paradox codes for field types }
    pxfAlpha        = $01;
    pxfDate         = $02;
    pxfShort        = $03;
    pxfLong         = $04;
    pxfCurrency     = $05;
    pxfNumber       = $06;
    pxfLogical      = $09;
    pxfMemoBLOb     = $0C;
    pxfBLOb         = $0D;
    pxfFmtMemoBLOb  = $0E;
    pxfOLE          = $0F;
    pxfGraphic      = $10;
    pxfTime         = $14;
    pxfTimestamp    = $15;
    pxfAutoInc      = $16;
    pxfBCD          = $17;
    pxfBytes        = $18;


type
  {Internal Record information}
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecordNumber: PtrInt;
    BookmarkFlag: TBookmarkFlag;
  end;
  
  PLongWord = ^Longword;

  { field information record used in TPxHeader below }
  PFldInfoRec         = ^TFldInfoRec;
  TFldInfoRec         = packed RECORD
    fType   : byte;
    fSize   : byte;
  end;


  PPxHeader           = ^TPxHeader;
  TPxHeader           =  packed RECORD
    recordSize              :  word;
    headerSize              :  word;
    fileType                :  byte;
    maxTableSize            :  byte;
    numRecords              :  longint;
    nextBlock               :  word;
    fileBlocks              :  word;
    firstBlock              :  word;
    lastBlock               :  word;
    unknown12x13            :  word;
    modifiedFlags1          :  byte;
    indexFieldNumber        :  byte;
    primaryIndexWorkspace   :  pointer;
    unknownPtr1A            :  pointer;
    unknown1Ex20            :  array[$001E..$0020] of byte;
    numFields               :  smallint;
    primaryKeyFields        :  smallint;
    encryption1             :  longint;
    sortOrder               :  byte;
    modifiedFlags2          :  byte;
    unknown2Bx2C            :  array[$002B..$002C] of byte;
    changeCount1            :  byte;
    changeCount2            :  byte;
    unknown2F               :  byte;
    tableNamePtrPtr         :  ^pchar;
    fldInfoPtr              :  PFldInfoRec;
    writeProtected          :  byte;
    fileVersionID           :  byte;
    maxBlocks               :  word;
    unknown3C               :  byte;
    auxPasswords            :  byte;
    unknown3Ex3F            :  array[$003E..$003F] of byte;
    cryptInfoStartPtr       :  pointer;
    cryptInfoEndPtr         :  pointer;
    unknown48               :  byte;
    autoIncVal              :  longint;
    unknown4Dx4E            :  array[$004D..$004E] of byte;
    indexUpdateRequired     :  byte;
    unknown50x54            :  array[$0050..$0054] of byte;
    refIntegrity            :  byte;
    unknown56x57            :  array[$0056..$0057] of byte;
    case smallint of
      3:   (fieldInfo35     :  array[1..255] of TFldInfoRec);
      4:   (fileVerID2      :  smallint;
            fileVerID3      :  smallint;
            encryption2     :  longint;
            fileUpdateTime  :  longint;  { 4.0 only }
            hiFieldID       :  word;
            hiFieldIDinfo   :  word;
            sometimesNumFields:smallint;
            dosCodePage     :  word;
            unknown6Cx6F    :  array[$006C..$006F] of byte;
            changeCount4    :  smallint;
            unknown72x77    :  array[$0072..$0077] of byte;
            fieldInfo       :  array[1..255] of TFldInfoRec);

    { This is only the first part of the file header.  The last field
      is described as an array of 255 elements, but its size is really
      determined by the number of fields in the table.  The actual
      table header has more information that follows. }
  end;

  {Paradox Data Block Header}
  PDataBlock  = ^TDataBlock;
  TDataBlock  = packed RECORD
    nextBlock     : word;
    prevBlock     : word;
    addDataSize   : smallint;
    fileData      : array[0..$0FF9] of byte;
    { fileData size varies according to maxTableSize }
  end;

{  APdoxBlk = packed record
    Next,
    Prev,
    Last: Word;
  end;}

  {10-byte Blob Info Block}
  APdoxBlob = packed record
    Offset,
    Length: LongWord;
    ModNum: Word;
  end;

  { TParadoxDataSet }

  TParadoxDataSet = class(TDataSet)
  private
    FActive  : Boolean;
    FStream  : TFileStream;
    FFileName: TFileName;
    FHeader  : PPxHeader;
    FaRecord : Longword;
    FaBlockstart : LongInt;
    FaBlock : PDataBlock;
    FaBlockIdx : word;
    FBlockReaded : Boolean;
    FBookmarkOfs        :LongWord;

    procedure SetFileName(const AValue: TFileName);
    function GetEncrypted: Boolean;
    procedure ReadBlock;
    procedure ReadNextBlockHeader;
    procedure ReadPrevBlockHeader;
    function GetVersion: real;
  protected
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function  GetRecordCount: Integer; override;
    function  IsCursorOpen: Boolean; override;
    procedure InternalFirst; override;
    procedure InternalHandleException; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  GetCanModify: Boolean;override;
    procedure SetRecNo(Value: Integer); override;
    function  GetRecNo: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Encrypted : Boolean read GetEncrypted;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
  published
    property TableName : TFileName read FFileName write SetFileName;
    property TableLevel : real read GetVersion;
    property FieldDefs;
    property Active;
    property AutoCalcFields;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
//    property BeforeRefresh;
//    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation


{ TParadoxDataSet }

procedure TParadoxDataSet.SetFileName(const AValue: TFileName);
begin
  if Active then
    Close;
  FFilename := AValue;
end;

function TParadoxDataSet.GetEncrypted: Boolean;
begin
  if not Assigned(FHeader) then exit;
  If (FHeader^.fileVersionID <= 4) or not (FHeader^.fileType in [0,2,3,5]) then
    Result := (FHeader^.encryption1 <> 0)
  else
    Result := (FHeader^.encryption2 <> 0)
end;

procedure TParadoxDataSet.ReadBlock;
var
  L   : longint;
begin
  L := FaBlockIdx-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FStream.Position := L;
  FStream.Read(FaBlock^, FHeader^.maxTableSize * $0400);
  FBlockReaded := True;
end;

procedure TParadoxDataSet.ReadNextBlockHeader;
var
  L   : longint;
begin
  if FaBlock^.nextBlock = 0 then exit; //last block
  //Increment Blockstart
  FaBlockStart := FaBlockStart+(FaBlock^.addDataSize div FHeader^.recordSize)+1;
  FaRecord := FaBlockStart+1;
  L := FaBlock^.nextBlock-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FaBlockIdx := FaBlock^.nextBlock;
  FBlockReaded := False;
  FStream.Position := L;
  FStream.Read(FaBlock^,6); //read only Block header
end;

procedure TParadoxDataSet.ReadPrevBlockHeader;
var
  L: LongWord;
begin
  L := FaBlock^.prevBlock-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FaBlockIdx := FaBlock^.prevBlock;
  FBlockReaded := False;
  FStream.Position := L;
  FStream.Read(FaBlock^,6); //read only Block header
  //decrement Blockstart
  L := ((FaBlock^.addDataSize div FHeader^.recordSize)+1);
  FaBlockStart := FaBlockStart-L;
  FaRecord := FaBlockStart+1;
end;

function TParadoxDataSet.GetVersion: real;
begin
  Result := 0;
  if not FActive then exit;
  if not Assigned(FHeader) then exit;
  case FHeader^.fileVersionID of
  $3:Result := 3.0;
  $4:Result := 3.5;
  $5..$9:Result := 4.0;
  $a..$b:Result := 5.0;
  $c:Result := 7.0;
  end;
end;

procedure TParadoxDataSet.InternalOpen;
begin
  FStream := TFileStream.Create(FFilename,fmOpenRead or fmShareDenyNone);
  FHeader := AllocMem($800);
  FStream.Position := 0;
  if not FStream.Read(FHeader^, $800) = sizeof(FHeader^) then
    DatabaseError('No valid Paradox file !');
  if not ((FHeader^.maxTableSize >= 1) and (FHeader^.maxTableSize <= 4)) then
    DatabaseError('No valid Paradox file !');
  if (FHeader^.fileVersionID <= 4) or not (FHeader^.fileType in [0,2,3,5]) then
    FHeader^.fldInfoPtr := addr(FHeader^.fieldInfo35)
   else
    FHeader^.fldInfoPtr := addr(FHeader^.fieldInfo);
  if Encrypted then exit;
  FaBlock := AllocMem(FHeader^.maxTableSize * $0400);
  BookmarkSize := SizeOf(longword);
  InternalFirst;
  InternalInitFieldDefs;
  if DefaultFields then CreateFields;
  BindFields(True);
  FActive := True;
end;

procedure TParadoxDataSet.InternalClose;
begin
  BindFields(FALSE);
  if DefaultFields then // Destroy the TField
    DestroyFields;
  Freemem(FHeader);
  Freemem(FaBlock);
  FHeader := nil;
  FActive := False;
end;

procedure TParadoxDataSet.InternalInitFieldDefs;
var
  i    : integer;
  F    : PFldInfoRec;
  FNamesStart : PChar;
begin
  FieldDefs.Clear;
  F := FHeader^.fldInfoPtr;  { begin with the first field identifier }
  FNamesStart := Pointer(F);
  //anyone an better solution for this ?
  inc(ptrrec(FNamesStart).ofs, sizeof(F^)*(FHeader^.numFields));//Jump over Fielddefs
  inc(ptrrec(FNamesStart).ofs, sizeof(Pointer));                //over Tablenameptr
  inc(ptrrec(FNamesStart).ofs, sizeof(PChar)*(FHeader^.numFields));//over Fieldnamepointers
  inc(ptrrec(FNamesStart).ofs, Strlen(FnamesStart)+1);            //over Tablename
  while FnamesStart^ = char(0) do
    inc(ptrrec(FNamesStart).ofs);                               //over Padding
  For i := 1 to FHeader^.numFields do
    begin
      case F^.fType of
      pxfAlpha:    Fielddefs.Add(StrPas(FNamesStart),ftString,F^.fSize);
      pxfDate:     Fielddefs.Add(StrPas(FNamesStart),ftDate,0);
      pxfShort:    Fielddefs.Add(StrPas(FNamesStart),ftSmallInt,F^.fSize);
      pxfLong:     Fielddefs.Add(StrPas(FNamesStart),ftInteger,F^.fSize);
      pxfCurrency: Fielddefs.Add(StrPas(FNamesStart),ftFloat,F^.fSize);
      pxfNumber:   Fielddefs.Add(StrPas(FNamesStart),ftFloat,F^.fSize);
      pxfLogical:  Fielddefs.Add(StrPas(FNamesStart),ftBoolean,F^.fSize);
      pxfMemoBLOb: Fielddefs.Add(StrPas(FNamesStart),ftMemo,F^.fSize);
      pxfBLOb:     Fielddefs.Add(StrPas(FNamesStart),ftBlob,F^.fSize);
      pxfFmtMemoBLOb:Fielddefs.Add(StrPas(FNamesStart),ftMemo,F^.fSize);
      pxfOLE:      Fielddefs.Add(StrPas(FNamesStart),ftBlob,F^.fSize);
      pxfGraphic:  Fielddefs.Add(StrPas(FNamesStart),ftBlob,F^.fSize);
      pxfTime:     Fielddefs.Add(StrPas(FNamesStart),ftTime,F^.fSize);
      pxfTimestamp:Fielddefs.Add(StrPas(FNamesStart),ftdateTime,0);
      pxfAutoInc:  Fielddefs.Add(StrPas(FNamesStart),ftAutoInc,F^.fSize);
      pxfBCD:      Fielddefs.Add(StrPas(FNamesStart),ftBCD,F^.fSize);
      pxfBytes:    Fielddefs.Add(StrPas(FNamesStart),ftString,F^.fSize);
      end;
      inc(ptrrec(FNamesStart).ofs, Strlen(FnamesStart)+1);
      inc(ptrrec(F).ofs, sizeof(F^));
    end;
end;

function TParadoxDataSet.AllocRecordBuffer: PChar;
begin
  if Assigned(Fheader) then
    Result := AllocMem(GetRecordSize)
  else
    Result := nil;
end;

procedure TParadoxDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  if Assigned(Buffer) then
    FreeMem(Buffer);
end;

function TParadoxDataSet.GetRecordCount: Integer;
begin
  if not Assigned(Fheader) then exit;
  Result := FHeader^.numRecords;
end;

function TParadoxDataSet.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

procedure TParadoxDataSet.InternalFirst;
begin
  FaBlockIdx := FHeader^.firstBlock;
  FaBlockstart := 0;
  FaRecord := 0;
  ReadBlock;
end;

procedure TParadoxDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TParadoxDataSet.InternalInitRecord(Buffer: PChar);
begin
end;

procedure TParadoxDataSet.InternalLast;
begin
  while FaBlockIdx <> FHeader^.lastBlock do
    ReadNextBlockHeader;
  inc(FaRecord,(FaBlock^.addDataSize div FHeader^.recordSize)+1);
end;

procedure TParadoxDataSet.InternalPost;
begin
end;

procedure TParadoxDataSet.InternalEdit;
begin
end;

procedure TParadoxDataSet.InternalSetToRecord(Buffer: PChar);
begin
  if (State <> dsInsert) then
    InternalGotoBookmark(@PRecInfo(Buffer + FHeader^.recordSize)^.RecordNumber);
end;

procedure TParadoxDataSet.InternalGotoBookmark(ABookmark: Pointer);
begin
  SetrecNo(PLongWord(ABookmark)^);
end;

procedure TParadoxDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //TODO
end;

function TParadoxDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FHeader^.recordSize)^.BookmarkFlag;
end;

function TParadoxDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  OK : Boolean;
  L: Longword;
begin
  Result := grOK;
  case GetMode of
  gmNext:
    begin
      inc(FaRecord);
      if (FaBlockIdx = FHeader^.lastBlock) and (FaRecord > FaBlockStart+(FaBlock^.addDataSize div FHeader^.recordSize)+1) then
        Result := grEOF
      else
        begin
          if FaRecord > FaBlockStart+1+(FaBlock^.addDataSize div FHeader^.recordSize) then
            ReadNextBlockHeader;
        end;
    end;
  gmPrior:
    begin
      dec(FaRecord);
      if (FaBlockIdx = FHeader^.firstBlock) and (FaRecord < 1) then
        Result := grBOF
      else
        begin
          if FaRecord <= FaBlockStart then
            begin
              ReadPrevBlockHeader;
              FaRecord := FaBlockStart+(FaBlock^.addDataSize div FHeader^.recordSize)+1;
            end;
        end;
    end;
  gmCurrent:
    begin
      if (FaRecord > RecordCount) or (FaRecord < 1) then
        result := grError;
    end;
  end;
  if Result = grOK then
    begin
      if not FBlockreaded then
        ReadBlock;
      L := ((faRecord-(FaBlockstart+1))*FHeader^.recordSize)+6;
      if (faRecord-(FaBlockstart+1)) >= 0 then
        begin
          Move(PChar(FaBlock)[L],Buffer[0],FHeader^.recordSize);
        end
      else
        result := grError;
      with PRecInfo(Buffer + FHeader^.recordSize)^ do
        begin
          BookmarkFlag := bfCurrent;
          RecordNumber := FaRecord;
        end;
    end;
end;

function TParadoxDataSet.GetRecordSize: Word;
begin
  Result := FHeader^.recordSize + sizeof(TRecInfo);
end;

procedure TParadoxDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FHeader^.recordSize)^.BookmarkFlag := Value;
end;

procedure TParadoxDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //TODO
end;

procedure TParadoxDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

function TParadoxDataSet.GetCanModify: Boolean;
begin
  Result:=False;
end;

procedure TParadoxDataSet.SetRecNo(Value: Integer);
begin
  if Value < FaRecord then
    begin
      while (Value <= FaBlockstart) do
        ReadPrevBlockHeader;
      FaRecord := Value;
    end
  else
    begin
      while (Value > FaBlockstart+((FaBlock^.addDataSize div FHeader^.recordSize)+1)) do
        ReadNextBlockHeader;
      FaRecord := Value;
    end;
end;

function TParadoxDataSet.GetRecNo: Integer;
begin
  Result := -1;
  if Assigned(ActiveBuffer) then
    Result := PRecInfo(ActiveBuffer + FHeader^.recordSize)^.RecordNumber;
end;

function TParadoxDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
type
  TNRec= array[0..16] of byte;
var
  b : Boolean;
  F    : PFldInfoRec;
  i: Integer;
  size: Integer;
  p: PChar;
  s: array[0..7] of byte;
  si:  SmallInt absolute s;
  int: LongInt  absolute s;
  d:   Double   absolute s;
begin
  Result := False;
  F := FHeader^.fldInfoPtr;  { begin with the first field identifier }
  p := ActiveBuffer;
  For i := 1 to FHeader^.numFields do
    begin
      if i = Field.FieldNo then
        break;
      If F^.fType = pxfBCD then { BCD field size value not used for field size }
        Inc(ptrrec(p).ofs, 17)
       else
        Inc(ptrrec(p).ofs, F^.fSize);
      Inc(ptrrec(F).ofs, sizeof(F^));
    end;
  If F^.fType = pxfBCD then { BCD field size value not used for field size }
    size := 17
   else
    size := F^.fSize;
  if F^.fType in [pxfDate..pxfNumber, pxfTime..pxfAutoInc] then
    begin
      for i := 0 to pred(size) do
        begin
          s[pred(size-i)] := byte(p[i]);
        end;
      s[pred(size)] := s[pred(size)] xor $80;
    end;

  case F^.fType of
  pxfAlpha,pxfMemoBLOb,pxfFmtMemoBLOb:
    begin
      if (Buffer <> nil) then
        StrLCopy(Buffer, p, Field.Size)
      else
        exit;
      Result := True;
    end;
  pxfDate:
    begin
      i := int;
      Move(i,Buffer^,sizeof(Integer));
      Result := True;
    end;
  pxfShort:
    begin
      i := si;
      Move(i,Buffer^,sizeof(Integer));
      Result := True;
    end;
  pxfLong,pxfAutoInc:
    begin
      i := int;
      Move(i,Buffer^,sizeof(Integer));
      Result := True;
    end;
  pxfCurrency,pxfNumber:
    begin
      Move(d,Buffer^,sizeof(d));
      Result := True;
    end;
  
  pxfLogical:
    begin
//      b := (p^ = #80);
//      Move(b,Buffer^,sizeof(Boolean));
//      Result := True;
    end;
  pxfTime:
    begin
      i := int;
      Move(i,Buffer^,sizeof(Integer));
      Result := True;
    end;
  pxfTimestamp:
    begin
    end;
  end;
end;

constructor TParadoxDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeader := nil;
end;

destructor TParadoxDataSet.Destroy;
begin
  inherited Destroy;
end;

end.

