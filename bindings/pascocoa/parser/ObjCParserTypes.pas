{
 ObjCParserTypes.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 objc parsing unit
}

//todo: pre-compile directives
//todo: enum and struct and a lot of other types...


unit ObjCParserTypes;

interface

{$ifdef fpc}{$mode delphi}{$endif fpc}

uses
  Classes;

type
  TTokenType = (tt_Ident, tt_Symbol, tt_None);

  TCharSet = set of Char;

  TTokenPair = record
    Open       : AnsiString;
    Close      : AnsiString;
  end;

  TTokenTable = class(TObject)
    SpaceChars : TCharSet;
    CmtBlock   : array of TTokenPair;
    CmtCount   : Integer;
    CmtLine    : TStrings;
    Symbols    : TCharSet;
    constructor Create;
    destructor Destroy; override;
  end;

  TTextParser = class(TObject)
  public
    Buf        : AnsiString;
    Index      : Integer;
    TokenTable : TTokenTable;
    function SkipComments: Boolean;
    function FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;
    constructor Create;
  end;

  { TEntity }

  TEntity = class(TObject)
  public
    owner : TEntity;
    Items : TList;
    constructor Create(AOwner: TEntity);
    destructor Destroy; override;
    procedure Parse(AParser: TTextParser); virtual; abstract;
  end;

  { TParameterDef }

  TResultTypeDef = class(TEntity)
    _isRef    : Boolean;
    _TypeName : AnsiString;
    _isConst  : Boolean; // (const Sometype)
    _Prefix   : AnsiString; // reserved-word  type descriptors
    procedure Parse(AParser: TTextParser); override;
  end;

  TParameterDef = class(TEntity)
    _Res  : TResultTypeDef;
    _Name : AnsiString;
    procedure Parse(AParser: TTextParser); override;
    function GetResultType: TResultTypeDef;
  end;

  { TParamDescr }

  TParamDescr = class(TEntity)
  public
    _Descr   : AnsiString;
    procedure Parse(AParser: TTextParser); override;
  end;

  { TClassMethodDef }

  TClassMethodDef = class(TEntity)
    _IsClassMethod  : Boolean;  // is class function as delphi would say
    _CallChar       : AnsiChar; // + or -
    _Name           : AnsiString;
    procedure Parse(AParser: TTextParser); override;
    function GetResultType: TResultTypeDef;
  end;

  { TSubSection }

  //todo: implement
  TSubSection = class(TEntity) // for public, protected and private sections
    _EntityName : AnsiString;
    procedure Parse(AParser: TTextParser); override;
  end;

  { TClassDef }
  
  TClassDef = class(TEntity)
  public
    _ClassName    : AnsiString;
    _SuperClass   : AnsiString;
    _Category     : AnsiString;
    procedure Parse(AParser: TTextParser); override;
  end;

  { TObjCHeader }

  TObjCHeader = class(TEntity)
  public
    _FileName     : AnsiString;
    constructor Create;
    procedure Parse(AParser: TTextParser); override;
  end;


const
  EoLnChars : TCharSet = [#10,#13];
  InvsChars : TCharSet = [#32,#9];

procedure SkipLine(const s: AnsiString; var index: Integer);
procedure SetCComments(Table: TTokenTable);
procedure SetCSymbols(var ch: TCharSet);

function CreateObjCTokenTable: TTokenTable;

implementation

function CreateObjCTokenTable: TTokenTable;
begin
  Result := TTokenTable.Create;
  SetCComments(Result);
  SetCSymbols(Result.Symbols);
  Result.SpaceChars := EoLnChars + InvsChars;
end;

procedure SetCSymbols(var ch: TCharSet);
begin
  ch := ['(',')', '{','}', ':', '-','+','<','>','*',';']
end;

procedure SetCComments(Table: TTokenTable);
begin
  SetLength(Table.CmtBlock, 1);
  Table.CmtCount := 1;
  Table.CmtBlock[0].Open := '/*';
  Table.CmtBlock[0].Close := '*/';
  Table.CmtLine.Add('//');
end;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if not (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

{ TTextParser }

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := false;
  if length(sbs) > length(s) - index then Exit;
  j := index;
  for i := 1 to length(sbs) do begin
    if sbs[i] <> s[j] then Exit;
    inc(j);
  end;
  Result := true;
end;

procedure SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString);
begin
  if closecmt = '' then begin
    index := length(s) + 1;
    Exit;
  end;
  while index <= length(s) do begin
    ScanTo(s, index, [closecmt[1]]);
    if IsSubStr(closecmt, s, index) then begin
      inc(index, length(closecmt));
      Exit;
    end else
      inc(index);
  end;
end;

procedure SkipLine(const s: AnsiString; var index: Integer);
begin
  ScanTo(s, index, EoLnChars);
  ScanWhile(s, index, EoLnChars);
end;

constructor TTextParser.Create;
begin

  Index := 1;

end;



function TTextParser.FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;
var
  srch  : TCharSet;
  blck  : TCharSet;
  i     : Integer;
  t     : AnsiString;
begin
  Result := Index <= length(Buf);
  if not Result then Exit;

  srch := TokenTable.SpaceChars;
  blck := [];
  for i := 0 to TokenTable.CmtCount - 1 do begin
    t := TokenTable.CmtBlock[i].Open[1];
    if t <> '' then blck := blck + [t[1]];
  end;
  for i := 0 to TokenTable.CmtLine.Count - 1 do begin
    t := TokenTable.CmtLine[i];
    if t <> '' then blck := blck + [t[1]];
  end;
  srch := srch + blck;

  Token := '';
  Result := false;
  TokenType := tt_Ident;
  while (not Result) and (index <= length(Buf)) do begin
    ScanWhile(Buf, index, TokenTable.SpaceChars);
    if (Buf[index] in TokenTable.Symbols) then begin
      if (not (Buf[index] in blck)) or (not SkipComments) then begin
        Result := true;
        TokenType := tt_Symbol;
        Token := Buf[index];
        inc(index);
        Exit;
      end;
    end else begin
      Token := Token + ScanTo(Buf, index, srch+TokenTable.Symbols);
      if (Buf[index] in blck)  then begin
        Result := SkipComments;
        Result := Result or (Buf[index] in TokenTable.SpaceChars);
        if not Result then begin
          Token := Token + Buf[index];
          inc(index);
        end;
      end else
        Result := true;
      Result := Result and (Token <> '');
    end;

  end; {of while}
  if not Result then TokenType := tt_None;
end;

function TTextParser.SkipComments: Boolean;
var
  i : Integer;
begin
  Result := false;
  for i := 0 to TokenTable.CmtCount - 1 do
    if IsSubStr(TokenTable.CmtBlock[i].Open, Buf, index) then begin
      inc(index, length(TokenTable.CmtBlock[i].Open));
      SkipCommentBlock(Buf, index, TokenTable.CmtBlock[i].Close);
      Result := true;
      Exit;
    end;
  for i := 0 to TokenTable.CmtLine.Count - 1 do
    if IsSubStr(TokenTable.CmtLine[i], Buf, index) then begin
      SkipLine(Buf, index);
      Result := true;
      Exit;
    end;
end;

{ TTokenTable }

constructor TTokenTable.Create;
begin
  CmtLine := TStringList.Create;
end;

destructor TTokenTable.Destroy;
begin
  CmtLine.Free;
  inherited;
end;

{ TEntity }

constructor TEntity.Create(AOwner: TEntity);
begin
  inherited Create;
  Owner := AOwner;
  Items := TList.Create;
end;

destructor TEntity.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

{ TClassDef }

procedure TClassDef.Parse(AParser:TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  cnt : Integer;
  mtd : TClassMethodDef;
begin
  AParser.FindNextToken(_ClassName, tt);
  if (not AParser.FindNextToken(s, tt)) then Exit;
  if tt = tt_Symbol then begin
    if s[1] = ':' then
      AParser.FindNextToken(_SuperClass, tt)
    else if s[1] = '(' then begin
      AParser.FindNextToken(_Category, tt);
      AParser.FindNextToken(s, tt);
    end else
      Exit;
  end;

  cnt := 0;
  repeat
    if not AParser.FindNextToken(s, tt) then begin
      s := '';
      exit;
    end;

    if s = '{' then inc(cnt)
    else if s = '}' then dec(cnt)
    else if (cnt = 0) then begin
      //todo: better parsing
      if s[1] ='#' then SkipLine(AParser.buf, AParser.Index);
      if (s = '+') or (s = '-') then begin
        dec(AParser.Index ); // roll back a single character
        mtd := TClassMethodDef.Create(Self);
        mtd.Parse(AParser);
        Items.Add(mtd);
      end;
    end;
  until (s = '@end') or (s = '');
end;

{ TObjCHeader }

constructor TObjCHeader.Create;
begin
  //obj-c header does not have any entity owners
  inherited Create(nil);
end;

procedure TObjCHeader.Parse(AParser:TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  cl  : TClassDef;
begin
  while AParser.FindNextToken(s, tt) do begin
    if s = '@interface' then begin
      cl := TClassDef.Create(Self);
      cl.Parse(AParser);
      Items.Add(cl);
    end;
  end;
end;

{ TClassMethodDef }

function TClassMethodDef.GetResultType: TResultTypeDef;
var
  i   : integer;
begin

  for i := 0 to Items.Count - 1 do

    if TObject(Items[i]) is TResultTypeDef then begin

      Result := TResultTypeDef(Items[i]);

      Exit;

    end;

  Result := nil;

end;



procedure TClassMethodDef.Parse(AParser:TTextParser);
var
  s     : AnsiString;
  tt    : TTokenType;
  res   : TResultTypeDef;
  para  : TParameterDef;
  des   : TParamDescr;
begin
  AParser.FindNextToken(s, tt);
  if (s <> '+') and (s <> '-') then Exit;
  _CallChar := s[1];
  _IsClassMethod := _CallChar = '+';

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and(s = '(') then begin
    // _Class methods can be with out type
    dec(AParser.Index);
    res := TResultTypeDef.Create(Self);
    res.Parse(AParser);
    Items.Add(res);
  end;
  AParser.FindNextToken(_Name, tt);

  if _Name = '_id' then
    _Name := '_id';

  while AParser.FindNextToken(s, tt) do begin
    if s = ';' then
      Exit
    else if s = ':' then begin
      para := TParameterDef.Create(Self);
      para.Parse(AParser);
      Items.Add(para);
    end else if tt = tt_Ident then begin
      des := TParamDescr.Create(Self);
      des._Descr := s;
      Items.Add(des)
    end;

  end;
//  AParser.FindNextToken()
end;

{ TParameterDef }

function TParameterDef.GetResultType: TResultTypeDef;
begin

  Result := _Res;

end;



procedure TParameterDef.Parse(AParser:TTextParser);
var
  tt  : TTokenType;
begin
  _Res := TResultTypeDef.Create(Self);
  Items.Add(_Res);
  _Res.Parse(AParser);
  AParser.FindNextToken(_Name, tt)
end;

{ TResultTypeDef }

const
  TypeDefReserved : array [0..1] of AnsiString = (
    'unsigned', 'const'
  );

function IsTypeDefReserved(const s: AnsiString): Boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to length(TypeDefReserved) - 1 do
    if TypeDefReserved[i] = s then begin
      Result := true;
      Exit;
    end;
end;

procedure TResultTypeDef.Parse(AParser: TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
begin

  AParser.FindNextToken(s, tt);

  if (tt <> tt_Symbol) and (s <> '(') then Exit;

  _prefix := '';
  _TypeName := '';
  repeat
    AParser.FindNextToken(s, tt);
    if isTypeDefReserved(s) then begin
      _prefix := _prefix + s;
      if s = 'unsigned' then _TypeName := _typeName + ' ' + s;
      s := '';
    end;
  until s <> '';
  _TypeName := _TypeName + s;

  if tt <> tt_Ident then Exit; // an error

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '*') then begin
    _isRef := true;
    AParser.FindNextToken(s, tt);
  end;

  if s <> ')' then ; // an error

end;



{ TParamDescr }


procedure TParamDescr.Parse(AParser: TTextParser);
var
  tt  : TTokenType;
begin
  AParser.FindNextToken(_Descr, tt);
end;

{ TSubSection }

procedure TSubSection.Parse(AParser: TTextParser);
begin
 //todo:
end;

end.
