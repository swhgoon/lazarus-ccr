{
 ObjCParserTypes.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev

 parsing objc header unit
}
unit ObjCParserTypes;

interface

{$ifdef fpc}{$mode delphi}{$endif fpc}

uses
  Classes, SysUtils;

const
  Err_Ident   = 'Identifier';
  Err_Expect  = '%s, excepted, but %s found';
  Err_BadPrecompile   = 'Bad precompile directive';

type
  TTokenType = (tt_Ident, tt_Symbol, tt_None, tt_Numeric);

  TCharSet = set of Char;

  TTokenPair = record
    Open       : AnsiString;
    Close      : AnsiString;
  end;

  TTokenTable = class(TObject)
    SpaceChars  : TCharSet;
    CmtBlock    : array of TTokenPair;
    CmtCount    : Integer;
    CmtLine     : TStrings;
    Symbols     : TCharSet;
    Precompile  : AnsiString;
    constructor Create;
    destructor Destroy; override;
  end;

  { TTextParser }

  TTextParser = class(TObject)
  protected
    function HandlePrecomiler: Boolean; virtual;

  public
    Buf           : AnsiString;
    Index         : Integer;
    TokenPos      : Integer;
    TokenTable    : TTokenTable;
    OnPrecompile  : TNotifyEvent;
    OnComment     : procedure (Sender: TObject; const Comment: AnsiString) of object;
    OnIgnoreToken : procedure (Sender: TObject; const Ignored: AnsiString) of object;
    Line          : Integer;

    Stack         : TList;
    Errors        : TStringList;
    IgnoreTokens  : TStringList;

    constructor Create;
    destructor Destroy; override;

    procedure BeginParse(AObject: TObject);
    procedure EndParse;

    function SkipComments: Boolean;

    function FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;

    procedure SetError(const ErrorCmt: AnsiString);

  end;

  { TEntity }

  TEntity = class(TObject)
  protected
    function DoParse(AParser: TTextParser): Boolean; virtual; abstract;
  public
    owner : TEntity;
    Items : TList;
    TagComment  : AnsiString;
    constructor Create(AOwner: TEntity);
    destructor Destroy; override;
    function Parse(AParser: TTextParser): Boolean; virtual;
  end;

  { TComment }

  //C tokens: /*, //
  TComment = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
     _Comment : WideString;
  end;

  TSkip = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Skip   : AnsiString;
  end;

  { TPrecompiler }

  //C token: #
  TPrecompiler = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Directive : AnsiString;
    _Params    : AnsiString;
  end;

  { TFunctionParam }
  TFunctionParam = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type   : TEntity;
    _Name   : AnsiString;
    _IsAny  : Boolean;
  end;

  { TFunctionParamsList }

  TFunctionParamsList = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TFunctionTypeDef = class(TEntity)
  protected
    function DoParse(APArser: TTextParser): Boolean; override;
  public    
    _ResultType   : TEntity;
    _ParamsList   : TFunctionParamsList;
    
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
  end;

  { TEnumValue }

  TEnumValue = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    _Value    : AnsiString;
  end;

  { TEnumTypeDef }

  //C token: enum
  {updated}
  TEnumTypeDef = class(TEntity)
  protected
    fValCount  : Integer;
    function GetValue(idx: integer): TEnumValue;
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    property Value[idx: Integer]: TEnumValue read GetValue;
    property ValuesCount: Integer read fValCount;
  end;

  { TStructField }


  TStructField = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name       : AnsiString;
    _IsArray    : Boolean;
    _ArraySize  : AnsiSTring;
    _BitSize    : Integer;
    _Type       : TEntity;
    _TypeName   : AnsiString;
  end;

  { TStructTypeDef }

  //C token: struct
  TStructTypeDef = class(TEntity)
  {update}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name         : AnsiString;
    //todo: remove
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
  end;

  TUnionTypeDef = class(TStructTypeDef)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name       : AnsiString;
    //todo: remove
  end;

  { TTypeDef }
  //C token - any type, including unsigned short

  TTypeDefSpecs = set of (td_Unsigned, td_Signed, td_Volitale, td_Const, td_InOut, td_Long, td_Short, td_Char, td_Int);

  {updated}
  TTypeDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    _Spec     : TTypeDefSpecs;
    _IsPointer    : Boolean;
    _IsPointerRef : Boolean;
  end;

  { TTypeNameDef }

  //C token: typdef
  TTypeNameDef = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Inherited  : AnsiString;
    _TypeName   : AnsiString;
    _Type       : TEntity;
  end;

  { TObjCParameterDef }

  TObjCResultTypeDef = class(TEntity)
  {updating}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type     : TEntity;
    _isRef    : Boolean;
    _isConst  : Boolean; // (const Sometype)
    _Prefix   : AnsiString; // reserved-word  type descriptors
  end;

  TObjCParameterDef = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type : TObjCResultTypeDef;
    _Name : AnsiString;
  end;

  { TParamDescr }

  TParamDescr = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Descr   : AnsiString;
  end;

  { TClassMethodDef }

  TClassMethodDef = class(TEntity)
  {update}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _IsClassMethod  : Boolean;  // is class function as delphi would say
    _CallChar       : AnsiChar; // + or -
    _Name           : AnsiString;
    function GetResultType: TObjCResultTypeDef;
  end;

  { TSubSection }

  //todo: implement
  TSubSection = class(TEntity) // for public, protected and private sections
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _EntityName : AnsiString;
  end;

  { TClassDef }

  TClassDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _ClassName    : AnsiString;
    _SuperClass   : AnsiString;
    _Category     : AnsiString;
    _Protocols    : TStringList;
    constructor Create(AOwner : TEntity);
    destructor Destroy; override;
  end;

  { TObjCHeader }

  TObjCHeader = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _FileName     : AnsiString;
    constructor Create;
  end;

const
  EoLnChars : TCharSet = [#10,#13];
  InvsChars : TCharSet = [#32,#9];

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
procedure SetCComments(Table: TTokenTable);
procedure SetCSymbols(var ch: TCharSet);

function CreateObjCTokenTable: TTokenTable;

function LastEntity(ent: TEntity): TEntity;
function ParseCExpression(AParser: TTextParser; var ExpS: AnsiString): Boolean;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;

function ParseTypeDef(Owner: TEntity; AParser: TTextParser): TEntity;
function ParseCVarDef(AParser: TTextParser; var Name: AnsiString; var isArray: Boolean; var ArraySize:AnsiString): Boolean;

function GetTypeNameFromEntity(Entity: TEntity): AnsiString;
function IsTypeDefIsPointer(Entity: TEntity): Boolean;

procedure FreeEntity(Item: TEntity);

procedure ParseCNumeric(const S: AnsiString; var idx: integer; var NumStr: AnsiSTring);
function CToPascalNumeric(const Cnum: AnsiString): AnsiString;

function IsTypePointer(AType: TEntity; DefResult: Boolean ): Boolean;
function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;

function IsTypeOrTypeDef(const Token: AnsiString): Boolean;

function ParseTypeOrTypeDef(AParser: TTextParser; Owner: TEntity; var Ent: TEntity): Boolean;

function IsTypeDefEntity(Ent: TEntity): Boolean;
function isEmptyStruct(AStruct: TStructTypeDef): Boolean;

implementation

function IsTypeDefEntity(Ent: TEntity): Boolean;
begin
  Result := (Ent is TTypeDef) or (Ent is TStructTypeDef)
    or (Ent is TUnionTypeDef) or (Ent is TTypeNameDef) or (Ent is TEnumTypeDef); 
end;

function IsTypeOrTypeDef(const Token: AnsiString): Boolean;
begin
  Result := false;
  if Token = '' then Exit;
  case Token[1] of
    't': Result := Token = 'typedef';
    'e': Result := Token = 'enum';
    's': Result := Token = 'struct';
    'u': Result := Token = 'union';
  end;
end;

function ParseTypeOrTypeDef(AParser: TTextParser; Owner: TEntity; var Ent: TEntity): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Ident) and IsTypeOrTypeDef(s);
  if (not Result) then begin
    AParser.Index := AParser.TokenPos;
    Exit;
  end;

  if s = 'typedef' then begin
    AParser.Index := AParser.TokenPos;
    Ent := TTypeNameDef.Create(Owner);
    Result := Ent.Parse(AParser);
  end else begin
    AParser.Index := AParser.TokenPos;  
    Ent := ParseTypeDef(Owner, AParser);
    Result := Assigned(ent);
    if Result then begin
      AParser.FindNextToken(s, tt);
      Result := (tt=tt_Symbol) and (s = ';');
    end;
  end;
end;

// isPointer returned the * is declared
// isPointerRef return the ** is declared
procedure ParsePointerDef(AParser: TTextParser; var isPointer, isPointerRef: Boolean);
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  isPointer := false;
  isPointerRef := false;
  if not AParser.FindNextToken(s, tt) then Exit;
  isPointer := (tt=tt_Symbol) and (s = '*');
  if isPointer then begin
    if not AParser.FindNextToken(s, tt) then Exit;
    
    if (tt=tt_Symbol) and (s = '*') then isPointerRef := true
    else AParser.Index := AParser.TokenPos;
  end else
    AParser.Index := AParser.TokenPos;
end;


function ParseCVarDef(AParser: TTextParser; var Name: AnsiString; var isArray: Boolean; var ArraySize:AnsiString): Boolean;
var
  tt    : TTokenType;
  s     : AnsiString;
begin
  Result := AParser.FindNextToken(Name, tt);
  if Result then Result := tt = tt_Ident;
  if not Result then begin
    AParser.SetError(ErrExpectStr('Identifier', Name) );
    Exit;
  end;
  Result := true;

  AParser.FindNextToken(s, tt);
  if not ((tt = tt_Symbol) and (s = '[')) then begin
    AParser.Index := AParser.TokenPos;
    Exit;
  end;

  isArray := true;
  ParseCExpression(APArser, ArraySize);
  AParser.FindNextToken(s, tt);
  if s <> ']' then begin
    Result := false;
    AParser.SetError( ErrExpectStr('[', ArraySize));
    AParser.Index := AParser.TokenPos;
  end;

end;

function IsTypePointer(AType: TEntity; DefResult: Boolean ): Boolean;
begin
  Result := DefResult;
  if not Assigned(AType) then Exit;
  if AType is TTypeDef then
    Result := TTypeDef(AType)._IsPointer
  else if AType is TStructTypeDef then
    Result := TStructTypeDef(AType)._isPointer;
end;

function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;
begin
  Result := Format(Err_Expect, [Expected, Found]);
end;

procedure FreeEntity(Item: TEntity);
var
  i    : Integer;
begin
  for i := 0 to Item.Items.Count - 1 do
    FreeEntity(TEntity(Item.Items[i]));
  Item.Free;
end;

function GetTypeNameFromEntity(Entity: TEntity): AnsiString;
begin
  Result := '';
  if Assigned(Entity) then begin
    if Entity is TStructTypeDef then // hmm... a common ancsessotor should be used?
      Result := TStructTypeDef(Entity)._Name
    else if Entity is TEnumTypeDef then
      Result := TEnumTypeDef(Entity)._Name
    else if Entity is TTypeDef then begin
      Result := TTypeDef(Entity)._Name;
    end;
  end;
end;

function IsTypeDefIsPointer(Entity: TEntity): Boolean;
begin
  Result := false;
  if Assigned(Entity) then begin
    if Entity is TStructTypeDef then // hmm... a common ancsessotor should be used?
      Result := TStructTypeDef(Entity)._isPointer
    else if Entity is TTypeDef then begin
      Result := TTypeDef(Entity)._isPointer;
    end;
  end;
end;


(* ANSI C reserved words
auto  break case char const continue default do double else enum
extern float for goto if int  long register  return short signed
sizeof static struct switch typedef union  unsigned void volatile while
*)

function ParseTypeDef(Owner: TEntity; AParser: TTextParser): TEntity;
var
  s   : AnsiString;
  tt  : TTokenType;
  res : Boolean;
  i   : Integer;
begin
  Result := nil;
  res := AParser.FindNextToken(s, tt);
  if not Res or (tt <> tt_Ident) then Exit;
  
  i := AParser.TokenPos;
  s := AnsiLowerCase(s);
  if (s = 'const') {or (s = 'volatile')}  then begin
    res := AParser.FindNextToken(s, tt);
    if s <> 'struct' then begin
      AParser.TokenPos := i;
      AParser.Index := i;
    end;
  end;
  
  if s = 'enum' then
    Result := TEnumTypeDef.Create(Owner)
  else if s = 'struct' then
    Result := TStructTypeDef.Create(Owner)
  else if s = 'union' then
    Result := TUnionTypeDef.Create(Owner)
  else
    Result := TTypeDef.Create(Owner);

  AParser.Index := AParser.TokenPos;
  if Assigned(Result) then 
    if not Result.Parse(AParser) then begin
      Result.Free;
      Result := nil;
    end;
end;

function LastEntity(ent: TEntity): TEntity;
var
  i   : integer;
  pre : TEntity;
begin
  pre := nil;
  while Assigned(ent) do begin
    pre := ent;
    i := pre.Items.Count - 1;
    if i >= 0 then ent := TEntity(pre.Items[i])
    else ent := nil;
  end;
  Result := pre;
end;

function CreateObjCTokenTable: TTokenTable;
begin
  Result := TTokenTable.Create;
  SetCComments(Result);
  SetCSymbols(Result.Symbols);
  Result.SpaceChars := EoLnChars + InvsChars;
end;

procedure SetCSymbols(var ch: TCharSet);
begin
  ch := ['(',')', '{','}', ':', '-','+','<','>','*',';', ',','|','&','[',']']
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

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := false;
  if (sbs = '') or (length(sbs) > length(s) - index) then Exit;
  j := index;
  for i := 1 to length(sbs) do begin
    if sbs[i] <> s[j] then Exit;
    inc(j);
  end;
  Result := true;
end;

function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;
begin
  Result := '';
  if closecmt = '' then begin
    index := length(s) + 1;
    Exit;
  end;
  while index <= length(s) do begin
    Result := Result + ScanTo(s, index, [closecmt[1]]);
    if IsSubStr(closecmt, s, index) then begin
      inc(index, length(closecmt));
      Exit;
    end else begin
      Result := Result + s[index];
      inc(index);
    end;
  end;
end;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
begin
  Result := ScanTo(s, index, EoLnChars);
  ScanWhile(s, index, EoLnChars); // todo: skip a single line!
end;

{ TTextParser }

constructor TTextParser.Create;
begin
  Index := 1;
  Line := 1;
  Stack := TList.Create;
  Errors := TStringList.Create;
  IgnoreTokens := TStringList.Create;
end;

destructor TTextParser.Destroy;
begin
  IgnoreTokens.Free;
  Errors.Free;
  Stack.Free;
  inherited Destroy;
end;

procedure TTextParser.BeginParse(AObject: TObject);
begin
  Stack.Add(AObject);
end;

procedure TTextParser.EndParse;
begin
  if Stack.Count > 0 then Stack.Delete(Stack.Count - 1);
end;

function TTextParser.HandlePrecomiler: Boolean;
var
  idx : Integer;
begin
  idx := Index;
  if Assigned(OnPrecompile) then
    OnPrecompile(Self);
  Result := Index <> idx;
end;

function ParseHexNumber(const S:AnsiString; var idx: Integer): AnsiString;
begin
  Result := ScanWhile(s, idx, ['0'..'9', 'A'..'F', 'a'..'f']);
end;

procedure ParseCNumeric(const S: AnsiString; var idx: integer; var NumStr: AnsiSTring);
var
  l : integer;
  i : Integer;
  f : AnsiString;
begin
  l := length(s);
  if (idx <= 0) or (idx > l) then Exit;
  
  if (s[idx] = '0') and (idx < l) and ((s[idx+1] = 'x') or (s[idx+1] = 'X')) then begin
    inc(idx,2);
    NumStr := '0x'+ParseHexNumber(s, idx);
  end else begin
    NumStr := ScanWhile(s, idx, ['0'..'9']);
    if (idx < l) and (s[idx] = '.') then begin
      i := idx + 1;
      f := ScanWhile(s, i, ['0'..'9']);
      if f <> '' then begin
        idx := i;
        NumStr := NumStr + '.' + f;
      end;
    end;
  end;

  ScanWhile(s, idx, ['U','L','u','l']);
end;

function isFloatNum(const num: AnsiString): Boolean;
begin
  Result := Pos('.', num)>0;
end;

function CToPascalNumeric(const Cnum: AnsiString): AnsiString;
var
  i   : Integer;
  num : Int64;
  c   : Int64;
begin
  if isFloatNum(cNum) then
    Result := cNum
  else if length(cNum) < 3 then
    Result := cNum
  else if cNum[1] <> '0' then
    Result := cNum
  else begin
    if cNum[2] = 'x'
      then Result := '$'+Copy(cNum, 3, length(cNum) - 2)
    else begin
      num := 0;
      c := 1;
      for i := length(cnum) downto 1 do begin
        if not (cnum[i] in['0'..'7']) then begin
          Result := cNum;
          Exit;
        end;
        num := num + c * (byte(cnum[i]) - byte('0'));
        c := c * 8;
      end;
      Result := IntToStr(num);
    end;
  end;
end;


function TTextParser.FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;
var
  srch  : TCharSet;
  blck  : TCharSet;
  i     : Integer;
  t     : AnsiString;
  spaces  : TCharSet;
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

  spaces := TokenTable.SpaceChars;
  try
    while (not Result) and (index <= length(Buf)) do begin
      ScanWhile(Buf, index, spaces);
      if not (IsSubStr(TokenTable.Precompile, Buf, Index) and HandlePrecomiler) then begin // 1. check is Compiler directive is found
        if (Buf[index] in TokenTable.Symbols) then begin                 // 2. symbol has been found, so it's not an ident
          if (not (Buf[index] in blck)) or (not SkipComments) then begin //   2.1 check if comment is found (comment prefixes match to the symbols)
            Result := true;                                              //   2.2 check if symbol is found
            if (Buf[index] = '.') and (index < length(Buf)) and (Buf[index+1] in ['0'..'9']) then begin
              // is float number
              inc(index);
              Token := '.' + ScanWhile(Buf, index, ['0'..'9']);
              TokenType := tt_Numeric;
            end else begin
              TokenType := tt_Symbol;
              Token := Buf[index];
              inc(index);
            end;
            Exit;
          end;
        end else if (Buf[index] in ['0'..'9']) then begin  // 3. a number is found, so it's possibl a number
          //todo: Hex and floats support!
          //todo: Negative numbers support;
          ParseCNumeric(Buf, index, Token);
          TokenType := tt_Numeric;
          Result := true;
          Exit;
        end else begin
          Token := Token + ScanTo(Buf, index, srch+TokenTable.Symbols); // scanning for token
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
      end;

      if Result and (IgnoreTokens.Count > 0) then begin
        if IgnoreTokens.IndexOf(Token) >= 0 then begin
          if Assigned(OnIgnoreToken) then
            OnIgnoreToken(Self, Token);
          Result := false;
          TokenType := tt_None;
          Token := '';
        end;
      end;
    end; {of while}
  finally
    if not Result
      then TokenType := tt_None
      else TokenPos := Index - length(Token);
    //todo: make an event or something
    if TokenType = tt_Numeric then
      Token := CToPascalNumeric(Token);
  end;
end;

function TTextParser.SkipComments: Boolean;
var
  i : Integer;
  cmt : AnsiSTring;
begin
  try
    cmt := '';
    Result := false;
    for i := 0 to TokenTable.CmtCount - 1 do
      if IsSubStr(TokenTable.CmtBlock[i].Open, Buf, index) then begin
        inc(index, length(TokenTable.CmtBlock[i].Open));
        cmt := SkipCommentBlock(Buf, index, TokenTable.CmtBlock[i].Close);
        Result := true;
        Exit;
      end;
    for i := 0 to TokenTable.CmtLine.Count - 1 do
      if IsSubStr(TokenTable.CmtLine[i], Buf, index) then begin
        cmt := SkipLine(Buf, index);
        Delete(cmt, 1, length(TokenTable.CmtLine[i]) );
        Result := true;
        Exit;
      end;
  finally
    if (Assigned(OnComment)) and (cmt <> '') then
      OnComment(Self, cmt);
  end;
end;

procedure TTextParser.SetError(const ErrorCmt: AnsiString);
begin
  Errors.Add(ErrorCmt);
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

function TEntity.Parse(AParser: TTextParser): Boolean;
begin
  Result := false;
  AParser.BeginParse(Self);
  try
    Result := DoParse(AParser);
  except
    on e: Exception do
      AParser.SetError('Internal error. Exception: ' + e.Message);
  end;
  AParser.EndParse;
end;

{ TClassDef }

constructor TClassDef.Create(AOwner: TEntity);
begin
  inherited Create(AOwner);
  _Protocols := TStringList.Create;
end;

destructor TClassDef.Destroy;
begin
  _Protocols.Free;
  inherited;
end;

function TClassDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  cnt : Integer;
  mtd : TClassMethodDef;
  ent : TEntity;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> '@interface' then begin
    AParser.SetError(ErrExpectStr('@interface', s));
    Exit;
  end;

  AParser.FindNextToken(_ClassName, tt);

  if (not AParser.FindNextToken(s, tt)) then Exit; // parsing super class or category
  if tt = tt_Symbol then begin
    if s[1] = ':' then
      AParser.FindNextToken(_SuperClass, tt)
    else if s[1] = '(' then begin
      AParser.FindNextToken(_Category, tt);
      AParser.FindNextToken(s, tt);
    end else
      AParser.Index := AParser.TokenPos;
  end;

  AParser.FindNextToken(s, tt); // parsing protocols
  if (tt = tt_Symbol) and (s = '<') then begin
    repeat
      if not AParser.FindNextToken(s, tt) then Exit;
      if (s <> '>') then _Protocols.Add(s);
      AParser.FindNextToken(s, tt); // "," or ">"
    until (s = '>');
  end else
    AParser.Index := AParser.TokenPos;


  cnt := 0; // pasring private declarations
  repeat
    if not AParser.FindNextToken(s, tt) then begin
      s := '';
      AParser.SetError('error while parsing class declaration');
      exit;
    end;

    //work around for not using preprocessor! #if @interface #else @interface #endif
    if s = '@interface' then SkipLine(AParser.buf, AParser.index)
    else if s = '{' then inc(cnt)
    else if s = '}' then dec(cnt)
    else if (cnt = 0) then begin
      //todo: better parsing
      // parsing methods
      if s[1] ='#' then SkipLine(AParser.buf, AParser.Index);
      if (s = '+') or (s = '-') then begin
        AParser.Index := AParser.TokenPos; // roll back to the start of method
        mtd := TClassMethodDef.Create(Self);
        mtd.Parse(AParser);
        Items.Add(mtd);
      end else if IsTypeOrTypeDef(s) then begin
        AParser.Index := AParser.TokenPos;
        if ParseTypeOrTypeDef(AParser, Self, ent) then
          Items.Add(ent);
        //AParser.FindNextToken(s, tt);
      end;

    end;
  until (s = '@end') or (s = ''); // looking for declaration end
  Result := true;
end;

{ TObjCHeader }

constructor TObjCHeader.Create;
begin
  //obj-c header does not have any entity owners
  inherited Create(nil);
end;

function TObjCHeader.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  ent : TEntity;
begin
  Result := false;
  while AParser.FindNextToken(s, tt) do begin
    if s = 'typedef' then begin
      AParser.Index := AParser.TokenPos;
      ent := TTypeNameDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
    end else if s = 'enum' then begin
      AParser.Index := AParser.TokenPos;
      ent := TEnumTypeDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
      AParser.FindNextToken(s, tt); // skipping last ';'
    end else if s = '@interface' then begin
      AParser.Index := AParser.TokenPos;
      ent := TClassDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
    end else begin
      // anything else is skipped, though should not!
      ent := TSkip.Create(Self);
      AParser.Index := AParser.TokenPos;
      TSkip(ent)._Skip := SkipLine(AParser.Buf, AParser.Index);
    end;
   if Assigned(ent) then Items.Add(ent);
  end;
  Result := true;
end;

{ TClassMethodDef }

function TClassMethodDef.GetResultType: TObjCResultTypeDef;
var
  i   : integer;
begin
  for i := 0 to Items.Count - 1 do
    if TObject(Items[i]) is TObjCResultTypeDef then begin
      Result := TObjCResultTypeDef(Items[i]);
      Exit;
    end;
  Result := nil;
end;



function TClassMethodDef.DoParse(AParser: TTextParser): Boolean;
var
  s     : AnsiString;
  tt    : TTokenType;
  res   : TObjCResultTypeDef;
  para  : TObjCParameterDef;
  des   : TParamDescr;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (s <> '+') and (s <> '-') then begin
    AParser.SetError( ErrExpectStr(' + or -, method descriptor ', s));
    Exit;
  end;

  _CallChar := s[1];
  _IsClassMethod := _CallChar = '+';

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and(s = '(') then begin
    // _Class methods can be with out type
    AParser.Index:=AParser.TokenPos;
    res := TObjCResultTypeDef.Create(Self);
    if not res.Parse(AParser) then begin
      res.Free;
      Exit;
    end;
    Items.Add(res);
  end else if (tt = tt_Ident) then begin
    // if type is not defined, that it's assumed to be obj-c 'id'
    res := TObjCResultTypeDef.Create(Self);
    res._Type := TTypeDef.Create(res);
    TTypeDef(res._Type)._Name := 'id';

    Items.Add(res);
    AParser.Index := AParser.TokenPos;
  end else
    APArser.SetError(ErrExpectStr('(', s));

  if not AParser.FindNextToken(_Name, tt) then begin
    AParser.SetError(ErrExpectStr('method name Identifier', s));
    Exit;
  end;

  while AParser.FindNextToken(s, tt) do begin
    if s = ';' then
      Break // successfuly parsed!
    else if s = ':' then begin
      para := TObjCParameterDef.Create(Self);
      if not para.Parse(AParser) then begin
        para.Free;
        Exit;
      end;
      Items.Add(para);
    end else if tt = tt_Ident then begin
      des := TParamDescr.Create(Self);
      des._Descr := s;
      Items.Add(des);
    end else begin
      AParser.SetError(ErrExpectStr('type identifier', s));
      Exit;
    end;
  end;
//  AParser.FindNextToken()
  Result := true;
end;

{ TParameterDef }

function TObjCParameterDef.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  _Type := TObjCResultTypeDef.Create(Self);
  if not _Type.Parse(AParser) then Exit;

  Items.Add(_Type);
  AParser.FindNextToken(_Name, tt);
  if tt <> tt_Ident then begin
    AParser.SetError(ErrExpectStr('Identifier', _Name));
    Exit;
  end;
  Result := true;
end;

function isParamFuncPointer(AParser: TTextParser): Boolean;
var
  i   : Integer;
  s   : AnsiString;
  tt  : TTokenType;
begin
  i := AParser.Index;
  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = '(');
  if not Result then Exit;

  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = '*');
  if not Result then Exit;

  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = ')');
  if not Result then Exit;
end;

{ TResultTypeDef }

function TObjCResultTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  fnt : TFunctionTypeDef;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (tt <> tt_Symbol) and (s <> '(') then begin
    AParser.SetError(ErrExpectStr('"("', s));
    Exit;
  end;

  _Type := TTypeDef.Create(Self);
  Result := _Type.Parse(AParser);
  if not Result then Exit;

  if Result then begin
    AParser.FindNextToken(s, tt);
    if (tt=tt_Symbol) and (s='<') then begin // skip protocol
      while (s <> '>') and AParser.FindNextToken(s, tt) do ;
      AParser.FindNextToken(s, tt);
    end;

    if s = '(' then begin // ptr funciton (*)?
      AParser.Index := AParser.TokenPos;
      if not isParamFuncPointer(APArser) then begin
        AParser.SetError(ErrExpectStr(')', s));
        Result := false;
        Exit;
      end;
      fnt := TFunctionTypeDef.Create(Self);
      fnt._ResultType := _Type;
      Result := fnt.Parse(AParser);
      _Type := fnt;
      if not Result then Exit;
      AParser.FindNextToken(s, tt);
    end;
    Result := s = ')';


    if not Result then
      AParser.SetError( ErrExpectStr(')', s));
  end;

end;



{ TParamDescr }

function TParamDescr.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  AParser.FindNextToken(_Descr, tt);
  if tt <> tt_Ident then begin
    AParser.SetError(ErrExpectStr('Identifier', '_Descr'));
    Exit; 
  end;
  Result := true;
end;

{ TSubSection }

function TSubSection.DoParse(AParser: TTextParser): Boolean;
begin
 //todo:
  Result := true;
end;

{ TPrecompiler }

function TPrecompiler.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  if not AParser.FindNextToken(_Directive, tt) then begin
    AParser.Index := AParser.TokenPos;
    AParser.SetError('precompiler directive not found');
    Exit;
  end;
  if (_Directive = '') or (_Directive[1] <> '#') then begin
    AParser.Index := AParser.TokenPos;
    AParser.SetError('identifier is not precompiler directive');
    Exit;
  end;
  _Params := SkipLine(AParser.Buf, AParser.Index);
  Result := true;
end;

{ TEnumTypeDef }

function TEnumTypeDef.GetValue(idx: integer): TEnumValue;
var
  i   : Integer;
  v   : Integer;
begin
  v := 0;
  for i := 0 to Items.Count - 1 do
    if (TObject(Items[i]) is TEnumValue) and (v=idx) then begin
      Result := TEnumValue(Items[i]);
      Exit;
    end else
      inc(v);
  Result := nil;
end;

function TEnumTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  token : AnsiString;
  tt    : TTokenType;
  nm    : AnsiString;
  vl    : TEnumValue;
begin
  Result := false;
  if not AParser.FindNextToken(token, tt) then Exit;
  if token <> 'enum' then begin
    AParser.SetError(ErrExpectStr('enum', token));
    Exit;
  end;

  if not AParser.FindNextToken(nm, tt) then begin
    AParser.SetError(ErrExpectStr('identifier', token));
    Exit;
  end;

  if tt <> tt_Ident then AParser.Index := AParser.TokenPos
  else _Name := nm;

  AParser.FindNextToken(nm, tt);
  if nm <> '{' then begin
    AParser.SetError(ErrExpectStr('"{" for enumeration', token));
    Exit;
  end;

  repeat
    vl := TEnumValue.Create(Self);
    if not vl.Parse(AParser) then begin
      vl.Free;
      Exit;
    end;

    if vl._Name <> '' then begin
      inc(fValCount);
      Items.Add(vl)
    end;

    AParser.FindNextToken(nm, tt);
    if tt = tt_Symbol then begin
      if (nm = ',') then begin
        AParser.FindNextToken(nm, tt);
        if tt = tt_Ident then
          AParser.Index := AParser.TokenPos;
      end;
    end else begin
      AParser.SetError(ErrExpectStr('"}"', token));
      Exit;
    end;

  until nm = '}';


  Result := true;
  //AParser.FindNextToken(nm, tt); // skip last ';'
end;

function ParseCOperator(AParser: TTextParser; var Vl: AnsiString): Boolean;
var
  nm  : AnsiSTring;
  tt  : TTokenType;
begin
  Result := false;
  if not AParser.FindNextToken(nm, tt) then Exit;
  Result := nm <> '';
  if not Result then Exit;
  vl := nm[1];
  case vl[1] of
    '+', '-', '*': Result := true;
    '|', '&': begin
      Result := true;
    end;
    '<', '>': begin
      vl := nm[1];
      Result := AParser.FindNextToken(nm, tt);
      if (not Result) or (nm = '') then Exit;
      Result := nm[1] = vl[1] ;
      if Result then vl := vl[1] + nm[1];
    end;
  else
    Result := false;
  end;
end;

function ParseCExpression(AParser: TTextParser; var ExpS: AnsiString): Boolean;
var
  i     : integer;
  nm    : AnsiString;
  tt    : TTokenType;
  brac  : Integer;
begin
//todo: better code. it's just a work around
//  i := AParser.Index;
  brac := 0;
  ExpS := '';
  Result := false;

  try
    while AParser.FindNextToken(nm, tt) do begin
      if (nm = #39) then begin
        ExpS := #39 + ScanTo(APArser.Buf, AParser.Index, [#39]) + #39;
        inc(AParser.Index);
        Result := true;
        Exit;
      end else if (tt = tt_Numeric) or (tt = tt_Ident) then begin
        ExpS := ExpS + nm;
        i := AParser.Index;
        if not ParseCOperator(AParser, nm) then begin
          AParser.Index := i;
          Break;
        end else
          ExpS := ExpS + ' ' + nm + ' ';
      end else if (tt = tt_Symbol) then begin
        if nm ='(' then inc(brac)
        else if nm = ')' then dec(brac);
      end else begin
        //i := AParser.Index;
        Exit;
      end;
    end;
    Result := true;

  finally
    if brac > 0 then
      while (brac > 0) and (AParser.FindNextToken(nm, tt)) do
        if nm = ')' then
          dec(brac);
  end;
end;

{ TEnumValue }

function TEnumValue.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  Result := false;
  AParser.FindNextToken(_Name, tt);
  if tt <> tt_Ident then begin
    AParser.SetError( ErrExpectStr('Identifier', _Name) );
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '=' then begin
    AParser.Index := AParser.TokenPos;
    _Value := '';
  end else begin
    if not ParseCExpression(AParser, _Value) then
      Exit;
  end;
  Result := true;
end;

{ TComment }

function TComment.DoParse(AParser: TTextParser): Boolean;
begin
  Result := true;
  //todo:! Comment parsing is now executed by TTextParser
end;

{ TTypeNameDef }

function TTypeNameDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'typedef' then begin
    AParser.SetError( ErrExpectStr('typedef', s));
    Exit;
  end;
  
  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then Exit;

  Result := AParser.FindNextToken(_TypeName, tt);
  if not Result then begin
    AParser.SetError( ErrExpectStr('Type name identifier', _TypeName) );
    Exit;
  end;
  _inherited := GetTypeNameFromEntity( _Type );
  AParser.FindNextToken(s, tt); // skip last ';';

  Result := true;
end;

{ TStructTypeDef }

function TStructTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  i   : Integer;
  st    : TStructField;
  prev  : TStructField;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'struct' then begin
    AParser.SetError(ErrExpectStr('struct', s));
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  i := AParser.TokenPos;
  if (tt = tt_Ident) then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    i := AParser.TokenPos;
  end;

  if not ((tt = tt_Symbol) and (s = '{')) then begin
    AParser.Index := i;
    ParsePointerDef(AParser, _isPointer, _isPointerRef);
    Result := true;
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '}' then
    AParser.Index := AParser.TokenPos;
  prev := nil;
  while (s <> '}') do begin
    //i := AParser.TokenPos;
    st := TStructField.Create(Self);
    if not Assigned(prev) then begin
      if not st.Parse(AParser) then Exit;
    end else begin
      Result := ParseCVarDef(APArser, st._Name, st._IsArray, st._ArraySize );
      if not Result then
        Exit;
      {if tt <> tt_Ident then begin
        AParser.SetError(ErrExpectStr('field name', st._Name));
        Exit;
      end;}
      st._TypeName := prev._TypeName;
    end;

    Items.Add(st);
    AParser.FindNextToken(s, tt);
    if s = ','
      then prev := st
      else prev := nil;

    if s = ';' then begin
      AParser.FindNextToken(s, tt);
      if s <> '}' then
        AParser.Index := AParser.TokenPos;
    end;{ else begin
      AParser.Index := AParser.TokenPos;
    end;}
  end;

  Result := true;
  //no skipping last ';', because after structure a variable can be defined
  //ie: struct POINT {int x; int y} point;
end;

{ TStructField }

function CVal(c: AnsiString; var v: Integer): Boolean; // todo: hex, oct handling (0x, x)
var
  err : Integer;
begin
  Val(c, v, err);
  Result := err = 0;
end;

function TStructField.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
  s   : AnsiString;
  fnc : TFunctionTypeDef;
//  fld : TStructField;
begin
  Result := false;
  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then Exit;

  _TypeName := GetTypeNameFromEntity(_Type);

  {if not (AParser.FindNextToken(s, tt)) or (tt <> tt_Ident) then begin
    AParser.SetError(ErrExpectStr('Identifier', s));
    Exit;
  end;}

  AParser.FindNextToken(s, tt);
  if (tt=tt_Symbol) and (s = '(') then begin
    fnc := TFunctionTypeDef.Create(Self);
    fnc._ResultType := _Type;
    _Type := fnc;
    _TypeName := '';

    ParsePointerDef(AParser, fnc._isPointer, fnc._isPointerRef);
    Result := ParseCVarDef(AParser, _Name, _IsArray, _ArraySize );
    if not Result then Exit;
    //AParser.FindNextToken(_Name, tt);
    {if (tt <> tt_Ident) then begin
      AParser.SetError( ErrExpectStr('Identifier', _Name));
      Result := false;
      Exit;
    end;}

    AParser.FindNextToken(s, tt);
    if (tt <> tt_Symbol) and (s <> ')') then begin
      AParser.SetError(ErrExpectStr(')', s));
      Result := false;
      Exit;
    end;

    Result := fnc.Parse(AParser);


  end else begin
    AParser.Index := AParser.TokenPos;
    Result := ParseCVarDef(AParser, _Name, _IsArray, _ArraySize );
    if not Result then Exit;

    AParser.FindNextToken(s, tt);
    if (tt = tt_Symbol) and (s = ':') then begin
      AParser.FindNextToken(s, tt);
      if tt <> tt_Numeric then begin
        AParser.SetError(ErrExpectStr('number', s));
        Exit;
      end;
      CVal(s, _BitSize);
    end else if (tt = tt_Symbol) and (s = '(') then begin
      //
    end else
      AParser.Index := AParser.TokenPos;
    Result := true;
    //success: (tt = tt_Symbol) and (s = ';')
  end;

end;

{ TTypeDef }

function IsSpecifier(const s: AnsiSTring; var SpecVal, SpecMask: TTypeDefSpecs): Boolean;
begin
  Result := true;
  if (s = 'volitle') then begin
    SpecVal := [td_Volitale];
    SpecMask := [td_Volitale];
  end else if (s = 'const') then begin
    SpecVal := [td_Const];
    SpecMask := [td_InOut, td_Const];
  end else if (s = 'signed') then begin
    SpecVal := [td_Signed];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'unsigned') then begin
    SpecVal := [td_Unsigned];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'long') then begin
    SpecVal := [td_Long];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'short') then begin
    SpecVal := [td_Short];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'char') then begin
    SpecVal := [td_Char];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'int') then begin
    SpecVal := [td_Int];
    SpecMask := [td_Int];
  end else if (s = 'inout') then begin
    SpecVal := [td_inout];
    SpecMask := [td_inout, td_const];
  end else
    Result := false;
end;

function TTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  vl  : TTypeDefSpecs;
  msk : TTypeDefSpecs;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (tt = tt_Ident) and (IsSpecifier(s, vl, msk)) then begin
    // search all specifiers
    while (tt = tt_Ident) and (IsSpecifier(s, vl, msk)) do begin
      if (_Spec * msk <> []) and (s <> 'long') then begin
        AParser.SetError( ErrExpectStr('Type identifier', s));
        Exit;
      end;
      _Spec := _Spec + vl;
      if (s <> 'const') and (s <> 'volatile') then begin
        if _Name = '' then _Name := s
        else _Name := _Name + ' ' + s;
      end;
      AParser.FindNextToken(s, tt);
    end; {of while}

    if ((_Spec * [td_Unsigned, td_Int, td_Short, td_Char, td_Long]) = [])  then begin
      // if int, short long or char is not specified
      // volatile or const are
      Result := tt = tt_Ident;
      if not Result then begin
        AParser.SetError(ErrExpectStr('Identifier', s));
        Exit;
      end;
      _Name := s;
      Result := true;
      //AParser.FindNextToken(s, tt);
    end else begin
      AParser.Index := AParser.TokenPos;
      Result := true;
    end;

  end else begin
    _Name := s;
    //AParser.FindNextToken(s, tt);
    Result := true;
  end;

  if (Result) {and (tt=tt_Symbol) and (s = '*') }then begin
//    AParser.Index := AParser.TokenPos;
    ParsePointerDef(AParser, _isPointer, _isPointerRef);
  end;

end;

{ TSkip }

function TSkip.DoParse(AParser: TTextParser): Boolean;
begin
  Result := true;
end;

{ TUnionTypeDef }

function TUnionTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  i   : Integer;
  st    : TStructField;
  prev  : TStructField;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'union' then begin
    AParser.SetError(ErrExpectStr('union', s));
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  i := AParser.TokenPos;
  if (tt = tt_Ident) then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    i := AParser.TokenPos;
  end;

  if not ((tt = tt_Symbol) and (s = '{')) then begin
    if (tt = tt_Symbol) and (s = '*')
      then _isPointer := true
      else AParser.Index := i;
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '}' then
    AParser.Index := AParser.TokenPos;
  prev := nil;
  while (s <> '}') do begin
    //i := AParser.TokenPos;
    st := TStructField.Create(Self);
    if not Assigned(prev) then begin
      if not st.Parse(AParser) then Exit;
    end else begin
      AParser.FindNextToken(st._Name, tt);
      if tt <> tt_Ident then begin
        AParser.SetError(ErrExpectStr('field name', st._Name));
        Exit;
      end;
      st._TypeName := prev._TypeName;
    end;

    Items.Add(st);
    AParser.FindNextToken(s, tt);
    if s = ','
      then prev := st
      else prev := nil;

    if s = ';' then begin
      AParser.FindNextToken(s, tt);
      if s <> '}' then AParser.Index := AParser.TokenPos;
    end else begin
      AParser.Index := AParser.TokenPos;
    end;
  end;

  Result := true;
  //no skipping last ';', because after structure a variable can be defined
  //ie: struct POINT {int x; int y} point;
end;

function isEmptyStruct(AStruct: TStructTypeDef): Boolean;
var
  i   : integer;
begin
  for i := 0 to AStruct.Items.Count - 1 do
    if TEntity(AStruct.Items[i]) is TStructField then begin
      Result := false;
      Exit;
    end;
  Result := true;
end;



{ TFunctionParamsList }

function TFunctionParamsList.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  ent : TEntity;
  i   : Integer;
begin
  Result := (AParser.FindNextToken(s, tt)) and (tt=tt_Symbol) and (s = '(');
  if not Result then begin
    AParser.SetError( ErrExpectStr('(', s));
    Exit;
  end;
  Result := AParser.FindNextToken(s, tt);

  if not Result then begin
    AParser.SetError( ErrExpectStr(')', s));
    Exit;
  end;

  i := AParser.TokenPos;
  if (tt = tt_Ident) and (s='void') then begin
    AParser.FindNextToken(s, tt);
    if not ((tt = tt_Symbol) and (s = ')')) then
      AParser.Index := i;
  end else
    AParser.Index := i;

  while (s <> ')') do begin
    ent := TFunctionParam.Create(Self);
    Result := ent.Parse(AParser);
    if not Result then begin
      ent.Free;
      Exit;
    end;
    Items.Add(ent);
    AParser.FindNextToken(s, tt);
    if (s <> ')') then begin
      if not ((tt=tt_Symbol) and (s = ',')) then
        AParser.Index := AParser.TokenPos;
    end;
  end;

  Result := true;
end;

function isAnyParam(AParser: TTextParser): Boolean;
var
  i   : integer;
  s   : AnsiString;
  tt  : TTokenType;
begin
  Result := false;
  i := AParser.Index;
  if AParser.FindNextToken(s, tt) and (s = '.') then
    if AParser.FindNextToken(s, tt) and (s = '.') then
      if AParser.FindNextToken(s, tt) and (s = '.') then
        Result := true;
  if not Result then AParser.Index := i;
end;

{ TFunctionParam }

function TFunctionParam.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  _IsAny := isAnyParam(AParser);
  if _IsAny then begin
    Result := true;
    Exit;
  end;

  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then begin
    AParser.SetError( ErrExpectStr('type identifier', '' ));
    Result := false;
    Exit;
  end;
  AParser.FindNextToken(s, tt);

  if tt <> tt_Ident then
    AParser.Index := AParser.TokenPos
  else
    _Name := s;
  Result:=true;
end;

{ TFunctionTypeDef }

function TFunctionTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  _ParamsList := TFunctionParamsList.Create(Self);
  Items.Add(_ParamsList);

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '(') then begin
    AParser.Index := AParser.TokenPos;
    Result := _ParamsList.Parse(AParser);
  end else if (tt = tt_Symbol) and (s = ';') then begin
    AParser.Index := AParser.TokenPos;
    Result := true;
  end else begin
    AParser.SetError(ErrExpectStr('(', s));
    Result := false;
  end;
end;

end.
