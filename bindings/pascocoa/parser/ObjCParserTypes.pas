{
 ObjCParserTypes.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 objc parsing unit
}

// todo: remove last ';' skipping. must be added lately


unit ObjCParserTypes;

interface

{$ifdef fpc}{$mode delphi}{$endif fpc}

uses
  Classes, SysUtils;

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

    Stack         : TList;

    constructor Create;
    destructor Destroy; override;
    
    procedure BeginParse(AObject: TObject);
    procedure EndParse;
    
    function SkipComments: Boolean;
    function FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;
  end;

  { TEntity }

  TEntity = class(TObject)
  protected
    procedure DoParse(AParser: TTextParser); virtual; abstract;
  public
    owner : TEntity;
    Items : TList;
    constructor Create(AOwner: TEntity);
    destructor Destroy; override;
    procedure Parse(AParser: TTextParser); virtual;
  end;
  
  { TComment }

  //C tokens: /*, //
  TComment = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
     _Comment : WideString;
  end;

  { TPrecompiler }

  //C token: #
  TPrecompiler = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Directive : AnsiString;
    _Params    : AnsiString;
  end;
  

  { TEnumValue }

  TEnumValue  = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Name     : AnsiString;
    _Value    : AnsiString;
  end;
  
  { TEnumTypeDef }
  
  //C token: enum
  TEnumTypeDef = class(TEntity)
  protected
    fValCount  : Integer;
    function GetValue(idx: integer): TEnumValue;
    procedure DoParse(AParser: TTextParser); override;
  public
    _Name     : AnsiString;
    property Value[idx: Integer]: TEnumValue read GetValue;
    property ValuesCount: Integer read fValCount;
  end;

  { TStructField }

  TStructField = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Name     : AnsiString;
    _BitSize  : Integer;
    _Type     : TEntity;
    _TypeName : AnsiString;
  end;

  { TStructTypeDef }

  //C token: struct
  TStructTypeDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Name     : AnsiString;
  end;
  
  { TTypeDef }
  //C token - any type, including unsigned short
  
  TTypeDefSpecs = set of (td_Unsigned, td_Signed, td_Volitale, td_Const, td_Long, td_Short);
  
  TTypeDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Name : AnsiString;
    _Spec : TTypeDefSpecs;
    _IsPointer  : Boolean;
  end;
  
  { TTypeNameDef }
  
  //C token: typdef
  TTypeNameDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Inherited  : AnsiString;
    _Type       : TEntity;
    _TypeName   : AnsiString;
  end;
  
  { TObjCParameterDef }

  TObjCResultTypeDef = class(TTypeDef)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _isRef    : Boolean;
    _isConst  : Boolean; // (const Sometype)
    _Prefix   : AnsiString; // reserved-word  type descriptors
  end;

  TObjCParameterDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Res  : TObjCResultTypeDef;
    _Name : AnsiString;
  end;

  { TParamDescr }

  TParamDescr = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
  public
    _Descr   : AnsiString;
  end;

  { TClassMethodDef }

  TClassMethodDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
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
    procedure DoParse(AParser: TTextParser); override;
  public
    _EntityName : AnsiString;
  end;

  { TClassDef }

  TClassDef = class(TEntity)
  protected
    procedure DoParse(AParser: TTextParser); override;
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
    procedure DoParse(AParser: TTextParser); override;
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
function ParseCExpression(AParser: TTextParser): AnsiString;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;

function ParseTypeDef(Owner: TEntity; AParser: TTextParser): TEntity;

implementation

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
begin
  Result := nil;
  res := AParser.FindNextToken(s, tt);
  if not Res or (tt <> tt_Ident) then Exit;

  s := AnsiLowerCase(s);
  if s = 'enum' then
    Result := TEnumTypeDef.Create(Owner)
  else if s = 'struct' then
    Result := TStructTypeDef.Create(Owner)
  else
    Result := TTypeDef.Create(Owner);

  AParser.Index := AParser.TokenPos;
  if Assigned(Result) then Result.Parse(AParser);
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
  ch := ['(',')', '{','}', ':', '-','+','<','>','*',';', ',']
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
  Stack := TList.Create;
end;

destructor TTextParser.Destroy;
begin
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
  try
    while (not Result) and (index <= length(Buf)) do begin
      ScanWhile(Buf, index, TokenTable.SpaceChars);
      if not (IsSubStr(TokenTable.Precompile, Buf, Index) and HandlePrecomiler) then begin // 1. check is Compiler directive is found
        if (Buf[index] in TokenTable.Symbols) then begin                 // 2. symbol has been found, so it's not an ident
          if (not (Buf[index] in blck)) or (not SkipComments) then begin //   2.1 check if comment is found (comment prefixes match to the symbols)
            Result := true;                                              //   2.2 check if symbol is found
            TokenType := tt_Symbol;
            Token := Buf[index];
            inc(index);
            Exit;
          end;
        end else if (Buf[index] in ['0'..'9']) then begin  // 3. a number is found, so it's possibl a number
          //todo: Hex and floats support!
          //todo: Negative numbers support;
          TokenType := tt_Numeric;
          Token := ScanWhile(Buf, index, ['0'..'9']);
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
    end; {of while}
  finally
    if not Result
      then TokenType := tt_None
      else TokenPos := Index - length(Token);
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

procedure TEntity.Parse(AParser: TTextParser);
begin
  AParser.BeginParse(Self);
  try
    DoParse(AParser);
  finally
    AParser.EndParse;
  end;
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

procedure TClassDef.DoParse(AParser:TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  cnt : Integer;
  mtd : TClassMethodDef;
begin
  AParser.FindNextToken(s, tt);
  if s <> '@interface' then begin
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
      Exit;
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
      exit;
    end;

    if s = '{' then inc(cnt)
    else if s = '}' then dec(cnt)
    else if (cnt = 0) then begin
      //todo: better parsing
      // parsing methods
      if s[1] ='#' then SkipLine(AParser.buf, AParser.Index);
      if (s = '+') or (s = '-') then begin
        dec(AParser.Index ); // roll back a single character
        mtd := TClassMethodDef.Create(Self);
        mtd.Parse(AParser);
        Items.Add(mtd);
      end;
    end;
  until (s = '@end') or (s = ''); // looking for declaration end
end;

{ TObjCHeader }

constructor TObjCHeader.Create;
begin
  //obj-c header does not have any entity owners
  inherited Create(nil);
end;

procedure TObjCHeader.DoParse(AParser:TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  ent : TEntity;
begin
  while AParser.FindNextToken(s, tt) do begin
    if s = 'typedef' then begin
      AParser.Index := AParser.TokenPos;
      ent := TTypeNameDef.Create(Self);
      ent.Parse(AParser);
    end else if s = 'enum' then begin
      AParser.Index := AParser.TokenPos;
      ent := TEnumTypeDef.Create(Self);
      ent.Parse(AParser);
      AParser.FindNextToken(s, tt); // skipping last ';'
    end else if s = '@interface' then begin
      AParser.Index := AParser.TokenPos;
      ent := TClassDef.Create(Self);
      ent.Parse(AParser);
    end else
      ent := nil;
    if Assigned(ent) then Items.Add(ent);
  end;
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



procedure TClassMethodDef.DoParse(AParser:TTextParser);
var
  s     : AnsiString;
  tt    : TTokenType;
  res   : TObjCResultTypeDef;
  para  : TObjCParameterDef;
  des   : TParamDescr;
begin
  AParser.FindNextToken(s, tt);
  if (s <> '+') and (s <> '-') then Exit;
  _CallChar := s[1];
  _IsClassMethod := _CallChar = '+';

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and(s = '(') then begin
    // _Class methods can be with out type
    AParser.Index:=AParser.TokenPos;
    res := TObjCResultTypeDef.Create(Self);
    res.Parse(AParser);
    Items.Add(res);
  end;
  AParser.FindNextToken(_Name, tt);

  while AParser.FindNextToken(s, tt) do begin
    if s = ';' then
      Exit
    else if s = ':' then begin
      para := TObjCParameterDef.Create(Self);
      para.Parse(AParser);
      Items.Add(para);
    end else if tt = tt_Ident then begin
      des := TParamDescr.Create(Self);
      des._Descr := s;
      Items.Add(des);
    end;

  end;
//  AParser.FindNextToken()
end;

{ TParameterDef }

procedure TObjCParameterDef.DoParse(AParser:TTextParser);
var
  tt  : TTokenType;
begin
  _Res := TObjCResultTypeDef.Create(Self);
  _Res.Parse(AParser);
  Items.Add(_Res);
  AParser.FindNextToken(_Name, tt);
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

procedure TObjCResultTypeDef.DoParse(AParser: TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  if (tt <> tt_Symbol) and (s <> '(') then Exit;
  inherited DoParse(AParser);
(*  _prefix := '';
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
  end;*)

  AParser.FindNextToken(s, tt);
  if s <> ')' then ; // an error

end;



{ TParamDescr }


procedure TParamDescr.doParse(AParser: TTextParser);
var
  tt  : TTokenType;
begin
  AParser.FindNextToken(_Descr, tt);
end;

{ TSubSection }

procedure TSubSection.DoParse(AParser: TTextParser);
begin
 //todo:
end;

{ TPrecompiler }

procedure TPrecompiler.DoParse(AParser: TTextParser);
var
  tt  : TTokenType;
  idx : Integer;
begin

  idx := AParser.Index;
  if not AParser.FindNextToken(_Directive, tt) then begin
    AParser.Index := idx;
    Exit;
  end;
  if (_Directive = '') or (_Directive[1] <> '#') then begin
    AParser.Index := idx;
    Exit;
  end;
  _Params := SkipLine(AParser.Buf, AParser.Index);
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

procedure TEnumTypeDef.DoParse(AParser: TTextParser);
var
  token : AnsiString;
  tt    : TTokenType;
  nm    : AnsiString;
  i     : Integer;
  vl    : TEnumValue;
begin
  if not AParser.FindNextToken(token, tt) then Exit;
  if token <> 'enum' then Exit;
  
  i := AParser.Index;
  if not AParser.FindNextToken(nm, tt) then Exit;
  if tt <> tt_Ident then AParser.Index := i
  else _Name := nm;
  
  AParser.FindNextToken(nm, tt);
  if nm <> '{' then Exit;
  repeat
    vl := TEnumValue.Create(Self);
    vl.Parse(AParser);
    if vl._Name <> '' then begin
      inc(fValCount);
      Items.Add(vl)
    end else begin
      vl.Free;
      Exit; // incorrect header! enumeration value cannot go without name!
    end;
      
    AParser.FindNextToken(nm, tt);
    if (nm <> ',') and (nm <> '}') then // if not , then  ; must be followed!
      Exit;
  until nm = '}';
  
  
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

function ParseCExpression(AParser: TTextParser): AnsiString;
var
  i   : integer;
  nm  : AnsiString;
  tt  : TTokenType;
begin
//  i := AParser.Index;
  Result := '';
  while AParser.FindNextToken(nm, tt) do begin
    if (tt = tt_Numeric) or (tt = tt_Ident) then begin
      Result := Result + nm;
      i := AParser.Index;
      if not ParseCOperator(AParser, nm) then begin
        AParser.Index := i;
        Exit;
      end else
        Result := Result + ' ' + nm + ' ';
    end else begin
      //i := AParser.Index;
      Exit;
    end;
  end;
end;

{ TEnumValue }

procedure TEnumValue.DoParse(AParser: TTextParser);
var
  i   : integer;
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(_Name, tt);
  if tt <> tt_Ident then Exit;

  i := AParser.Index;
  AParser.FindNextToken(s, tt);
  if s <> '=' then begin
    AParser.Index := i;
    _Value := '';
  end else
    _Value := ParseCExpression(AParser);
end;

{ TComment }

procedure TComment.DoParse(AParser: TTextParser);
begin
  //todo:! Comment parsing is now executed by TTextParser
end;

{ TTypeNameDef }

procedure TTypeNameDef.DoParse(AParser: TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  if s <> 'typedef' then Exit;
  _Type := ParseTypeDef(Self, AParser);
  AParser.FindNextToken(_TypeName, tt);
  _inherited := GetTypeNameFromEntity(_Type);
  AParser.FindNextToken(s, tt); // skip last ';';
end;


{ TStructTypeDef }

procedure TStructTypeDef.DoParse(AParser: TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  i   : Integer;
  st  : TStructField;
begin
  AParser.FindNextToken(s, tt);
  if s <> 'struct' then Exit;
  AParser.FindNextToken(s, tt);
  i := AParser.TokenPos;
  if (tt = tt_Ident) then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    AParser.Index := i;
  end;
  
  if (tt <> tt_Symbol) and (s <> '{') then begin
    AParser.Index := i;
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  while s <> '}' do begin
    //i := AParser.TokenPos;
    st := TStructField.Create(Self);
    st.Parse(AParser);
    Items.Add(st);
    AParser.FindNextToken(s, tt);
  end;
  
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

procedure TStructField.DoParse(AParser: TTextParser);
var
  tt  : TTokenType;
  s   : AnsiString;
begin
  _Type := ParseTypeDef(Self, AParser);
  if Assigned(_Type) then Exit;
  _TypeName := GetTypeNameFromEntity(_Type);

  if not (AParser.FindNextToken(_Name, tt)) or (tt <> tt_Ident) then Exit;
  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = ':') then begin
    AParser.FindNextToken(s, tt);
    CVal(s, _BitSize);
    AParser.FindNextToken(s, tt);
  end;
  //success: (tt = tt_Symbol) and (s = ';')
end;

{ TTypeDef }

function IsSpecifier(const s: AnsiSTring; var SpecVal, SpecMask: TTypeDefSpecs): Boolean;
begin
  Result := true;
  if (s = 'volitle') then begin
    SpecVal := [td_Volitale];
    SpecMask := [td_Volitale, td_Const];
  end else if (s = 'const') then begin
    SpecVal := [td_Volitale];
    SpecMask := [td_Volitale, td_Const];
  end else if (s = 'signed') then begin
    SpecVal := [td_Signed];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'unsigned') then begin
    SpecVal := [td_Unsigned];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'long') then begin
    SpecVal := [td_Long];
    SpecMask := [td_Long, td_Short];
  end else if (s = 'short') then begin
    SpecVal := [td_Short];
    SpecMask := [td_Long, td_Short];
  end else
    Result := false;
end;

procedure TTypeDef.DoParse(AParser: TTextParser);
var
  s   : AnsiString;
  tt  : TTokenType;
  vl  : TTypeDefSpecs;
  msk : TTypeDefSpecs;
begin
  AParser.FindNextToken(s, tt);
  while (tt = tt_Ident) and (IsSpecifier(s, vl, msk)) do begin
    if _Spec * msk <> [] then Exit;
    _Spec := _Spec + vl;
    AParser.FindNextToken(s, tt);
  end;

  if tt = tt_Ident then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    if (tt = tt_Symbol) and (s = '*') then begin
      _isPointer := true;
    end else begin
      AParser.Index := AParser.TokenPos;
    end;
  end else ; //error
end;

end.
