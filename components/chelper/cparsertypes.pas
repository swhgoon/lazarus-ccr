{ The unit is part of Lazarus Chelper package

  Copyright (C) 2010 Dmitry Boyarintsev skalogryz dot lists at gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit cparsertypes;

interface

{$ifdef fpc}{$mode delphi}{$h+}
{$else}
{$warn unsafe_code off}
{$warn unsafe_type off}
{$warn unsafe_cast off}
{$endif}

uses
  Classes, SysUtils, TextParsingUtils;

const
  Err_Ident   = 'Identifier';
  Err_Expect  = '%s, excepted, but "%s" found';
  Err_BadPrecompile   = 'Bad precompile directive';

type
  TTokenType = (tt_Ident, tt_Symbol, tt_Numeric, tt_String);

  TTokenPair = record
    Open       : AnsiString;
    Close      : AnsiString;
  end;

  { TTokenTable }

  TTokenTable = class(TObject)
  private
    fSymbMaxLen : Integer;
    fSymbStrs   : TStringList;
  public
    SpaceChars  : TCharSet;
    CmtBlock    : array of TTokenPair;
    CmtCount    : Integer;
    CmtLine     : TStrings;
    StringStart : TCharSet;
    Symbols     : TCharSet;
    Precompile  : AnsiString;
    MultiLine   : AnsiChar; 
    constructor Create;
    destructor Destroy; override;
    function AddSymbol(const asym: AnsiString): Boolean;
    function isSymbol(const asym: AnsiSTring): Boolean;
    property SymbMaxLen : Integer read fSymbMaxLen;
  end;

  TTextParser = class;

  TPrecompilerEvent = procedure (Sender: TTextParser; PrecompEntity: TObject) of object;

  TCMacroStruct = class(TObject)
    MacroName   : AnsiString;
    MacroParams : TStringList;
    ReplaceText : AnsiString;

    constructor Create;
    destructor Destroy; override;
  end;

  { TCMacroHandler }

  TCMacroHandler = class(TObject)
  public
    MacrosNames : TStringList;
    constructor Create;
    destructor Destroy; override;
    function ParseMacro(const Parser: TTextParser; var MacroStr, ReplaceStr: AnsiString): Boolean;
    function isMacroDefined(const Macro: AnsisTring): Boolean;

    procedure AddSimpleMacro(const MacroStr, ReplaceStr: AnsiString);
    procedure AddParamMacro(const MacroStr, ReplaceStr: AnsiString; Params: TStrings);

    procedure Clear;
  end;

  { TTextParser }

  TTextParser = class(TObject)
  protected
    ProcessingMacro : Boolean;
    function HandlePrecomiler: Boolean; virtual;
    function HandleMacro(var MacroStr: AnsiString; var ReplaceStr: AnsiString): Boolean;

    function IsMultiLine: Boolean;
    procedure SkipSingleEoLnChars;

    function AddChildToStackEntity(ent: TObject): Boolean;
  public
    Buf           : AnsiString;

    Token         : AnsiString;
    TokenType     : TTokenType;
    TokenCode     : Integer;      // code for reserved tokens and symbols, otherwiser -1. 0 is EOF


    Index         : Integer;      // current index where text parsing goes on
    TokenPos      : Integer;      // position of currently found token by (FindTextToken)
    MacrosDelta   : Integer;      // the difference between Buf Index and Original Text index, caused by Macros substitution
    TokenTable    : TTokenTable;
    OnPrecompile  : TPrecompilerEvent;
    OnComment     : procedure (Sender: TObject; const Comment: AnsiString) of object;
    OnIgnoreToken : procedure (Sender: TObject; const Ignored: AnsiString) of object;
    Line          : Integer;

    Stack         : TList;
    Errors        : TStringList;
    MacroHandler  : TCMacroHandler;

    UseCommentEntities     : Boolean;
    UsePrecompileEntities  : Boolean;

    Comments      : TList;

    constructor Create;
    destructor Destroy; override;

    procedure BeginParse(AObject: TObject);
    procedure EndParse;

    function GetBufWideStr(const Cmd: AnsiString): WideString;

    function SkipComments: Boolean;

    function NextToken: Boolean;
    function FindNextToken(var AToken: AnsiString; var ATokenType: TTokenType): Boolean;

    procedure SetError(const ErrorCmt: AnsiString);
  end;

  { TEntity }

  TEntity = class(TObject)
  protected
    function DoParse(AParser: TTextParser): Boolean; virtual;

  public
    Offset      : Integer;
    Items       : TList;

    TagComment  : AnsiString;
    Specifiers  : TStringList;
    
    constructor Create(AOffset: Integer=-1); virtual;
    destructor Destroy; override;
    function Parse(AParser: TTextParser): Boolean; virtual;
    procedure Assign(AEntity: TEntity); virtual;
  end;
  TEntityClass = class of TEntity;

  TCPrepocessor = class(TEntity);

  { TCPrepDefine }

  TCPrepDefine = class(TCPrepocessor)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    Params    : TStringList;
    _Name     : AnsiString;
    SubsText  : AnsiString;
    destructor Destroy; override;
  end;

  TCPrepInclude = class(TCPrepocessor)
  protected
    Params    : TStringList;
    Included  : AnsiString;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepElse = class(TCPrepocessor)
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepEndif = class(TCPrepocessor)
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepIf = class(TCPrepocessor)
    _Cond   : AnsiString;
    IfOp    : AnsiString;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepPragma = class(TCPrepocessor)
    _Text : AnsiString;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  //C tokens: /*, //
  TCommentType = (ctLine, ctBlock);

  { TComment }

  TComment = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
     CommenType : TCommentType;
     _Comment   : AnsiString; // in case sources are UTF8 or Unicode
  end;

  { TPrecompiler }

  TPrecompiler = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Directive : AnsiString;
    _Params    : AnsiString;
  end;

// parsing function
function ParseNextEntity(AParser: TTextParser): TEntity;
function ParseCExpression(AParser: TTextParser; var ExpS: AnsiString): Boolean;
procedure ParseCNumeric(const S: AnsiString; var idx: integer; var NumStr: AnsiSTring);
function ParseCString(const S: AnsiString; var idx: Integer; var CStr: AnsiString): Boolean;
function ParseCMacroParam(AParser: TTextParser; var ExpS: AnsiString): Boolean;

// utility function
function SkipEndOfLineChars(const Src: AnsiString; idx: integer): Integer;
function CToPascalNumeric(const Cnum: AnsiString): AnsiString;
function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;

// Parser data management functions
function CreateObjCTokenTable: TTokenTable;
procedure SetCComments(Table: TTokenTable);
procedure SetCSymbols(var ch: TCharSet);

function CreateCParser(const CHeaderText: AnsiString;
  WithCMacroHandler: Boolean = false): TTextParser;


type
  TCustomEntityProc = function (Parent: TEntity; Parser: TTextParser): TEntity;

type

  { TSimpleType }

  TSimpleType = class(TEntity)
  public
    Name : AnsiString;
  end;

  { TExpression }

  TExpPart = record
    Token     : AnsiString;
    TokenType : TTokenType;
  end;
  TExpression = class(TEntity)
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    Tokens  : array of TExpPart;
    Count   : Integer;
    procedure PushToken(const AToken: AnsiString; ATokenType: TTokenType);
  end;

procedure ErrorExpect(Parser: TTextParser; const Expect: AnsiString);
function ConsumeToken(Parser: TTextParser; const Token: AnsiString): Boolean;

function ParseCType(Parser: TTextParser): TEntity;

type
  TNamePart = class;

  TFuncParam = record
    prmtype : TEntity;
    name    : TNamePart;
  end;

  TNameKind = (nk_Ident, nk_Ref, nk_Array, nk_Func);

  { TNamePart }

  TNamePart = class(TObject)
  private
    fChild  : TNamePart;
    fOwner  : TNamePart;
  public
    Kind      : TNameKind;
    RefCount  : Integer;
    Id        : AnsiString;
    arrayexp  : array of TExpression;
    params    : array of TFuncParam;
    constructor Create(AKind: TNameKind);
    procedure AddParam(prmtype: TEntity; prmname: TNamePart);
    procedure AddArrayExpr(expr: TExpression);
    property child: TNamePart read fchild write fChild;    // int (*p)[10]; "[10]" is child of (*p)
    property owner: TNamePart read fowner write fOwner;
  end;

function ParseNames(Parser: TTextParser; var NameType: TEntity; Names: TList; AllowMultipleNames: Boolean=True): Boolean;
function ParseName(Parser: TTextParser; var NameType: TEntity; var name: TNamePart): Boolean;

type

  { TVarFuncEntity }

  TVarFuncEntity = class(TEntity)
  protected
    function DoParse(AParser:TTextParser): Boolean; override;
  public
    RetType     : TEntity;
    Names       : TList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
    function FirstName: TNamePart;
  end;


  TStructTypeField = record
    v         : TVarFuncEntity;
    isbitted  : Integer;
    bits      : TExpression;
  end;

  { TStructType }

  TStructType = class(TEntity)
  public
    Name    : AnsiString;
    fields  : array oF TStructTypeField;
    function AddField(ev: TVarFuncEntity): Integer;
  end;

  { TUnionType }

  TUnionType = class(TEntity)
  public
    Name    : AnsiString;
    fields  : array oF TStructTypeField;
    function AddField(ev: TVarFuncEntity): Integer;
  end;

  { TTypeDefInst }

  TTypeDef = class(TEntity)
  public
    origintype  : TEntity;
    names       : TList;
    constructor Create(AOffset: Integer=-1);
    destructor Destroy; override;
  end;


  TEnumItem = record
    Name    : AnsiString;
    Value   : TExpression;
    Offset  : Integer;
  end;

  { TEnumType }

  TEnumType = class(TEntity)
    Name    : AnsiString;
    items   : array of TEnumItem;
    function AddItem(const name: AnsiString; x: TExpression; Offset: Integer = -1): Integer;
  end;

function ParseStruct(AParser: TTextParser): TStructType;
function ParseUnion(AParser: TTextParser): TUnionType;
function ParseTypeDef(AParser: TTextParser): TTypeDef;
function ParseEnum(AParser: TTextParser): TEnumType;

implementation

function SkipEndOfLineChars(const Src: AnsiString; idx: integer): Integer;
begin
  if idx < length(Src) then begin
    if (Src[idx] = #10) and (Src[idx+1]=#13) then inc(idx)
    else if (Src[idx] = #13) and (Src[idx+1]=#10) then inc(idx);
  end;
  Result := idx+1;
end;

function CreateCParser(const CHeaderText: AnsiString; WithCMacroHandler: Boolean): TTextParser;
begin
  Result := TTextParser.Create;
  Result.TokenTable := CreateObjCTokenTable;
  if WithCMacroHandler then
    Result.MacroHandler := TCMacroHandler.Create;
  Result.Buf := CHeaderText;
end;

function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;
begin
  Result := Format(Err_Expect, [Expected, Found]);
end;

(* ANSI C reserved words
auto  break case char const continue default do double else enum
extern float for goto if int  long register  return short signed
sizeof static struct switch typedef union  unsigned void volatile while
*)

function CreateObjCTokenTable: TTokenTable;
begin
  Result := TTokenTable.Create;
  SetCComments(Result);
  SetCSymbols(Result.Symbols);

  Result.AddSymbol('!=');
  Result.AddSymbol('==');
  Result.AddSymbol('+=');
  Result.AddSymbol('-=');
  Result.AddSymbol('*=');
  Result.AddSymbol('/=');
  Result.AddSymbol('%=');
  Result.AddSymbol('|=');
  Result.AddSymbol('&=');
  Result.AddSymbol('<<');
  Result.AddSymbol('>>');
  Result.AddSymbol('++');
  Result.AddSymbol('--');
  Result.AddSymbol('||');
  Result.AddSymbol('&&');

  Result.SpaceChars := EoLnChars + InvsChars;
  Result.Precompile := '#';
  Result.MultiLine := '\';
  Result.StringStart := ['"', #39];
end;

procedure SetCSymbols(var ch: TCharSet);
begin
  ch := ['!','~','^','(',')','{','}','%','/',':','=','-','+','<','>','*',';', ',','|','&','[',']'{, #39 ,'"'} ]
end;

procedure SetCComments(Table: TTokenTable);
begin
  SetLength(Table.CmtBlock, 1);
  Table.CmtCount := 1;
  Table.CmtBlock[0].Open := '/*';
  Table.CmtBlock[0].Close := '*/';
  Table.CmtLine.Add('//');
end;

function isFloatNum(const num: AnsiString): Boolean;
begin
  Result := Pos('.', num)>0;
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

function ParseCString(const S: AnsiString; var idx: Integer; var CStr: AnsiString): Boolean;
var
  quit  : Boolean;
  i     : Integer;
  ch    : AnsiChar;
begin
  Result := false;
  CStr := '';
  if not (S[idx] in ['"', #39]) then Exit;

  quit := false;
  i := idx+1;
  ch := S[idx];

  while (not quit) and (i <= length(s)) do begin
    ScanTo(s, i, [ch, #10, #13] );
    quit := (i > length(s)) or (s[i] in [ch, #10, #13]);
    if quit and (i <= length(s)) and ((s[i] ='"')) then
      if ((s[i] = ch) and (s[i-1] = '\')) then begin
        inc(i);
        quit := false;
      end;
  end;

  Result := (i <= length(s)) and (s[i] = ch);
  if Result then begin
    inc(i);
    CStr := Copy(s, idx, i-idx);
    idx := i;
  end;
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

function ParseCMacroParam(AParser: TTextParser; var ExpS: AnsiString): Boolean;
var
  brac  : Integer;
  idx   : Integer;
begin
  idx := AParser.Index;
  brac:=0;

  while AParser.NextToken do begin
    if AParser.Token='(' then inc(brac)
    else if (AParser.Token=')') then begin
      if brac>0 then dec(brac)
      else begin
        AParser.Index:=aParser.TokenPos;
        Break;
      end;
    end else if (AParser.Token=',') and (brac=0) then begin
      AParser.Index:=AParser.TokenPos;
      Break;
    end;
  end;
  ExpS:=Copy(APArser.Buf, idx, AParser.Index-idx);
  Result:=True;
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
        ExpS := #39 + ScanTo(AParser.Buf, AParser.Index, [#39]) + #39;
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
        else if (nm = ')') then begin
          if brac=0 then dec(brac)
          else begin
            AParser.Index:=AParser.TokenPos;
            Break;
          end;
        end;
      end else begin
        //i := AParser.Index;
        Exit;
      end;
    end;
    Result := true;

  finally
    while (brac > 0) and (AParser.FindNextToken(nm, tt)) do
      if nm = ')' then
        dec(brac);
  end;
end;

{ TTextParser }

constructor TTextParser.Create;
begin
  Index := 1;
  Line := 1;
  Stack := TList.Create;
  Errors := TStringList.Create;
  //IgnoreTokens := TStringList.Create;
  UsePrecompileEntities := true;
  Comments := TList.Create;
end;

destructor TTextParser.Destroy;
begin
  Comments.Free;
  //IgnoreTokens.Free;
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
  s   : AnsiString;
  df  : TCPrepocessor;
  i   : integer;
begin
  Result := false;
  if ProcessingMacro then Exit;

  ProcessingMacro := true;
  try
    idx := Index;
    i := idx+1;
    ScanWhile(Buf, i, WhiteSpaceChars);
    s := ScanTo(Buf, i, WhiteSpaceChars);
    if s = 'define' then df := TCPrepDefine.Create(idx)
    else if s = 'include' then df := TCPrepInclude.Create(idx)
    else if s = 'else' then df := TCPrepInclude.Create(idx)
    else if s = 'endif' then df := TCPrepEndif.Create(idx)
    else if s = 'pragma' then df := TCPrepPragma.Create(idx)
    else if (s = 'if') or (s = 'elif') or (s = 'ifdef') or (s = 'ifndef') then begin
      df := TCPrepIf.Create(idx);
      TCPrepIf(df).IfOp:=s;
    end else
      df := nil;

    Result := Assigned(df);
    if Result then begin
      Index:=i;
      Result := df.Parse(Self);
      if UsePrecompileEntities then AddChildToStackEntity(df);
      if Assigned(OnPrecompile) then
        OnPrecompile(Self, df);
    end;

    if not Result then begin
      SetError('cannot handle preprocessor');
      Exit;
    end;
  finally
    ProcessingMacro := false;
  end;
end;

function TTextParser.FindNextToken(var AToken: AnsiString; var ATokenType: TTokenType): Boolean;
begin
  Result:=NextToken;
  AToken:=Token;
  ATokenType:=TokenType;
end;

function TTextParser.SkipComments: Boolean;
var
  i         : Integer;
  idx       : Integer;
  cmt       : AnsiString;
  comment   : TComment;
  ct        : TCommentType;
begin
  cmt := '';
  Result := false;

  for i := 0 to TokenTable.CmtCount - 1 do begin
    Result:=IsSubStr(TokenTable.CmtBlock[i].Open, Buf, index);
    if Result then begin
      idx:=index;
      inc(index, length(TokenTable.CmtBlock[i].Open));
      cmt := SkipCommentBlock(Buf, index, TokenTable.CmtBlock[i].Close);
      ct:=ctBlock;
      Break;
    end;
  end;

  if not Result then
    for i := 0 to TokenTable.CmtLine.Count - 1 do begin
      Result:=IsSubStr(TokenTable.CmtLine[i], Buf, index);
      if Result then begin
        idx:=index;
        cmt := SkipLine(Buf, index);
        Delete(cmt, 1, length(TokenTable.CmtLine[i]) );
        ct:=ctLine;
        Break;
      end;
    end;

  if Result then begin
    if UseCommentEntities then begin
      comment := TComment.Create(idx);
      comment._Comment := cmt;
      comment.CommenType:=ct;
      Comments.Add(Comment);
    end;
    if (Assigned(OnComment)) and (cmt <> '') then OnComment(Self, cmt);
  end;
end;


function TTextParser.NextToken:Boolean;
var
  srch      : TCharSet;
  blck      : TCharSet;
  i, j      : Integer;
  t         : AnsiString;
  spaces    : TCharSet;
  Repl      : AnsiString;
  p         : Integer;
begin
  Result := Index <= length(Buf);
  if not Result then begin
    Token:='';
    Exit;
  end;

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
      if isMultiline then begin
        ScanTo(Buf, index, EoLnChars);
        SkipSingleEoLnChars;

      end else begin
        if (IsSubStr(TokenTable.Precompile, Buf, Index) and HandlePrecomiler) then
           // 1. check is Preprocessor directive is found
        else if (Buf[index] in TokenTable.Symbols) then begin                 // 2. symbol has been found, so it's not an ident
          if (not (Buf[index] in blck)) or (not SkipComments) then begin //   2.1 check if comment is found (comment prefixes match to the symbols)
            Result := true;                                              //   2.2 check if symbol is found
            if (Buf[index] = '.') and (index < length(Buf)) and (Buf[index+1] in ['0'..'9']) then begin
              // is float number
              inc(index);
              Token := '.' + ScanWhile(Buf, index, ['0'..'9']);
              TokenType := tt_Numeric;
            end else begin
              j:=index;

              //todo: improve!
              while (j-index<=TokenTable.SymbMaxLen) and (Buf[j] in (TokenTable.Symbols)) do inc(j);

              if TokenTable.isSymbol( Copy( buf, index, j-index) ) then begin
                Token:=Copy( buf, index, j-index);
                index:=j;
              end else begin
                Token := Buf[index];
                inc(index);
              end;
              TokenType := tt_Symbol;
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
        end else if (Buf[index] in TokenTable.StringStart) then begin
          ParseCString(Buf, index, Token);
          TokenType := tt_String;
          Result := true;
          Exit;
        end else begin
          Token := Token + ScanTo(Buf, index, srch+TokenTable.Symbols+[TokenTable.MultiLine]); // scanning for token
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

      if (Token <> '') and (TokenType = tt_Ident) and Result then begin
        p := Index - length(Token);
        TokenPos:=p;
        if HandleMacro(Token, Repl) then begin
          inc(MacrosDelta, length(Token)-length(Repl));
          Delete(buf, p, length(Token));
          Insert(Repl, Buf, p);
          Index := p;
          Result := false;
          TokenType := tt_Ident;
          Token := '';
        end else
          TokenPos:=p;
      end;

    end; {of while}
  finally
    if not Result
      then TokenType := tt_Ident
      else TokenPos := Index - length(Token);
    //todo: make an event or something
    if TokenType = tt_Numeric then
      Token := CToPascalNumeric(Token);
  end;
end;

procedure TTextParser.SetError(const ErrorCmt: AnsiString);
begin
  Errors.Add(ErrorCmt);
end;

function TTextParser.HandleMacro(var MacroStr: AnsiString; var ReplaceStr: AnsiString): Boolean;
begin
  Result := false;
  if ProcessingMacro or not Assigned(MacroHandler) then Exit;

  ProcessingMacro := true;
  try
    Result := MacroHandler.isMacroDefined(MacroStr);
    if not Result then Exit;

    Index := TokenPos;
    Result := MacroHandler.ParseMacro(Self, MacroStr, ReplaceStr);
  finally
    ProcessingMacro := false;
  end;
end;

function TTextParser.GetBufWideStr(const Cmd: AnsiString): WideString;
begin
  Result := Cmd;
end;

function TTextParser.AddChildToStackEntity(ent: TObject): Boolean;
var
  parent : TEntity;
begin
  Result := Assigned(stack) and (stack.Count>0);
  if not Result then Exit;

  parent := stack[stack.Count-1];
  if Assigned(parent) and (parent is TEntity) then
    (parent as TEntity).Items.Add(ent);
end;

function TTextParser.IsMultiLine: Boolean;
begin
  Result := TokenTable.MultiLine <> #0;
  if not Result then Exit;
  Result := (Buf[index] = TokenTable.MultiLine);
end;

procedure TTextParser.SkipSingleEoLnChars;
var
  next  : integer;
begin
  next := index + 1;
  if next > length(Buf) then next := -1;

  if next < 0 then
    inc(index)
  else begin
    if (Buf[index] = #10) and (Buf[next] = #13) then
      Index := next+1
    else if (Buf[index] = #13) and (Buf[next] = #10) then
      Index := next + 1
    else
      inc(Index);
  end;
end;

{ TTokenTable }

constructor TTokenTable.Create;
begin
  CmtLine:=TStringList.Create;
  fSymbStrs:=TStringList.Create;
end;

destructor TTokenTable.Destroy;
begin
  fSymbStrs.Free;
  CmtLine.Free;
  inherited;
end;

function TTokenTable.AddSymbol(const asym:AnsiString):Boolean;
begin
  Result:=False;
  if asym='' then Exit;
  fSymbStrs.Add(asym);
  if length(asym)>fSymbMaxLen then fSymbMaxLen:=length(asym);
end;

function TTokenTable.isSymbol(const asym:AnsiSTring):Boolean;
begin
  if asym='' then
    Result:=false
  else begin
    if length(asym)=1 then
      Result:=(asym[1] in Symbols) or (fSymbStrs.IndexOf(asym)>=0)
    else
      Result:=fSymbStrs.IndexOf(asym)>=0;
  end;
end;

{ TEntity }

procedure TEntity.Assign(AEntity: TEntity);
begin
  TagComment := AEntity.TagComment;
end;

function TEntity.DoParse(AParser:TTextParser):Boolean;
begin
  Result:=False;
end;

constructor TEntity.Create(AOffset: Integer);
begin
  inherited Create;
  Offset := AOffset;
  Items := TList.Create;
  Specifiers := TStringList.create;
end;

destructor TEntity.Destroy;
begin
  Specifiers.Free;
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


{ TPrecompiler }

function TPrecompiler.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  if not AParser.FindNextToken(_Directive, tt) then begin
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

{ TComment }

function TComment.DoParse(AParser: TTextParser): Boolean;
begin
  Result := true;
end;

function RemoveMacroSlash(const macro: AnsiString): AnsiString;
var
  i : integer;
begin
  for i := length(macro) downto 1 do
    if not (macro[i] in WhiteSpaceChars) then begin
      if macro[i] = '\' then Result := Copy(macro, 1, i-1);
      Exit;
    end;
  Result := macro;
end;


function ConsumePreproc(AParser: TTextParser; const preprocname: AnsiString): Boolean;
begin
  AParser.NextToken;
  Result:=AParser.Token='#'+preprocname;
  if Result then Exit
  else begin
    if AParser.Token<>'#' then Exit;
    AParser.NextToken;
    Result:=AParser.Token=preprocname;
  end;
end;

{ TCPrepDefine }

function TCPrepDefine.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
  prs : AnsiString;

  SpaceChars : TCharSet;
  SymChars   : TCharSet;
begin
  AParser.FindNextToken(_name, tt);
  Result := tt = tt_Ident;
  if not Result then Exit;


  if (AParser.Index<=length(AParser.Buf)) and (AParser.Buf[AParser.Index]='(') then begin
    AParser.NextToken; // skipping "("
    AParser.NextToken; // the first ident
    Params:=TStringList.Create;
    while AParser.Token<>')' do begin
      if AParser.TokenType=tt_Ident then begin
        Params.Add(AParser.Token);
        AParser.NextToken;
      end;
      if AParser.Token=',' then AParser.NextToken;
    end;
  end;

  SpaceChars := AParser.TokenTable.SpaceChars;
  SymChars := AParser.TokenTable.Symbols;
  with AParser.TokenTable do SpaceChars := SpaceChars - [#10,#13];
  with AParser.TokenTable do Symbols := [#10, #13];

  try
    AParser.FindNextToken(prs, tt);
    while (prs <> '') and (not (prs[1] in [#10, #13])) do begin
      SubsText := SubsText + ' ' + prs;
      AParser.FindNextToken(prs, tt);
    end;
    RemoveMacroSlash(SubsText);
    if prs <> '' then
      AParser.Index := AParser.TokenPos;
  finally
    AParser.TokenTable.SpaceChars := SpaceChars;
    AParser.TokenTable.Symbols := SymChars;
  end;
end;

destructor TCPrepDefine.Destroy;
begin
  Params.Free;
  inherited Destroy;
end;

{ TCPrepInclude }

function TCPrepInclude.DoParse(AParser: TTextParser): Boolean;
var
  s     : AnsiString;
  tt    : TTokenType;
  exp   : AnsiString;
  chars : TCharSet;
begin
  chars := AParser.TokenTable.Symbols;
  try
    AParser.TokenTable.Symbols := AParser.TokenTable.Symbols + ['"'];

    //i := AParser.TokenPos;

    AParser.FindNextToken(s, tt);
    Result := (s = '"') or (s = '<');
    if not Result then Exit;

    if s = '"' then exp := '"'
    else if s = '<' then exp := '>';

    repeat
      AParser.FindNextToken(s, tt);
      if (s = '/') or (s = '\') or (tt = tt_Ident) then
        Included := Included + s;
    until (tt =tt_Symbol) and ((s <> '\') or (s <> '/'));

    Result := s = exp;
    SkipLine(AParser.buf, AParser.Index);
  finally
    AParser.TokenTable.Symbols := chars ;
  end;
end;

{ TCPrepElse }

function TCPrepElse.DoParse(AParser: TTextParser): Boolean;
begin
  SkipLine(AParser.buf, AParser.Index);
  Result:=True;
end;

{ TCPrepEndif }

function TCPrepEndif.DoParse(AParser: TTextParser): Boolean;
begin
  SkipLine(AParser.buf, AParser.Index);
  Result:=True;
end;

{ TCPrepIf }

function TCPrepIf.DoParse(AParser: TTextParser): Boolean;
begin
  _Cond := SkipLine(AParser.buf, AParser.Index);
  Result:=True;
end;

{ TCPrepPragma }

function TCPrepPragma.DoParse(AParser: TTextParser): Boolean;
begin
  _Text := SkipLine(AParser.buf, AParser.Index);
  Result:=True;
end;

{ TCMacroHandler }

procedure TCMacroHandler.AddSimpleMacro(const MacroStr,
  ReplaceStr: AnsiString);
begin
  AddPAramMacro(MacroStr, ReplaceStr, nil);
end;

procedure TCMacroHandler.AddParamMacro(const MacroStr,ReplaceStr:AnsiString;
  Params:TStrings);
var
  cm  : TCMacroStruct;
  i   : Integer;
begin
  cm := TCMacroStruct.Create;
  cm.MacroName := MacroStr;
  cm.ReplaceText := ReplaceStr;
  if Assigned(Params) then cm.MacroParams.Assign(Params);

  i := MacrosNames.IndexOf(MacroStr);
  if i >= 0 then begin
    MacrosNames.Objects[i].Free;
    MacrosNames.Delete(i);
  end;
  MacrosNames.AddObject(MacroStr, cm);
end;

procedure TCMacroHandler.Clear;
var
  i : Integer;
begin
  for i := 0 to MacrosNames.Count - 1 do MacrosNames.Objects[i].Free;
  MacrosNames.Clear;
end;

constructor TCMacroHandler.Create;
begin
  MacrosNames := TStringList.Create;
end;

destructor TCMacroHandler.Destroy;
begin
  Clear;
  MacrosNames.Free;
  inherited;
end;

function TCMacroHandler.isMacroDefined(const Macro: AnsisTring): Boolean;
begin
  Result := MacrosNames.IndexOf(Macro) >= 0;
end;

function MakeMacroText(const ParamNames, RepValues: TStrings; const SourceText: AnsiString): AnsiString;
var
  p : TTextParser;
  i : Integer;
begin
  if SourceText='' then Result:='';

  p:=CreateCParser(SourceText, False);
  Result:='';
  try
    i:=1;
    while p.NextToken do begin
      if (p.TokenType=tt_Ident) and (ParamNames.IndexOf(p.Token)>=0) then begin
        Result:=Result+Copy(p.Buf, i, p.TokenPos-i)+' ' + RepValues.Values[p.Token]+' ';
        i:=p.Index;
      end;
    end;
    if i<length(p.Buf) then
      Result:=Result+Copy(p.Buf, i, p.TokenPos-i);
  finally
    p.Free;
  end;

end;

function TCMacroHandler.ParseMacro(const Parser: TTextParser; var MacroStr,
  ReplaceStr: AnsiString): Boolean;
var
  s, x  : AnsiString;
  name  : AnsiString;
  tt    : TTokenType;
  i     : Integer;
  idx   : Integer;
  //j   : Integer;
  cm    : TCMacroStruct;
  RVal  : TStringList;
begin
  Parser.FindNextToken(s, tt);
  i := MacrosNames.IndexOf(s);
  Result := (i >= 0);
  if not Result then begin
    Parser.Index := Parser.TokenPos;
    Exit;
  end;
  name:=s;
  idx:=Parser.TokenPos;

  cm := TCMacroStruct(MacrosNames.Objects[i]);

  if Assigned(cm.MacroParams) and (cm.MacroParams.Count > 0) then begin
    //j := Parser.TokenPos;
    Parser.NextToken;
    Result:=Parser.Token='(';

    if not Result then begin
      Result := False;
      Parser.SetError('error while parsing macros usage');
      Exit;
    end;

    RVal := TStringList.Create;
    try
      i := 0;
      while Parser.Token<>')' do begin
        ParseCMacroParam(Parser, x);

        Result:=i<cm.MacroParams.Count;
        if not Result then begin
          Parser.SetError('too many params for the Macro: '+ name);
          Exit;
        end;
        RVal.Values [ cm.MacroParams[i]]:=x;

        Parser.NextToken;
        if Parser.Token=',' then Parser.NextToken;
        inc(i);
      end;

      if i<cm.MacroParams.Count then begin
        Parser.SetError('not enough params for the Macro: '+ name);
        Exit;
      end;

      MacroStr:=Copy(Parser.Buf, idx, Parser.Index-idx);

      ReplaceStr:=MakeMacroText(cm.MacroParams, RVal, cm.ReplaceText);
    finally
      RVal.Free;
    end;

  end else begin
    MacroStr := cm.MacroName;
    ReplaceStr := cm.ReplaceText;
  end;
end;

{ TCMacroStruct }

constructor TCMacroStruct.Create;
begin
  MacroParams := TStringList.Create;
end;

destructor TCMacroStruct.Destroy;
begin
  MacroParams.Free;
  inherited;
end;


{ TObjCClassProperty }

function ParseGetterSetterName(AParser: TTextParser): AnsiString;
var
  tt: TTokenType;
  s : string;
begin
  Result := '';
  AParser.FindNextToken(s, tt);
  if (tt <> tt_Symbol) and (s <> '=') then Exit;
  AParser.FindNextToken(Result, tt);
end;


function isSomeSpecifier(const s: AnsiString): Boolean;
begin
  Result:=length(s)>0;
  if Result then
    case s[1] of
      'a': Result:=s='auto';
      'c': Result:=s='const';
      'e': Result:=s='extern';
      'r': Result:=s='register';
      's': Result:=s='static';
      'i': Result:=s='inline';
      'o': Result:=s='overload';
      'v': Result:=(s='volitile') or (s='virtual');
    else
      Result:=False;
    end;
end;

function isCallConv(const s: AnsiString): AnsiString;
var
  i : Integer;
  c : AnsiString;
begin
  Result:='';
  if s='' then Exit;

  c:=s;
  for i:=1 to length(c) do
    if c[i]<>'_' then begin
      if i>1 then c:=Copy(c, i, length(c));
      Break;
    end;

  case c[1] of
    'c': if (c='cdecl') or (c='clrcall') then Result:=c;
    'f': if c='fastcall' then Result:=c;
    's': if c='stdcall' then Result:=c;
    't': if c='thiscall' then Result:=c;
    'p': if c='pascal' then Result:=c;
    'r': if c='register' then Result:=c;
  end;
end;


procedure ParseSepcifiers(AParser: TTextParser; st: TStrings);
begin
  while isSomeSpecifier(AParser.Token) do begin
    st.Add(AParser.Token);
    AParser.NextToken;
  end;
end;


function ParseNextEntity(AParser: TTextParser): TEntity;
var
  s   : AnsiString;
  tt  : TTokenType;

  tp  : TEntity;
  nm  : TNamePart;
  v   : TVarFuncEntity;
begin
  Result := nil;
  if not AParser.FindNextToken(s, tt) then Exit;

  if s = 'typedef' then begin
    Result:=ParseTypeDef(AParser);
  end else begin
    v:=TVarFuncEntity.Create(AParser.TokenPos);
    ParseNames(AParser, tp, v.Names);

    // declarations like:
    // fn (int i);
    // are parsed wrongly, because name of the function "fn" is consumed by typedef
    // while it's named of the function, and the returning type is unspecified.
    // the name of function must be added to the name operations tree, and type should be set to nil
    nm:=v.FirstName;
    if Assigned(tp) and (tp is TSimpleType) and Assigned(nm) and (nm.Kind=nk_Func) and not Assigned(nm.child) then begin
      nm.child:=TNamePart.Create(nk_Ident);
      nm.child.Id:=TSimpleType(tp).Name; // making an untyped function
      tp.Free;
      tp:=nil;
    end;
    TVarFuncEntity(v).RetType:=tp;
    if (v.Names.Count=0) and Assigned(TVarFuncEntity(v).RetType) then begin
      Result:=TVarFuncEntity(v).RetType;
      TVarFuncEntity(v).RetType:=nil;
      v.Free;
    end else
      Result:=v;
  end;
end;

procedure ErrorExpect(Parser:TTextParser;const Expect:AnsiString);
begin
  Parser.SetError('Excepcted: '+ Expect);
end;

function ConsumeToken(Parser:TTextParser;const Token:AnsiString):Boolean;
begin
  Result:=Parser.Token=Token;
  if Result then Parser.NextToken
  else Parser.SetError('Token expected: '+Token);
end;

function ParseCType(Parser: TTextParser): TEntity;
var
  simple  : TSimpleType;
  isunsig : Boolean;
  islong  : Boolean;
begin
  Result:=nil;
  if (Parser.Token='struct') then
    Result:=ParseStruct(Parser)
  else if (Parser.Token='union') then
    Result:=ParseUnion(Parser)
  else if (Parser.Token='enum') then
    Result:=ParseEnum(Parser)
  else begin
    if Parser.TokenType<>tt_Ident then Exit;

    simple:=TSimpleType.Create(Parser.TokenPos);
    simple.Name:=Parser.Token;

    isunsig:=(simple.Name='unsigned') or (simple.Name='signed');
    islong:=(simple.Name='long');

    Result:=simple;
    Parser.NextToken;

    if (Parser.Token='long') and islong then begin
      simple.name:=simple.name+' '+Parser.Token;
      Parser.NextToken;
    end;

    if isunsig and (Parser.Token='short') then begin
      simple.name:=simple.name+' '+Parser.Token;
      Parser.NextToken;
    end;

    if isunsig and (Parser.Token='char') then begin
      simple.name:=simple.name+' '+Parser.Token;
      Parser.NextToken
    end;

    if (Parser.Token='int') and (isunsig or islong) then begin
      simple.name:=simple.name+' '+Parser.Token;
      Parser.NextToken
    end;

    if islong and (Parser.Token='double') then begin
      simple.name:=simple.name+' '+Parser.Token;
      Parser.NextToken
    end;
  end;
end;

function isEndOfExpr(const t: AnsiString; CommaIsEnd: Boolean): Boolean;
begin
  Result:=(t=']') or (t=';') or (t=')') or (CommaIsEnd and (t=',')) or (t='}');
end;

function ParseCExpr(Parser: TTextParser; CommaIsEnd: Boolean=False): TExpression;
var
  x   : TExpression;
  lvl : Integer;
begin
  if isEndOfExpr(Parser.Token, CommaIsEnd) then
    Result:=nil
  else begin
    lvl:=0;
    x := TExpression.Create(Parser.Index);

    repeat
      if (Parser.Token='(') or (Parser.Token='[') then
        inc(lvl)
      else begin
        if (lvl=0) and isEndOfExpr(Parser.Token, CommaIsEnd) then
          Break
        else if (Parser.Token=')') or (Parser.Token=']') then
          dec(lvl)
      end;
      x.PushToken(Parser.Token, Parser.TokenType);
    until not Parser.NextToken;
    Result:=x;   
  end;
end;

{ TExpression }

function TExpression.DoParse(AParser: TTextParser): Boolean;  
begin
  Result:=False;
end;

procedure TExpression.PushToken(const AToken:AnsiString; ATokenType: TTokenType);
begin
  if Count=length(Tokens) then begin
    if Count=0 then SetLength(Tokens, 2)
    else SetLength(Tokens, Count*2);
  end;
  Tokens[Count].Token:=AToken;
  Tokens[Count].TokenType:=ATokenType;
  inc(Count);
end;


procedure ParseFuncParams(Parser: TTextParser; FuncName: TNamePart);
var
  prmtype   : TEntity;
  prmname   : TNamePart;
begin
  Parser.NextToken;
  while Parser.Token<>')' do begin

    if ParseName(Parser, prmtype, prmname) then begin
      FuncName.AddParam(prmtype, prmname)
    end else
      Exit; // failure

    if Parser.Token<>')' then begin
      if Parser.Token=',' then
        Parser.NextToken
      else begin
        ErrorExpect(Parser,')');
        Break;
      end;
    end;
  end;
  Parser.NextToken;
end;

function ParseNamePart(Parser: TTextParser): TNamePart;
var
  prefix    : TNamePart;
  id        : TNamePart;
  postfix   : TNamePart;
// todo: store const them as part of the name
begin
  if Parser.Token='const' then Parser.NextToken; // skip const qualifier

  if Parser.Token='*' then begin
    prefix:=TNamePart.Create(nk_Ref);
    while Parser.Token='*' do begin
      inc(prefix.refcount);
      Parser.NextToken;
      if Parser.Token='const' then Parser.NextToken; // skip const qualifier
    end;
  end else
    prefix:=nil;

  if Parser.Token='(' then begin
    Parser.NextToken;
    id:=ParseNamePart(Parser);
    ConsumeToken(Parser, ')');
  end else if Parser.TokenType=tt_Ident then begin
    id:=TNamePart.Create(nk_Ident);
    id.id:=Parser.Token;
    Parser.NextToken;
  end else
    id:=nil;

  postfix:=nil;
  if Parser.Token='[' then begin
    while Parser.Token='[' do begin
      if Assigned(postfix) then begin
        postfix.child:=TNamePart.Create(nk_Array);
        postfix:=postfix.child
      end else
        postfix:=TNamePart.Create(nk_Array);
      Parser.NextToken;
      postfix.AddArrayExpr(ParseCExpr(Parser));
      if not ConsumeToken(Parser, ']') then Break;
    end;
  end else if Parser.Token='(' then begin
    postfix:=TNamePart.Create(nk_Func);
    ParseFuncParams(Parser, postfix);
  end;

  Result:=id;
  if Assigned(postfix) then begin
    postfix.child:=Result;
    Result.owner:=postfix;
    Result:=postfix;
  end;

  if Assigned(prefix) then begin
    if Assigned(Result) and (Result.Kind=nk_Ref) then begin
      inc(Result.RefCount, prefix.RefCount);
      prefix.Free;
    end else begin
      prefix.child:=Result;
      if Assigned(Result) then Result.owner:=prefix;
      Result:=prefix;
    end;
  end;
end;

function ParseNames(Parser: TTextParser; var NameType: TEntity; Names: TList; AllowMultipleNames: Boolean): Boolean;
var
  Name  : TNamePart;
  done  : Boolean;
  specs : TStringList;
  s     : AnsiString;
begin
  specs:=TStringList.Create;

  ParseSepcifiers(Parser, specs);
  NameType:=ParseCType(Parser);

  s:=isCallConv(Parser.Token);
  if s<>'' then begin
    specs.Add(s);
    Parser.NextToken;
  end;

  Result:=Assigned(NameType);
  if Result then NameType.Specifiers.Assign(specs);
  specs.Free;

  if not Result then Exit;

  try
    repeat
      Name:=ParseNamePart(Parser);
      if Assigned(Name) then Names.Add(Name);
      if not AllowMultipleNames then begin
        Result:=True;
        Exit;
      end;
      done:=(Parser.Token<>',') and (Parser.Token=')');
      if not done then begin
        if Parser.Token <> ',' then begin
          ErrorExpect(Parser, ')');
          Exit;
        end;
        Parser.NextToken;
      end;
    until done;
  finally
  end;
end;

function ParseName(Parser: TTextParser; var NameType: TEntity; var name: TNamePart): Boolean;
var
  nm  : TList;
begin
  nm:=TList.Create;
  try
    name:=nil;
    NameType:=nil;
    Result:=ParseNames(Parser, NameType, nm, False);
    if Result and (nm.Count>0) then name:=TNamePart(nm[0]);
  finally
    nm.Free;
  end;
end;

{ TNamePart }

constructor TNamePart.Create(AKind:TNameKind);
begin
  inherited Create;
  Kind:=AKind;
end;

procedure TNamePart.AddParam(prmtype:TEntity;prmname:TNamePart);
var
  i : Integer;
begin
  i:=length(Params);
  SetLength(Params, i+1);
  Params[i].prmtype:=prmtype;
  Params[i].name:=prmname;
end;

procedure TNamePart.AddArrayExpr(expr:TExpression);
var
  i : Integer;
begin
  i:=length(arrayexp);
  SetLength(arrayexp, i+1);
  arrayexp[i]:=expr;
end;

{ TVarFuncEntity }

function TVarFuncEntity.DoParse(AParser:TTextParser):Boolean;
begin
  Result:=False;
end;

constructor TVarFuncEntity.Create(AOffset: Integer);
begin
  inherited Create(AOffset);
  Names:=TList.Create;
end;

destructor TVarFuncEntity.Destroy;
begin
  inherited Destroy;
end;

function TVarFuncEntity.FirstName:TNamePart;
begin
  if Names.Count>0 then Result:=TNamePart(Names[0]) else Result:=nil;
end;

{ TStructType }

function TStructType.AddField(ev:TVarFuncEntity):Integer;
var
  i : Integer;
begin
  i:=length(fields);
  SetLength(fields, i+1);
  fields[i].v:=ev;
  Result:=i;
end;

function ParseStruct(AParser: TTextParser): TStructType;
var
  i   : Integer;
  st  : TStructType;
  v   : TVarFuncEntity;
begin
  Result:=nil;
  if AParser.Token<>'struct' then Exit;

  st:=TStructType.Create(AParser.TokenPos);
  AParser.NextToken;

  Result:=st;
  if AParser.TokenType=tt_Ident then begin
    Result.Name:=AParser.Token;
    AParser.NextToken;
  end;

  if AParser.Token='{' then begin
    AParser.NextToken;
    repeat
      v:=TVarFuncEntity.Create(AParser.TokenPos);
      if not ParseNames(AParser, v.RetType, v.Names) then begin
        ErrorExpect(AParser, 'type name');
        v.Free;
        Exit;
      end;
      i:=st.AddField(v);
      if AParser.Token=':' then begin
        AParser.NextToken;
        st.fields[i].bits:=ParseCExpr(AParser);
      end;
      if AParser.Token=';' then AParser.NextToken;
    until (AParser.Token='}');

    ConsumeToken(AParser, '}');
  end;
  Result:=st;
end;

function ParseUnion(AParser:TTextParser):TUnionType;
var
  i   : Integer;
  st  : TUnionType;
  v   : TVarFuncEntity;
begin
  Result:=nil;
  if AParser.Token<>'union' then Exit;

  st:=TUnionType.Create(AParser.TokenPos);
  AParser.NextToken;

  Result:=st;
  if AParser.TokenType=tt_Ident then begin
    Result.Name:=AParser.Token;
    AParser.NextToken;
  end;

  if AParser.Token<>'{' then begin
    ErrorExpect(AParser, '{');
    Exit;
  end;
  AParser.NextToken;

  try
    repeat
      v:=TVarFuncEntity.Create(AParser.TokenPos);
      if not ParseNames(AParser, v.RetType, v.Names) then begin
        ErrorExpect(AParser, 'type name');
        v.Free;
        Exit;
      end;
      i:=st.AddField(v);
      if AParser.Token=':' then begin
        AParser.NextToken;
        st.fields[i].bits:=ParseCExpr(AParser);
      end;
      if AParser.Token=';' then AParser.NextToken;
    until (AParser.Token='}');

    ConsumeToken(AParser, '}');
    Result:=st;
  finally
    if not Assigned(Result) then st.Free;
  end;
end;

function ParseTypeDef(AParser: TTextParser): TTypeDef;
var
  td  : TTypeDef;
begin
  Result:=nil;
  if AParser.Token<>'typedef' then Exit;
  try
    td:=TTypeDef.Create(AParser.TokenPos);
    AParser.NextToken;
    Result:=td;

    ParseNames(AParser, td.origintype, td.names, true);
  finally
    if not Assigned(Result) then
      td.Free;
  end;
end;

function ParseEnum(AParser: TTextParser): TEnumType;
var
  en  : TEnumType;
  nm  : AnsiString;
  x   : TExpression;
  ofs : Integer;
begin
  Result:=nil;
  en:=nil;
  try
    if AParser.Token<>'enum' then Exit;
    en:=TEnumType.Create(AParser.TokenPos);
    AParser.NextToken;
    if AParser.TokenType=tt_Ident then begin
      en.Name:=AParser.Token;
      AParser.NextToken;
    end;
    if AParser.Token='{' then begin
      AParser.NextToken;
      while AParser.Token<>'}' do begin
        if AParser.TokenType<>tt_Ident then begin
          ErrorExpect(AParser, 'identifier');
          Exit;
        end;
        nm:=AParser.Token;
        ofs:=AParser.TokenPos;
        AParser.NextToken;
        if AParser.Token='=' then begin
          AParser.NextToken;
          x:=ParseCExpr(AParser, True);
          if not Assigned(x) then Exit;
        end else
          x:=nil;
        en.AddItem(nm, x, ofs);
        if AParser.Token=',' then AParser.NextToken;
      end;
      AParser.NextToken;
    end;
    Result:=en;
  finally
    if not Assigned(Result) then en.Free;
  end;
end;

{ TUnionType }

function TUnionType.AddField(ev:TVarFuncEntity):Integer;
var
  i : Integer;
begin
  i:=length(fields);
  SetLength(fields, i+1);
  fields[i].v:=ev;
  Result:=i;
end;

{ TEnumType }

function TEnumType.AddItem(const name:AnsiString;x:TExpression; Offset: Integer): Integer;
var
  i : Integer;
begin
  i:=length(items);
  SetLength(items, i+1);
  items[i].Name := name;
  items[i].Value := x;
  items[i].Offset:=Offset;
  Result:=i;
end;

{ TTypeDef }

constructor TTypeDef.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  names:=TList.Create;
end;

destructor TTypeDef.Destroy;
begin
  names.Free;
  inherited Destroy;
end;

end.
