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
unit ctopasconvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cparsertypes, TextParsingUtils, codewriter, cparserutils;

type

  { TConvertSettings }

  TConvertSettings = class
    RecordsArePacked  : Boolean;
    FuncsAreExternal  : Boolean;
    EnumsAsConst      : Boolean;
    UsedNames         : TStringList;
    CtoPasTypes       : TStringList;

    DefaultCType      : AnsiString;

    // for unkown types ONLY! (not available at CtoPasTypes);
    TypeNamePrefix    : AnsiString;
    RefTypeNamePrefix : AnsiString;
    FuncConv          : AnsiString;
    FuncDeclPostfix   : AnsiString;
    ParamPrefix       : AnsiString;

    CustomDefines     : AnsiString;


    constructor Create;
    destructor Destroy; override;
    function GetUniqueName(const n: ansistring): Ansistring;
    function GetTypeName(const CTypeName: AnsiString): Ansistring;
  end;

// endPoint contains
//  Y - line number (starting from 1),
//  X - column (starting from 1);
function ConvertCode(const t: AnsiString; var endPoint: TPoint; AllText: Boolean; cfg: TConvertSettings = nil): AnsiString;

// converts C-expression to Pascal expression, replace symbols with pascal equvialents.
// WARN: * the function doesn't handle macroses (treats them as identifiers)
//       * it doesn't recognizes typecasting
//       * it doesn't recognize the correct order of operations.
function PasExp(x: TExpression): AnsiString;

// returns true, if x is single number expression. V is the value of the number
function isNumberExp(x: TExpression; var v: Int64): Boolean;

// returns array limit base on x expression.
// if expression is a single number (N), then evaluates the N-1 number and returns it as string
// if expression is complex, returns pascal expression exp-1.
// i.e.   int a[10] ->       a: array [0..9] of Integer;
//        int a[10*2] ->     a: array [0..10*2-1] of Integer;
//        int a[MAXCONST] -> a: array [0..MAXCONST-1] of Integer;
function PasArrayLimit(x: TExpression): AnsiString;

implementation

type
  TFuncWriterProc = procedure (wr: TCodeWriter; const FunctName, FuncRetName: AnsiString;
    const Params, ParamTypes: array of AnsiString) of object;

  TVarListItem = record
    VarName : AnsiString;
    VarType : AnsiString;
    Comment : AnsiString;
  end;

  { TVarList }

  TVarList = class(TObject)
  public
    Items       : array of TVarListItem;
    ItemsCount  : Integer;
    procedure Add(const VarName, VarType, Comment: AnsiString); overload;
    procedure Add(const Comment: AnsiString); overload;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure WriteList(wr: TCodeWriter);
  end;

  { TCodeConvertor }

  TCodeConvertor = class(TObject)
  protected
    CmtList         : TList;
    Breaker         : TLineBreaker;
    LastOffset      : Integer;
    function FindCommentForLine(ln: Integer): TComment;
  protected
    fWriters        : TList;
    AuxTypeCounter  : Integer;

    procedure DefFuncWrite(wr: TCodeWriter; const FuncName, FuncRetType: AnsiString;
      const Params, ParamTypes: array of AnsiString);

    function NextAuxTypeName(const Prefix: AnsiString): AnsiString;

    function GetPasTypeName(RetType: TEntity; TypePart: TNamePart): AnsiString;

    procedure DeclarePasType(TypeEntity: TEntity; const PasTypeName: AnsiString);
    procedure DeclareFuncType(const PasTypeName, RetTypeName: AnsiString; const params: array of TFuncParam);

    procedure WriteLnCommentForOffset(AOffset: Integer; NeedOffset: Boolean=True);
    function NextCommentBefore(AOffset: Integer): Integer;
    procedure WriteLnCommentsBeforeOffset(AOffset: Integer);

    procedure WriteFuncDecl(const FnName, PasRetType: AnsiString; const params : array of TFuncParam);
    procedure WriteFuncOrVar(cent: TVarFuncEntity; StartVar, WriteComment: Boolean); // todo: deprecate!
    procedure WriteTypeDef(tp: TTypeDef);
    procedure WriteEnum(en: TEnumType);
    procedure WriteEnumAsConst(en: TEnumType);
    procedure WriteUnion(st: TUnionType);
    procedure WriteStruct(st: TStructType);
    procedure WriteCommentToPas(cent: TComment);
    procedure WriteExp(x: TExpression);
    procedure WritePreprocessor(cent: TCPrepDefine);

    procedure PushWriter;
    procedure PopWriter;
  public
    wr        : TCodeWriter;
    cfg       : TConvertSettings;
    WriteFunc : TFuncWriterProc;
    DebugEntities : Boolean;
    constructor Create(ASettings: TConvertSettings);
    destructor Destroy; override;
    procedure WriteCtoPas(cent: TEntity; comments: TList; const ParsedText: AnsiString);
  end;

procedure TVarList.Add(const VarName,VarType,Comment:AnsiString);
begin
  if ItemsCount=length(Items) then begin
    if ItemsCount=0 then SetLength(Items, 4)
    else SetLength(Items, ItemsCount*2);
  end;
  Items[ItemsCount].VarName:=VarName;
  Items[ItemsCount].VarType:=VarType;
  Items[ItemsCount].Comment:=Comment;
  inc(ItemsCount);
end;

procedure TVarList.Add(const Comment:AnsiString);
begin
  Add('', '', Comment);
end;

constructor TVarList.Create;
begin

end;

destructor TVarList.Destroy;
begin
  inherited Destroy;
end;

procedure TVarList.Clear;
begin
  ItemsCount:=0;
end;


function MaxStrLen(const s: AnsiString; Max: Integer): Integer; inline;
begin
  if Max>length(s) then Result:=Max
  else Result:=length(s);
end;

function StrToLen(const s: AnsiString; Len: Integer; const SpChar: AnsiChar = ' '): AnsiString;
begin
  if length(s)<len then begin
    SetLength(Result, len);
    FillChar(Result[1], Len, SpChar);
    if length(s)>0 then Move(s[1], Result[1], length(s));
  end else
    Result:=s;
end;

procedure TVarList.WriteList(wr:TCodeWriter);
var
  MaxNameLen  : Integer;
  MaxTypeLen  : Integer;
  i           : Integer;
begin
  if ItemsCount=0 then Exit;

  MaxNameLen:=0;
  MaxTypeLen:=0;

  for i:=0 to ItemsCount-1 do begin
    MaxNameLen:=MaxStrLen(Items[i].VarName, MaxNameLen);
    MaxTypeLen:=MaxStrLen(Items[i].VarType, MaxTypeLen);
  end;
  inc(MaxNameLen);
  inc(MaxTypeLen, 2); // ';' + ' ' after type name

  for i:=0 to ItemsCount-1 do
    with Items[i] do
      if Comment<>'' then
        wr.Wln( StrToLen(VarName, MaxNameLen)+': '+StrToLen(VarType+';', MaxTypeLen) + ' '+Comment)
      else
        wr.Wln( StrToLen(VarName, MaxNameLen)+': '+VarType+';');
end;


type

  { TStopComment }

  TStopComment = class(TObject)
  public
    FirstComment  : boolean;
    CommentFound  : boolean;
    CommentEnd    : Integer;
    Precomp       : TEntity;
    PrecompEnd    : Integer;

    procedure OnComment(Sender: TObject; const Str: ansistring);
    procedure OnPrecompiler(Sender: TTextParser; PrecompEntity: TObject);
  end;

procedure TStopComment.OnComment(Sender: TObject; const Str: ansistring);
var
  parser: TTextParser;
begin
  parser := TTextParser(Sender);
  if not FirstComment then
  begin
    FirstComment := parser.Stack.Count = 0;
    CommentEnd := parser.Index;
  end;
  CommentFound := True;
end;

procedure TStopComment.OnPrecompiler(Sender: TTextParser; PrecompEntity: TObject);
begin
  if not FirstComment and (PrecompEntity is TEntity) then
  begin
    FirstComment:=True;
    Precomp:=PrecompEntity as TEntity;
    PrecompEnd:=Sender.Index;
  end;
end;

function ParseNextEntityOrComment(AParser: TTextParser): TEntity;
var
  cmt : TStopComment;
  ent     : TEntity;
  entidx  : Integer;
begin
  cmt := TStopComment.Create;
  AParser.UseCommentEntities := True;
  AParser.OnComment := @cmt.OnComment;
  AParser.OnPrecompile:=@cmt.OnPrecompiler;
  Result:=nil;

  ent := ParseNextEntity(AParser);
  entidx:=AParser.Index;

  if cmt.FirstComment then begin
    if Assigned(cmt.Precomp) then begin
      Result:=cmt.Precomp;
      AParser.Index:=cmt.PrecompEnd;
    end else if (AParser.Comments.Count > 0) then
    begin
      Result := TComment(AParser.Comments[0]);
      AParser.Index := cmt.CommentEnd;
    end;
  end;

  cmt.Free;
  if (not Assigned(Result)) or (Assigned(ent) and (ent.Offset<Result.Offset)) then begin
    Result:=ent;
    AParser.Index:=entidx;
  end;
end;

function GetRefAsterix(const AstCount: integer): ansistring;
begin
  if Astcount = 0 then
    Result := '';
  SetLength(Result, Astcount);
  FillChar(Result[1], AstCount, '*');
end;


function isNumberExp(x: TExpression; var v: Int64): Boolean;
var
  err : Integer;
begin
  Result:=Assigned(x) and (x.count=1);
  if Result then begin
    Val(x.Tokens[0].Token, v, err);
    Result:=err=0;
  end;
end;

function PasArrayLimit(x: TExpression): AnsiString;
var
  i   : Int64;
begin
  if isNumberExp(x, i) then
    Result:=IntToStr(i-1)
  else
    Result:=PasExp(x) + '-1';
end;


procedure WriteArray(arr: TNamePart; wr: TCodeWriter);
var
  i : Integer;
begin
  wr.W('array ');
  for i := 0 to length(arr.arrayexp) - 1 do wr.W('[0..' + PasArrayLimit(arr.arrayexp[i])+']');
  wr.W(' of ');
end;


type
  TMacrosMaker = class(TObject)
  public
    hnd : TCMacroHandler;
    constructor Create(AHandler: TCMacroHandler);
    procedure Precompiler(Sender: TTextParser; PrecompEntity: TObject);
  end;

constructor TMacrosMaker.Create(AHandler: TCMacroHandler);
begin
  hnd:=AHandler;
end;

procedure TMacrosMaker.Precompiler(Sender: TTextParser; PrecompEntity: TObject);
var
  d : TCPrepDefine;
begin
  if not (PrecompEntity is TCPrepDefine) then Exit;

  d:=TCPrepDefine(PrecompEntity);
  if not Assigned(d.Params) or (d.Params.Count=0) then begin
    hnd.AddSimpleMacro(d._Name, d.SubsText);
  end else begin
    hnd.AddParamMacro(d._Name, d.SubsText, d.Params);
  end;
end;

procedure PrepareMacros(const t: AnsiString; hnd: TCMacroHandler);
var
  p : TTextParser;
  m : TMacrosMaker;
begin
  if t='' then Exit;
  if not Assigned(hnd) then Exit;

  m := TMacrosMaker.Create(hnd);
  p:=CreateCParser(t, false);
  p.OnPrecompile:=@m.Precompiler;

  while p.NextToken do ; // parse through

  p.Free;
  m.Free;
end;

function GetEmptyLinesCount(const t: AnsiString; StartOfs, EndOfs: Integer): Integer;
var
  i : Integer;
begin
  i:=StartOfs;
  if i<=0 then Exit;

  Result:=0;
  while (i<EndOfs) and (i<=length(t)) do begin
    if t[i] in [#13,#10] then begin
      inc(Result); inc(i);
      if (i<=length(t)) and (t[i] in [#13,#10]) and (t[i]<>t[i-1]) then inc(i);
    end;
    inc(i);
  end;
end;

function GetEmptyLines(const t: AnsiString; StartOfs, EndOfs: Integer): AnsiString;
var
  i : Integer;
  c : Integer;
begin
  c:=GetEmptyLinesCount(t, StartOfs, EndOfs);
  for i:=1 to c do Result:=Result+LineEnding;
end;

function ConvertCode(const t: AnsiString; var endPoint: TPoint; AllText: Boolean; cfg: TConvertSettings): AnsiString;
var
  p         : TTextParser;
  ent       : TEntity;
  i         : integer;
  le        : integer;
  cnv       : TCodeConvertor;
  macros    : TCMacroHandler;
  owncfg    : Boolean;
  lastsec   : AnsiString; // last code section
  ofs       : Integer;
begin
  Result:='';
  ent:=nil;
  owncfg:=not Assigned(cfg);
  lastsec:='';
  if owncfg then cfg := TConvertSettings.Create;
  try
    macros:=TCMacroHandler.Create;

    if cfg.CustomDefines<>'' then PrepareMacros(cfg.CustomDefines, macros);

    p := CreateCParser(t);
    p.MacroHandler:=macros;
    try
      repeat
        try
          ofs := p.Index;
          ent := ParseNextEntityOrComment(p);
        except
          ent:=nil;
        end;

        if Assigned(ent) then begin
          cnv := TCodeConvertor.Create(cfg);
          try
            cnv.wr.Section:=lastsec;
            if lastsec<>'' then cnv.wr.IncIdent;

            cnv.WriteCtoPas(ent, p.Comments, t);

            lastsec:=cnv.wr.Section;
          except
            on e: Exception do Result:=Result+LineEnding+ 'error while converting C code: ' + e.Message;
          end;
          Result := Result+GetEmptyLines(p.Buf, ofs, ent.Offset)+cnv.wr.Text;
          cnv.Free;
        end;

        for i:=0 to p.Comments.Count-1 do TComment(p.Comments[i]).Free;
        p.Comments.Clear;

      until (ent=nil) or not AllText;

      i := 1;
      le := 0;
      endPoint.X := 0;
      endPoint.Y := 0;
      while i < p.Index do begin
        Inc(endPoint.Y);
        le := i;
        SkipLine(t, i);
      end;
      endPoint.X := p.Index - le + 1 + p.MacrosDelta;



    finally
      p.Free;
      macros.Free;
    end;
  except
    on e: Exception do Result:=Result+LineEnding+' internal error: '+ e.Message;
  end;
  if owncfg then cfg.Free;
end;

{ TCodeConvertor }

constructor TCodeConvertor.Create(ASettings:TConvertSettings);
begin
  cfg:=ASettings;
  wr:=TCodeWriter.Create;
  WriteFunc:=@DefFuncWrite;
end;

destructor TCodeConvertor.Destroy;
var
  i : Integer;
begin
  if Assigned(fWriters) then begin
    for i:=0 to fWriters.Count-1 do TObject(fWriters[i]).Free;
    fWriters.Free;
  end;
  wr.Free;
  inherited Destroy;
end;

procedure TCodeConvertor.WriteCommentToPas(cent: TComment);
var
  u: ansistring;
begin
  u := cent._Comment;
  if cent.CommenType = ctBlock then
  begin
    u := StringReplace(u, '*)', '* )', [rfReplaceAll]);
    wr.Wln('(*' + u + ' *)');
  end
  else
  begin
    wr.Wln('//' + u);
  end;
end;

procedure TCodeConvertor.WriteExp(x:TExpression);
begin
  wr.W(PasExp(x));
end;

function CtoPasSymbol(const t: AnsiString): AnsiString;
begin
  if (t='>>') then Result:='shr'
  else if (t='<<') then Result:='shl'
  else if (t='%') then Result:='mod'
  else if (t='|') or (t='||') then Result:='or'
  else if (t='&') or (t='&&') then Result:='and'
  else if (t='^') then Result:='xor'
  else if (t='!') or (t='~') then Result:='not'
  else if (t='!=') then Result:='<>'
  else Result:=t;
end;

function CtoPasString(const t: AnsiString; cfg: TConvertSettings): AnsiString;
begin
  Result:=#39+Copy(t, 2, length(t)-2)+#39;
end;

procedure TCodeConvertor.WritePreprocessor(cent:TCPrepDefine);
var
  p   : TTextParser;
  s   : AnsiString;
begin
  if cent.SubsText<>'' then begin
    SetPasSection(wr, 'const');
    p:=CreateCParser(cent.SubsText, false);
    s:='';
    while p.NextToken do begin
      case p.TokenType of
        tt_String:  s:=s+' '+CtoPasString(p.Token, cfg);
        tt_Symbol:  s:=s+' '+CtoPasSymbol(p.Token);
      else
        s:=s+' '+p.Token;
      end;
    end;
    p.Free;
    wr.W(cfg.GetUniqueName(cent._Name) + ' =' + s+';');

    WriteLnCommentForOffset(cent.Offset);
  end;
end;

procedure TCodeConvertor.PushWriter;
begin
  if not Assigned(fWriters) then fWriters:=TList.Create;
  fWriters.Add(wr);
  wr:=TCodeWriter.Create;
end;

procedure TCodeConvertor.PopWriter;
var
  t : TCodeWriter;
  s4 : AnsiString;
  s5 : AnsiString;
  i : Integer;
begin
  if not Assigned(fWriters) or (fWriters.Count=0) then Exit;
  t:=wr;
  i:=fWriters.Count-1;
  if i<0 then wr:=nil else wr:=TCodeWriter(fWriters[i]);

  fWriters.Delete(i);
  if t.Text<>'' then begin
    // HACK: Push/Pop writing takes place for new type declarations only
    //  if there're multiple pop/push operations, the resulting code might look like:
    //  type
    //    A1 = something
    //  type
    //    A2 = something
    //  It's possible to merge them into:
    //  type
    //    A1 = something
    //    A2 = something
    s4:=Copy(t.Text, 1, 4);
    s5:=Copy(t.text, 1, 5);
    if Assigned(wr) then begin
      if (s4='type') and (Copy(wr.Text, 1, 4)=s4) then
        wr.Text:=t.Text+Copy(wr.Text, 4+sizeof(LineEnding)+1, length(wr.Text))
      else if (s5='const') and (Copy(wr.Text, 1, 5)=s5) then
        wr.Text:=t.Text+Copy(wr.Text, 5+sizeof(LineEnding)+1, length(wr.Text))
      else
        wr.Text:=t.Text+wr.Text;
    end;
  end;
  t.Free;
end;

procedure TCodeConvertor.DeclareFuncType(const PasTypeName, RetTypeName: AnsiString; const params: array of TFuncParam);
begin
  SetPasSection(wr, 'type');
  wr.W(PasTypeName + ' = ');
  WriteFuncDecl('', RetTypeName, params);
end;

procedure TCodeConvertor.WriteLnCommentForOffset(AOffset:Integer; NeedOffset: Boolean);
var
  cmt : TComment;
begin
  cmt:=FindCommentForLine( Breaker.LineNumber(AOffset));
  if Assigned(cmt) then begin
    LastOffset:=cmt.Offset;
    if NeedOffset then wr.W('  ');
    WriteCommentToPas(cmt);
  end else
    wr.Wln;
end;

function TCodeConvertor.NextCommentBefore(AOffset:Integer):Integer;
var
  i : Integer;
  c : TComment;
begin
  Result:=-1;
  for i:=0 to CmtList.Count-1 do begin
    c:=TComment(CmtList[i]);
    if (c.Offset>LastOffset) and (c.Offset<AOffset) then begin
      Result:=c.Offset;
      Exit;
    end else if c.Offset>AOffset then
      Exit;
  end;
end;

procedure TCodeConvertor.WriteLnCommentsBeforeOffset(AOffset:Integer);
var
  i : Integer;
begin
  i:=NextCommentBefore(AOffset);
  while i>=0 do begin
    WriteLnCommentForOffset(i, False);
    i:=NextCommentBefore(AOffset);
  end;
end;

// returns the name for simple types, or empty structs:
//   struct num n; - returns 'num' (name of the struct),
// but
//   struct num {int f;} n; returns '', because struct is NOT simple named type
function GetSimpleName(ent: TEntity): AnsiString;
begin
  if ent is TSimpleType then
    Result:=TSimpleType(ent).Name
  else if (ent is TStructType) and ( length(TStructType(ent).fields)=0) then
    Result:=TStructType(ent).Name
  else if (ent is TEnumType) and (length(TEnumType(ent).items)=0) then
    Result:=TEnumType(ent).Name
  else
    Result:='';
end;

// returns the declared typename
// for
//   struct num n;
//   struct num {int f;} n;
// returns 'num' (name of the struct),
function GetComplexTypeName(ent: TEntity): AnsiString;
begin
  if ent is TStructType then
    Result:=TStructType(ent).Name
  else if ent is TUnionType then
    Result:=TUnionType(ent).Name
  else if ent is TEnumType then
    Result:=TEnumType(ent).Name
  else
    Result:='';
end;

function TCodeConvertor.GetPasTypeName(RetType: TEntity; TypePart: TNamePart): AnsiString;
var
  CtypeName : AnsiString;
  pasRef    : AnsiString;
  pasType   : AnsiString;
  rt        : AnsiString;
  i         : Integer;
begin
  if isNamePartPtrToFunc(TypePart) then begin
    PushWriter;
    rt := GetPasTypeName(RetType, TypePart.owner.owner);
    PopWriter;

    Result:=NextAuxTypeName('TAuxCallback');
    DeclareFuncType(Result, rt, TypePart.owner.params);
    wr.Wln(';');

  end else begin

    CtypeName:=GetSimpleName(RetType);
    if CtypeName<>'' then begin
      pasRef:=cfg.RefTypeNamePrefix+cfg.GetTypeName(CtypeName);
    end else begin
      CtypeName:=GetComplexTypeName(RetType);
      if CTypeName='' then CtypeName:=NextAuxTypeName('TAuxType');
      DeclarePasType(RetType, CtypeName);
      cfg.CtoPasTypes.Values[CtypeName]:=CTypeName;
      pasRef:=cfg.RefTypeNamePrefix+Copy(CtypeName, 2, length(CTypeName));
      wr.Wln(';');
    end;

    if Assigned(TypePart) and (TypePart.Kind=nk_Ref) then begin
      pasType:=cfg.GetTypeName(CtypeName);
      for i:=1 to TypePart.RefCount do begin
        CTypeName:=CTypeName+'*';
        rt:=cfg.CtoPasTypes.Values[CTypeName];
        if rt='' then begin
          PushWriter;
          SetPasSection(wr, 'type');
          wr.Wln(pasRef+' = ^'+pasType+';');
          pasType:=pasRef;
          PopWriter;

          // filling required reference type
          cfg.CtoPasTypes.Values[CTypeName]:=pasType;

        end else
          pasType:=rt;
        pasRef:=cfg.RefTypeNamePrefix+pasType;
      end;
      Result:=pasType;
    end else begin
      Result:=cfg.GetTypeName(CtypeName);
    end;
  end;
end;

function isVoidParams(const params : array of TFuncParam): Boolean;
begin
  Result:=length(params)=0;
  if Result then Exit;
  Result:=length(params)=1;
  if Result then
    Result:=(params[0].prmtype is TSimpleType) and
            (TSimpleType(params[0].prmtype).Name='void') and
            (params[0].name=nil);
end;

procedure TCodeConvertor.WriteFuncDecl(const FnName, PasRetType: AnsiString; const params : array of TFuncParam);
var
  i        : Integer;
  ptypes   : array of String;
  pnames   : array of String;
  tp       : TNamePart;
begin
  PushWriter;
  if not isVoidParams(params) then begin
    SetLength(ptypes, length(params));
    SetLength(pnames, length(params));
    for i:=0 to length(params)-1 do begin
      tp:=params[i].name;
      if Assigned(tp) then begin
        while Assigned(tp.child) do tp:=tp.child;
        if tp.Kind=nk_Ident then begin
          pnames[i]:=cfg.GetUniqueName(tp.Id);
          tp:=tp.owner;
        end;
      end;
      if pnames[i]='' then pnames[i] := cfg.ParamPrefix+IntToStr(i);
      ptypes[i]:=GetPasTypeName(params[i].prmtype, tp);
    end;
  end else begin
    ptypes:=nil;
    pnames:=nil;
  end;
  PopWriter;

  wr.CheckLineLen:=True;
  WriteFunc(wr, FnName, PasRetType, pnames, ptypes);
  wr.CheckLineLen:=False;

  if cfg.FuncConv<>'' then wr.W('; '+cfg.FuncConv);
  if cfg.FuncDeclPostfix<>'' then wr.W('; '+cfg.FuncDeclPostfix);
end;

function isDeclExternal(cfg: TConvertSettings; DeclType: TEntity; isFunc: Boolean): Boolean;
begin
  Result:=(isfunc and cfg.FuncsAreExternal) or
          (Assigned(DeclType) and (DeclType.Specifiers.IndexOf('extern')>=0));
end;

procedure TCodeConvertor.WriteFuncOrVar(cent: TVarFuncEntity; StartVar, WriteComment: Boolean);
var
  i, j  : integer;
  Name  : TNamePart;
  n     : TNamePart;
  id    : AnsiString;
  ref   : TNamePart;
  rt    : AnsiString;
  isfunc  : Boolean;
begin
  for j := 0 to cent.Names.Count - 1 do
  begin
    Name:=GetIdPart(TNamePart(cent.Names[j]));
    if not Assigned(name) then begin
      wr.Wln(' bad declaration synax!');
      Exit;
    end;
    isfunc:=False;
    id:=cfg.GetUniqueName(name.Id);
    n:=name.owner;
    if not Assigned(n) then begin
      PushWriter;
      rt:=GetPasTypeName(cent.RetType, Name);
      PopWriter;
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ' + rt);
    end else if (n.Kind=nk_Func) then begin
      SetPasSection(wr, '');
      rt:=GetPasTypeName(cent.RetType, n.owner);
      WriteFuncDecl(id, rt, n.params);
      isfunc:=True;
    end else if (n.Kind=nk_Ref) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ');
      ref:=n;
      n:=n.owner;
      if not Assigned(n) then begin
        wr.W( GetPasTypeName(cent.RetType, ref) );
      end else
        case n.Kind of
          nk_Array: begin
            for i:=1 to ref.RefCount do wr.W('^');
            WriteArray(n, wr);
            wr.W(GetPasTypeName(cent.RetType, n.owner))
          end;
          nk_Func: begin
            PushWriter;
            rt:=GetPasTypeName(cent.RetType, n.owner);
            PopWriter;
            WriteFuncDecl('', rt, n.params);
          end;
        end;

    end else if (n.Kind=nk_Array) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ');
      WriteArray(n, wr);
      wr.W(GetPasTypeName(cent.RetType, n.owner));
    end;
    wr.W(';');
    if isDeclExternal(cfg, cent.RetType, isfunc) then wr.W(' external;');
    if WriteComment then WriteLnCommentForOffset(cent.Offset);
  end;
end;

procedure TCodeConvertor.WriteCtoPas(cent: TEntity; comments: TList; const ParsedText: AnsiString);
begin
  CmtList:=comments;
  Breaker:=TLineBreaker.Create;
  Breaker.SetText(ParsedText);

  if cent is TVarFuncEntity then begin
    WriteFuncOrVar(cent as TVarFuncEntity, True, True)
  end else if cent is TTypeDef then
    WriteTypeDef(cent as TTypeDef)
  else if (cent is TStructType) or (cent is TEnumType) or (cent is TUnionType) then begin
    DeclarePasType(cent, GetComplexTypeName(cent));
    wr.Wln(';');
  end else if cent is TComment then
    WriteCommentToPas(cent as TComment)
  else if cent is TCPrepDefine then
    WritePreprocessor(cent as TCPrepDefine)
  else begin
    if DebugEntities then
      wr.Wln(cent.ClassName);
  end;

  Breaker.Free;
end;

// typedef allows to declare multiple type alias names, with the same base type.   //
// i.e.:                                                                           //
// typedef struct mystruct_{int a,b;}                                              //
//                  t[10], *t_ptr, (*f)(int i);                                    //
//                                                                                 //
// typedef writting alogrithm:                                                     //
// 1. find the base type name.                                                     //
// 2. if no name is found, generate AuxType name                                   //
// 3. declare all types using the base name (treating the base name as simpletype) //
//                                                                                 //
// found simple declaration, that can be used as base name:                        //
// typedef struct {int a;} t, *t_ptr;  - t is base name                            //
procedure TCodeConvertor.WriteTypeDef(tp: TTypeDef);
var
  nm        : TNamePart;
  n         : TNamePart;
  fn        : TNamePart;
  rt        : AnsiString;
  tpart     : TNamePart;
  i         : Integer;

  name        : AnsiString;
  basetype    : TSimpleType;
  basetypeown : Boolean;

begin
  if tp.names.Count=0 then Exit; // no names specified!

  // 1. selecting base name (and TSimpleType) type
  if tp.origintype is TSimpleType then begin
    basetype:=TSimpleType(tp.origintype);
    basetypeown:=False;
  end else begin
    name:=GetComplexTypeName(tp.origintype);
    if name='' then
      for i:=0 to tp.names.Count-1 do begin
        if TNamePart(tp.names[i]).Kind=nk_Ident then begin
          name:=TNamePart(tp.names[i]).Id;
          Break;
        end;
      end;
    // 2. no suitable name found in typedef, generating auxtype
    if name='' then begin
      PushWriter;
      name:=GetPasTypeName(tp.origintype, nil);
      PopWriter;
    end else begin
      DeclarePasType(tp.origintype, name);
      wr.Wln(';');
    end;
    basetype:=TSimpleType.Create;
    basetype.Name:=name;
    basetypeown:=True;
  end;

  // 3. writting down all types
  for i:=0 to tp.names.Count-1 do begin
    nm:=GetIdPart(TNamePart(tp.names[i]));
    if not Assigned(nm) then Exit;
    SetPasSection(wr,'type');

    n:=nm.owner;

    if not Assigned(n) then begin
      if nm.Id<>basetype.Name then wr.W(nm.Id+' = '+basetype.Name)
      else Continue;
    end else begin
      fn:=n.owner;
      if n.Kind=nk_Array then begin
        PushWriter;
        name:=GetPasTypeName(basetype, n.owner);
        PopWriter;
        wr.W(nm.Id+' = ');
        WriteArray(n, wr);
        wr.W(name);;
      //typedef functions and typedef function pointers are converted the same way.
      end else if (n.Kind=nk_Ref) and (not Assigned(fn) or (fn.Kind<>nk_Func)) then begin
        PushWriter;
        name:=GetPasTypeName(basetype, n);
        PopWriter;
        wr.W(nm.Id+' = '+name);
        fn:=n.owner;
      end else if isNamePartPtrToFunc(n) or (Assigned(n) and (n.kind=nk_Func) ) then begin

        if isNamePartPtrToFunc(n) then begin
          tpart:=n.owner.owner // rettype of function pointer
        end else begin
          tpart:=n.owner;
          cfg.CtoPasTypes.Values[nm.id+'*']:=nm.id;
        end;

        PushWriter;
        rt := GetPasTypeName(basetype, tpart);
        PopWriter;

        if n.Kind=nk_Func then fn:=n;
        DeclareFuncType(nm.id, rt, fn.params);
      end;
    end;
    wr.Wln(';');
  end;
  if basetypeown then basetype.Free;
end;

procedure TCodeConvertor.WriteStruct(st:TStructType);
var
  i : Integer;
begin
  if cfg.RecordsArePacked then wr.W('packed ');
  wr.Wln('record');
  wr.IncIdent;
  //todo: bit fields support
  for i:=0 to length(st.fields)-1 do begin
    WriteLnCommentsBeforeOffset(st.fields[i].v.Offset);
    WriteFuncOrVar(st.fields[i].v, False, True);
  end;
  wr.DecIdent;
  wr.W('end');
end;

procedure TCodeConvertor.WriteEnum(en:TEnumType);
var
  b : Boolean;
  i : Integer;
begin
  if cfg.EnumsAsConst then
    WriteEnumAsConst(en)
  else begin
    WriteLnCommentsBeforeOffset(en.Offset);
    wr.W('(');
    wr.IncIdent;
    b:=wr.CheckLineLen;
    wr.CheckLineLen:=True;
    for i:=0 to length(en.items)-2 do begin
      WriteLnCommentsBeforeOffset(en.Items[i].Offset);
      wr.W(en.items[i].Name);
      if Assigned(en.items[i].Value) then begin
        wr.W(' = ');
        WriteExp(en.items[i].Value);
      end;
      wr.W(',');
      WriteLnCommentForOffset(en.Items[i].Offset);
    end;
    i:=length(en.items)-1;
    WriteLnCommentsBeforeOffset(en.Items[i].Offset);
    wr.W(en.items[i].Name);
    if Assigned(en.items[i].Value) then begin
      wr.Wln(' = ');
      WriteExp(en.Items[i].Value);
    end else
      wr.Wln;
    WriteLnCommentForOffset(en.Items[i].Offset);
    wr.DecIdent;
    wr.W(')');
    wr.CheckLineLen:=b;
  end;
end;

procedure TCodeConvertor.WriteEnumAsConst(en:TEnumType);
var
  i       : Integer;
  v       : Int64;
  last    : AnsiString;
  useval  : Boolean;
begin
  if length(en.items)>0 then begin
    PushWriter;
    WriteLnCommentsBeforeOffset(en.Offset);
    SetPasSection(wr, 'const');
    v:=0;
    last:='';
    useval:=True;

    for i:=0 to length(en.items)-1 do begin
      WriteLnCommentsBeforeOffset(en.items[i].Offset);

      wr.W(en.items[i].Name + ' = ');
      if Assigned(en.items[i].Value) then begin
        WriteExp(en.items[i].Value);
        useval:=isNumberExp(en.items[i].Value, v);
      end else begin
        if useval
        then wr.W(IntToStr(v))
        else wr.W(last+' + 1');
      end;
      wr.W(';');
      WriteLnCommentForOffset(en.items[i].Offset);
      inc(v);
      last:=en.Items[i].Name;
    end;

    PopWriter;
  end;
  wr.W('Integer');
end;

procedure TCodeConvertor.WriteUnion(st:TUnionType);
var
  i : Integer;
begin
  if cfg.RecordsArePacked then wr.W('packed ');
  wr.WLn('record');
  wr.Wln('case Integer of');
  wr.IncIdent;
  for i:=0 to length(st.fields)-1 do begin
    WriteLnCommentsBeforeOffset(st.fields[i].v.Offset);
    wr.w(IntToStr(i)+':(');
    WriteFuncOrVar(st.fields[i].v, False, False);
    wr.W(');');
    WriteLnCommentForOffset(st.fields[i].v.Offset);
  end;
  wr.DecIdent;
  wr.w('end');
end;

function TCodeConvertor.NextAuxTypeName(const Prefix:AnsiString):AnsiString;
begin
  if Prefix='' then Result:='AuxType'+IntToStr(AuxTypeCounter)
  else Result:=Prefix+IntToStr(AuxTypeCounter);
  inc(AuxTypeCounter);
end;

procedure TCodeConvertor.DeclarePasType(TypeEntity: TEntity; const PasTypeName: AnsiString);
begin
  SetPasSection(wr, 'type');
  wr.W(PasTypeName + ' = ');
  if TypeEntity is TStructType then
    WriteStruct(TStructType(TypeEntity))
  else if TypeEntity is TEnumType then begin
    WriteEnum(TEnumType(TypeEntity))
  end else if TypeEntity is TUnionType then begin
    WriteUnion(TUnionType(TypeEntity))
  end else if TypeEntity is TSimpleType then
    wr.W( cfg.GetTypeName(TSimpleType(TypeEntity).Name))
  else begin
    {SetPasSection(wr, 'type');
    wr.W(PasTypeName + ' = ');}
    wr.W('todo: '+TypeEntity.ClassName);
  end;
  //todo: ...
end;

function TCodeConvertor.FindCommentForLine(ln:Integer):TComment;
var
  i : Integer;
begin
  Result:=nil;
  if not Assigned(CmtList) then Exit;
  for i:=0 to CmtList.Count-1 do
    if Breaker.LineNumber(TComment(CmtList[i]).Offset)=ln then begin
      Result:=TComment(CmtList[i]);
      Exit;
    end;
end;

procedure TCodeConvertor.DefFuncWrite(wr:TCodeWriter;const FuncName,FuncRetType:AnsiString;
  const Params,ParamTypes: array of AnsiString);
var
  isProc  : Boolean;
  tp      : AnsiString;
  p       : AnsiString;
  i       : Integer;

const
  FnKind : array [Boolean] of AnsiString = ('procedure','function');
begin
  isProc:=FuncRetType<>'';

  wr.W ( FnKind[isProc]  );
  if FuncName<>'' then wr.W(' '+FuncName);
  if length(Params)>0 then begin
    tp:=ParamTypes[0];
    p:='';
    wr.W('(');
    for i:=0 to length(Params)-1 do begin
      if ParamTypes[i]=tp then begin
        if p='' then p:=Params[i] else p:=p+', '+Params[i];
      end else begin
        wr.W(p+': '+tp+'; ');
        p:=Params[i];
        tp:=ParamTypes[i];
      end;
    end;
    wr.W(p+': '+tp+')');
  end;
  if FuncRetType<>'' then wr.W(': '+FuncRetType);
end;


{ TConvertSettings }

procedure FillPasReserved(st: TStrings);
begin
  with st do
  begin
    // turbo pascal reserved
    Add('absolute');
    Add('and');
    Add('array');
    Add('asm');
    Add('begin');
    Add('case');
    Add('const');
    Add('constructor');
    Add('destructor');
    Add('div');
    Add('do');
    Add('downto');
    Add('else');
    Add('end');
    Add('file');
    Add('for');
    Add('function');
    Add('goto');
    Add('if');
    Add('implementation');
    Add('in');
    Add('inherited');
    Add('inline');
    Add('interface');
    Add('label');
    Add('mod');
    Add('nil');
    Add('not');
    Add('object');
    Add('of');
    Add('on');
    Add('operator');
    Add('or');
    Add('packed');
    Add('procedure');
    Add('program');
    Add('record');
    Add('reintroduce');
    Add('repeat');
    Add('self');
    Add('set');
    Add('shl');
    Add('shr');
    Add('string');
    Add('then');
    Add('to');
    Add('type');
    Add('unit');
    Add('until');
    Add('uses');
    Add('var');
    Add('while');
    Add('with');
    Add('xor');
    // object pascal reserved
    Add('as');
    Add('class');
    Add('dispinterface');
    Add('except');
    Add('exports');
    Add('finalization');
    Add('finally');
    Add('initialization');
    Add('inline');
    Add('is');
    Add('library');
    Add('on');
    Add('out');
    Add('packed');
    Add('property');
    Add('raise');
    Add('resourcestring');
    Add('threadvar');
    Add('try');
    // free pascal reserved
    Add('dispose');
    Add('exit');
    Add('false');
    Add('new');
    Add('true');
    // modifiers
    Add('absolute');
    Add('abstract');
    Add('alias');
    Add('assembler');
    Add('cdecl');
    Add('cppdecl');
    Add('default');
    Add('export');
    Add('external');
    Add('far');
    Add('far16');
    Add('forward');
    Add('index');
    Add('local');
    Add('name');
    Add('near');
    Add('nostackframe');
    Add('oldfpccall');
    Add('override');
    Add('pascal');
    Add('private');
    Add('protected');
    Add('public');
    Add('published');
    Add('read');
    Add('register');
    Add('reintroduce');
    Add('safecall');
    Add('softfloat');
    Add('stdcall');
    Add('virtual');
    Add('write');
    // common types
    Add('integer');
    Add('char');
    Add('longword');
    Add('word');
    Add('qword');
    Add('int64');
    Add('byte');
  end;
end;

constructor TConvertSettings.Create;
begin
  UsedNames := TStringList.Create;
  UsedNames.CaseSensitive := False;
  FillPasReserved(UsedNames);
  EnumsAsConst := True;
  FuncsAreExternal := True;
  RecordsArePacked := True;

  DefaultCType := 'int';
  FuncConv := 'cdecl';
  FuncDeclPostfix:='';
  TypeNamePrefix := '';
  RefTypeNamePrefix := 'P';
  ParamPrefix:='par';

  CtoPasTypes := TStringList.Create;
  CtoPasTypes.Values['bool'] := 'LongBool';
  CtoPasTypes.Values['double'] := 'Double';
  CtoPasTypes.Values['float'] := 'Single';
  CtoPasTypes.Values['float*'] := 'PSingle';
  CtoPasTypes.Values['int'] := 'Integer';
  CtoPasTypes.Values['int*'] := 'PInteger';
  CtoPasTypes.Values['void'] := '';
  CtoPasTypes.Values['void*'] := 'Pointer';
  CtoPasTypes.Values['void**'] := 'PPointer';
  CtoPasTypes.Values['char'] := 'Char';
  CtoPasTypes.Values['char*'] := 'PChar';
  CtoPasTypes.Values['char**'] := 'PPChar';
  CtoPasTypes.Values['signed char'] := 'SmallInt';
  CtoPasTypes.Values['long'] := 'Longword';
  CtoPasTypes.Values['long*'] := 'PLongword';
  CtoPasTypes.Values['long long'] := 'Int64';
  CtoPasTypes.Values['long long*'] := 'PInt64';
  CtoPasTypes.Values['unsigned long long'] := 'QWord';
  CtoPasTypes.Values['unsigned long long*'] := 'PQWord';
  CtoPasTypes.Values['short'] := 'SmallInt';
  CtoPasTypes.Values['short*'] := 'PSmallInt';
  CtoPasTypes.Values['unsigned'] := 'LongWord';
  CtoPasTypes.Values['unsigned short'] := 'Word';
  CtoPasTypes.Values['unsigned short*'] := 'PWord';
  CtoPasTypes.Values['unsigned char'] := 'Byte';
  CtoPasTypes.Values['unsigned char*'] := 'PByte';
  CtoPasTypes.Values['unsigned long'] := 'LongWord';
  CtoPasTypes.Values['unsigned int'] := 'LongWord';
  CtoPasTypes.Values['unsigned long int'] := 'LongWord';
  CtoPasTypes.Values['signed long'] := 'Integer';
  CtoPasTypes.Values['...'] := 'array of const';
  CtoPasTypes.Values['va_list'] := 'array of const';
end;

destructor TConvertSettings.Destroy;
begin
  CtoPasTypes.Free;
  UsedNames.Free;
  inherited Destroy;
end;

function TConvertSettings.GetUniqueName(const n: ansistring): ansistring;
begin
  Result := n;
  while UsedNames.IndexOf(Result) >= 0 do
    Result := Result + '_';
end;

function TConvertSettings.GetTypeName(const CTypeName: ansistring): ansistring;
begin
  Result := CtoPasTypes.Values[CTypeName];
  if (Result = '') and (CTypeName<>'void') then
  begin
    Result := TypeNamePrefix + CTypeName;
    Result := GetUniqueName(Result);
  end;
end;

function PasExp(x: TExpression): AnsiString;
var
  i : Integer;
begin
  Result:='';
  for i:=0 to x.Count-1 do begin
    if x.Tokens[i].TokenType=tt_Symbol then
      Result:=Result+CtoPasSymbol(x.Tokens[i].Token)+' '
    else
      Result:=Result+x.Tokens[i].Token+' ';
  end;
  Result:=Copy(Result, 1, length(Result)-1);
end;


end.

