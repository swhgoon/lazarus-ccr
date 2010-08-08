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
function ConvertCode(const t: AnsiString; var endPoint: TPoint; cfg: TConvertSettings = nil): AnsiString;

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

    procedure WriteLnCommentForOffset(AOffset: Integer);
    function NextCommentBefore(AOffset: Integer): Integer;
    procedure WriteLnCommentsBeforeOffset(AOffset: Integer);

    procedure WriteFuncDecl(const FnName, PasRetType: Ansistring; const params : array of TFuncParam);
    procedure WriteFuncOrVar(cent: TVarFuncEntity; StartVar: Boolean); // todo: deprecate!
    procedure WriteTypeDef(tp: TTypeDef);
    procedure WriteEnum(en: TEnumType);
    procedure WriteEnumAsConst(en: TEnumType);
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

procedure WriteArray(arr: TNamePart; wr: TCodeWriter);
var
  i : Integer;
begin
  wr.W('array ');
  for i := 0 to length(arr.arrayexp) - 1 do wr.W('[0..' + arr.arrayexp[i].Text + '-1]');
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

function ConvertCode(const t: AnsiString; var endPoint: TPoint; cfg: TConvertSettings): AnsiString;
var
  p         : TTextParser;
  ent       : TEntity;
  i         : integer;
  le        : integer;
  cnv       : TCodeConvertor;
  macros    : TCMacroHandler;
  owncfg    : Boolean;
begin
  Result:='';
  ent:=nil;
  owncfg:=not Assigned(cfg);
  if owncfg then cfg := TConvertSettings.Create;
  try
    macros:=TCMacroHandler.Create;

    if cfg.CustomDefines<>'' then PrepareMacros(cfg.CustomDefines, macros);

    p := CreateCParser(t);
    p.MacroHandler:=macros;
    try
      try
        ent := ParseNextEntityOrComment(p);
      except
        on E: Exception do Result:='error while parsing C-code: '+e.Message;
      end;

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

      if Assigned(ent) then begin

        cnv := TCodeConvertor.Create(cfg);
        try
          cnv.WriteCtoPas(ent, p.Comments, t);
        except
          on e: Exception do Result:=Result+LineEnding+ 'error while converting C code: ' + e.Message;
        end;
        Result := cnv.wr.Text;
        cnv.Free;
      end else
        Result:='unable to parse C expression';

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
  wr.W('0 {todo writeexp}');
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
    //wr.Wln(cfg.GetUniqueName(cent._Name) + ' = ' + Trim(cent.SubsText)+';');
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

procedure TCodeConvertor.WriteLnCommentForOffset(AOffset:Integer);
var
  cmt : TComment;
begin
  cmt:=FindCommentForLine( Breaker.LineNumber(AOffset));
  if Assigned(cmt) then begin
    LastOffset:=cmt.Offset;
    wr.W('  ');
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
    WriteLnCommentForOffset(i);
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
function GetCDeclTypeName(ent: TEntity): AnsiString;
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
      CtypeName:=GetCDeclTypeName(RetType);
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
    Result:=(params[0].prmtype is TSimpleType) and (TSimpleType(params[0].prmtype).Name='void');
end;

procedure TCodeConvertor.WriteFuncDecl(const FnName, PasRetType: Ansistring; const params : array of TFuncParam);
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

procedure TCodeConvertor.WriteFuncOrVar(cent: TVarFuncEntity; StartVar: Boolean);
var
  i, j  : integer;
  Name  : TNamePart;
  n     : TNamePart;
  id    : AnsiString;
  ref   : TNamePart;
  rt    : AnsiString;
begin
  for j := 0 to cent.Names.Count - 1 do
  begin
    Name:=GetIdPart(TNamePart(cent.Names[j]));
    if not Assigned(name) then begin
      wr.Wln(' bad declaration synax!');
      Exit;
    end;
    id:=name.Id;
    n:=name.owner;

    if not Assigned(n) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ' + GetPasTypeName(cent.RetType, Name))
    end else if (n.Kind=nk_Func) then begin
      SetPasSection(wr, '');
      rt:=GetPasTypeName(cent.RetType, n.owner);
      WriteFuncDecl(id, rt, n.params);
      if cfg.FuncsAreExternal then wr.W('; external');
    end else if (n.Kind=nk_Ref) then begin
      if StartVar then SetPasSection(wr, 'var');
      wr.W(id + ' : ');
      ref:=n;
      n:=n.owner;
      if not Assigned(n) then
        wr.W( GetPasTypeName(cent.RetType, ref) )
      else
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

    WriteLnCommentForOffset(cent.Offset)
  end;
end;

procedure TCodeConvertor.WriteCtoPas(cent: TEntity; comments: TList; const ParsedText: AnsiString);
begin
  CmtList:=comments;
  Breaker:=TLineBreaker.Create;
  Breaker.SetText(ParsedText);

  if cent is TVarFuncEntity then
    WriteFuncOrVar(cent as TVarFuncEntity, True)
  else if cent is TTypeDef then
    WriteTypeDef(cent as TTypeDef)
  else if cent is TStructType then
    DeclarePasType(cent as TStructType, TStructType(cent).Name)
  else if cent is TEnumType then
    DeclarePasType(cent as TEnumType, TEnumType(cent).Name)
  else if cent is TComment then
    WriteCommentToPas(cent as TComment)
  else if cent is TCPrepDefine then
    WritePreprocessor(cent as TCPrepDefine)
  else
    wr.Wln(cent.ClassName);

  Breaker.Free;
end;

procedure TCodeConvertor.WriteTypeDef(tp: TTypeDef);
var
  nm   : TNamePart;
  n    : TNamePart;
  fn   : TNamePart;
  rt   : AnsiString;
  stn  : AnsiString;
  tpart : TNamePart;
begin
  nm:=GetIdPart(tp.name);
  if not Assigned(nm) then Exit;
  SetPasSection(wr,'type');

  n:=nm.owner;

  if not Assigned(n) then begin
    stn:=GetCDeclTypeName(tp.origintype);
    if stn='' then stn:=nm.Id;
    DeclarePasType(tp.origintype, stn);
    if stn<>nm.Id then begin
      wr.Wln(';');
      wr.W(nm.Id+' = '+stn);
    end;

  end else begin
    fn:=n.owner;
    if n.Kind=nk_Array then begin
      wr.W(nm.Id+' = ');
      WriteArray(n, wr);
      wr.W(GetPasTypeName(tp.origintype, n.owner));
    //typedef functions and typedef function pointers are converted the same way.
    end else if (n.Kind=nk_Ref) and (not Assigned(fn) or (fn.Kind<>nk_Func)) then begin
      wr.W(nm.Id+' = '+GetPasTypeName(tp.origintype, n));
      fn:=n.owner;
    end else if isNamePartPtrToFunc(n) or (Assigned(n) and (n.kind=nk_Func) ) then begin

      if isNamePartPtrToFunc(n) then begin
        tpart:=n.owner.owner // rettype of function pointer
      end else begin
        tpart:=n.owner;
        cfg.CtoPasTypes.Values[nm.id+'*']:=nm.id;
      end;

      PushWriter;
      rt := GetPasTypeName(tp.origintype, tpart);
      PopWriter;

      if n.Kind=nk_Func then fn:=n;
      DeclareFuncType(nm.id, rt, fn.params);
    end;
  end;
  wr.Wln(';');
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
    WriteFuncOrVar(st.fields[i].v, False);
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
begin
  if length(en.items)>0 then begin
    PushWriter;
    WriteLnCommentsBeforeOffset(en.Offset);
    SetPasSection(wr, 'const');
    for i:=0 to length(en.items)-1 do begin
      WriteLnCommentsBeforeOffset(en.items[i].Offset);
      wr.W(en.items[i].Name + ' = ');
      if Assigned(en.items[i].Value) then
        WriteExp(en.items[i].Value)
      else
        wr.W(IntToStr(i));
      wr.W(';');
      WriteLnCommentForOffset(en.items[i].Offset);
    end;
    PopWriter;
  end;
  wr.W('Integer');
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
  else if TypeEntity is TEnumType then
    WriteEnum(TEnumType(TypeEntity))
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
  CtoPasTypes.Values['char*'] := 'PChar';
  CtoPasTypes.Values['char**'] := 'PPChar';
  CtoPasTypes.Values['long'] := 'Longword';
  CtoPasTypes.Values['long*'] := 'PLongword';
  CtoPasTypes.Values['long long'] := 'Int64';
  CtoPasTypes.Values['long long*'] := 'PInt64';
  CtoPasTypes.Values['unsigned long long'] := 'QWord';
  CtoPasTypes.Values['unsigned long long*'] := 'PQWord';
  CtoPasTypes.Values['short'] := 'SmallInt';
  CtoPasTypes.Values['short*'] := 'PSmallInt';
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

end.

