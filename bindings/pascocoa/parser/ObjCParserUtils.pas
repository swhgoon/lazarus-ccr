{
 ObjCParserUtils.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 converting obj-c to objfpc unit
}

//todo: a lot of things =)

unit  ObjCParserUtils;

interface

  {$ifdef fpc}{$mode delphi}{$H+}{$endif fpc}

uses
  Classes, SysUtils, ObjCParserTypes;
  
procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);

function ObjCToDelphiType(const objcType: AnsiString; isPointer: Boolean): AnsiString;

function StrFromFile(const FileName: AnsiString): AnsiString;

function IsMethodConstructor(cl: TClassDef; m: TClassMethodDef): Boolean;
function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
function GetProcFuncHead(const FuncName, OfClass, Params, ResType: AnsiString; const FuncDest: AnsiString = ''): AnsiString;
function GetMethodParams(const m: TClassMethodDef): AnsiString;
function GetMethodResultType(const m: TClassMethodDef): AnsiString;
function IsPascalReserved(const s: AnsiString): Boolean;

implementation

function IsPascalReserved(const s: AnsiString): Boolean;
var
  ls  : AnsiString;
begin
  //todo: a hash table should be used?
  Result := true;
  if s = '' then Exit;
  ls := AnsiLowerCase(s);
  case ls[1] of
    'a': Result := (ls = 'absolute') or (ls = 'abstract') or (ls = 'and') or (ls = 'array') or (ls = 'as') or (ls= 'asm') or (ls = 'assembler');
    'b': Result := (ls = 'begin') or (ls = 'break');
    'c': Result := (ls = 'cdecl') or (ls = 'class') or (ls = 'const') or (ls = 'constructor') or (ls = 'continue') or (ls = 'cppclass');
    'd': Result := (ls = 'deprecated') or (ls = 'destructor') or (ls = 'div') or (ls = 'do') or (ls = 'downto');
    'e': Result := (ls = 'else') or (ls = 'end') or (ls = 'except') or (ls = 'exit') or (ls = 'export') or (ls = 'exports') or (ls = 'external');
    'f': Result := (ls = 'fail') or (ls = 'false') or (ls = 'far') or (ls = 'file') or (ls = 'finally') or (ls = 'for') or (ls = 'forward') or (ls = 'function');
    'g': Result := (ls = 'goto');
    'i':
      Result := (ls = 'if') or (ls = 'implementation') or (ls = 'in') or (ls = 'index') or (ls = 'inherited') or (ls = 'initialization') or (ls = 'inline')
        or  (ls = 'interface') or (ls = 'interrupt') or (ls = 'is');
    'l': Result := (ls = 'label') or (ls = 'library');
    'm': Result := (ls = 'mod');  
    'n': Result := {(ls = 'name') or} (ls = 'near') or (ls = 'nil') or (ls = 'not');
    'o': Result := (ls = 'object') or (ls = 'of') or (ls = 'on') or (ls = 'operator') or (ls = 'or') or (ls = 'otherwise');
    'p':
      Result := (ls = 'packed') or (ls = 'popstack') or (ls = 'private') or (ls = 'procedure') or (ls = 'program') or (ls = 'property')
        or (ls = 'protected') or (ls = 'public');
    'r': Result := (ls = 'raise') or (ls = 'record') or (ls = 'reintroduce') or (ls = 'repeat');
    's': Result := (ls = 'self') or (ls = 'set') or (ls = 'shl') or (ls = 'shr') or (ls = 'stdcall') or (ls = 'string');
    't': Result := (ls = 'then') or (ls = 'to') or (ls = 'true') or (ls = 'try') or (ls = 'type');
    'u': Result := (ls = 'unimplemented') or (ls = 'unit') or (ls = 'until') or (ls = 'uses');
    'v': Result := (ls = 'var') or (ls = 'virtual');
    'w': Result := (ls = 'while') or (ls = 'with');
    'x': Result := (ls = 'xor');
  end;



end;

function GetMethodResultType(const m: TClassMethodDef): AnsiString;
var
  res : TObjCResultTypeDef;
begin
  res := m.GetResultType;
  if not Assigned(res) then Result := ''
  else Result := ObjCToDelphiType(m.GetResultType._Name, m.GetResultType._IsPointer);
end;

function GetMethodParams(const m: TClassMethodDef): AnsiString;
var
  i     : Integer;
  p     : TObject;
  vname : AnsiString;
  vtype : AnsiString;
begin
  Result := '';
  vname := '';
  vtype := '';
  for i := 0 to m.Items.Count - 1 do begin
    p := TObject(m.Items[i]);
    if p is TParamDescr then
      vname := TParamDescr(p)._Descr
    else if p is TObjCParameterDef then begin
      if vname = '' then vname := TObjCParameterDef(p)._Name;
      vtype := ObjCToDelphiType(TObjCParameterDef(p)._Res._Name, TObjCParameterDef(p)._Res._IsPointer);
      if Result <> '' then Result := Result + '; ';
      Result := Result + vname + ': ' + vtype;
      vname := '';
    end;
  end;

end;

function GetProcFuncHead(const FuncName, OfClass, Params, ResType, FuncDest: AnsiString): AnsiString;
begin
  if FuncDest = '' then begin
    if ResType = '' then Result := 'procedure '
    else Result := 'function ';
  end else
    Result := FuncDest + ' ';

  if OfClass <> '' then Result := Result + OfClass+'.';
  Result := Result + FuncName;
  if Params <> '' then
    Result := Result + '('+Params+')';
  if ResType <> '' then Result := Result+': '+ResType;
  Result := Result + ';';
end;

function StrFromFile(const FileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    fs.Read(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

function ObjCToDelphiType(const objcType: AnsiString; isPointer: Boolean): AnsiString;
var
  l : AnsiString;
begin
  Result := objcType;
  l := AnsiLowerCase(objcType);
  if l = '' then Exit;
  case l[1] of
    'v':
      if l = 'void' then begin
        if not isPointer then Result := ''
        else Result := 'Pointer';
      end;
    'i':
      if l = 'id' then Result := 'objc.id'
      else if l = 'int' then Result := 'Integer';
    'b':
      if l = 'bool' then Result := 'LongBool';
    'l':
      if l = 'long' then Result := 'Integer';
    's':
      if l = 'short' then Result := 'SmallInt';
    'u':
      if (l = 'unsigned long') or (l = 'usigned int') then
        Result := 'LongWord'
      else if (l = 'unsigned short') then
        Result := 'Word';
    'f':
      if l = 'float' then Result := 'Single';
  end;
end;


function IsMethodConstructor(cl: TClassDef; m: TClassMethodDef): Boolean;
var
  res : TObjCResultTypeDef;
  l   : AnsiString;
begin
  Result := m._IsClassMethod;
  if not Result then begin
    //todo: C is case sensetive, so is it possible to have a initialing function name like
    //      'InitWithSomething', rather than 'initWithSomething' (that is should be)???
    //todo: to make sure, it's not a name,like 'Initialzation';
    l := AnsiLowerCase(m._Name);
    if Pos('init', l) = 1 then Result := true;
  end;
  if not Result then Exit;

  res := m.GetResultType;
  l := res._Name;
  Result := (l = 'id') or (l = cl._ClassName);
end;

function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
var
//  i     : integer;
  ft    : AnsiString;
  res   : AnsiString;
begin
  res := GetMethodResultType(m);
  if IsMethodConstructor(cl, m) then begin
    ft := 'constructor';
    res := '';
  end else
    ft := '';

  if ForImplementation
    then Result := GetProcFuncHead(m._Name, cl._ClassName, GetMethodParams(m), res, ft)
    else Result := GetProcFuncHead(m._Name, '', GetMethodParams(m), res, ft)
end;

// returns define pas file name form Objective C name, like
// NSApplication.h  ->  NSAPPLICATION_PAS_H
// SomePath/SomePath/SomeFileName.h -> SOMEFILENAME_PAS_H
function GetIfDefFileName(const FileName, DefExt: AnsiString): AnsiString;
var
  i : integer;
//  s : AnsiString;
begin
  //todo: don't like it...
  Result := Copy(FileName, 1, length(FileName) - length(ExtractFileExt(FileName)));
  Result := AnsiUpperCase(Result);
  for i := 1 to length(Result) do
    if Result[i] = '.' then
      Result[i] := '_';
  Result := Result + '_PAS_'+DefExt;
end;

// returns include pas file name form Objective C name, like
// <AppKit/NSApplication.h>  ->  NSApplication.inc
// "SomePath/SomePath/SomeFileName.h> -> SomeFileName.h
function GetIncludeFile(const s: AnsiString): AnsiString;
var
  i   : Integer;
begin
  //todo: don't like it...
  Result := '';
  if s = '' then Exit;
//  i := length(s);
{  if (s[i] = '"') or (s[i] = '>') then
    dec(i);}
  i := length(s) - 1;
  // dummy, but it works =)
  while (i > 0) and (s[i] in ['.', 'A'..'Z', 'a'..'z', '0'..'9']) do dec(i);

  Result := Copy(s, i + 1, length(s) - i);
  if Result <> '' then begin
    if Result[length(Result)] in ['"', '>'] then Result :=
      Copy(Result, 1, length(Result) - 1);
    Result := Copy(Result, 1, length(Result) - length(ExtractFileExt(Result))) + '.inc';
  end;
end;

// returns pascal style of precomiler "if defined" section
// exclusion is done for Cocoa known precompiler definion, for ex:
// MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3 -> MAC_OS_X_VERSION_10_3
// any other #ifdef excpresions would be passed "as is" even if are incorrect
// for pascal
function PrecompileIfDefToPascal(const prm: AnsiString): AnsiString;
var
  i   : Integer;
const
  VerExclude = 'MAC_OS_X_VERSION_MAX_ALLOWED >=';
begin
  // really slow... and... don't like this anyway!
  Result := prm;
  i := Pos(VerExclude, prm);
  if i > 0 then begin
    i := i + length(VerExclude);
    while (i <= length(Result)) and (Result[i] = ' ') do inc(i);
    if i <= length(Result) then
      Result := Copy(prm, i, length(Result) - i + 1);
  end;
end;

// converts TProcpmiler entity to pascal entity
// #import or #include -> {$Include Something.inc}
// #define SOMETHING -> {$define SOMETHING}
// #ifdef SOMETHING -> {$ifdef SOMETHING}
// etc...
function WriteOutPrecompToPascal(Prec: TPrecompiler): AnsiString;
var
  dir : AnsiString;
begin
  dir := AnsiLowerCase(Prec._Directive);
  if (dir = '#import') or (dir = '#include') then
    Result := Format('{$include %s}', [GetIncludeFile(Prec._Params)])
  else if (dir = '#if') then
    Result := Format('{$ifdef %s}', [PrecompileIfDefToPascal(Prec._Params)])
  else if (dir = '#else') then
    Result := '{$else}'
  else if (dir = '#endif') then
    Result := '{$endif}';
end;

procedure WriteOutCommentStr(const AComment, Prefix: AnsiString; Subs: TStrings);
var
  i     : Integer;
  j     : Integer;
  k     : Integer;
  cmtln : AnsiString;
begin
  i := 1;
  while i <= length(AComment) do begin
    // scan for multylined comments
    cmtln := ScanTo(AComment, i, [#10, #13]);
    if i < length(AComment) then begin
      if (AComment[i] = #10) and (AComment[i+1] = #13) then inc(i)
      else if (AComment[i] = #13) and (AComment[i+1] = #10) then inc(i);
    end;
    inc(i);

    // break long comments into lines
    j := 1;
    while j <= length(cmtln) do begin
      k := j;
      inc(j, 80);
      if j > length(cmtln) then j := length(cmtln);
      ScanTo(cmtln, j, [#32, #10, #13, #9]);
      subs.Add(Prefix + '// ' + Copy(cmtln, k, j - k));
      inc(j);
    end;
  end;
end;

procedure WriteOutIfComment(Items: TList; Index: Integer; const Prefix: AnsiString; Subs: TStrings);
var
  j   : integer;
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;
  
  j := Index;
  while (j >= 0) and (TObject(Items[j]) is TComment) do dec(j);
  inc(j);
  for j := j to index do
  //if TObject(Items[Index]) is TComment then
    WriteOutCommentStr( TComment(Items[j])._Comment, Prefix, Subs);
end;

// clears empty precompile statements, like
// {$ifdef SOMETHING}
// {$endif}
// and
// {$ifdef SOMETHING}
// {$else}
// {$endif}
// will be removed
procedure ClearEmptyPrecompile(subs: TStrings);
var
  i   : integer;
  j   : Integer;
begin
  // don't like it either...
  i := subs.Count - 1; if i < 0 then Exit;
  j := i;
  
  if Pos('{$endif', subs[i]) = 0 then Exit;
  dec(i); if i < 0 then Exit;
  
  if Pos('{$else', subs[i]) > 0 then
    dec(i); if i < 0 then Exit;
  
  if Pos('{$ifdef', subs[i]) > 0 then
    for i := j downto i do
      subs.Delete(i);
end;

procedure BeginSection(const SectionName: AnsiString; st: TStrings);
begin
  st.Add('{$ifdef '+SectionName+'}');
end;

procedure BeginExcludeSection(const DefineName: AnsiString; st: TStrings);
begin
  st.Add('{$ifndef '+DefineName+'}');
  st.Add('{$define '+DefineName+'}');
end;

procedure EndSection(st: TStrings);
begin
  st.Add('{$endif}');
end;

// todo: remove Prefix param...
procedure WriteOutIfDefPrecompiler(prec: TPrecompiler; const Prefix: AnsiString; subs: TStrings);
var
  ppas  : AnsiString;
  isend : Boolean;
begin
  ppas := WriteOutPrecompToPascal(prec);
  isend := IsSubStr('{$endif', ppas, 1);
  if isend or IsSubStr('{$ifdef', ppas, 1) or IsSubStr('{$else', ppas, 1) then
    subs.Add(Prefix + ppas);
  if isend then ClearEmptyPrecompile(subs);
end;

procedure WriteOutClassToHeader(cl : TClassDef; subs: TStrings; conststr: TStrings);
var
  i   : Integer;
//  j   : Integer;
  s   : AnsiString;
  ss  : AnsiString;
  mtd : TClassMethodDef;
  obj : TObject;
begin
  if conststr.IndexOf(cl._ClassName) < 0 then begin
    conststr.Add(cl._ClassName);
    s := Format('  Str_%s = '#39'%s'#39';', [cl._ClassName, cl._ClassName]);
    subs.Add(s);
  end;
  for i := 0 to cl.Items.Count - 1 do begin
    obj := TObject(cl.Items[i]);
    if obj is TClassMethodDef then begin
      mtd := TClassMethodDef(cl.Items[i]);
      if conststr.IndexOf(mtd._Name) < 0 then begin
        conststr.Add(mtd._Name);
        ss := Format('  Str_%s = '#39'%s'#39';', [mtd._Name, mtd._Name]);
        subs.add(ss);
      end;
    end else if obj is TPrecompiler then begin
      WriteOutIfDefPrecompiler(TPrecompiler(obj), '  ', subs);
    end;
  end; {of for}
  subs.Add('');
end;

procedure ParseDefine(const s: AnsiString; var DefWhat, DefTo: AnsiString);
var
  i   : Integer;
begin
  i := 1;
  ScanWhile(s, i, [#9, #32, #10, #13]);
  if i < length(s) then begin
    DefWhat := ScanTo(s, i, [#9, #32, #10, #13]);
    ScanWhile(s, i, [#9, #32]);
    DefTo := Copy(s, i, length(s) - i + 1);
  end else
    DefTo := '';
end;

procedure WriteOutPrecompDefine(const Prec: TPrecompiler; Prefix: AnsiString; st: TStrings);
var
  a, b: AnsiString;
begin
  if Prec._Directive = '#define' then begin
    ParseDefine(Prec._Params, a, b);
    if b <> ''
      then st.Add(Prefix + Format('%s = %s;', [a, b]))
      else st.Add(Prefix + Format('{$define %s}', [a])); 
  end;
end;

procedure WriteOutPrecompInclude(Prec: TPrecompiler; st: TStrings);
var
  dlph  : AnsiString;
begin
  dlph := WriteOutPrecompToPascal(Prec);
  if IsSubStr('{$include', dlph, 1) then st.Add(dlph);
end;

function GetPascalEnumValue(const Name, Param: AnsiString): AnsiString;
begin
  Result := Name;
  if Param <> '' then
    Result := Result + ' = ' + Param
end;


function ReplaceStr(const sub, subrep, s: AnsiString): AnsiString;
var
  i   : Integer;
  j   : Integer;
begin
  i := Pos(sub, s);
  if i = 0 then begin
    Result := s;
    Exit;
  end;
  j := i + length(sub);
  Result := Copy(s, 1, i - 1) + subrep + Copy(s, j, length(s) - j + 1);
end;

function GetPascalConstValue(const Vl: AnsiString): AnsiString;
begin
  //todo: improve! check at h2pas
  Result := ReplaceStr('<<', 'shl', vl);
  Result := ReplaceStr('>>', 'shr', Result);
end;

procedure WriteOutEnumValues(enm: TEnumTypeDef; const Prefix: AnsiString; st: TStrings);
var
  vl  : TEnumValue;
  s   : AnsiString;
  i   : Integer;
  j   : Integer;
begin
  j := st.Count;
  for i := 0 to enm.Items.Count - 1 do begin
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if st.Count > j then st[st.Count-1]:=st[st.Count-1]+', ';
      s := GetPascalEnumValue(vl._Name, GetPascalConstValue(vl._Value));
      s := Prefix + s;
      st.Add(s);
    end;
  end;
end;

function Min(a, b: Integer): Integer;
begin
  if a < b then Result := a
  else Result := b;
end;

procedure MatchFixes(const Name: AnsiString; var prefix, postfix: AnsiString);
var
  i     : integer;
  ni, pi: integer;
//  nc, pc: AnsiChar;
begin
  for i := 1 to Min(length(Name), length(prefix)) do
    if Name[i] <> prefix[i] then begin
      prefix := Copy(prefix, 1, i - 1);
      Break;
    end;
    
  ni := length(Name);
  pi := length(postfix);
  for i := 1 to Min(length(Name), length(postfix)) do begin
    if Name[ni] <> postfix[pi] then begin // this cause a bug
      postfix := Copy(Name, ni + 1, length(Name) - ni);
      Break;
    end;
    dec(ni);
    dec(pi);
  end;
end;

function EvaluateEnumName(enm: TEnumTypeDef): AnsiString;
var
  prefix  : AnsiString;
  postfix : AnsiSTring;
  vl      : TEnumValue;
  known   : integer;
  i       : Integer;
begin
  known := 0;
  Result := '';
  for i := 0 to enm.Items.Count - 1 do begin
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if known = 0 then begin
        prefix := vl._Name;
        postfix := vl._Name;
      end else
        MatchFixes(vl._Name, prefix, postfix);
      inc(known)
    end;
  end;
  if (known <= 1) or (length(Result) < 3) then Result := 'todoEnumName' // if only one enumaration or none, name cannot be defined...
  else Result := prefix + postfix;
end;

procedure WriteOutEnumToHeader(enm: TEnumTypeDef; st: TStrings);
var
//  i   : Integer;
  s   : AnsiString;
begin
  if enm._Name = '' then s := EvaluateEnumName(enm)
  else s := enm._Name;
  st.Add(Format('  %s = (', [s] ));
  WriteOutEnumValues(enm, '    ', st );
  st.Add('  );');
  st.Add('');
end;

procedure WriteOutTypeDefToHeader(typedef: TTypeNameDef; const Prefix: AnsiString; subs: TStrings);
begin
  subs.Add( Prefix + Format('%s = %s;', [typedef._TypeName, typedef._Inherited]));
end;

procedure WriteOutHeaderSection(hdr: TObjCHeader; st: TStrings);
var
  i       : Integer;
  cl      : TClassDef;
  subs    : TStringList;
//  s       : AnsiString;
  consts  : TStringList;
const
  SpacePrefix = '  ';
begin
  BeginSection('HEADER', st);
  BeginExcludeSection( GetIfDefFileName(hdr._FileName, 'H'), st);
  subs := TStringList.Create;
  consts := TStringList.Create;
  
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin
        if (TObject(hdr.Items[i]) is TClassDef) then begin
          cl := TClassDef(hdr.Items[i]);
          WriteOutClassToHeader(cl, subs, consts);
        end else if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutPrecompInclude(TPrecompiler(hdr.Items[i]), st);
          WriteOutPrecompDefine(TPrecompiler(hdr.Items[i]), '  ', subs);
        end;
      end;

    if subs.Count > 0 then begin
      st.Add('const');
      st.AddStrings(subs);
      subs.Clear;
    end;
    
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin
        if (TObject(hdr.Items[i]) is TEnumTypeDef) then begin
          WriteOutIfComment(hdr.Items, i - 1, SpacePrefix, subs);
          WriteOutEnumToHeader(TEnumTypeDef(hdr.Items[i]), subs);
        end else if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutIfDefPrecompiler(TPrecompiler(hdr.Items[i]), SpacePrefix, st);
        end else if (TObject(hdr.Items[i]) is TTypeNameDef) then begin
          WriteOutTypeDefToHeader(TTypeNameDef(hdr.Items[i]), SpacePrefix, subs);
        end;
      end; {of if}
    
    if subs.Count > 0 then begin
      st.Add('type');
      st.AddStrings(subs);
      subs.Clear;
    end;

  finally
    EndSection(st);
    EndSection(st);
    subs.Free;
    consts.Free;
  end;
end;

procedure WriteOutClassToClasses(cl: TClassDef; subs: TStrings);
var
  i   : Integer;
//  cnt : Integer;
  s   : AnsiString;
  j   : Integer;
  obj : TObject; // or TEntity
  
  mtds   : TStringList; // name of methods
//  over   : TStringList; // overloaded names
const
  SpacePrefix = '    ';
begin
  subs.Add('  { '+cl._ClassName +' }');
  subs.Add('');
  s := '  ' + cl._ClassName + ' = class';
  if cl._SuperClass <> '' then begin
    subs.Add(s + '('+cl._SuperClass+')');
    subs.Add('  public');
    subs.Add('    function getClass: objc.id; override;');
  end else begin
    subs.Add(s + '{from category '+ cl._Category +'}');
    subs.Add('  public');
  end;

  mtds := TStringList.Create;
  try
    for j := 0 to cl.Items.Count - 1 do begin
      obj := TObject(cl.Items[j]);
      if obj is TClassMethodDef then begin
        i := mtds.indexOf(TClassMethodDef(obj)._Name);
        if i < 0 then
          mtds.Add( TClassMethodDef(obj)._Name)
        else
          mtds.Objects[i] := TObject(Integer(mtds.Objects[i]) + 1);
      end;
    end;

    for j := 0 to cl.Items.Count - 1 do begin
      obj := TObject(cl.Items[j]);
      if obj is TClassMethodDef then begin
        WriteOutIfComment(cl.Items, j - 1, '    ', subs);
        s := GetMethodStr(cl, TClassMethodDef(cl.Items[j]), false);
        i := mtds.IndexOf(TClassMethodDef(cl.Items[j])._Name);
        if Integer(mtds.Objects[i]) > 0 then s := s + ' overload;';
        subs.Add(SpacePrefix + s);
      end else if obj is TPrecompiler then begin
        WriteOutIfDefPrecompiler(TPrecompiler(obj), SpacePrefix, subs);
      end;
    end;
  finally
    mtds.Free;
  end;
  
  subs.Add('  end;');
  subs.Add('');
end;

procedure WriteOutClassesSection(hdr: TObjCHeader; st: TStrings);
var
  i     : integer;
//  cl    : TClassDef;
//  j     : integer;
//  s     : AnsiString;
  subs  : TStringList;
begin
  BeginSection('CLASSES', st);
  BeginSection(GetIfDefFileName(hdr._FileName, 'C'), st);
  subs := TStringList.Create;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TPrecompiler) then
        WriteOutPrecompInclude(TPrecompiler(hdr.Items[i]), st);

    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TClassDef) then begin
        WriteOutIfComment(hdr.Items, i - 1, '    ', subs);
        WriteOutClassToClasses(TClassDef(hdr.Items[i]), subs);
      end;

    if subs.Count > 0 then begin
      st.Add('type');
      st.AddStrings(subs);
    end;
    
  finally
    EndSection(st);
    subs.Free;
  end;
end;

function isAnyParam(mtd: TClassMethodDef): boolean;
var
  i   : integer;
begin
  Result := false;
  for i := 0 to mtd.Items.Count - 1 do
    if TObject(mtd.Items[i]) is TObjCParameterDef then begin
      Result := true;
      Exit;
    end;
end;

const
  MtdPrefix = 'TMtd_';
  MtdPostfix = '';

procedure ObjCMethodToProcType(mtd: TClassMethodDef; var typeName: AnsiString; subs: TStrings);
var
//  i   : integer;
  s   : AnsiString;
begin
  typeName := MtdPrefix + mtd._Name + MtdPostFix;
  subs.Add('type');
//    function GetProcFuncHead(const FuncName, OfClass, Params, ResType, FuncDest: AnsiString): AnsiString;
  s := typeName + ' = ' + GetProcFuncHead('', '',  'param1: objc.id; param2: SEL; ' + GetMethodParams(mtd),  GetMethodResultType(mtd), '' );
  subs.Add('  ' + s + ' cdecl;');
end;

function GetParamsNames(mtd: TClassMethodDef): AnsiString;
var
  i   : Integer;
  obj : TObject;
  vname : AnsiString;
begin
  vname := '';
  Result := '';
  for i := 0 to mtd.Items.Count - 1 do begin
    obj := TObject(mtd.Items[i]);
    if obj is TParamDescr then begin
      if vName <> '' then Result := Result + vname + ', ';
      vname := TParamDescr(obj)._Descr;
    end else if obj is TObjCParameterDef then begin
      if vname = '' then vname := TObjCParameterDef(obj)._Name;
    end;
  end;
  Result := Result + vname;
//  Result := Copy(Result, 1, length(Result) - 2);
end;

procedure WriteOutMethodToImplementation(mtd: TClassMethodDef; subs: TStrings);
var
  cl  : TClassDef;
  res : Ansistring;
  sp  : AnsiString;
  s   : AnsiString;
//  isConsts  : Boolean;
  typeName  : AnsiString;
begin
  typeName := '';
  if not Assigned(mtd.Owner) or (not (TObject(mtd.Owner) is TClassDef)) then Exit; // method cannot be without owning class
  cl := TClassDef(mtd.Owner);
  
  subs.Add(GetMethodStr(cl, mtd, true));

  if IsMethodConstructor(cl, mtd) then begin
    subs.Add('begin');
    subs.Add('  //todo: constructors are not implemented, yet');
    subs.Add('end;');
  end else if not isAnyParam(mtd) then begin
    subs.Add('begin');
    try
      sp := Format('objc_msgSend(Handle, sel_registerName(PChar(Str_%s)), [])', [mtd._Name]);
      res := GetMethodResultType(mtd);
      
      if res <> '' then begin
        if res = 'objc.id' then sp := 'Result := ' +sp
        else sp := 'Result := '+res+'('+sp+')'
      end;
      subs.Add('  ' + sp+';');
    finally
      subs.Add('end;');
    end;
  end else begin
    ObjCMethodToProcType(mtd, typeName, subs);
    subs.Add('var');
    subs.Add(Format('  vmethod: %s;', [typeName]));
    subs.Add('begin');
    subs.Add(Format('  vmethod := %s(@objc_msgSend);', [typeName]));
    s := Format('vmethod(Handle, sel_registerName(PChar(Str_%s)), %s)', [mtd._Name, GetParamsNames(mtd)]);
    if ObjCToDelphiType(mtd.GetResultType._Name, mtd.GetResultType._IsPointer) <> '' then
      s := 'Result := ' + s;
    s := s + ';';
    subs.Add('  ' + s);
    subs.Add('end;');
  end;
  subs.Add('');
end;


procedure WriteOutClassToImplementation(cl: TClassDef; subs: TStrings);
var
  i   : integer;
  obj : TObject;
begin
  subs.Add('{ '+cl._ClassName + ' }');
  
  if cl._Category <> '' then begin
    subs.Add('  //todo: classes of category');
    Exit;
  end;
  
  subs.Add('');
  subs.Add(GetProcFuncHead('getClass', cl._ClassName, '', 'objc.id'));
  subs.Add('begin');
  subs.Add('  Result := objc_getClass(Str_'+cl._ClassName+');');
  subs.Add('end');
  subs.Add('');
  
  for i := 0 to cl.Items.Count - 1 do begin
    obj := TObject(cl.Items[i]);
    if obj is TClassMethodDef then
      WriteOutMethodToImplementation ( TClassMethodDef(cl.Items[i]), subs)
    else if obj is TPrecompiler then
      WriteOutIfDefPrecompiler( TPrecompiler(obj), '', subs);
  end;
end;

procedure WriteOutImplementationSection(hdr: TObjCHeader; st: TStrings);
var
  i   : Integer;
begin
  BeginSection('IMPLEMENTATION', st);
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin
        if (TObject(hdr.Items[i]) is TClassDef) then
          WriteOutClassToImplementation(TClassDef(hdr.Items[i]), st);
      end;
  finally
    EndSection(st);
  end;
end;


function AppleEnumType(items: TList; TypeDefIdx: Integer): Boolean;
var
  EnumIdx : integer;
  typedef : TTypeNameDef;
  enumdef : TEnumTypeDef;
const
  AppleInherit = 'NSUInteger';
begin
  Result := false;
  EnumIdx := TypeDefIdx - 1;
  if (EnumIdx < 0) or (EnumIdx >= items.Count) then Exit;
  
  if (TObject(items.Items[TypeDefIdx]) is TTypeNameDef) and
    (TObject(items.Items[EnumIdx]) is TEnumTypeDef) then begin
    typedef := TTypeNameDef(items.Items[TypeDefIdx]);
    enumdef := TEnumTypeDef(items.Items[EnumIdx]);
  end else
    Exit;

  if typedef._Inherited = AppleInherit then enumdef._Name := typedef._TypeName;
  Result := true;
end;



procedure FixAppleCategories(Items: TList; category: TClassDef);
var
  i   : Integer;
  j   : Integer;
  cl  : TClassdef;
begin
  for i := 0 to Items.Count - 1 do
    if TObject(Items[i]) is TClassDef then begin
      cl := TClassDef(Items[i]);
      if cl._SuperClass <> '' then
        for j := 0 to category.Items.Count - 1 do begin
          cl.Items.Add(category.Items[j]);
          TEntity(category.Items[j]).owner := cl;
        end; {of if}
    end; {of if}
end;

procedure AppleHeaderFix(ent : TEntity);
var
  i   : Integer;
  j   : Integer;
  obj : TEntity;
begin
//  i := 0;
  for i := 0 to ent.Items.Count - 1 do begin
    obj := TEntity(ent.Items[i]);
    if (obj is TTypeNameDef) and (AppleEnumType(ent.Items, i)) then begin
      ent.Items[i] := nil;
      obj.Free;
    end else if (obj is TClassDef) and (TClassDef(obj)._SuperClass = '') then begin
      FixAppleCategories(ent.Items, TClassDef(obj));
      ent.Items[i] := nil;
      obj.Free;
    end else if (obj is TParamDescr) then begin
      if IsPascalReserved(TParamDescr(obj)._Descr) then
        TParamDescr(obj)._Descr := '_'+TParamDescr(obj)._Descr;
    end else if (obj is TObjCParameterDef) then begin
      if IsPascalReserved(TObjCParameterDef(obj)._Name) then
        TObjCParameterDef(obj)._Name := '_' + TObjCParameterDef(obj)._Name;
    end;
  end;

  j := 0;
  for i := 0 to ent.Items.Count - 1 do
    if Assigned(ent.Items[i]) then begin
      ent.Items[j] := ent.Items[i];
      inc(j);
    end;
  ent.Items.Count := j;

  for i := 0 to ent.Items.Count - 1 do
    AppleHeaderFix( TEntity(ent.Items[i]));
end;

procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);
var
//  i   : integer;
  cmt : TComment;
begin
  try
    if hdr.Items.Count <= 0 then Exit;
    AppleHeaderFix(hdr);

    // .inc header-comment is the first comment entity in .h file , if any
    if TObject(hdr.Items[0]) is TComment then begin
      cmt := TComment(hdr.Items[0]);
      st.Add('(*' + cmt._Comment + '*)');
      cmt.Free;
      hdr.Items.Delete(0);
    end;

    WriteOutHeaderSection(hdr, st);
    WriteOutClassesSection(hdr, st);
    WriteOutImplementationSection(hdr, st);
  except
  end;
end;

end.
