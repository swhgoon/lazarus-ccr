{
 ObjCParserUtils.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 converting obj-c to objfpc unit
}

//todo: a lot of things =)

unit ObjCParserUtils;

interface

{$ifdef fpc}{$mode delphi}{$H+}{$endif fpc}

uses
  Classes, SysUtils, ObjCParserTypes;
  
procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);

function ObjCToDelphiType(const objcType: AnsiString): AnsiString;

function StrFromFile(const FileName: AnsiString): AnsiString;

function IsMethodConstructor(cl: TClassDef; m: TClassMethodDef): Boolean;
function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
function GetProcFuncHead(const FuncName, OfClass, Params, ResType: AnsiString; const FuncDest: AnsiString = ''): AnsiString;
function GetMethodParams(const m: TClassMethodDef): AnsiString;
function GetMethodResultType(const m: TClassMethodDef): AnsiString;

implementation

function GetMethodResultType(const m: TClassMethodDef): AnsiString;
begin
  if not Assigned(m.GetResultType) then Result := ''
  else Result := ObjCToDelphiType(m.GetResultType._TypeName);
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
    else if p is TParameterDef then begin
      if vname = '' then vname := TParameterDef(p)._Name;
      vtype := ObjCToDelphiType(TParameterDef(p)._Res._TypeName);
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
  if ResType <> '' then Result := Result+':'+ResType;
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

function ObjCToDelphiType(const objcType: AnsiString): AnsiString;
var
  l : AnsiString;
begin
  Result := objcType;
  l := AnsiLowerCase(objcType);
  if l = '' then Exit;
  case l[1] of
    'v':
      if l = 'void' then Result := '';
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
  res : TResultTypeDef;
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
  l := res._TypeName;
  Result := (l = 'id') or (l = cl._ClassName);
end;

function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
var
  i     : integer;
  ft    : AnsiString;
begin
  if IsMethodConstructor(cl, m) then ft := 'constructor'
  else ft := '';
  if ForImplementation
    then Result := GetProcFuncHead(m._Name, cl._ClassName, GetMethodParams(m), GetMethodResultType(m), ft)
    else Result := GetProcFuncHead(m._Name, '', GetMethodParams(m), GetMethodResultType(m), ft)
end;

// returns define pas file name form Objective C name, like
// NSApplication.h  ->  NSAPPLICATION_PAS_H
// SomePath/SomePath/SomeFileName.h -> SOMEFILENAME_PAS_H
function GetIfDefFileName(const FileName: AnsiString): AnsiString;
var
  i : integer;
  s : AnsiString;
begin
  //todo: don't like it...
  Result := Copy(FileName, 1, length(FileName) - length(ExtractFileExt(FileName)));
  Result := AnsiUpperCase(Result);
  for i := 1 to length(Result) do
    if Result[i] = '.' then
      Result[i] := '_';
  Result := Result + '_PAS_H';
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
  i := length(s);
  if (s[i] = '"') or (s[i] = '>') then dec(i);
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

procedure BeginSection(const FileName, SectionName: AnsiString; st: TStrings);
var
  nm : AnsiString;
begin
  nm := GetIfDefFileName(FileName);
  st.Add('{$ifdef '+SectionName+'}');
  st.Add('{$ifndef '+nm+'}');
  st.Add('{$define '+nm+'}');
  st.Add('');
end;

procedure EndSection(st: TStrings);
begin
  st.Add('{$endif}');
  st.Add('{$endif}');
end;

// todo: remove Prefix param...
procedure WriteOutIfDefPrecompiler(prec: TPrecompiler; subs: TStrings; const Prefix: AnsiString);
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
  j   : Integer;
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
      WriteOutIfDefPrecompiler(TPrecompiler(obj), subs, '  ');
    end;
  end; {of for}
  subs.Add('');
end;

procedure WriteOutPrecompToHeader(Prec: TPrecompiler; st: TStrings);
var
  dlph  : AnsiString;
begin
  dlph := WriteOutPrecompToPascal(Prec);
  if IsSubStr('{$include', dlph, 1) then st.Add(dlph);
end;

function GetPascalEnumValue(const Name, Param: AnsiString): AnsiString;
begin
  Result := Name;
  if Param <> '' then Result := Result + ' = ' + Param;
end;

function GetPascalConstValue(const Vl: AnsiString): AnsiString;
begin
  Result := vl;
end;

procedure WriteOutEnumValues(enm: TEnumTypeDef; st: TStrings; const Prefix: AnsiString);
var
  vl  : TEnumValue;
  s   : AnsiString;
  i   : Integer;
  j   : Integer;
begin
  j := st.Count;
  for i := 0 to enm.Items.Count - 1 do
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if st.Count > j then st[st.Count-1]:=st[st.Count-1]+', ';
      s := GetPascalEnumValue(vl._Name, GetPascalConstValue(vl._Value));
      s := Prefix + s;
      st.Add(s);
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
  nc, pc: AnsiChar;
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
      //writeln('postfix ', ni + 1, ' ', length(Name) - ni);
      postfix := Copy(Name, ni + 1, length(Name) - ni);
//      writeln('postfixing: ', postfix);
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
  for i := 0 to enm.Items.Count - 1 do begin
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if known = 0 then begin
        prefix := vl._Name;
        postfix := vl._Name;
      end else
        MatchFixes(vl._Name, prefix, postfix);
      //writeln(vl._Name, ' "', prefix, '", "', postfix,'"');
      inc(known)
    end;
  end;
  if (known <= 1) or (length(Result) < 3) then Result := 'todoEnumName' // if only one enumaration or none, name cannot be defined...
  else Result := prefix + postfix;
end;

procedure WriteOutEnumToHeader(enm: TEnumTypeDef; st: TStrings);
var
  i   : Integer;
  s   : AnsiString;
begin
  if enm._Name = '' then s := EvaluateEnumName(enm)
  else s := enm._Name;
  st.Add(Format('  %s = (', [s] ));
  WriteOutEnumValues(enm, st, '    ');
  st.Add('  );');
  st.Add('');
end;

procedure WriteOutHeaderSection(hdr: TObjCHeader; st: TStrings);
var
  i       : Integer;
  cl      : TClassDef;
  subs    : TStringList;
  s       : AnsiString;
  consts  : TStringList;
begin
  BeginSection(hdr._FileName, 'HEADER', st);
  subs := TStringList.Create;
  consts := TStringList.Create;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin
        if (TObject(hdr.Items[i]) is TClassDef) then begin
          cl := TClassDef(hdr.Items[i]);
          WriteOutClassToHeader(cl, subs, consts);
        end else if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutPrecompToHeader(TPrecompiler(hdr.Items[i]), st);
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
          WriteOutEnumToHeader(TEnumTypeDef(hdr.Items[i]), subs);
        end else if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutIfDefPrecompiler(TPrecompiler(hdr.Items[i]), st, '  ');
        end;
      end; {of if}
    
    if subs.Count > 0 then begin
      st.Add('type');
      st.AddStrings(subs);
      subs.Clear;
    end;

  finally
    EndSection(st);
    subs.Free;
    consts.Free;
  end;
end;

procedure WriteOutClassToClasses(cl: TClassDef; subs: TStrings);
var
  i   : Integer;
  cnt : Integer;
  s   : AnsiString;
  j   : Integer;
  obj : TObject; // or TEntity
  
  mtds   : TStringList; // name of methods
  over   : TStringList; // overloaded names
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
        s := GetMethodStr(cl, TClassMethodDef(cl.Items[j]), false);
        i := mtds.IndexOf(TClassMethodDef(cl.Items[j])._Name);
        if Integer(mtds.Objects[i]) > 0 then s := s + ' overload;';
        subs.Add(SpacePrefix + s);
      end else if obj is TPrecompiler then begin
        WriteOutIfDefPrecompiler(TPrecompiler(obj), subs, SpacePrefix);
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
  cl    : TClassDef;
  j     : integer;
  s     : AnsiString;
  subs  : TStringList;
begin
  BeginSection(hdr._FileName, 'CLASSES', st);
  subs := TStringList.Create;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TClassDef) then
        WriteOutClassToClasses(TClassDef(hdr.Items[i]), subs);

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
    if TObject(mtd.Items[i]) is TParameterDef then begin
      Result := true;
      Exit;
    end;
end;

const
  MtdPrefix = 'TMtd_';
  MtdPostfix = '';

procedure ObjCMethodToProcType(mtd: TClassMethodDef; var typeName: AnsiString; subs: TStrings);
var
  i   : integer;
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
    end else if obj is TParameterDef then begin
      if vname = '' then vname := TParameterDef(obj)._Name;
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
  isConsts  : Boolean;
  typeName  : AnsiString;
begin
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
    if ObjCToDelphiType(mtd.GetResultType._TypeName) <> '' then
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
      WriteOutIfDefPrecompiler( TPrecompiler(obj), subs, '');
  end;
end;

procedure WriteOutImplementationSection(hdr: TObjCHeader; st: TStrings);
var
  i   : Integer;
begin
  BeginSection(hdr._FileName, 'IMPLEMENTATION', st);
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


procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);
begin
  WriteOutHeaderSection(hdr, st);
  WriteOutClassesSection(hdr, st);
  WriteOutImplementationSection(hdr, st);
end;

end.
