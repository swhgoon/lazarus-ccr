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


function GetIfDefFileName(const FileName: AnsiString): AnsiString;
var
  i : integer;
  s : AnsiString;
begin
  Result := Copy(FileName, 1, length(FileName) - length(ExtractFileExt(FileName)));
  Result := AnsiUpperCase(Result);
  for i := 1 to length(Result) do
    if Result[i] = '.' then
      Result[i] := '_';
  Result := Result + '_PAS_H';
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

procedure WriteOutClassToHeader(cl : TClassDef; subs: TStrings; conststr: TStrings);
var
  i   : Integer;
  s   : AnsiString;
  ss  : AnsiString;
  mtd : TClassMethodDef;
begin
  if conststr.IndexOf(cl._ClassName) < 0 then begin
    conststr.Add(cl._ClassName);
    s := Format('  Str_%s = '#39'%s'#39';', [cl._ClassName, cl._ClassName]);
    subs.Add(s);
  end;
  for i := 0 to cl.Items.Count - 1 do
    if TObject(cl.Items[i]) is TClassMethodDef then begin
      mtd := TClassMethodDef(cl.Items[i]);
      if conststr.IndexOf(mtd._Name) < 0 then begin
        conststr.Add(mtd._Name);
        ss := Format('  Str_%s = '#39'%s'#39';', [mtd._Name, mtd._Name]);
        subs.add(ss);
      end;
    end;
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
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TClassDef) then begin
        cl := TClassDef(hdr.Items[i]);
        WriteOutClassToHeader(cl, subs, consts);
      end;

    if subs.Count > 0 then begin
      st.Add('const');
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
  s   : AnsiString;
  j   : Integer;
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
  for j := 0 to cl.Items.Count - 1 do
    if TObject(cl.Items[j]) is TClassMethodDef then begin
      s := GetMethodStr(cl, TClassMethodDef(cl.Items[j]), false);
      subs.Add('    ' + s);
    end;
  subs.Add('  end;');
  subs.Add('');
end;

procedure WriteOutClassesSection(hdr: TObjCHeader; st: TStrings);
var
  i   : integer;
  cl  : TClassDef;
  j   : integer;
  s   : AnsiString;
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
  
  
  for i := 0 to cl.Items.Count - 1 do
    if TObject(cl.Items[i]) is TClassMethodDef then
      WriteOutMethodToImplementation ( TClassMethodDef(cl.Items[i]), subs);
end;

procedure WriteOutImplementationSection(hdr: TObjCHeader; st: TStrings);
var
  i   : Integer;
begin
  BeginSection(hdr._FileName, 'IMPLEMENTATION', st);
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TClassDef) then
        WriteOutClassToImplementation(TClassDef(hdr.Items[i]), st);
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
