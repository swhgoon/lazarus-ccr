{
 Project1.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev

 main parser unit
}
program Project1;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$else}
  {$APPTYPE CONSOLE}
{$endif}
uses
  Classes,
  SysUtils,
  ObjCParserUtils,
  ObjCParserTypes;

type
  // this object is used only for precomile directives handling

  { TPrecompileHandler }
  TPrecompileHandler = class(TObject)
  public
    hdr : TObjCHeader;
    procedure OnPrecompile(Sender: TObject);
    procedure OnComment(Sender: TObject; const Comment: AnsiString);
    constructor Create(AHeader: TObjCHeader);
  end;

procedure TPrecompileHandler.OnPrecompile(Sender: TObject);
var
  parser    : TTextParser;
  preEntity : TPrecompiler;
  lst       : TEntity;
  prc       : TNotifyEvent;
begin
  parser := Sender as TTextParser;
  //todo: change for something nicier =)
  prc := parser.OnPrecompile;
  parser.OnPrecompile := nil;
  try
    if parser.Stack.Count > 0 then
      lst := TEntity(parser.Stack[parser.Stack.Count-1])
    else
      lst := nil;

    preEntity := TPrecompiler.Create(lst);
    preEntity.Parse(parser);
    lst.Items.Add(preEntity);
  finally
    parser.OnPrecompile := prc;
  end;
end;

procedure TPrecompileHandler.OnComment(Sender: TObject; const Comment: AnsiString);
var
  parser  : TTextParser;
  cmt     : TComment;
  ent     : TEntity;
begin
  if length(Comment) < 2 then Exit;
  parser := TTextParser(Sender);

  if parser.Stack.Count > 0
    then ent := TEntity(parser.Stack[parser.Stack.Count-1])
    else ent := nil;

  if not Assigned(ent) then Exit;
  cmt := TComment.Create(ent);
  cmt._Comment := Comment;
  if IsSubStr('/*', cmt._Comment, 1) then begin
    cmt._Comment[1] := '(';
    if isSubStr('*/', cmt._Comment, length(cmt._Comment) - 1) then
      cmt._Comment[ length(cmt._Comment)] := ')';
  end;
  ent.Items.Add(cmt);
end;

constructor TPrecompileHandler.Create(AHeader: TObjCHeader);
begin
  hdr := AHeader;
end;

function ReadAndParseFile(const FileName: AnsiString; outdata: TStrings; var Err: AnsiString): Boolean;
var
  hdr     : TObjCHeader;
  parser  : TTextParser;
  prec    : TPrecompileHandler ;
  s       : AnsiString;
  i, cnt  : integer;
begin
  Result :=false;
  if not FileExists(FileName) then begin
    Err :=  'File not found: ' + FileName;
    Exit;
  end;
  
  s := StrFromFile(FileName);
  hdr := TObjCHeader.Create;
  prec := TPrecompileHandler.Create(hdr);
  parser := TTextParser.Create;
  parser.TokenTable := CreateObjCTokenTable;

  try
    parser.Buf := s;
    try
      parser.TokenTable.Precompile := '#';
      parser.OnPrecompile := prec.OnPrecompile;
      parser.OnComment := prec.OnComment;
      hdr._FileName := ExtractFileName(FileName);
      Result := hdr.Parse(parser);
      if not Result then begin
        if parser.Errors.Count > 0 then Err := parser.Errors[0]
        else Err := 'undesribed error';

        Err := Err + #13#10;
        cnt := 120;
        i := parser.Index - cnt;
        if i <= 0 then begin
          i := 1;
          cnt := parser.Index;
        end;
        Err := Err + Copy(parser.Buf, i, cnt);
      end;

    except
    end;
    WriteOutIncludeFile(hdr, outdata);
  finally
    parser.TokenTable.Free;
    parser.Free;
    prec.Free;
    //FreeEntity(hdr);
  end;
end;

procedure ParseAll;
var
  ch    : char;
  srch  : TSearchRec;
  res   : Integer;
  i     : Integer;
  pth   : AnsiString;
  incs  : AnsiString;
  st    : TStringList;
  f     : Text;
  err   : AnsiString;


begin
  writeln('would you like to parse all current directory files .h to inc?');
  readln(ch);
  if (ch <> 'Y') and (ch <> 'y') then begin
    writeln('as you wish, bye!');
    Exit;
  end;

  pth := IncludeTrailingPathDelimiter( GetCurrentDir);
  writeln('looking for .h files in ', pth);
  res := FindFirst(pth + '*.h', -1, srch);
  if res = 0 then begin
    st := TStringList.Create;
    try
      repeat
        write('found: ', srch.Name);
        write(' parsing...');
        //writeln('parsing: ', pth+srch.Name);
        if ReadAndParseFile(pth+srch.Name, st, err) then begin
          write(' parsed ');
          incs := pth + Copy(srch.Name,1, length(srch.Name) - length(ExtractFileExt(srch.Name)));
          incs := incs + '.inc';
          //writeln(incs);
          assignfile(f, incs); rewrite(f);
          try
            for i := 0 to st.Count - 1 do
              writeln(f, st[i]);
          finally
            closefile(f);
          end;
          st.Clear;
          writeln(' converted!');
        end else begin
          writeln('Error: ', err);
          readln;
        end;
      until FindNext(srch) <> 0;

    finally
      FindClose(srch);
      st.Free;
    end;
  end;
end;

const
  ParamKey = '-';

function isParamValue(const s: AnsiString; var ParName, ParValue: AnsiString): Boolean;
var
  i   : Integer;
begin
  Result := false;
  if s = '' then Exit;
  Result := (s[1] = ParamKey);
  if not Result then Exit;
  i := 1;
  ScanWhile(s, i, [ParamKey]);
  ParName := ScanTo(s, i, [#32, #9, '=']);
  ScanWhile(s, i, [#32, #9, '=']);
  ParValue := Copy(s, i, length(s) - i + 1);
end;

procedure AddSpaceSeparated(const s: AnsiString; Strings: TStringList);
var
  i   : Integer;
  ns  : AnsiString;
begin
  i := 1;
  while i <= length(s) do begin
    ScanTo(s, i, ['A'..'Z', 'a'..'z']);
    ns := ScanTo(s, i, [#32, #9, '"']);
    if ns <> '' then Strings.Add(ns);
  end;
end;

function GetConvertSettings(Settings : TConvertSettings; var FileName: AnsiString): Boolean;
var
  i   : integer;
  prm : AnsiString;
  vlm : AnsiString;
  Params  : TStringList;
begin
  Params := TStringList.Create;
  Params.CaseSensitive := false;
  try
    for i := 1 to ParamCount do begin
      if isParamValue(ParamStr(i), prm, vlm) then begin
        prm := AnsiLowerCase(prm);
        if prm = 'mu' then prm := 'mainunit'
        else if prm = 'ii' then prm := 'ignoreinclude';
        Params.Values[prm] := vlm;
      end else
        FileName := ParamStr(i);
    end;

    vlm := Params.Values['mainunit'];
    if vlm <> '' then
      Settings.ConvertPrefix.Add ('{%mainunit '+vlm+'}');

    vlm := Params.Values['ignoreinclude'];
    if vlm <> '' then begin
      AddSpaceSeparated(vlm, Settings.IgnoreIncludes);
      {for i := 0 to Settings.IgnoreIncludes.Count - 1 do begin
        vlm := Settings.IgnoreIncludes[i];
        vlm := Copy(vlm, 1, length(vlm) - length(ExtractFileExt(vlm)));
        vlm := vlm + '.inc';
        Settings.IgnoreIncludes[i] := vlm;
      end;}
    end;

  finally
    Params.Free;
  end;
  Result := true;
end;

var
  inpf  : AnsiString;
  st    : TStrings;
  i     : integer;
  err   : AnsiString;
begin
  try
    GetConvertSettings(ConvertSettings, inpf);
    if not FileExists(inpf) then begin
      //ParseAll;
      Exit;
    end;

    st := TStringList.Create;
    try
      if not ReadAndParseFile(inpf, st, err) then 
        writeln('Error: ', err)
      else
        for i := 0 to st.Count - 1 do
          writeln(st[i]);
    except
    end;
    st.Free;
  except
    on e: exception do
      writeln(e.Message);
  end;
end.

