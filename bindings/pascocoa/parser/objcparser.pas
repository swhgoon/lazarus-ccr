{
 Project1.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 main parser unit
}


program Project1;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, ObjCParserUtils, ObjCParserTypes;

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
    //writeln('added to: ', lst.ClassName, ' ', preEntity._Directive + preEntity._Params);
    lst.Items.Add(preEntity);
    //write('// debug: ');
    //writeln('precompile: ', preEntity._Directive, ', params:', preEntity._Params);
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
  //writeln(' > ', Comment);

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

procedure ReadAndParseFile(const FileName: AnsiString; outdata: TStrings);
var
  hdr     : TObjCHeader;
  parser  : TTextParser;
  prec    : TPrecompileHandler;
  s       : AnsiString;
begin
  if not FileExists(FileName) then
    Exit;
  
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
      hdr.Parse(parser);
    except
    end;
    WriteOutIncludeFile(hdr, outdata);
  finally
    hdr.Free;
    parser.TokenTable.Free;
    parser.Free;
    prec.Free;
  end;
end;

var
  inpf  : AnsiString;
  f  : Text;
  st : TStrings;
  fn : AnsiString;
  i  : integer;
begin
  inpf := ParamStr(1);
  if not FileExists(inpf) then begin
    Exit;
  end;

  st := TStringList.Create;
  try
    ReadAndParseFile(inpf, st);
    for i := 0 to st.Count - 1 do
      writeln(st[i]);
  except
  end;
  st.Free;

end.

