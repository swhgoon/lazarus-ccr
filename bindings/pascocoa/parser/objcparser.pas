{
 Project1.pas

 Copyright (C) 2008 Dmitry 'Skalogryz' Boyarintsev
 
 main parser unit
}


program Project1;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, ObjCParserUtils, ObjCParserTypes;

procedure ReadAndParseFile(const FileName: AnsiString; outdata: TStrings);
var
  hdr : TObjCHeader;
  txt : TTextParser;
  s   : AnsiString;
begin
  if not FileExists(FileName) then
    Exit;
  
  s := StrFromFile(FileName);
  hdr := TObjCHeader.Create;
  txt := TTextParser.Create;
  txt.TokenTable := CreateObjCTokenTable;
  try
    txt.Buf := s;
    try
      hdr._FileName := ExtractFileName(FileName);
      hdr.Parse(txt);
    except
    end;
    WriteOutIncludeFile(hdr, outdata);
  finally
    hdr.Free;
    txt.TokenTable.Free;
    txt.Free;
  end;
end;

procedure GetParams(var InpFile, OutFile: AnsiString);
begin
  InpFile := ParamStr(1);
  OutFile := ParamStr(2);
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

