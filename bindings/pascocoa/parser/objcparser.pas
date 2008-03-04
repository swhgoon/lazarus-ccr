program Project1;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, ObjCParserUtils, ObjCParserTypes;
  { add your units here }

procedure ReadAndParseFile(const FileName: AnsiString; outdata: TStrings);
var
  hdr : TObjCHeader;
  txt : TTextParser;
  s   : AnsiString;
begin
  if not FileExists(FileName) then Exit;
  s := StrFromFile(FileName);
  hdr := TObjCHeader.Create;
  txt := TTextParser.Create;
  txt.TokenTable := CreateObjCTokenTable;
  try
    txt.Buf := s;
    try
      hdr.Parse(txt);
    except
    end;
    outdata.Add('');
    outdata.Add('// ' + FileName);
    outdata.Add('');
    ReportHeader(hdr, outdata);
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
  inpf, outf : AnsiString;
  f  : Text;
  st : TStrings;
  fn : AnsiString;
  i  : integer;
begin
  inpf := ''; outf := '';
  GetParams(inpf, outf);
  writeln('input file: ', inpf);
  writeln('output file: ', outf);
  if not FileExists(inpf) then begin
    writeln('input file does not exists or anavailable');
    Exit;
  end;

  st := TStringList.Create;
  try
    Assign(f, inpf); Reset(f);
    while not eof(f) do begin
      readln(f, fn);
      try
        ReadAndParseFile(fn, st);
      except
      end;
    end;
    Close(f);

    Assign(f, outf); Rewrite(f);
    try
      for i := 0 to st.Count - 1 do
       writeln(f, st[i]);
    except
    end;
    Close(f);
    
  finally
    st.Free;
  end;
end.

