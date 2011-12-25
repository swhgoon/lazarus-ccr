{ C-to-Pas converter command-line utility part of Lazarus Chelper package

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
program cconvert;

{$mode objfpc}{$H+}

uses
  SysUtils,Classes,
  ctopasconvert, cparsertypes, cparserutils, cconvconfig, objcparsing,
  commonsrcgen;

var
  ConfigFile    : AnsiString = '';
  OutputFile    : AnsiString = '';
  ConfigFileRO  : Boolean = false;
  ParseAll      : Boolean = false;
  ShowCodeSize  : Boolean = False; // show the size of code processed
  isPascalUnit  : Boolean = False; // convert to pascal unit

function StringFromFile(const FileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  Result:='';
  if not FileExists(FileName) then Exit;
  try
    fs:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, fs.Size);
      fs.Read(Result[1], fs.Size);
    finally
      fs.Free;
    end;
  except
  end;
end;

procedure InitSettings(cfg: TConvertSettings);
var
  i   : Integer;
  p   : AnsiString;
  fn  : AnsiString;
begin
  i:=1;
  while i<=Paramcount do begin
    p:=AnsiLowerCase(ParamStr(i));
    if p='-cfg' then begin
      inc(i);
      fn:=Trim(Paramstr(i));
      ConfigFile:=fn;
      if FileExists(fn) then cconvconfig.LoadFromFile(fn, cfg);
    end else if p='-ro' then
      ConfigFileRO:=True
    else if p='-defines' then begin
      inc(i);
      cfg.CustomDefines:=cfg.CustomDefines+' ' + StringFromFile(ParamStr(i));
    end else if p='-o' then begin
      inc(i);
      OutputFile:=ParamStr(i);
    end else if p='-all' then begin
      ParseAll:=True
    end else if p='-pasunit' then
      isPascalUnit:=True;
    inc(i);
  end;
end;

procedure PrintHelp;
begin
  writeln('cconvert - c to pascal convert utility');
  writeln('possible options:');
  writeln(' -all              - convert the whole header to pascal, instead of a first entity');
  writeln(' -o filename       - specify the output file. if not specified, outputs to stdout');
  writeln(' -ro               - prevent the configuration file from modifications (adding new types, etc)');
  writeln(' -cfg filename     - specifies the configuration file');
  writeln(' -defines filename - macros definition file. should be in C-preprocessor format');
  writeln(' -showunparsed     - writes out unprased entities by their classname (for debugging only)');
  writeln(' -codesize         - show two numbers of the code processed (used by Chelper)');
  writeln(' -pasunit          - generates a pascal unit');
end;

procedure ReadParams(var InputFileName: String);
var
  i : integer;
  s : string;
begin
  for i:=1 to ParamCount do begin
    s:=LowerCase(ParamStr(i));
    if (s='-h') or (s='-help') or (s='-?') then begin
      PrintHelp;
      Halt;
    end else if s='-showunparsed' then
      DoDebugEntities:=True;
  end;
  InputFileName:=ParamStr(ParamCount);
end;


function GetPascalUnitName(const UnitName: String): String;
begin
  Result:=ChangeFileExt(UnitName, '');
end;

procedure AddPascalUnit(outs: TStrings; const UnitName: String);
begin
  if not Assigned(outs) then Exit;
  outs.Insert(0, 'unit '+UnitName+';');
  outs.Insert(1, '');
  outs.Insert(2, 'interface');
  outs.Add(      'implementation');
  outs.Add(      'end.');
end;

var
  inps, outs : TStringList;
  i   : Integer;
  p   : TPoint;
  cfg : TConvertSettings;
  err : TErrorInfo;
  fn  : String;
begin
  if ParamCount=0 then Exit;
  ReadParams(fn);
  if not FileExists(fn) then begin
    writeln('file doesn''t exist: ', fn);
    Exit;
  end;


  inps := TStringList.Create;
  outs := TStringList.Create;

  cfg:=TConvertSettings.Create;
  try
    InitSettings(cfg);

    inps.LoadFromFile(ParamStr(ParamCount));
    outs.Text:=ConvertCode(inps.Text, p, ParseAll, err, cfg);;

    if ShowCodeSize then outs.Insert(0, Format('%d %d', [p.Y,p.X]));
    if err.isError then outs.Insert(0, Format('error %d %d %s',[err.ErrorPos.Y, err.ErrorPos. X, err.ErrorMsg]) );

    if isPascalUnit then begin
      AddPascalUnit(outs, GetPascalUnitName(fn));
    end;


    if OutputFile<>'' then
      outs.SaveToFile(OutputFile)
    else
      for i:=0 to outs.Count-1 do
        writeln(outs[i]);
  finally
    if not ConfigFileRO and (ConfigFile<>'') then begin
      ForceDirectories(ExtractFilePath(ConfigFile));
      try
        cconvconfig.SaveToFile(ConfigFile, cfg);
      except
      end;
    end;
    cfg.Free;
    inps.Free;
    outs.Free;
  end;
end.

