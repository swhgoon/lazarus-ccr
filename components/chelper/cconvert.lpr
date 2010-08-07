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
  ctopasconvert,cparserutils,cconvconfig;

var
  ConfigFile    : AnsiString = '';
  OutputFile    : AnsiString = '';
  ConfigFileRO  : Boolean = false;

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
    end;
    inc(i);
  end;
end;

var
  inps, outs : TStringList;
  i   : Integer;
  p   : TPoint;
  cfg : TConvertSettings;
begin
  inps := TStringList.Create;
  outs := TStringList.Create;

  cfg:=TConvertSettings.Create;
  try
    InitSettings(cfg);

    inps.LoadFromFile(ParamStr(ParamCount));
    outs.Text:=ConvertCode(inps.Text, p, cfg);
    if OutputFile<>'' then begin
      outs.Insert(0, Format('%d %d', [p.Y,p.X]));
      outs.SaveToFile(OutputFile)
    end else begin
      writeln(p.Y,' ',p.X);
      for i:=0 to outs.Count-1 do
        writeln(outs[i]);
    end;
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

