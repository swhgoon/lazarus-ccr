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
unit converteridesettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctopasconvert, IniFiles;

var
  ConvSettings  : TConvertSettings=nil;
  ExtTool       : AnsiString='';
  UseExtTool    : Boolean=False;
  ExtTimeOut    : LongWord = 5000;
  ConvFile      : AnsiString='';
  DefineFile    : AnsiString='';

procedure StringToFile(const Str, DstFileName: AnsiString);
function StringFromFile(const SrcFileName: AnsiString): AnsiString;

procedure WriteIDESettings(const FileName: AnsiString);
procedure ReadIDESettings(const FileName: AnsiString);

implementation

procedure StringToFile(const Str, DstFileName: AnsiString);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(DstFileName, fmCreate);
  if Str<>'' then fs.Write(Str[1], length(Str));
  fs.Free;
end;

function StringFromFile(const SrcFileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  Result:='';
  try
    if not FileExists(SrcFileName) then Exit;
    fs:=TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, fs.Size);
      if fs.Size>0 then fs.Read(Result[1], fs.Size);
    finally
      fs.Free;
    end;
  except
  end;
end;

procedure WriteIDESettings(const FileName:AnsiString);
var
  ini : TIniFile;
begin
  try
    ini:=TIniFile.Create(FileName);
    try
      ini.WriteBool('Tool', 'UseExt', UseExtTool);
      ini.WriteString('Tool', 'Exe', ExtTool);
      ini.WriteString('Tool', 'DefineFile', DefineFile);
    finally
      ini.Free;
    end;
  except
  end;
end;

procedure ReadIDESettings(const FileName:AnsiString);
var
  ini : TIniFile;
begin
  try
    ini:=TIniFile.Create(FileName);
    try
      UseExtTool:=ini.ReadBool('Tool', 'UseExt', UseExtTool);
      ExtTool:=ini.ReadString('Tool', 'Exe', ExtTool);
      DefineFile:=ini.ReadString('Tool', 'DefineFile',DefineFile);
    finally
      ini.Free;
    end;
  except
  end;
end;

initialization
  ConvSettings := TConvertSettings.Create;

finalization
  ConvSettings.Free;

end.

