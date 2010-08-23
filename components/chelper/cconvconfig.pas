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
unit cconvconfig;

{$mode delphi}

interface

uses
  Classes, SysUtils, ctopasconvert, IniFiles;

procedure LoadFromFile(const FileName: AnsiString; cfg: TConvertSettings);
procedure SaveToFile(const FileName: AnsiString; cfg: TConvertSettings);

implementation

procedure LoadFromFile(const FileName: AnsiString; cfg: TConvertSettings);
var
  ini : TIniFile;
begin
  if not Assigned(cfg) then Exit;
  try
    ini:=TIniFile.Create(FileName);
    try
      // C to Pas Types
      ini.ReadSectionValues('Types', cfg.CtoPasTypes);
      cfg.RecordsArePacked:=ini.ReadBool('Main','RecordsArePacked', cfg.RecordsArePacked);
      cfg.FuncsAreExternal:=ini.ReadBool('Main','FuncsAreExternal', cfg.FuncsAreExternal);
      cfg.EnumsAsConst:=ini.ReadBool('Main','EnumAsConst', cfg.EnumsAsConst);

      cfg.TypeNamePrefix:=ini.ReadString('Main','TypeNamePrefix',cfg.TypeNamePrefix);
      cfg.RefTypeNamePrefix:=ini.ReadString('Main','RefTypeNamePrefix',cfg.RefTypeNamePrefix);
      cfg.FuncConv:=ini.ReadString('Main','FuncConv',cfg.FuncConv);
      cfg.FuncDeclPostfix:=ini.ReadString('Main','FuncDeclPostfix',cfg.FuncDeclPostfix);
      cfg.ExtLibName:=ini.ReadString('Main','ExtLibName',cfg.ExtLibName);
      cfg.ParamPrefix:=ini.ReadString('Main','ParamPrefix',cfg.ParamPrefix);
    finally
      ini.Free;
    end;
  except
  end;
end;

procedure SaveToFile(const FileName: AnsiString; cfg: TConvertSettings);
var
  ini : TIniFile;
  i   : Integer;
begin
  if not Assigned(cfg) then Exit;
  try
    ini:=TIniFile.Create(FileName);
    try

      // C to Pas Types
      for i:=0 to cfg.CtoPasTypes.Count-1 do
        ini.WriteString('Types', cfg.CtoPasTypes.Names[i], cfg.CtoPasTypes.ValueFromIndex[i]);
      ini.WriteBool('Main','RecordsArePacked', cfg.RecordsArePacked);
      ini.WriteBool('Main','FuncsAreExternal', cfg.FuncsAreExternal);
      ini.WriteBool('Main','EnumAsConst', cfg.EnumsAsConst);

      ini.WriteString('Main','TypeNamePrefix',cfg.TypeNamePrefix);
      ini.WriteString('Main','RefTypeNamePrefix',cfg.RefTypeNamePrefix);
      ini.WriteString('Main','FuncConv',cfg.FuncConv);
      ini.WriteString('Main','FuncDeclPostfix',cfg.FuncDeclPostfix);
      ini.WriteString('Main','ParamPrefix',cfg.ParamPrefix);
      ini.WriteString('Main','ExtLibName',cfg.ExtLibName);
    finally
      ini.Free;
    end;
  except
  end;
end;

end.

