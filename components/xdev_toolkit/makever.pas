program MakeVer;

{
  Makes INI-style version file from Delphi .dof file.

  Author:    Phil Hess.
  Copyright: Copyright (C) 2007 Phil Hess. All rights reserved.
  License:   Modified LGPL.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF} 
{$R+,Q+}

uses
  SysUtils,
  Classes,
  IniFiles;
  
const
  ProgramName    = 'MakeVer';
  ProgramVersion = '0.02';
  
  DofFileExt     = '.dof';      {Delphi project options file extension}
  VerFileExt     = '.version';  {Linux/Mac version info file extension}
  
  VersionSection = 'Version Info Keys';

var
  DofFileName : string;
  VerFileName : string;
  DofIniFile  : TIniFile;
  VerIniFile  : TIniFile;
  VerStrList  : TStringList;
{$IFNDEF FPC}
  MatchFound  : TFilenameCaseMatch;
{$ENDIF}
  ItemNum     : Integer;

begin

  if ParamCount = 0 then  {List program useage?}
    begin
    WriteLn(ProgramName, ', version ', ProgramVersion,
            ' - makes INI-style version file from Delphi .dof file.');
    WriteLn('Usage: ', ProgramName, ' filename', DofFileExt);
    Halt;
    end;

   {Get name of Delphi project options file from command line}
  DofFileName := ParamStr(1);
  if ExtractFileExt(DofFileName) = '' then
    DofFileName := DofFileName + DofFileExt;
{$IFNDEF FPC}
  DofFileName := ExpandFileNameCase(DofFileName, MatchFound);
{$ELSE}
  DofFileName := ExpandFileName(DofFileName);
{$ENDIF}
    
  VerFileName := ChangeFileExt(DofFileName, VerFileExt);
  
  if not FileExists(DofFileName) then
    begin
    WriteLn(DofFileName, ' does not exist');
    Halt;
    end;
    
  DofIniFile := TIniFile.Create(DofFileName);
  VerStrList := TStringList.Create;
  DofIniFile.ReadSectionValues(VersionSection, VerStrList); {Load vers strings}
  VerIniFile := TIniFile.Create(VerFileName);
  for ItemNum := 0 to Pred(VerStrList.Count) do {Write to version file}
    begin
    VerIniFile.WriteString(VersionSection, VerStrList.Names[ItemNum],
                           VerStrList.Values[VerStrList.Names[ItemNum]]);
    end;    
  VerIniFile.UpdateFile;  {Save to file}
  VerIniFile.Free;
  DofIniFile.Free;    
  WriteLn(VerFileName, ' successfully created');

end.

