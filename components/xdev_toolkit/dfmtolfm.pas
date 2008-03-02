program DfmToLfm;

{
  Converts Delphi form design file to a Lazarus form file by
   deleting properties that are not supported by LCL and 
   optionally making changes to font properties. The resulting
   Lazarus form file can then be converted to a Lazarus resource
   file with LazRes.
  Note that the Delphi form file must be a text file.
  List of properties to delete and other configuration settings
   are read from DfmToLfm.ini.
  This utility (and Lazarus LazRes) can be used whenever design 
   changes are made to the form in Delphi. 
  Note: Use MakePasX to make the form's code file cross-platform
   (a one-time conversion).
   
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
  IniFiles;

const
  ProgramName    = 'DfmToLfm';
  ProgramVersion = '0.02';
  
  DfmFileExt     = '.dfm';  {Delphi form file extension}
  LfmFileExt     = '.lfm';  {Lazarus form file extension}
  CfgFileExt     = '.ini';  {Extension for file with same name as program
                              containing configuration settings}

  NoFontChanges  = 0;       {No font switch on command line}
  UseParentFont  = 1;       {-p font switch on command line}
  DeleteFontName = 2;       {-d font switch on command line}
  
  MaxNestedObjs  = 20;      {Maximum depth of nested controls on form}
  MaxFontProps   = 5;       {Maximum font properties that can be saved}          
  
type
  TStackRec   = record      {Info about form objects}
    ClassName   : string;
    FontPropCnt : Integer;
    FontAdded   : Boolean;
    FontProps   : array [1..MaxFontProps] of string;
    end;

var
  CfgFileName : string;
{$IFNDEF FPC}
  MatchFound  : TFilenameCaseMatch;
{$ENDIF}
  FontSwitch  : Integer;
  CfgFileObj  : TMemIniFile;
  DfmFileName : string;
  LfmFileName : string;
  DfmFileVar  : TextFile;
  LfmFileVar  : TextFile;
  StackLevel  : Integer;
  StackRec    : array [1..MaxNestedObjs] of TStackRec;
  DeleteLine  : Boolean;
  InStr       : string;
  StripStr    : string;
  SkipStr     : string;
  ParentLevel : Integer;
  FontPropNum : Integer;

begin

   {Base configuration file name on program file location and program name}
  CfgFileName := 
   ExtractFilePath(ParamStr(0)) + LowerCase(ProgramName) + CfgFileExt;
{$IFNDEF FPC}
  CfgFileName := ExpandFileNameCase(CfgFileName, MatchFound);
{$ENDIF}

  if ParamCount = 0 then  {List program syntax and exit?}
    begin
    WriteLn(ProgramName, ', version ', ProgramVersion,
            ' - converts a Delphi form file to a Lazarus form file.');
    WriteLn('Usage: ', ProgramName, ' filename', DfmFileExt, ' [-p|-d]');
    WriteLn('Switches:');
    WriteLn('  -p  Add parent''s font to controls with no font ',
            '(useful with Windows).');
    WriteLn('  -d  Delete font name from controls ',
            '(useful with GTK and GTK2).');
    WriteLn('Looks for configuration data in file ', CfgFileName); 
    Halt;
    end;

   {Check for command line switches}
  FontSwitch := NoFontChanges;
  if FindCmdLineSwitch('p', ['-'], True) then
    FontSwitch := UseParentFont
  else if FindCmdLineSwitch('d', ['-'], True) then
    FontSwitch := DeleteFontName;

   {Load configuration file}
  if not FileExists(CfgFileName) then
    begin
    WriteLn('Can''t load program configuration file ', CfgFileName);
    Halt;
    end;
  CfgFileObj := TMemIniFile.Create(CfgFileName);
  
   {Get name of Delphi form file from command line}
  DfmFileName := ParamStr(1);
  if ExtractFileExt(DfmFileName) = '' then
    DfmFileName := DfmFileName + DfmFileExt;
{$IFNDEF FPC}
  DfmFileName := ExpandFileNameCase(DfmFileName, MatchFound);
{$ELSE}
  DfmFileName := ExpandFileName(DfmFileName);
{$ENDIF}
    
   {Base Lazarus form file name on Delphi form file name}
  LfmFileName := ChangeFileExt(DfmFileName, LfmFileExt);
  
   {Open Delphi form file}
  AssignFile(DfmFileVar, DfmFileName);
  try
    Reset(DfmFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t open Delphi form file ', DfmFileName);
      Halt;
      end;
    end;

   {Create Lazarus form file}
  AssignFile(LfmFileVar, LfmFileName);
  try 
    Rewrite(LfmFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t create Lazarus form file ', LfmFileName);
      Halt;
      end;
    end;

  StackLevel := 0;
  while not Eof(DfmFileVar) do  {Read and process Delphi form file}
    begin
    DeleteLine := False;
    ReadLn(DfmFileVar, InStr);  {Read property from form file}
    StripStr := StringReplace(InStr, ' ', '', [rfReplaceAll]); {Strip spaces}

    if ((CompareText('object ', Copy(Trim(InStr), 1, 7)) = 0) or
        (CompareText('end', StripStr) = 0)) and {End of object's props reached?}
       (StackLevel > 1) and  {Object is nested?} 
       (not CfgFileObj.ValueExists(
             'NoFont', StackRec[StackLevel].ClassName)) and  {Class has font?}
       (StackRec[StackLevel].FontPropCnt = 0) and  {Object has no font?}
       (FontSwitch = UseParentFont) and  {Okay to insert parent font in object?}
       (not StackRec[StackLevel].FontAdded) then  {Font not inserted yet?}
      begin
      ParentLevel := StackLevel;
      repeat
        Dec(ParentLevel);
      until (ParentLevel = 0) or
            (StackRec[ParentLevel].FontPropCnt > 0);
      if ParentLevel > 0 then  {A parent has font?}
        begin  {Add font properties to current object}
        for FontPropNum := 1 to StackRec[ParentLevel].FontPropCnt do
          begin
          WriteLn(LfmFileVar, StringOfChar(' ', (StackLevel-ParentLevel)*2), 
                  StackRec[ParentLevel].FontProps[FontPropNum]);
          end;
        end;
      StackRec[StackLevel].FontAdded := True;
      end;

    if CompareText('object ', Copy(Trim(InStr), 1, 7)) = 0 then
      begin  {Push object's class name on stack}
      Inc(StackLevel);
      if Pos(': ', InStr) > 0 then  {Named control?}
        StackRec[StackLevel].ClassName :=
         Trim(Copy(InStr, Pos(': ', InStr)+2, MaxInt))
      else  {Unnamed control}
        StackRec[StackLevel].ClassName := Trim(Copy(Trim(InStr), 7, MaxInt));
      StackRec[StackLevel].FontPropCnt := 0;
      StackRec[StackLevel].FontAdded := False;
      end

    else if CompareText('end', StripStr) = 0 then
      begin  {Pop current class from stack}
      Dec(StackLevel);
      end

    else if CompareText('font.', Copy(Trim(InStr), 1, 5)) = 0 then
      begin  {Font property}
      if FontSwitch <> NoFontChanges then
        begin
        if FontSwitch = UseParentFont then
          begin  {Save font property in case need it for child objects}
          if StackRec[StackLevel].FontPropCnt < MaxFontProps then
            begin
            Inc(StackRec[StackLevel].FontPropCnt);
            StackRec[StackLevel].FontProps[StackRec[StackLevel].FontPropCnt] :=
             InStr;
            end;
          end
        else  {FontSwitch = DeleteFontName}
          begin
          if CompareText('font.name', Copy(Trim(InStr), 1, 9)) = 0 then
            DeleteLine := True;
          end;
        end;
       {Check if font property should be deleted from current object}
      if CfgFileObj.ValueExists('DeleteProps',
                                StackRec[StackLevel].ClassName + '.' +
                                Copy(StripStr, 1, Pos('=', StripStr)-1)) then
        DeleteLine := True;
      end

    else  {Other property}
      begin  {Check if property should be deleted from current object}
      if CfgFileObj.ValueExists('DeleteProps', 
                                Copy(StripStr, 1, Pos('=', StripStr)-1)) or
         CfgFileObj.ValueExists('DeleteProps',
                                StackRec[StackLevel].ClassName + '.' +
                                Copy(StripStr, 1, Pos('=', StripStr)-1)) then
        begin  {Property or class.property in list of props to delete?} 
        DeleteLine := True;
        if Copy(StripStr, Length(StripStr), 1) = '(' then  {Delete > 1 line?}
          begin
          repeat
            ReadLn(DfmFileVar, SkipStr);
            SkipStr := Trim(SkipStr);
          until Copy(SkipStr, Length(SkipStr), 1) = ')';
          end;
        end;
      end;  

    if not DeleteLine then  {Include line in Lazarus form file?}
      begin
      try
         {If Delphi form file does have Height and Width, reduce
           to size of its ClientHeight or ClientWidth.}
        if ((StackLevel = 1) and
                 (CompareText('Height=', Copy(StripStr, 1, 7)) = 0)) then
          WriteLn(LfmFileVar,
                  '  Height = ', 
                  IntToStr(StrToInt(Copy(StripStr, 8, MaxInt)) - 34))
        else if ((StackLevel = 1) and
                 (CompareText('Width=', Copy(StripStr, 1, 6)) = 0)) then
          WriteLn(LfmFileVar,
                  '  Width = ', 
                  IntToStr(StrToInt(Copy(StripStr, 7, MaxInt)) - 8))

         {LCL TGroupBox child controls' Top measures from a lower position
           within group box than with VCL, so reduce Top value}
        else if (StackLevel > 1) and
                (CompareText('Top=', Copy(StripStr, 1, 4)) = 0) and
                (CompareText('TGroupBox', 
                             StackRec[Pred(StackLevel)].ClassName) = 0) then
          WriteLn(LfmFileVar,
                  Copy(InStr, 1, Succ(Pos('=', InStr))),
                  IntToStr(StrToInt(Copy(StripStr, 5, MaxInt)) - 16))

(*  This incorrect swapping has been fixed in FPC 2.2 based Lazarus releases.
         {Lazarus IDE appears to swap Top and Left properties for non-visual
           controls, so swap them for Orpheus table cell controls.}
        else if ((CompareText('Top=', Copy(StripStr, 1, 4)) = 0) or
                 (CompareText('Left=', Copy(StripStr, 1, 5)) = 0)) and
                ((CompareText('TOvcTC', 
                              Copy(StackRec[StackLevel].ClassName, 1, 6)) = 0) or
                 (CompareText('TO32TC', 
                              Copy(StackRec[StackLevel].ClassName, 1, 6)) = 0) or
                 (CompareText('TOvcController', 
                              StackRec[StackLevel].ClassName) = 0)) then
          begin
          if CompareText('Top=', Copy(StripStr, 1, 4)) = 0 then
            WriteLn(LfmFileVar, 
                    StringReplace(InStr, 'Top', 'Left', [rfIgnoreCase]))
          else
            WriteLn(LfmFileVar,
                    StringReplace(InStr, 'Left', 'Top', [rfIgnoreCase]));
          end
*)
  
        else  {No change to property}
          WriteLn(LfmFileVar, InStr);

         {Delphi form files don't always include Height or Width properties,
           which are required by Lazarus, so add them based on ClientHeight 
           and ClientWidth properties, which apparently act the same as
           Height and Width in Lazarus (unlike Delphi).}
        if (CompareText('ClientHeight=', Copy(StripStr, 1, 13)) = 0) or
           (CompareText('ClientWidth=', Copy(StripStr, 1, 12)) = 0) then
          WriteLn(LfmFileVar, 
                  StringReplace(InStr, 'Client', '',  [rfIgnoreCase]));
      except
        on EInOutError do
          begin
          WriteLn('Can''t write to Lazarus form file ', LfmFileName);
          Halt;
          end; 
        end;
      end;
    end;  {while not Eof}  

  CloseFile(DfmFileVar);
  try   
    CloseFile(LfmFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t close Lazarus form file ', LfmFileName);
      Halt;
      end;
    end;
  CfgFileObj.Free;
  WriteLn(LfmFileName, ' successfully created');

end.

