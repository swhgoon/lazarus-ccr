program DfmToLfm;

{
  Converts Delphi form design file to a Lazarus form file by
   deleting properties that are not supported by LCL and 
   optionally making changes to font properties. (The resulting
   Lazarus form file can then be converted to a Lazarus resource
   file with LazRes, although this second step is only needed
   now with Lazarus 0.9.28 and earlier.)

  Note that the Delphi form file must be a text file.

  List of properties to delete and other configuration settings
   are read from dfmtolfm.ini.

  This utility (and Lazarus LazRes, if needed) can be used whenever 
   design changes are made to the form in Delphi. 

  Note: You can use MakePasX to make the form's code file
   cross-platform (a one-time conversion).

  Author:    Phil Hess.
  Copyright: Copyright (C) 2007-2011 Phil Hess. All rights reserved.
  License:   Modified LGPL.
}

(*
  Note: This converter can also convert a Lazarus form file to
   another Lazarus form file (.lfm -->.lfm). This can be useful
   if you need to conditionally include a different form depending 
   on widgetset target. Example:

  {$IFNDEF LCLCarbon}
   {$R *.lfm}  //include generic form with Windows and Linux
  {$ELSE}
   {$R *.mac.lfm}  //include prettied form with Mac (-m -s switches)
  {$ENDIF}
  
  In this case, you would make changes in Lazarus only to the 
   generic form file, then convert it to use with Mac:
     dfmtolfm myform.lfm myform.mac.lfm -m -s
*)


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
  ProgramVersion = '0.04';
  
  DfmFileExt     = '.dfm';  {Delphi form file extension}
  LfmFileExt     = '.lfm';  {Lazarus form file extension}
  CfgFileExt     = '.ini';  {Extension for file with same name as program
                              containing configuration settings}

  NoFontChanges  = 0;       {No font switch on command line}
  UseParentFont  = 1;       {-p font switch on command line}
  DeleteFontName = 2;       {-d font switch on command line}
  SubstFontName  = 3;       {-s font switch on command line}
  
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
  MacSwitch   : Boolean;
  CfgFileObj  : TMemIniFile;
  InFileName  : string;
  IsDfmInFile : Boolean;
  OutFileName : string;
  InFileVar   : TextFile;
  OutFileVar  : TextFile;
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
    WriteLn(ProgramName, ', version ', ProgramVersion);
    WriteLn('Converts a Delphi (or Lazarus) form file to a Lazarus form file.');
    WriteLn;
    WriteLn('Usage: ', LowerCase(ProgramName), ' infile[', DfmFileExt, '|',
            LfmFileExt, '] [outfile.lfm] [-p|-d|-s][-m]');
    WriteLn;
    WriteLn('Switches:');
    WriteLn('  -p  Add parent''s font to controls with no font ',
            '(useful with Windows).');
    WriteLn('  -d  Delete font name from controls ',
            '(useful with GTK and GTK2).');
    WriteLn('  -s  Substitute font names (useful with non-Windows targets).');
    WriteLn('  -m  Mac prettifier.');
    WriteLn;
    WriteLn('Example:');
    WriteLn('  ', LowerCase(ProgramName),
            ' MainForm.dfm -s -m  (Creates MainForm.lfm, substituting fonts');
    WriteLn('    and prettying form for use on Mac.)');
    WriteLn;
    WriteLn('Notes:');
    WriteLn('  ', ProgramName, ' will look for its configuration data here:');
    WriteLn('    ', CfgFileName);
    WriteLn;
    WriteLn('  See also the comments at top of ', LowerCase(ProgramName), 
            '.pas.');
    Halt;
    end;

   {Check for command line switches}
  FontSwitch := NoFontChanges;
  if FindCmdLineSwitch('p', ['-'], True) then
    FontSwitch := UseParentFont
  else if FindCmdLineSwitch('d', ['-'], True) then
    FontSwitch := DeleteFontName
  else if FindCmdLineSwitch('s', ['-'], True) then
    FontSwitch := SubstFontName;
  MacSwitch := FindCmdLineSwitch('m', ['-'], True);

   {Load configuration file}
  if not FileExists(CfgFileName) then
    begin
    WriteLn('Can''t load program configuration file ', CfgFileName);
    Halt;
    end;
  CfgFileObj := TMemIniFile.Create(CfgFileName);
  
   {Get name of input form file from command line}
  InFileName := ParamStr(1);
  if ExtractFileExt(InFileName) = '' then  {No extension?}
    InFileName := InFileName + DfmFileExt;  {Assume it's a Delphi form file}
{$IFNDEF FPC}
  InFileName := ExpandFileNameCase(InFileName, MatchFound);
{$ELSE}
  InFileName := ExpandFileName(InFileName);
{$ENDIF}
  IsDfmInFile := SameText(ExtractFileExt(InFileName), DfmFileExt);

   {Get name of output form file from command line or generate it}
  OutFileName := '';
  if (ParamStr(2) <> '') and (Copy(ParamStr(2), 1, 1) <> '-') then
    OutFileName := ParamStr(2)  {Output file specified}
  else if IsDfmInFile then
    OutFileName := ChangeFileExt(InFileName, LfmFileExt);
     {Base Lazarus form file name on Delphi form file name}
  if OutFileName = '' then
    begin
    WriteLn('No output file specified');
    Halt;  {If converting a Lazarus form file, have to specify output file}
    end;

{$IFNDEF FPC}
  OutFileName := ExpandFileNameCase(OutFileName, MatchFound);
{$ELSE}
  OutFileName := ExpandFileName(OutFileName);
{$ENDIF}

  if SameText(InFileName, OutFileName) then
    begin
    WriteLn('Output file is same as input file');
    Halt;
    end;

   {Open input form file}
  AssignFile(InFileVar, InFileName);
  try
    Reset(InFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t open input form file ', InFileName);
      Halt;
      end;
    end;

   {Create output form file}
  AssignFile(OutFileVar, OutFileName);
  try 
    Rewrite(OutFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t create output form file ', OutFileName);
      Halt;
      end;
    end;

  StackLevel := 0;
  while not Eof(InFileVar) do  {Read and process input form file}
    begin
    DeleteLine := False;
    ReadLn(InFileVar, InStr);  {Read property from form file}
    StripStr := StringReplace(InStr, ' ', '', [rfReplaceAll]); {Strip spaces}

    if (SameText('object ', Copy(Trim(InStr), 1, 7)) or
        SameText('end', StripStr)) and  {End of object's props reached?}
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
          WriteLn(OutFileVar, StringOfChar(' ', (StackLevel-ParentLevel)*2), 
                  StackRec[ParentLevel].FontProps[FontPropNum]);
          end;
        end;
      StackRec[StackLevel].FontAdded := True;
      end;

    if SameText('object ', Copy(Trim(InStr), 1, 7)) then
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

    else if SameText('end', StripStr) then
      begin  {Pop current class from stack}
      Dec(StackLevel);
      end

    else if SameText('font.', Copy(Trim(InStr), 1, 5)) then
      begin  {Font property}
      if FontSwitch = UseParentFont then
        begin  {Save font property in case need it for child objects}
        if StackRec[StackLevel].FontPropCnt < MaxFontProps then
          begin
          Inc(StackRec[StackLevel].FontPropCnt);
          StackRec[StackLevel].FontProps[StackRec[StackLevel].FontPropCnt] :=
          InStr;
          end;
        end
      else if FontSwitch = DeleteFontName then
        begin
        if SameText('font.name', Copy(Trim(InStr), 1, 9)) then
          DeleteLine := True;
        end;
       {Check if font property should be deleted from current object}
      if IsDfmInFile and
         CfgFileObj.ValueExists('DeleteProps',
                                StackRec[StackLevel].ClassName + '.' +
                                Copy(StripStr, 1, Pos('=', StripStr)-1)) then
        DeleteLine := True;
      end

    else if Copy(StripStr, Length(StripStr), 1) = '<' then  {Skip to end>?}
      begin
      repeat
        WriteLn(OutFileVar, InStr);
        ReadLn(InFileVar, InStr);
      until Trim(InStr) = 'end>';
      end

    else if Pos('=', StripStr) > 0 then  {Other property?}
      begin  {Check if property should be deleted from current object}
      if IsDfmInFile and
         (CfgFileObj.ValueExists('DeleteProps', 
                                 Copy(StripStr, 1, Pos('=', StripStr)-1)) or
          CfgFileObj.ValueExists('DeleteProps',
                                 StackRec[StackLevel].ClassName + '.' +
                                 Copy(StripStr, 1, Pos('=', StripStr)-1))) then
        begin  {Property or class.property in list of props to delete?} 
        DeleteLine := True;
        if Copy(StripStr, Length(StripStr), 1) = '(' then  {Delete > 1 line?}
          begin
          repeat
            ReadLn(InFileVar, SkipStr);
            SkipStr := Trim(SkipStr);
          until Copy(SkipStr, Length(SkipStr), 1) = ')';
          end;
        end;
      end;  

    if not DeleteLine then  {Include line in output form file?}
      begin
      try
         {If Delphi form file does have Height and Width, reduce
           to size of its ClientHeight or ClientWidth.}
        if IsDfmInFile and (StackLevel = 1) and
           SameText('Height=', Copy(StripStr, 1, 7)) then
          WriteLn(OutFileVar,
                  '  Height = ', 
                  IntToStr(StrToInt(Copy(StripStr, 8, MaxInt)) - 34))
        else if IsDfmInFile and (StackLevel = 1) and
                SameText('Width=', Copy(StripStr, 1, 6)) then
          WriteLn(OutFileVar,
                  '  Width = ', 
                  IntToStr(StrToInt(Copy(StripStr, 7, MaxInt)) - 8))

         {LCL TGroupBox child controls' Top measures from a lower position
           within group box than with VCL, so reduce Top value}
        else if IsDfmInFile and (StackLevel > 1) and
                SameText('Top=', Copy(StripStr, 1, 4)) and
                SameText('TGroupBox', StackRec[Pred(StackLevel)].ClassName) then
          WriteLn(OutFileVar,
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
            WriteLn(OutFileVar, 
                    StringReplace(InStr, 'Top', 'Left', [rfIgnoreCase]))
          else
            WriteLn(OutFileVar,
                    StringReplace(InStr, 'Left', 'Top', [rfIgnoreCase]));
          end
*)
  
        else if (FontSwitch = SubstFontName) and
                SameText('font.name', Copy(StripStr, 1, 9)) then
          begin
          StripStr := Copy(InStr, Pos('=', InStr)+3, MaxInt); {Name after quote}
          Delete(StripStr, Length(StripStr), 1);  {Delete closing quote}
          if MacSwitch and
             CfgFileObj.ValueExists('MacFontSubstitutes', StripStr) then
            WriteLn(OutFileVar,
                    Copy(InStr, 1, Succ(Pos('=', InStr))), '''',
                    CfgFileObj.ReadString('MacFontSubstitutes', StripStr, ''), 
                    '''')
          else if CfgFileObj.ValueExists('FontSubstitutes', StripStr) then
            WriteLn(OutFileVar,
                    Copy(InStr, 1, Succ(Pos('=', InStr))), '''',
                    CfgFileObj.ReadString('FontSubstitutes', StripStr, ''), 
                    '''')
          else
            WriteLn(OutFileVar, InStr);
          end

        else if MacSwitch and
                (StackLevel > 1) and
                (SameText('TButton', StackRec[StackLevel].ClassName) or
                 SameText('TBitBtn', StackRec[StackLevel].ClassName)) and
                SameText('Height=', Copy(StripStr, 1, 7)) and
                (StrToInt(Copy(StripStr, 8, MaxInt)) > 22) then
          WriteLn(OutFileVar,
                  Copy(InStr, 1, Succ(Pos('=', InStr))), '22')
           {Reduce button height so it's displayed as oval on Mac.
            TODO: TSpeedButton too?}
           
        else if MacSwitch and
                (StackLevel > 1) and
                SameText('TabOrder=', Copy(StripStr, 1, 9)) and
                CfgFileObj.ValueExists('MacNoFocus',
                                       StackRec[StackLevel].ClassName) then
          begin
          WriteLn(OutFileVar, InStr);  {No change to TabOrder property}
          WriteLn(OutFileVar,
                  Copy(InStr, 1, Length(InStr)-Length(Trim(InStr))), {Spaces}
                  'TabStop = False');  {Control can't receive focus}
          end

        else  {No change to property}
          WriteLn(OutFileVar, InStr);

         {Delphi form files don't always include Height or Width properties,
           which are required by Lazarus, so add them based on ClientHeight 
           and ClientWidth properties, which apparently act the same as
           Height and Width in Lazarus (unlike Delphi).}
        if IsDfmInFile and
           (SameText('ClientHeight=', Copy(StripStr, 1, 13)) or
            SameText('ClientWidth=', Copy(StripStr, 1, 12))) then
          WriteLn(OutFileVar, 
                  StringReplace(InStr, 'Client', '',  [rfIgnoreCase]));
      except
        on EInOutError do
          begin
          WriteLn('Can''t write to output form file ', OutFileName);
          Halt;
          end; 
        end;
      end;
    end;  {while not Eof}  

  CloseFile(InFileVar);
  try   
    CloseFile(OutFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t close output form file ', OutFileName);
      Halt;
      end;
    end;
  CfgFileObj.Free;
  WriteLn(OutFileName, ' successfully created');

end.

