program MakePasX;

{
  Makes Delphi form code file or project file cross-platform
   so it can be compiled by both Delphi and Lazarus/FPC.
  Note that this is a one-time conversion.
  Note: Use DfmToLfm to convert form's design file to LCL
   whenever changes are made to form in Delphi.
   
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
  SysUtils;

const
  ProgramName    = 'MakePasX';
  ProgramVersion = '0.03';
  
  PasFileExt     = '.pas';  {Pascal code file extension}
  DelProjFileExt = '.dpr';  {Delphi project file extension}
  TmpFileExt     = '.tmp';  {Create converted file with this extension}
  BakFileExt     = '.bak';  {Rename original file with this extension}

var
  OldFileName : string;
  NewFileName : string;
  OldFileVar  : TextFile;
  NewFileVar  : TextFile;
{$IFNDEF FPC}
  MatchFound  : TFilenameCaseMatch;
{$ENDIF}
  IsProject   : Boolean;
  InStr       : string;
  FoundUses   : Boolean;
  Done        : Boolean;
  UnitPos     : Integer;
  HasAppInit  : Boolean;
  HasForm     : Boolean;

begin

  if ParamCount = 0 then  {List program useage?}
    begin
    WriteLn(ProgramName, ', version ', ProgramVersion,
            ' - makes Delphi code file cross-platform.');
    WriteLn('Usage: ', ProgramName, ' filename[', PasFileExt, '|', 
            DelProjFileExt, ']');
    Halt;
    end;

   {Get name of Pascal code file to convert from command line}
  OldFileName := ParamStr(1);
  if ExtractFileExt(OldFileName) = '' then  {No extension?}
    OldFileName := OldFileName + PasFileExt;  {Assume it's not a project file}
{$IFNDEF FPC}
  OldFileName := ExpandFileNameCase(OldFileName, MatchFound);
{$ELSE}
  OldFileName := ExpandFileName(OldFileName);
{$ENDIF}
  
  IsProject := CompareText(ExtractFileExt(OldFileName), DelProjFileExt) = 0;
    
  NewFileName := ChangeFileExt(OldFileName, TmpFileExt);
  
   {Open code file}
  AssignFile(OldFileVar, OldFileName);
  try
    Reset(OldFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t open Pascal code file ', OldFileName);
      Halt;
      end;
    end;

   {Create new code file}
  AssignFile(NewFileVar, NewFileName);
  try 
    Rewrite(NewFileVar);
  except
    on EInOutError do
      begin
      WriteLn('Can''t create new code file ', NewFileName);
      Halt;
      end;
    end;

  FoundUses := False;
  HasAppInit := False;
  HasForm := False;
  while not Eof(OldFileVar) do  {Read and convert Pascal code file}
    begin
    ReadLn(OldFileVar, InStr);  {Read line of code}
    
    if not IsProject then  {Form code file?}
      begin
      if (CompareText(InStr, 'uses') = 0) and  {Found uses section?}    
         (not FoundUses) then  {And in interface section?}
        begin  {Note assumes "uses" appears on separate line from list of units}
        FoundUses := True;
        WriteLn(NewFileVar, InStr);
        WriteLn(NewFileVar, 
                '  {$IFNDEF LCL} Windows, Messages, ',
                '{$ELSE} LclIntf, LMessages, LclType, {$ENDIF}');  //LResources not needed anymore
//                '{$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}');
        ReadLn(OldFileVar, InStr);
        repeat
          UnitPos := Pos('WINDOWS,', UpperCase(InStr));
          if UnitPos > 0 then
            Delete(InStr, UnitPos, 8);
          if Copy(InStr, UnitPos, 1) = ' ' then
            Delete(InStr, UnitPos, 1);
          UnitPos := Pos('MESSAGES,', UpperCase(InStr));
          if UnitPos > 0 then
            Delete(InStr, UnitPos, 9);         
          if Copy(InStr, UnitPos, 1) = ' ' then
            Delete(InStr, UnitPos, 1);
          UnitPos := Pos('WINTYPES,', UpperCase(InStr));  {Synonym for Windows}
          if UnitPos > 0 then
            Delete(InStr, UnitPos, 9);         
          if Copy(InStr, UnitPos, 1) = ' ' then
            Delete(InStr, UnitPos, 1);
          UnitPos := Pos('WINPROCS,', UpperCase(InStr));  {Synonym for Windows}
          if UnitPos > 0 then
            Delete(InStr, UnitPos, 9);         
          if Copy(InStr, UnitPos, 1) = ' ' then
            Delete(InStr, UnitPos, 1);
          WriteLn(NewFileVar, InStr);
          Done := Pos(';', InStr) > 0;
          if not Done then
            ReadLn(OldFileVar, InStr);
        until Done;                           
        end  {uses section}
      
      else if CompareText(Copy(Trim(InStr), 1, 10), 
                          '{$R *.dfm}') = 0 then  {Form's resource file?}
        begin
        WriteLn(NewFileVar, '{$IFNDEF LCL}');
        WriteLn(NewFileVar, InStr);
        WriteLn(NewFileVar, '{$ELSE}');     //Added this   
        WriteLn(NewFileVar, '{$R *.lfm}');  //Added this              
        WriteLn(NewFileVar, '{$ENDIF}');
        HasForm := True;
        end  

      else if (CompareText(InStr, 'end.') = 0) and HasForm then  {End of unit?}
        begin
(*  // not needed anymore
         {Note: Make sure IFDEF goes after initialization since Delphi
           inserts new event handlers immediately before initialization line.}
        WriteLn(NewFileVar, 'initialization');
        WriteLn(NewFileVar, '{$IFDEF LCL}');
        WriteLn(NewFileVar, '{$I ', ChangeFileExt(ExtractFileName(OldFileName), 
                            '.lrs}  {Include form''s resource file}'));
        WriteLn(NewFileVar, '{$ENDIF}');
        WriteLn(NewFileVar);
*)
        WriteLn(NewFileVar, InStr);
        end

      else  {Nothing to change with this line} 
        WriteLn(NewFileVar, InStr);
      end
      
    else  {Delphi project file}
      begin
      if (CompareText(InStr, 'uses') = 0) and  {Found uses section?}    
         (not FoundUses) then  {And in interface section?}
        begin  {Note assumes "uses" appears on separate line from list of units}
        FoundUses := True;
        WriteLn(NewFileVar, InStr);
        WriteLn(NewFileVar, '{$IFDEF LCL}');
        WriteLn(NewFileVar, '  Interfaces,');
        WriteLn(NewFileVar, '{$ENDIF}');
        end
      else if (CompareText(Copy(Trim(InStr), 1, 10), '{$R *.res}') = 0) or
              (CompareText(Copy(Trim(InStr), 1, 10), '{$R *.r32}') = 0) or
              (CompareText(Copy(Trim(InStr), 1, 10), '{$R *.r16}') = 0) then
        begin  {Program's resource file}
        WriteLn(NewFileVar, '{$IFDEF MSWINDOWS}');
        WriteLn(NewFileVar, InStr);
        WriteLn(NewFileVar, '{$ENDIF}');
        end
      else if CompareText(Copy(Trim(InStr), 1, 3), '{$R') = 0 then
        begin  {Might be a type library or XP manifest resource file}
        WriteLn(NewFileVar, '{$IFNDEF FPC}');
        WriteLn(NewFileVar, InStr);
        WriteLn(NewFileVar, '{$ENDIF}');
        end
      else if Pos('APPLICATION.INITIALIZE', UpperCase(InStr)) > 0 then
        begin
        HasAppInit := True;
        WriteLn(NewFileVar, InStr);
        end
      else
        begin
        if (not HasAppInit) and
           ((Pos('APPLICATION.CREATEFORM', UpperCase(InStr)) > 0) or
            (Pos('APPLICATION.RUN', UpperCase(InStr)) > 0)) then
          begin
          WriteLn(NewFileVar, '  Application.Initialize;');  {Laz needs this}
          HasAppInit := True;
          end; 
        WriteLn(NewFileVar, InStr);
        end;
      end;  
            
    end;  {while not Eof}
    
  DeleteFile(ChangeFileExt(OldFileName, BakFileExt));
  CloseFile(OldFileVar);
  RenameFile(OldFileName, ChangeFileExt(OldFileName, BakFileExt));
  CloseFile(NewFileVar);
  if not IsProject then
    RenameFile(NewFileName, ChangeFileExt(NewFileName, PasFileExt))
  else
    RenameFile(NewFileName, ChangeFileExt(NewFileName, DelProjFileExt));
  WriteLn(OldFileName, ' successfully converted.');
    
end.

