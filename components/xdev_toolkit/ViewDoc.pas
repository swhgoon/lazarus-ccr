unit ViewDoc;

{

  Unit of functions for viewing documents with a word processor.
  
  Author:    Phil Hess.
  Copyright: Copyright (C) 2007 Phil Hess. All rights reserved.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.

}

interface

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF} 

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Registry,
  ShellApi;
{$ENDIF}
{$IFDEF DARWIN}  {OS X}
  BaseUnix,
  Unix;
{$ENDIF}
{$IFDEF LINUX}
  FileUtil,
  Unix;
{$ENDIF}

type
  TViewerOptions = set of (ovwUseAsTemplate, ovwAddToDeleteList);

function GetViewerCount : Integer;

function GetViewerName(Viewer : Integer) : string;

function ViewDocument(const FileName : string;
                            Viewer   : Integer;
                            Options  : TViewerOptions;
                        var ErrorMsg : string) : Boolean;

function DeleteViewedDocs : Boolean;


implementation

const
{$IFDEF MSWINDOWS}
  MaxViewers = 3;  {Number of supported word processors}
  
   {Names of word processors}
  ViewerName : array [1..MaxViewers] of string =
   ('Microsoft Word',
    'OpenOffice',
    'AbiWord');
    
   {Executable files}
  ViewerExe  : array [1..MaxViewers] of string =
   ('WINWORD.EXE',
    'SOFFICE.EXE',
    'AbiWord.exe');
    
   {Command line startup switches.
    If non-blank, start word processor with a new document
     based on the specified template. If blank, open document
     read-only to force user to save under different name.}
  ViewerSwitch : array [1..MaxViewers] of string =
   ('/t',    
    '-n ',
    '');

  ViewerRegKey : array [1..MaxViewers] of string =
   ('',
    '',
    'SOFTWARE\Classes\AbiSuite.AbiWord\shell\open\command');
{$ENDIF}  

{$IFDEF DARWIN}  {OS X}
  MaxViewers = 4;
  
  ViewerName : array [1..MaxViewers] of string =
   ('Microsoft Word',
    'Pages',
    'NeoOffice',
    'AbiWord');
    
   {OS X Open command doesn't support passing switches to app}
  ViewerSwitch : array [1..MaxViewers] of string =
   ('',
    '',
    '',
    ''); 

  MaxViewerFolders = 7;

  ViewerFolders : array [1..MaxViewerFolders] of string =
   ('Microsoft Word',
    'Microsoft Office 2004/Microsoft Word',
    'Microsoft Office X/Microsoft Word',
    'Pages.app',
    'iWork ''06/Pages.app',
    'NeoOffice.app',
    'AbiWord.app');   
{$ENDIF}

{$IFDEF LINUX}
  MaxViewers = 2;
  
  ViewerName : array [1..MaxViewers] of string =
   ('OpenOffice',
    'AbiWord');
    
  ViewerExe : array [1..MaxViewers] of string =
   ('soffice.bin',
    'abiword');
    
  ViewerSwitch : array [1..MaxViewers] of string =
   ('-n ',
    '');
{$ENDIF}


var
  DeleteList : TStringList;  {List of files to delete when program exits;
                               object is created and destroyed in unit's
                               initialization and finalization sections}


function GetViewerCount : Integer;
 {Return number of viewers defined.}
begin
  Result := MaxViewers;
end;


function GetViewerName(Viewer : Integer) : string;
 {Return viewer's name.}
begin
  Result := ViewerName[Viewer];
end;


function LocateViewer(Viewer : Integer) : string;
 {Return path to viewer's executable file,
   or blank string if can't locate viewer.}
{$IFDEF MSWINDOWS}
var
  Reg : TRegistry;
begin
  Result := '';
   {With Windows, installed programs usually have Registry entries
     under the App Paths section, including complete path to program.}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if ViewerRegKey[Viewer] = '' then
      begin
      if Reg.OpenKeyReadOnly(
          '\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + 
          ViewerExe[Viewer]) then
        begin  {Found key, so assume program is installed}
        try
          if Reg.ReadString('') <> '' then  {Key has (Default) registry entry?}
            Result := Reg.ReadString('');
        except  {Trap exception if registry entry does not contain a string}
          end;
        end;
      end
    else  {Non-standard registry key}
      begin
      if Reg.OpenKeyReadOnly(ViewerRegKey[Viewer]) then
        begin  {Found key, so assume program is installed}
        try
          if Reg.ReadString('') <> '' then  {Key as (Default) registry entry?}
            begin
            Result := Reg.ReadString('');
            if Copy(Result, 1, 1) = '"' then  {Strip first quoted item?}
              Result := Copy(Result, 2, Pos('"', Copy(Result, 2, MaxInt))-1);
            end;
        except
          end;
        end;
      end;
  finally
    Reg.Free;
  end;
{$ENDIF}

{$IFDEF DARWIN}  //TODO: Search for app like OS X LaunchServices does.
var
  FolderIdx  : Integer;
  LocIdx     : Integer;
  PathPrefix : string;
begin
  Result := '';
  FolderIdx := 0;
  while (FolderIdx < MaxViewerFolders) and (Result = '') do
    begin
    Inc(FolderIdx);
    if Pos(LowerCase(ViewerName[Viewer]),
           LowerCase(ViewerFolders[FolderIdx])) > 0 then
      begin
      LocIdx := 0;
      while (LocIdx < 4) and (Result = '') do
        begin
        Inc(LocIdx);
        case LocIdx of
          1 : PathPrefix := '/Applications/';
          2 : PathPrefix := '~/Applications/';
          3 : PathPrefix := '~/Desktop/';
          4 : PathPrefix := '~/'
          end;
        if FileExists(PathPrefix + ViewerFolders[FolderIdx]) then
          Result := PathPrefix + ViewerFolders[FolderIdx]; 
        end;
      end;
    end;
{$ENDIF}

{$IFDEF LINUX}
begin
   {Search path for specified file name, returning its
     expanded file name that includes path to it.}
  Result := SearchFileInPath(
             ViewerExe[Viewer], '', GetEnvironmentVariable('PATH'),
             PathSeparator, [sffDontSearchInBasePath]);
{$ENDIF}
end;  {LocateViewer}


function LaunchViewer(const ProgPath   : string;
                      const Params     : string;
                      const DefaultDir : string) : Integer;
 {Start viewer program with specified command line parameters
   by shelling to it, returning shell's code.}
{$IFDEF MSWINDOWS}
var
  ProgPathBuf   : array [0..MAX_PATH] of Char;
  ParamsBuf     : array [0..MAX_PATH] of Char;
  DefaultDirBuf : array [0..MAX_PATH] of Char;
begin
  StrPCopy(ProgPathBuf, ProgPath);
  StrPCopy(ParamsBuf, Params);
  StrPCopy(DefaultDirBuf, DefaultDir);
  Result := ShellExecute(0, nil, ProgPathBuf, ParamsBuf, DefaultDirBuf,
                         SW_SHOWNORMAL);
{$ENDIF}

{$IFDEF DARWIN}
begin
  Result := Shell('Open -a ' + ProgPath + ' ' + Params);
{$ENDIF}

{$IFDEF LINUX}
begin
  Result := Shell(ProgPath + ' ' + Params);
{$ENDIF}
end;  {LaunchViewer}
                      

function ViewDocument(const FileName : string;
                            Viewer   : Integer;
                            Options  : TViewerOptions;
                        var ErrorMsg : string) : Boolean;
 {View FileName with Viewer. If successful, return True; if 
   error, return False and error message in ErrorMsg.}
var
  ProgPath    : string;
  Switches    : string;
  ShellStatus : Integer;
{$IFDEF DARWIN}
  FileInfo    : Stat;
{$ENDIF}
begin
  Result := False;
  ErrorMsg := 'Unexpected error';
  
  if not FileExists(FileName) then
    begin
    ErrorMsg := 'File does not exist.';
    Exit;
    end;

  if ovwAddToDeleteList in Options then
    DeleteList.Add(FileName);

  if Viewer = 0 then  {Use first word processor found?}
    begin
    ProgPath := '';
    while (Viewer < MaxViewers) and (ProgPath = '') do
      begin
      Inc(Viewer);
      ProgPath := LocateViewer(Viewer);
      end;
    if ProgPath = '' then
      begin
      ErrorMsg := 'Unable to locate a word processor.';
      Exit;
      end;
    end
  else  {Use specified word processor}
    begin
    ProgPath := LocateViewer(Viewer);
    if ProgPath = '' then
      begin
      ErrorMsg := ViewerName[Viewer] + ' does not appear to be installed.';
      Exit;
      end;
    end;

  Switches := '';
  if ovwUseAsTemplate in Options then
    begin
    Switches := ViewerSwitch[Viewer];
    if Switches = '' then  {No "template" switch to pass?} 
     {Set file read-only so user has to save under different name}
{$IFDEF MSWINDOWS}
      FileSetAttr(FileName, faReadOnly);     
{$ELSE}  {OS X and Linux}
      begin
      FpStat(FileName, FileInfo);
      FpChmod(FileName, FileInfo.st_mode and ($FFFF XOR S_IWUSR));
      end;
{$ENDIF}
    end;

  ShellStatus := LaunchViewer('"' + ProgPath + '"', 
                              Switches + '"' + FileName + '"', '');
{$IFDEF MSWINDOWS}
  if ShellStatus <= 32 then  {Windows shell error?}
{$ELSE}
  if ShellStatus = 127 then  {Unix shell error?}
{$ENDIF}
    begin
    ErrorMsg := 'Shell error ' + IntToStr(ShellStatus) + 
                ' attempting to start ' + ViewerName[Viewer] + '.';
    Exit;
    end;

  ErrorMsg := '';
  Result := True;
end;  {ViewDocument}


function DeleteViewedDocs : Boolean;
 {Attempt to delete documents in deletion list, returning
   True if all documents deleted or False if unable to
   delete all documents.}
var
  DocNum   : Integer;
{$IFDEF DARWIN}
  FileInfo : Stat;
{$ENDIF}
begin
  Result := True;
  for DocNum := DeleteList.Count - 1 downto 0 do
    begin
    if FileExists(DeleteList.Strings[DocNum]) then
      begin
{$IFDEF MSWINDOWS}
      if (FileGetAttr(DeleteList.Strings[DocNum]) and faReadOnly) <> 0 then
        FileSetAttr(DeleteList.Strings[DocNum],
                    FileGetAttr(DeleteList.Strings[DocNum]) - faReadOnly); 
{$ELSE}  {OS X and Linux}
      FpStat(DeleteList.Strings[DocNum], FileInfo);
      if (FileInfo.st_Mode or S_IWUSR) = 0 then  {File read-only?}
        FpChmod(DeleteList.Strings[DocNum], FileInfo.st_Mode or S_IWUSR);
{$ENDIF}
      if SysUtils.DeleteFile(DeleteList.Strings[DocNum]) then
        DeleteList.Delete(DocNum)
      else
        Result := False;  {At least one doc not deleted}
      end;
    end;  {for DocNum}
end;  {DeleteViewedDocs}


initialization
  DeleteList := TStringList.Create;
  
finalization
  DeleteViewedDocs;
  DeleteList.Free;
  
end.

