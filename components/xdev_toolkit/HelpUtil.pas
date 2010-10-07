unit HelpUtil;

{

  Isolates platform-specific help access for Lazarus LCL.
  
  Assumes Application.HelpFile is set.
  
  Create THelpUtilManager object in main form's FormCreate handler and
   call its Free method in main form's FormDestroy handler.

  Display help topic by calling Application.HelpContext.

  Author:    Phil Hess.
  Copyright: Copyright (C) 2007 Phil Hess. All rights reserved.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.

}

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  Unix,
{$ENDIF}
{$IFDEF LINUX}
  FileUtil,
{$ENDIF}
  Forms,
  Dialogs,
  HelpIntfs;

type
  THelpUtilManager = class(THelpManager)
    destructor Destroy; override;
    procedure ShowError(      ShowResult : TShowHelpResult; 
                        const ErrMsg     : string); override;
    function ShowHelpForQuery(    Query         : THelpQuery; 
                                  AutoFreeQuery : Boolean;
                              var ErrMsg        : string): TShowHelpResult; override;
    end;
    

implementation


const
  HELP_CONTEXT  = 1;
  HELP_QUIT     = 2;

{$IFDEF MSWINDOWS}  {LCL doesn't have TApplication.HelpCommand, so call Win API}
function DoHelpCommand(Command : Word;
                       Data    : LongInt) : Boolean;
begin
  Result := WinHelp(Application.MainForm.Handle, 
                    PChar(Application.HelpFile),
                    Command, Data);
end;  {DoHelpCommand}
{$ENDIF}  


 {THelpUtilManager}

destructor THelpUtilManager.Destroy;
begin
{$IFDEF MSWINDOWS}
  DoHelpCommand(HELP_QUIT, 0);
   {Shut down help application if running and not in use
     by another instance of program}
{$ENDIF}
  inherited Destroy;
end;

procedure THelpUtilManager.ShowError(      ShowResult : TShowHelpResult; 
                                     const ErrMsg     : string);
begin
  if ShowResult = shrHelpNotFound then
    MessageDlg('Help not implemented.', mtError, [mbOK], 0)
  else if ShowResult = shrViewerNotFound then
    MessageDlg('Help viewer not found.', mtError, [mbOK], 0)
  else if ShowResult = shrViewerError then
    MessageDlg('Unable to start help viewer.', mtError, [mbOK], 0)
  else if ShowResult <> shrSuccess then
    MessageDlg(ErrMsg, mtError, [mbOK], 0);
end;

function THelpUtilManager.ShowHelpForQuery(    Query         : THelpQuery; 
                                               AutoFreeQuery : Boolean;
                                           var ErrMsg        : string): TShowHelpResult;
{$IFDEF LINUX}
  function SearchForBrowser(const BrowserFileName : string) : string;
  begin
    Result :=
     SearchFileInPath(BrowserFileName, '', GetEnvironmentVariable('PATH'),
                      PathSeparator, [sffDontSearchInBasePath]);
  end;

  function GetBrowserPath : string;
  begin
    Result := SearchForBrowser('firefox');
    if Result = '' then
      Result := SearchForBrowser('konqueror');  {KDE browser}
    if Result = '' then
      Result := SearchForBrowser('epiphany');  {GNOME browser}
    if Result = '' then
      Result := SearchForBrowser('mozilla');
    if Result = '' then
      Result := SearchForBrowser('opera'); 
  end;
{$ENDIF}

begin
  if Query is THelpQueryContext then  {Is a help context request?}
    begin
 {$IFDEF MSWINDOWS}
    DoHelpCommand(HELP_CONTEXT, THelpQueryContext(Query).Context);
    Result := shrSuccess;
 {$ELSE}  
  {$IFDEF DARWIN} 
    if Shell('Open -a "HelpViewer" "' + Application.HelpFile + '"') = 127 then
//     Note: Renamed from Help Viewer.app to HelpViewer.app with 10.5.
//
//     Note: With OS X earlier than 10.4 (Tiger), if connected to network 
//      but not connected to Internet, takes Help Viewer 1-2 minutes to 
//      recover and turn off rotating ball! Can comment out previous Shell
//      and uncomment next line to use default browser instead. 
//    if Shell('Open "' + Application.HelpFile + '"') = 127 then
      Result := shrViewerError
    else
      Result := shrSuccess;
  {$ELSE}  {For now, shell to first browser found, passing help file name}  
    if GetBrowserPath <> '' then  {Found a browser?}
      begin
      if Shell(GetBrowserPath + ' ' + Application.HelpFile) = 127 then
        Result := shrViewerError
      else
        Result := shrSuccess;
      end
    else
      Result := shrViewerNotFound;
  {$ENDIF}
 {$ENDIF}
    end
  else  {Not a help context request?}
    Result := inherited ShowHelpForQuery(Query, AutoFreeQuery, ErrMsg);
end;  {THelpUtilManager.ShowHelpForQuery}


end.

