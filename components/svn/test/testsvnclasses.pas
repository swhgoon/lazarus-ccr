unit TestSvnClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  svnclasses;

type

  { TTestSvnClasses }

  TTestSvnClasses= class(TTestCase)
  private
    function GetInfoFileName: string;
    function GetLogFileName: string;
    function GetPropFileName: string;
  published
    procedure TestHookUp;
    procedure TestLoadInfo;
    procedure TestInfoCreateUrl;
    procedure TestLoadLog;
    procedure TestLoadSimpleLogPaths;
    procedure TestLoadComplexLogPaths;
    procedure TestLoadLogTwice;
    procedure TestLogCommonPath;
    procedure TestLogFiles;
    procedure TestPropList;
    procedure TestPropListLoadForFiles;
  end;

implementation

function TTestSvnClasses.GetInfoFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'info.xml';
end;

function TTestSvnClasses.GetLogFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'log.xml';
end;

function TTestSvnClasses.GetPropFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'proplist.txt';
end;

procedure TTestSvnClasses.TestHookUp;
  procedure CheckFile(const FileName: string);
  begin
    AssertTrue(FileName + ' does not exist', FileExists(FileName));
  end;
begin
  CheckFile(GetInfoFileName);
  CheckFile(GetLogFileName);
end;

procedure TTestSvnClasses.TestLoadInfo;
var
  SvnInfo: TSvnInfo;
begin
  SvnInfo := TSvnInfo.Create;
  try
    SvnInfo.LoadFromFile(GetInfoFileName);
    AssertEquals('Wrong revision', 10685, SvnInfo.Entry.Revision);
    AssertEquals('Wrong path', '.', SvnInfo.Entry.Path);
    AssertEquals('Wrong kind', ord(ekDirectory), ord(SvnInfo.Entry.Kind));
    AssertEquals('Wrong URL',
      'svn+ssh://www.freepascal.org/FPC/svn/lazarus/trunk',
      SvnInfo.Entry.URL);
    AssertEquals('Wrong repository root',
      'svn+ssh://www.freepascal.org/FPC/svn/lazarus',
      SvnInfo.Entry.Repository.Root);
    AssertEquals('Wrong repository UUID',
      '4005530d-fff6-0310-9dd1-cebe43e6787f',
      SvnInfo.Entry.Repository.UUID);
    AssertEquals('Wrong commit revision', 10680, SvnInfo.Entry.Commit.Revision);
    AssertEquals('Wrong commit author', 'jesus', SvnInfo.Entry.Commit.Author);
    AssertEquals('Wrong commit date',
      '2007-02-25T22:55:08.029980Z', SvnInfo.Entry.Commit.Date);
  finally
    SvnInfo.Free;
  end;
end;

procedure TTestSvnClasses.TestInfoCreateUrl;
var
  SvnInfo: TSvnInfo;
begin
  SvnInfo := TSvnInfo.Create('.');
  try
      AssertEquals('Wrong repository UUID',
        '8e941d3f-bd1b-0410-a28a-d453659cc2b4',
        SvnInfo.Entry.Repository.UUID);
  finally
    SvnInfo.Free;
  end;
end;

procedure TTestSvnClasses.TestLoadLog;
var
  SvnLog: TSvnLog;
  LogEntry: TLogEntry;
begin
  SvnLog := TSvnLog.Create;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
    LogEntry := SvnLog.LogEntry[0];
    AssertEquals('Wrong log revision', 10660, LogEntry.Revision);
    AssertEquals('Wrong log author', 'vincents', LogEntry.Author);
    AssertEquals('Wrong log date',
      '2007-02-20T10:57:42.928052Z', LogEntry.Date);
    AssertEquals('Wrong log display date',
      '2007-02-20 10:57:42', LogEntry.DisplayDate);
    AssertEquals('Wrong log message',
      'TAChart: added icon, added to make bigide', LogEntry.Message);
  finally
    SvnLog.Free;
  end;
end;

procedure TTestSvnClasses.TestLoadSimpleLogPaths;
var
  SvnLog: TSvnLog;
  LogEntry: TLogEntry;
  LogPath: TLogPath;
begin
  SvnLog := TSvnLog.Create;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
    LogEntry := SvnLog.LogEntry[4];
    AssertEquals('Wrong log revision', 10664, LogEntry.Revision);
    AssertEquals('Wrong number of paths', 1, LogEntry.PathCount);
    LogPath := LogEntry.Path[0];
    AssertEquals('Wrong path',
      '/trunk/lcl/interfaces/win32/win32callback.inc', LogPath.Path);
    AssertEquals('Wrong commit action', ord(caModify), ord(LogPath.Action));
  finally
    SvnLog.Free;
  end;
end;

procedure TTestSvnClasses.TestLoadComplexLogPaths;
var
  SvnLog: TSvnLog;
  LogEntry: TLogEntry;
  
  procedure AssertLogPath(i: integer; action: TCommitAction;
    const path, copyfrompath: string; copyfromrev: integer);
  var
    LogPath: TLogPath;
  begin
    LogPath := LogEntry.Path[i];
    AssertEquals('Wrong commit action', ord(action), ord(LogPath.Action));
    AssertEquals('Wrong path', path, LogPath.Path);
    AssertEquals('Wrong copy from revision', copyfromrev, LogPath.CopyFromRevision);
    AssertEquals('Wrong copy from path', copyfrompath, LogPath.CopyFromPath);
  end;
begin
  SvnLog := TSvnLog.Create;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
    LogEntry := SvnLog.LogEntry[3];
    AssertEquals('Wrong log revision', 10663, LogEntry.Revision);
    AssertEquals('Wrong number of paths', 5, LogEntry.PathCount);
    AssertLogPath(0, caDelete, '/trunk/components/tachart/TAEngine.pas', '', 0);
    AssertLogPath(1, caAdd, '/trunk/components/tachart/taengine.pas',
      '/trunk/components/tachart/TAEngine.pas', 10662);
  finally
    SvnLog.Free;
  end;
end;

procedure TTestSvnClasses.TestLoadLogTwice;
var
  SvnLog: TSvnLog;
begin
  SvnLog := TSvnLog.Create;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
  finally
    SvnLog.Free;
  end;
end;

procedure TTestSvnClasses.TestLogCommonPath;
var
  SvnLog: TSvnLog;
  procedure AssertCommonPath(i: integer;const ACommonPath: string);
  var
    LogEntry: TLogEntry;
  begin
    LogEntry := SvnLog.LogEntry[i];
    AssertEquals('Wrong common path '+IntToStr(i), ACommonPath, LogEntry.CommonPath);
  end;

begin
  SvnLog := TSvnLog.Create;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
    AssertCommonPath(4, '/trunk/lcl/interfaces/win32/');
    AssertCommonPath(5, '/trunk/lcl/interfaces/win32/');
    AssertCommonPath(3, '/trunk/components/tachart/');
    AssertCommonPath(0, '/trunk/');
  finally
    SvnLog.Free;
  end;
end;

procedure TTestSvnClasses.TestLogFiles;
var
  SvnLog: TSvnLog;
  Files: TStrings;
  i: Integer;
const
  DeletedFileCount : array[0..5] of byte = (0,1,1,2,0,0);
begin
  SvnLog := TSvnLog.Create;
  Files := nil;
  try
    SvnLog.LoadFromFile(GetLogFileName);
    AssertEquals('Wrong number of log entries', 6, SvnLog.LogEntryCount);
    for i := 0 to SvnLog.LogEntryCount - 1 do begin
      Files := SvnLog.LogEntry[i].GetFileList;
      AssertEquals('Wrong number of files for entry ' + IntToStr(i),
        SvnLog.LogEntry[i].PathCount - DeletedFileCount[i], Files.Count);
      FreeAndNil(Files);
    end;
  finally
    SvnLog.Free;
    Files.Free
  end;
end;

procedure TTestSvnClasses.TestPropList;
var
  SvnPropInfo: TSvnPropInfo;
  
  procedure AssertFileProp(i: integer; const FileName: string);
  var
    FileProp: TSvnFileProp;
  begin
    FileProp := SvnPropInfo.FileItem[i];
    AssertEquals('Wrong file name', FileName, FileProp.FileName);
    AssertEquals('Wrong number of properties', 2, FileProp.Properties.Count);
    AssertEquals('Wrong property name', 'svn:mime-type', FileProp.Properties.Names[0]);
    AssertEquals('Wrong property value', 'text/plain', FileProp.Properties.ValueFromIndex[0]);
  end;
begin
  SvnPropInfo := TSvnPropInfo.Create;
  try
    SvnPropInfo.LoadFromFile(GetPropFileName);
    AssertEquals('Wrong number of files', 3, SvnPropInfo.FileCount);
    AssertFileProp(0, 'testsvnclasses.pas');
    AssertFileProp(1, 'testsvncommand.pas');
    AssertFileProp(2, 'fpcunitsvnpkg.lpi');
  finally
    SvnPropInfo.Free;
  end;
end;

procedure TTestSvnClasses.TestPropListLoadForFiles;
var
  SvnPropInfo: TSvnPropInfo;
  FileNames: TStrings;
begin
  FileNames:= TStringList.Create;
  FileNames.Add('testsvnclasses.pas');
  FileNames.Add('fpcunitsvnpkg.lpi');
  SvnPropInfo := TSvnPropInfo.Create;
  try
    SvnPropInfo.LoadForFiles(FileNames);
    AssertEquals('Wrong number of files', 2, SvnPropInfo.FileCount);
    AssertEquals('Wrong file name', FileNames[0], SvnPropInfo[0].FileName);
    AssertEquals('Wrong file name', FileNames[1], SvnPropInfo[1].FileName);
  finally
    FileNames.Free;
    SvnPropInfo.Free;
  end;
end;

initialization

  RegisterTest(TTestSvnClasses); 
end.

