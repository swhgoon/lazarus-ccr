{ $Id }
{ svnsync-like utility written with freepascal

  fpsvnsync synchronizes two svn repositories without the need to set
  revision properties.

  Copyright (C) 2007 Vincent Snijders (vincents@freepascal.org)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
program fpsvnsync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  FileUtil,
  SvnClasses, SvnCommand;

type

  { TSvnMirrorApp }

  TSvnMirrorApp = class(TCustomApplication)
  private
    FSourceWC: string;
    FDestWC: string;
    function GetRevision(Directory: string): integer;
    function GetRepositoryRoot(Directory: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  end;

{ TSvnMirrorApp }

function TSvnMirrorApp.GetRevision(Directory: string): integer;
var
  SvnInfo: TSvnInfo;
begin
  SvnInfo := TSvnInfo.Create(Directory);
  Result := SvnInfo.Entry.Revision;
  SvnInfo.Free;
end;

function TSvnMirrorApp.GetRepositoryRoot(Directory: string): string;
var
  SvnInfo: TSvnInfo;
begin
  SvnInfo := TSvnInfo.Create(Directory);
  Result := SvnInfo.Entry.Repository.Root;
  SvnInfo.Free;
end;

procedure TSvnMirrorApp.Run;
var
  SourceHead: integer;
  Revision: integer;
  XmlOutput: TMemoryStream;
  //SvnResult: LongInt;
  SvnLog: TSvnLog;
  SubPath: string;
  DestRoot: string;

  procedure GetLog;
  var
    Command: string;
  begin
    writeln('Getting log message for revision ', Revision);
    Command := Format('log --xml -v -r%d %s', [Revision,FSourceWC]);
    SvnLog.LoadFromCommand(command);
    SvnLog.LogEntry[0].SortPaths;
    SubPath := SvnLog.LogEntry[0].CommonPath;
    writeln('Finding common path from log messages: ', SubPath);
  end;

  procedure UpdateWC(const WorkingDir, SubPath: string; Revision: integer);
  var
    Command: string;
    UpdatePath: string;
    RevisionStr: string;
  begin
    UpdatePath := WorkingDir+SubPath;
    if Revision > 0 then
      RevisionStr := IntToStr(Revision)
    else
      RevisionStr := 'HEAD';
    writeln(format('Updating %s to revision %s', [UpdatePath, RevisionStr]));
    Command := Format('up -r%s %s', [RevisionStr, UpdatePath]);
    writeln('svn ', Command);
    writeln('Result: ', ExecuteSvnCommand(Command));
  end;

  procedure GetDiff;
  var
    Command: string;
    Diff: TStrings;
  begin
    writeln('Getting diffs between revision ', Revision-1,' and ', Revision);
    Command := Format('diff -c%d %s', [Revision, FSourceWC+SubPath]);
    writeln('svn ', Command);
    XmlOutput.Clear;
    ExecuteSvnCommand(Command, XmlOutput);
    XmlOutput.Position := 0;
    Diff := TStringList.Create;
    Diff.LoadFromStream(XmlOutput);
    writeln('Diff contains ', Diff.Count, ' lines');
    if pos('Property changes on', Diff.Text)>0 then begin
      writeln('Properties changed');
      writeln(Diff.Text);
      Diff.Free;
      halt(1);
    end;
    Diff.Free;
  end;

  procedure DeleteFiles;
  var
    LogEntry: TLogEntry;
    LogPath: TLogPath;
    i: integer;
    DestFile: string;
  begin
    LogEntry := SvnLog.LogEntry[0];
    for i := 0 to LogEntry.PathCount-1 do begin
      LogPath := LogEntry.Path[i];
      if LogPath.Action=caDelete then begin
        DestFile := FDestWC + LogPath.Path;
        writeln('Deleting ', DestFile);
        ExecuteSvnCommand('delete '+DestFile);
      end;
    end;
  end;

  procedure CopyChanges;
  var
    LogEntry: TLogEntry;
    LogPath: TLogPath;
    i: integer;
    SourceFile, DestFile, Command: string;
  begin
    LogEntry := SvnLog.LogEntry[0];
    for i := 0 to LogEntry.PathCount-1 do begin
      LogPath := LogEntry.Path[i];
      DestFile := FDestWC + LogPath.Path;
      if LogPath.Action in [caModify, caAdd] then begin
        SourceFile := FSourceWC + LogPath.Path;
        if LogPath.CopyFromPath<>'' then begin
          if ExtractFileName(LogPath.CopyFromPath)=ExtractFileName(DestFile) then
            // to prevent that svn complains that the target is not a directory
            Command := format('copy "%1:s%2:s@%0:d" "%3:s"',
              [LogPath.CopyFromRevision, DestRoot, LogPath.CopyFromPath,
                ExtractFileDir(DestFile)])
          else
            Command := format('copy "%1:s%2:s@%0:d" "%3:s"',
              [LogPath.CopyFromRevision, DestRoot,
                LogPath.CopyFromPath, DestFile]);
          writeln('svn '+ Command);
          ExecuteSvnCommand(Command);
        end;
        writeln('Copy ', SourceFile, ' to ', DestFile);
        if DirectoryExists(SourceFile) then
          ForceDirectory(DestFile)
        else
          CopyFile(SourceFile, DestFile, true);
        if LogPath.Action=caAdd then begin
          Command := format('add "%s"', [DestFile]);
          writeln(Command);
          writeln('Result: ',ExecuteSvnCommand(Command));
        end;
      end;
    end;
  end;

  procedure ApplyPropChanges;
  var
    Files: TStrings;
    SourcePropInfo: TSvnPropInfo;
    DestPropInfo: TSvnPropInfo;
    SourceFileName: string;
    SourceFileProp: TSvnFileProp;
    DestFileName: string;
    DestFileProp: TSvnFileProp;
    i: Integer;

    function CreatePropInfo(const BaseDir: string): TSvnPropInfo;
    begin
      Result := TSvnPropInfo.Create;
      Files := SvnLog.LogEntry[0].GetFileList(BaseDir);
      Result.LoadForFiles(Files);
      Files.Free;
    end;

    procedure CopyFileProp(SourceProp, DestProp: TSvnFileProp);
    var
      j, pass: integer;
      IsSvnEolProp: boolean;
      Command: string;
    begin
      if SourceProp.Properties.Text=DestProp.Properties.Text then exit;

      writeln('Properties changed for ', DestProp.FileName);
      writeln('Source properties');
      writeln(SourceProp.Properties.Text);
      writeln('Destination properties');
      writeln(DestProp.Properties.Text);

      for j:=0 to DestProp.Properties.Count-1 do begin
        Command := format('propdel %s "%s"',
          [DestProp.Properties.Names[j], DestProp.FileName]);
        writeln('svn ', Command);
        writeln('svn result: ', ExecuteSvnCommand(Command));
      end;
      // first pass set svn:eolstyle, later it might not be possible
      // because of the mime style is non-text.
      for pass := 1 to 2 do begin
        for j:=0 to SourceProp.Properties.Count-1 do begin
          // if there is no value, don't set the property
          if (SourceProp.Properties.ValueFromIndex[j]='')
            then continue;

          IsSvnEolProp := SourceProp.Properties.Names[j]='svn:eol-style';
          if ((pass=1) and (IsSvnEolProp=true)) or
             ((pass=2) and (IsSvnEolProp=false)) then begin
            Command := format('propset %s "%s" "%s"',
              [SourceProp.Properties.Names[j],
               SourceProp.Properties.ValueFromIndex[j],
               DestProp.FileName]);
            writeln('svn ', Command);
            writeln('svn result: ', ExecuteSvnCommand(Command));
          end;
        end;
      end;
    end;

  begin
    SourcePropInfo := CreatePropInfo(FSourceWC);
    DestPropInfo := CreatePropInfo(FDestWC);
    Files := SvnLog.LogEntry[0].GetFileList('');

    if SourcePropInfo.FileCount<>Files.Count then begin
        writeln('Source FileName number mismatch: ',
          SourcePropInfo.FileCount, '<>', Files.Count);
        for i := 0 to SourcePropInfo.FileCount - 1 do
          writeln('Source ',i ,': ',SourcePropInfo.FileItem[i].FileName);
        halt(2);
    end;

    if DestPropInfo.FileCount<>Files.Count then begin
        writeln('Destination FileName number mismatch: ',
          DestPropInfo.FileCount, '<>', Files.Count);
        for i := 0 to DestPropInfo.FileCount - 1 do
          writeln('Dest ',i ,': ',DestPropInfo.FileItem[i].FileName);
        halt(2);
    end;

    for i := 0 to Files.Count-1 do begin
      SourceFileName := FSourceWC + Files[i];
      DestFileName := FDestWC + Files[i];
      SourceFileProp := SourcePropInfo.GetFileItem(SourceFileName);
      DestFileProp := DestPropInfo.GetFileItem(DestFileName);

      if SourceFileProp=nil then begin
        writeln('Missing source file properties for ', SourceFileName);
        halt(3);
      end;

      if DestFileProp=nil then begin
        writeln('Missing destination file properties for ', DestFileName);
        halt(3);
      end;

      CopyFileProp(SourceFileProp, DestFileProp);
    end;

    Files.Free;
    SourcePropInfo.Free;
    DestPropInfo.Free;
  end;

  procedure CommitChanges;
  var
    Command: string;
    MessageFile: string;
    Message: TStrings;
    LogEntry: TLogEntry;
  begin
    writeln('Commit to destination');
    LogEntry := SvnLog.LogEntry[0];
    MessageFile := SysUtils.GetTempFileName;
    Message := TStringList.Create;
    Message.Add(SvnLog.LogEntry[0].Message);
    Message.Add(
      Format('Commited by %s at %s', [LogEntry.Author, LogEntry.DisplayDate]));
    Message.SaveToFile(MessageFile);
    writeln(Message.Text);
    Message.Free;
    Command := Format('commit -F "%s" "%s"', [MessageFile, FDestWC+LogEntry.CommonPath]);
    writeln('svn ', Command);
    writeln('svn commit result: ', ExecuteSvnCommand(Command));
    DeleteFile(MessageFile);
  end;

begin
  try
    SourceHead := GetRevision('-rHEAD '+FSourceWC);
    writeln(FSourceWC, ' HEAD at revision ', SourceHead);

    Revision := GetRevision('-rHEAD '+FDestWC);
    writeln(FDestWC, ' HEAD at revision ', Revision);

    DestRoot := GetRepositoryRoot(FDestWC);
    writeln('------');
  except on E: Exception do
    begin
      writeln(E.Message);
      halt(9);
    end;
  end;
  XmlOutput := TMemoryStream.Create;

  SvnLog := TSvnLog.Create;
  while (Revision<SourceHead) do begin
    inc(Revision);

    GetLog;

    UpdateWC(FDestWC, SvnLog.LogEntry[0].CommonPath, Revision-1);
    UpdateWC(FSourceWC, SvnLog.LogEntry[0].CommonPath, Revision);

    writeln('Doing adds/deletes');
    DeleteFiles;
    CopyChanges;
    //GetDiff;
    ApplyPropChanges;
    CommitChanges;

    writeln;
  end;

  XmlOutput.Free;
  SvnLog.Free;
end;

constructor TSvnMirrorApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if ParamCount=2 then begin
    FSourceWC := ParamStr(1);
    FDestWC := ParamStr(2);
  end
  else
  begin
    FSourceWC := 'd:\lazarus\lazmirror\source';
    FDestWC := 'd:\lazarus\lazmirror\dest';
    FSourceWC := 'C:\lazarus\lazmirror\source';
    FDestWC := 'C:\lazarus\lazmirror\dest';
  end;

end;

destructor TSvnMirrorApp.Destroy;
begin
  inherited Destroy;
end;

var
  SvnMirrorApp: TSvnMirrorApp;

begin
  SvnMirrorApp := TSvnMirrorApp.Create(nil);
  try
    SvnMirrorApp.Run;
  finally
    SvnMirrorApp.Free;
  end;
end.

