{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the iPhone Laz Extension                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit lazfilesutils;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Unix}BaseUnix,{$endif}
  Classes, SysUtils, FileUtil, Masks,
  LazIDEIntf,ProjectIntf;


function ResolveProjectPath(const path: string; project: TLazProject = nil): string;

function BreakPathsStringToOption(const Paths, Switch: String; const Quotes: string = '"'; project: TLazProject = nil): String;

function RelativeToFullPath(const BasePath, Relative: string): String;
function NeedQuotes(const path: string): Boolean;

function CopySymLinks(const SrcDir, DstDir, FilterMask: string): Boolean;

implementation

{$ifdef Unix}
function CopySymLinks(const SrcDir, DstDir, FilterMask: string): Boolean;
var
  allfiles  : TStringList;
  i         : Integer;
  pth       : string;
  MaskList  : TMaskList;
  curdir    : string;
  linkdir   : string;
  linkname  : string;
begin
  Result:=DirectoryExistsUTF8(SrcDir) and ForceDirectoriesUTF8(DstDir);
  if not Result then Exit;

  //todo: don't use FindAllFiles(), use sub dir search

  allfiles:=FindAllFiles(SrcDir, AllFilesMask, False);
  Result:=Assigned(allfiles);
  if not Result then Exit;

  MaskList := TMaskList.Create(FilterMask);

  curdir:=IncludeTrailingPathDelimiter(SrcDir);
  linkdir:=IncludeTrailingPathDelimiter(DstDir);
  for i:=0 to allfiles.Count-1 do begin
    pth:=allfiles[i];
    if (FilterMask='') or (not MaskList.Matches(pth)) then begin
      linkname:=linkdir+Copy(pth, length(curdir), length(pth));
      fpSymlink(PAnsiChar(pth), PAnsiChar(linkname));
    end;
  end;
  allfiles.Free;
end;
{$else}
function CopySymLinks(const SrcDir, DstDir, FilterMask: string): Boolean;
begin
  Result:=false;
end;
{$endif}


function GetNextDir(const Path: string; var index: integer; var Name: string): Boolean;
var
  i : Integer;
begin
  Result:=index<=length(Path);
  if not Result then Exit;

  if Path[index]=PathDelim then inc(index);
  Result:=index<=length(Path);
  if not Result then Exit;

  for i:=index to length(Path) do
    if Path[i]=PathDelim then begin
      Name:=Copy(Path, index, i - index);
      index:=i+1;
      Exit;
    end;
  Name:=Copy(Path, index, length(Path) - index+1);
  index:=length(Path)+1;
end;

function RelativeToFullPath(const BasePath, Relative: string): String;
var
  i  : integer;
  nm : string;
begin
  Result:=ExcludeTrailingPathDelimiter(BasePath);
  i:=1;
  while GetNextDir(Relative, i, nm) do
    if nm = '..' then
      Result:=ExtractFileDir(Result)
    else if nm <> '.' then
      Result:=IncludeTrailingPathDelimiter(Result)+nm;
end;

function NeedQuotes(const path: string): Boolean;
var
  i : integer;
const
  SpaceChars = [#32,#9];
begin
  for i:=1 to length(path) do
    if path[i] in SpaceChars then begin
      Result:=true;
      Exit;
    end;
  Result:=false;
end;

function QuoteStrIfNeeded(const path: string; const quotes: String): String;
begin
  if NeedQuotes(path) then
    Result:=quotes+path+quotes
  else
    Result:=path;
end;

function ResolveProjectPath(const path: string; project: TLazProject): string;
var
  base : string;
begin
  if project=nil then project:=LazarusIDE.ActiveProject;

  if FilenameIsAbsolute(Path) then
    Result:=Path
  else begin
    base:='';
    base:=ExtractFilePath(project.ProjectInfoFile);
    Result:=RelativeToFullPath(base, Path);
  end;
end;

function BreakPathsStringToOption(const Paths, Switch, Quotes: String; project: TLazProject): String;
var
  i, j  : Integer;
  fixed : String;
begin
  Result:='';
  if not Assigned(project) then
    project:=LazarusIDE.ActiveProject;

  if not Assigned(project) then Exit;

  j:=1;
  for i:=1 to length(paths)-1 do
    if Paths[i]=';' then begin
      fixed:=Trim(Copy(paths,j, i-j)  );
      if fixed<>'' then begin
        fixed:=ResolveProjectPath(fixed, project);
        Result:=Result+' ' + Switch + QuoteStrIfNeeded(fixed, quotes);
      end;
      j:=i+1;
    end;

  fixed:=Trim(Copy(paths,j, length(paths)-j+1)  );
  if fixed<>'' then begin
    fixed:=ResolveProjectPath(fixed, project);
    Result:=Result+' ' + Switch + QuoteStrIfNeeded(fixed, quotes);
  end;
end;

end.

