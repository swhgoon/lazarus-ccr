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
  Classes, SysUtils, FileUtil,
  LazIDEIntf,ProjectIntf;


function ResolveProjectPath(const path: string; project: TLazProject = nil): string;

function BreakPathsStringToOption(const Paths, Switch: String; const Quotes: string = '"'; project: TLazProject = nil): String;

function RelativeToFullPath(const BasePath, Relative: string): String;
function NeedQuotes(const path: string): Boolean;

implementation

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

