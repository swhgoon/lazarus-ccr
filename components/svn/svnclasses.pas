{ Classes for interpreting the output of svn commands

  Copyright (C) 2007 Vincent Snijders vincents@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit SvnClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  contnrs,
  DOM, XMLRead,
  SvnCommand;
  
type

  TEntryKind = (ekUnknown, ekFile, ekDirectory);
  TCommitAction = (caUnknown, caModify, caAdd, caDelete);
  
  { TSvnBase }

  TSvnBase = class
  private
    procedure LoadFromXml(ADoc: TXMLDocument); virtual; abstract;
  public
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(FileName: string);
    procedure LoadFromCommand(command: string);
  end;

  { TCommit }

  TCommit = class
  private
    FAuthor: string;
    FDate: string;
    FRevision: integer;
    procedure LoadFromNode(ANode: TDomNode);
  public
    procedure Clear;
    property Author: string read FAuthor write FAuthor;
    property Date: string read  FDate write FDate;
    property Revision: integer read FRevision write FRevision;
  end;
  
  { TRepository }

  TRepository = class
  private
    FRoot: string;
    FUUID: string;
    procedure LoadFromNode(ANode: TDomNode);
  public
    procedure Clear;
    property Root: string read FRoot write FRoot;
    property UUID: string read FUUID write FUUID;
  end;
  
  { TEntry }

  TEntry = class
  private
    FCommit: TCommit;
    FKind: TEntryKind;
    FPath: string;
    FRepository: TRepository;
    FRevision: integer;
    FUrl: string;
    procedure LoadFromNode(ANode: TDomNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Commit: TCommit read FCommit;
    property Kind: TEntryKind read FKind write FKind;
    property Path: string read FPath write FPath;
    property URL: string read FUrl write FUrl;
    property Repository: TRepository read FRepository;
    property Revision: integer read FRevision write FRevision;
  end;
  
  { TSvnInfo }

  TSvnInfo = class(TSvnBase)
  private
    FEntry: TEntry;
    procedure LoadFromXml(ADoc: TXMLDocument); override;
  public
    constructor Create;
    constructor Create(const Uri: string);
    destructor Destroy; override;
    procedure Clear;
    property Entry: TEntry read FEntry;
  end;
  
  { TLogPath }

  TLogPath = class
  private
    FAction: TCommitAction;
    FCopyFromPath: string;
    FCopyFromRevision: integer;
    FPath: string;
    procedure LoadFromNode(ANode: TDomElement);
  public
    property Action : TCommitAction read FAction write FAction;
    property CopyFromRevision: integer read FCopyFromRevision write FCopyFromRevision;
    property CopyFromPath: string read FCopyFromPath write FCopyFromPath;
    property Path: string read FPath write FPath;
  end;

  { TLogEntry }

  TLogEntry = class
  private
    FAuthor: string;
    FDate: string;
    FLogPaths: TFPObjectList;
    FMessage: string;
    FRevision: integer;
    function GetCommonPath: string;
    function GetDisplayDate: string;
    function GetLogPath(index: integer): TLogPath;
    function GetLogPathCount: integer;
    procedure LoadFromNode(ANode: TDOMElement);
  public
    constructor Create;
    destructor Destroy; override;
    function GetFileList(const BaseDir: string = ''): TStrings;
    procedure SortPaths;
    property Author: string read FAuthor write FAuthor;
    property CommonPath: string read GetCommonPath;
    property Date: string read  FDate write FDate;
    property DisplayDate: string read GetDisplayDate;
    property Message: string read FMessage write FMessage;
    property Path[index: integer] :TLogPath read GetLogPath;
    property PathCount: integer read GetLogPathCount;
    property Revision: integer read FRevision write FRevision;
  end;
  
  { TSvnLog }

  TSvnLog = class(TSvnBase)
  private
    FLogEntries: TFPObjectList;
    function GetLogEntry(index: integer): TLogEntry;
    function GetLogEntryCount: integer;
    procedure LoadFromXml(ADoc: TXMLDocument); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property LogEntry[index: integer] :TLogEntry read GetLogEntry;
    property LogEntryCount: integer read GetLogEntryCount;
  end;
  
  { TSvnFileProp }

  TSvnFileProp = class
  private
    FFileName: string;
    FProperties: TStrings;
  public
    constructor Create;
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property Properties: TStrings read FProperties;
  end;
  
  { TSvnPropInfo }

  TSvnPropInfo = class
  private
    FFiles: TFPHashObjectList;
    function GetFile(index: integer): TSvnFileProp;
    function GetFileCount: integer;
    function ContainsFile(const AFileName: string) : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(FileName: string);
    procedure LoadForFiles(FileNames: TStrings);
    function GetFileItem(const s: string): TSvnFileProp;
    property FileItem[index: integer]: TSvnFileProp read GetFile; default;
    property FileCount: integer read GetFileCount;
  end;

implementation

const
  ActionStrings : array[TCommitAction] of char =
    (' ','M','A','D');

function GetChildTextContent(ANode: TDomNode; const AName: string) : string;
var
  ChildNode: TDOMNode;
begin
  Result := '';
  ChildNode :=  ANode.FindNode(AName);
  if assigned(ChildNode) then
    Result := ChildNode.TextContent;
end;

{ TSvnBase }

procedure TSvnBase.LoadFromStream(s: TStream);
var
  ADoc: TXMLDocument;
begin
  ReadXMLFile(ADoc, s);
  try
    LoadFromXml(ADoc);
  finally
    ADoc.Free;
  end;
end;

procedure TSvnBase.LoadFromFile(FileName: string);
var
  ADoc: TXMLDocument;
begin
  ReadXMLFile(ADoc, FileName);
  try
    LoadFromXml(ADoc);
  finally
    ADoc.Free;
  end;
end;

procedure TSvnBase.LoadFromCommand(command: string);
var
  XmlOutput: TMemoryStream;
begin
  XmlOutput := TMemoryStream.Create;
  try
    ExecuteSvnCommand(command, XmlOutput);
    //DumpStream(XmlOutput);
    XmlOutput.Position := 0;
    LoadFromStream(XmlOutput);
  finally
    XmlOutput.Free;
  end;
end;

{ TSvnInfo }

procedure TSvnInfo.LoadFromXml(ADoc: TXMLDocument);
begin
  Clear;
  Entry.LoadFromNode(ADoc.DocumentElement.FindNode('entry'));
end;

constructor TSvnInfo.Create;
begin
  inherited Create;
  FEntry := TEntry.Create;
end;

constructor TSvnInfo.Create(const Uri: string);
begin
  Create;
  LoadFromCommand('info --xml '+Uri);
end;

destructor TSvnInfo.Destroy;
begin
  FEntry.Free;
  inherited Destroy;
end;

procedure TSvnInfo.Clear;
begin
  FEntry.Clear;
end;

{ TEntry }

procedure TEntry.LoadFromNode(ANode: TDomNode);
var
  EntryNode: TDomElement;
  KindString: string;
  UrlNode: TDomNode;
begin
  if ANode=nil then exit;
  
  if ANode.NodeType = ELEMENT_NODE then begin
    EntryNode := TDomElement(ANode);
    FRevision := StrToIntDef(EntryNode.GetAttribute('revision'),0);
    FPath := EntryNode.GetAttribute('path');
    KindString := EntryNode.GetAttribute('kind');
    if KindString = 'file' then
      FKind := ekFile
    else if KindString = 'dir' then
      FKind := ekDirectory
    else
      FKind := ekUnknown;
    UrlNode :=  EntryNode.FindNode('url');
    if assigned(UrlNode) then
      FUrl := UrlNode.TextContent;
      
    FRepository.LoadFromNode(EntryNode.FindNode('repository'));
    FCommit.LoadFromNode(EntryNode.FindNode('commit'));
  end;
end;

constructor TEntry.Create;
begin
  inherited Create;
  FCommit := TCommit.Create;
  FRepository := TRepository.Create;
end;

destructor TEntry.Destroy;
begin
  FCommit.Free;
  FRepository.Free;
  inherited Destroy;
end;

procedure TEntry.Clear;
begin
  FPath := '';
  FKind := ekUnknown;
  FUrl := '';
  FRevision := 0;
  FCommit.Clear;
  FRepository.Clear;
end;

{ TRepository }

procedure TRepository.LoadFromNode(ANode: TDomNode);
begin
  if ANode=nil then exit;

  FRoot := GetChildTextContent(ANode, 'root');
  FUUID :=  GetChildTextContent(ANode, 'uuid');
end;

procedure TRepository.Clear;
begin
  FRoot := '';
  FUUID := '';
end;

{ TCommit }

procedure TCommit.LoadFromNode(ANode: TDomNode);
begin
  if ANode=nil then exit;

  if ANode.NodeType = ELEMENT_NODE then begin
    FRevision := StrToIntDef(TDomElement(ANode).GetAttribute('revision'),0);
    FAuthor :=  GetChildTextContent(ANode, 'author');
    FDate :=  GetChildTextContent(ANode, 'date');
  end;
end;

procedure TCommit.Clear;
begin
  FAuthor := '';
  FDate := '';
  FRevision := 0;
end;

{ TSvnLog }

function TSvnLog.GetLogEntry(index: integer): TLogEntry;
begin
  Result := TLogEntry(FLogEntries[index]);
end;

function TSvnLog.GetLogEntryCount: integer;
begin
  Result := FLogEntries.Count;
end;

procedure TSvnLog.LoadFromXml(ADoc: TXMLDocument);
var
  LogEntryElement: TDomNode;
  NewLogEntry: TLogEntry;
begin
  Clear;

  LogEntryElement := ADoc.FindNode('log').FirstChild;
  while assigned(LogEntryElement) do begin
    if (LogEntryElement.NodeType=ELEMENT_NODE)
      and (LogEntryElement.NodeName='logentry') then
    begin
      NewLogEntry := TLogEntry.Create;
      NewLogEntry.LoadFromNode(TDomElement(LogEntryElement));
      FLogEntries.Add(NewLogEntry);
    end;
    LogEntryElement := LogEntryElement.NextSibling;
  end;
end;

constructor TSvnLog.Create;
begin
  inherited Create;
  FLogEntries := TFPObjectList.Create(true);
end;

destructor TSvnLog.Destroy;
begin
  FLogEntries.Free;
  inherited Destroy;
end;

procedure TSvnLog.Clear;
begin
  FLogEntries.Clear;
end;

{ TLogEntry }

function TLogEntry.GetLogPath(index: integer): TLogPath;
begin
  Result := TLogPath(FLogPaths[index]);
end;

function TLogEntry.GetCommonPath: string;
var
  i: integer;
  NextPath: string;
begin
  if FLogPaths.Count = 0 then exit('');
  
  Result := ExtractFilePath(Path[0].Path);
  i := 1;
  while i<FLogPaths.Count do begin
    NextPath :=  Path[i].Path;
    while (Copy(NextPath,1,length(Result))<>Result) do
      Result := ExtractFilePath(ExtractFileDir(Result));
    inc(i);
  end;
end;

function TLogEntry.GetDisplayDate: string;
begin
  Result := Copy(FDate, 1, 10) + ' ' + Copy(FDate,12,8);
end;

function TLogEntry.GetFileList(const BaseDir: string = ''): TStrings;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i:= 0 to PathCount -1 do
    if Path[i].Action in [caModify, caAdd] then
      Result.Add(SetDirSeparators(BaseDir + Path[i].Path));
end;

function TLogEntry.GetLogPathCount: integer;
begin
  Result := FLogPaths.Count;
end;

procedure TLogEntry.LoadFromNode(ANode: TDOMElement);
var
  PathsElement: TDomNode;
  PathElement: TDomNode;
  NewLogPath: TLogPath;
begin
  FRevision := StrToIntDef(ANode.GetAttribute('revision'),0);
  FAuthor :=  GetChildTextContent(ANode, 'author');
  FDate :=  GetChildTextContent(ANode, 'date');
  FMessage := GetChildTextContent(ANode, 'msg');
  
  PathsElement := ANode.FindNode('paths');
  if assigned(PathsELement) then begin
    PathElement := PathsElement.FirstChild;
    while assigned(PathElement) do begin
      if (PathElement.NodeType=ELEMENT_NODE)
        and (PathElement.NodeName='path') then
      begin
        NewLogPath := TLogPath.Create;
        NewLogPath.LoadFromNode(TDomElement(PathElement));
        FLogPaths.Add(NewLogPath);
      end;
      PathElement := PathElement.NextSibling;
    end;
  end;
end;

function PathCompare(Item1, Item2: Pointer): Integer;
var
  Path1, Path2: TLogPath;
begin
  Path1 := TLogPath(Item1);
  Path2 := TLogPath(Item2);
  Result := CompareStr(Path1.Path, Path2.Path);
end;

procedure TLogEntry.SortPaths;
begin
  FLogPaths.Sort(@PathCompare);
end;

constructor TLogEntry.Create;
begin
  inherited Create;
  FLogPaths := TFPObjectList.Create(true);
end;

destructor TLogEntry.Destroy;
begin
  FLogPaths.Free;
  inherited Destroy;
end;

{ TLogPath }

procedure TLogPath.LoadFromNode(ANode: TDomElement);
var
  ActionStr: string;
  i: TCommitAction;
begin
  FPath := ANode.TextContent;
  FCopyFromRevision := StrToIntDef(ANode.GetAttribute('copyfrom-rev'),0);
  FCopyFromPath := ANode.GetAttribute('copyfrom-path');
  ActionStr := ANode.GetAttribute('action');
  FAction := caUnknown;
  for i := low(TCommitAction) to high(TCommitAction) do
    if ActionStrings[i]=ActionStr then begin
      FAction := i;
      break;
    end;
end;

{ TSvnFileProp }

constructor TSvnFileProp.Create;
begin
  FProperties := TStringList.Create;
end;

constructor TSvnFileProp.Create(const AFileName: string);
begin
  Create;
  FFileName := AFileName;
end;

destructor TSvnFileProp.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

{ TSvnPropInfo }

function TSvnPropInfo.GetFile(index: integer): TSvnFileProp;
begin
  Result := TSvnFileProp(FFiles[index]);
end;

function TSvnPropInfo.GetFileCount: integer;
begin
  Result := FFiles.Count;
end;

function TSvnPropInfo.ContainsFile(const AFileName: string): boolean;
begin
  Result := true;
end;

constructor TSvnPropInfo.Create;
begin
  FFiles := TFPHashObjectList.Create(true);
end;

destructor TSvnPropInfo.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TSvnPropInfo.LoadFromStream(s: TStream);
var
  Lines: TStrings;
  Line: string;
  FileProp: TSvnFileProp;
  i: Integer;
  QuotePos, ColonPos: integer;
  PropName, PropValue: String;
const
  PropertiesOn = 'Properties on ';
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromStream(s);
    //writeln(Lines.Text);
    i := 0;
    while (i<Lines.Count) do begin
      Line := Lines[i];
      if copy(Line, 1, length(PropertiesOn))=PropertiesOn then begin
        QuotePos := PosEx('''', Line, Length(PropertiesOn)+2);
        FileProp := TSvnFileProp.Create(
          Copy(Line, Length(PropertiesOn)+2, QuotePos - Length(PropertiesOn)-2));
        FFiles.Add(FileProp.FileName, FileProp);
        inc(i);
        while (i<Lines.Count) do begin

          Line := Lines[i];
          if (Length(Line)<2) or (Line[1]<>' ') then begin
            // new file, so unget line
            dec(i);
            break;
          end;
          ColonPos := Pos(' : ', Line);
          PropName := Copy(Line, 3, ColonPos - 3);
          PropValue := Copy(Line, ColonPos + 3, Length(Line)-ColonPos-2);
          // try for a multiline property
          inc(i);
          while (i<Lines.Count) do begin
            if ((length(Lines[i])>=2) and (copy(Lines[i],1,2)='  '))
              or (copy(Lines[i], 1, length(PropertiesOn))=PropertiesOn) then begin
              // new property, unget line
              dec(i);
              break;
            end;
            PropValue := PropValue + LineEnding + Lines[i];
            inc(i);
          end;
          FileProp.Properties.Values[PropName] := PropValue;
          inc(i);
        end;
      end
      else
        inc(i);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TSvnPropInfo.LoadFromFile(FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TSvnPropInfo.LoadForFiles(FileNames: TStrings);
var
  Output: TMemoryStream;
  Files: string;
  i: integer;
begin
  Output := TMemoryStream.Create;
  try
    Files := '';
    for i := 0 to FileNames.Count-1 do
      Files := Files + ' ' + FileNames[i];
    ExecuteSvnCommand('proplist -v' + Files, Output);
    Output.Position := 0;
    LoadFromStream(Output);
    for i := 0 to FileNames.Count -1 do begin
      if FFiles.FindIndexOf(FileNames[i])<0 then begin
        FFiles.Add(FileNames[i], TSvnFileProp.Create(FileNames[i]));
      end;
    end;
  finally
    Output.Free;
  end;
end;

function TSvnPropInfo.GetFileItem(const s: string): TSvnFileProp;
begin
  Result := TSvnFileProp(FFiles.Find(s));
end;

end.

