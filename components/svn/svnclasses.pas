unit svnclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  contnrs,
  DOM, XMLRead;
  
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
    function GetLogPath(index: integer): TLogPath;
    function GetLogPathCount: integer;
    procedure LoadFromNode(ANode: TDOMElement);
  public
    constructor Create;
    destructor Destroy; override;
    property Author: string read FAuthor write FAuthor;
    property Date: string read  FDate write FDate;
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

function TLogEntry.GetLogPathCount: integer;
begin
  Result := FLogPaths.Count;
end;

procedure TLogEntry.LoadFromNode(ANode: TDOMElement);
var
  PathsELement: TDomNode;
  PathElement: TDomNode;
  NewLogPath: TLogPath;
begin
  FRevision := StrToIntDef(ANode.GetAttribute('revision'),0);
  FAuthor :=  GetChildTextContent(ANode, 'author');
  FDate :=  GetChildTextContent(ANode, 'date');
  FMessage := GetChildTextContent(ANode, 'msg');
  
  PathsElement := ANode.FindNode('paths');
  if assigned(PathsELement) then begin
    PathElement := PathsELement.FirstChild;
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

end.

