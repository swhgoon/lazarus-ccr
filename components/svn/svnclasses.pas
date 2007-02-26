unit svnclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DOM, XMLRead;
  
type

  TEntryKind = (ekUnknown, ekFile, ekDirectory);

  { TCommit }

  TCommit = class
  private
    FAuthor: string;
    FDate: string;
    FRevision: integer;
  public
    procedure Clear;
    property Author: string read FAuthor write FAuthor;
    property Date: string read  FDate write FDate;
    property Revision: integer read FRevision write FRevision;
  end;
  
  { TRepository }

  TRepository = class
  private
    FUrl: string;
    FUUID: string;
  public
    procedure Clear;
    property URL: string read FUrl write FUrl;
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

  TSvnInfo = class
  private
    FEntry: TEntry;
    procedure LoadFromXml(ADoc: TXMLDocument);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(FileName: string);
    property Entry: TEntry read FEntry;
  end;

implementation

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

procedure TSvnInfo.LoadFromStream(s: TStream);
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

procedure TSvnInfo.LoadFromFile(FileName: string);
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

procedure TRepository.Clear;
begin
  FUrl := '';
  FUUID := '';
end;

{ TCommit }

procedure TCommit.Clear;
begin
  FAuthor := '';
  FDate := '';
  FRevision := 0;
end;

end.

