{$INCLUDE wst_global.inc}
unit wst_fpc_xml;

interface

uses
  Classes, SysUtils, DOM;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  { TDOMNodeSelectListImp }

  TDOMNodeSelectListImp = class(TDOMNodeList)
  private
    FFilter: DOMString;
    FUseFilter: Boolean;
  protected
    procedure BuildList(); override;
  public
    constructor Create(ANode: TDOMNode; const AFilter: DOMString);
  end;

  function FilterList(const ANode : TDOMNode; const ANodeName : DOMString) : TDOMNodeList ;

  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNode);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNamedNodeMap);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function CreateDoc() : TXMLDocument ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}

  function NodeToBuffer(ANode : TDOMNode):string ;
  
implementation
uses XMLWrite;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
var
  chdLst : TDOMNodeList;
begin
  if ANode.HasChildNodes then begin
    chdLst := ANode.ChildNodes;
    try
      Result := chdLst.Count;
    finally
      chdLst.Release();
    end;
  end else begin
    Result := 0;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;
begin
  Result := ANodeList.Count;
end;

function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;
begin
  Result := ANodeList.Length;
end;

procedure ReleaseDomNode(ADomNode : TDOMNode);overload;
begin
  ADomNode.Free();
end;

procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;
begin
  ADomNode.Release();
end;

procedure ReleaseDomNode(ADomNode : TDOMNamedNodeMap);overload;
begin
  ADomNode.Free();
end;

function CreateDoc() : TXMLDocument ;
begin
  Result := TXMLDocument.Create();
  Result.Encoding := 'UTF-8';
end;

function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ANode.FindNode(ANodeName);
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locStream : TStringStream;
begin
  locStream := TStringStream.Create('');
  try
    WriteXML(ANode,locStream);
    Result := locStream.DataString;
  finally
    locStream.Free();
  end;
end;

function FilterList(const ANode : TDOMNode; const ANodeName : DOMString) : TDOMNodeList ;
begin
  Result := TDOMNodeSelectListImp.Create(ANode,ANodeName);
end;

{ TDOMNodeSelectListImp }

type
  TDOMNodeCracked = class(TDOMNode);
procedure TDOMNodeSelectListImp.BuildList();
var
  Child: TDOMNode;
begin
  FList.Clear;
  FRevision := TDOMNodeCracked(FNode).GetRevision();

  Child := FNode.FirstChild;
  while ( Child <> nil ) do begin
    if ( Child.NodeType = ELEMENT_NODE ) and
       ( ( not FUseFilter ) or ( TDOMElement(Child).TagName = FFilter ) )
    then begin
      FList.Add(Child);
    end;
    Child := Child.NextSibling
  end;
end;

constructor TDOMNodeSelectListImp.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  inherited Create(ANode);
  FFilter := AFilter;
  FUseFilter := ( FFilter <> '*' );
end;

end.
