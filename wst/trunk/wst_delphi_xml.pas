unit wst_delphi_xml;

interface
uses
  SysUtils, Classes, xmldom, XMLIntf;

const
  LineEnding = sLineBreak;

type

  TDOMNode = IDOMNode;
  TDOMNodeList = IDOMNodeList;
  TXMLDocument = IDOMDocument;
  TDOMElement = IDOMElement;

  function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;
  procedure ReleaseDomNode(ADomNode : IInterface);overload;
  procedure ReleaseDomNode(var ADomNode : TXMLDocument);overload;

  function CreateDoc() : TXMLDocument ;
  procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);
  procedure ReadXMLFile(ADoc : TXMLDocument; AStream : TStream);
  function NodeToBuffer(ANode : TDOMNode):string ;
  
implementation
uses XmlDoc;

function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
var
  i, c : Integer;
  lst : TDOMNodeList;
begin
  Result := nil;
  if ANode.hasChildNodes then begin
    lst := ANode.childNodes;
    c := lst.length;
    for i  := 0 to Pred(c) do begin
      if ( ANodeName = lst.item[i].nodeName ) then begin
        Result := lst[i];
        Break;
      end;
    end;
  end;
end;

procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);
begin
  (ADoc as IDOMPersist).saveToStream(AStream);
end;

procedure ReadXMLFile(ADoc : TXMLDocument; AStream : TStream);
begin
  (ADoc as IDOMPersist).loadFromStream(AStream);
end;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
begin
  if ANode.HasChildNodes then begin
    Result := ANode.childNodes.length;
  end else begin
    Result := 0;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;
begin
  Result := ANodeList.length;
end;

procedure ReleaseDomNode(ADomNode : IInterface);
begin
end;

procedure ReleaseDomNode(var ADomNode : TXMLDocument);
begin

end;

function CreateDoc() : TXMLDocument ;
var
  locDoc : IXMLDocument;
begin
  locDoc := XmlDoc.TXMLDocument.Create(nil);
  locDoc.Active := True;
  Result := locDoc.DOMDocument;
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locStream : TStringStream;
  locNodeEx : IDOMNodeEx;
begin
  if Supports(ANode,IDOMNodeEx,locNodeEx) then begin
    Result := locNodeEx.xml;
  end else begin
    raise Exception.Create('This Xml library do not provide "IDOMNodeEx" support.');
  end;
end;

end.
