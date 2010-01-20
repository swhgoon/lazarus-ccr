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
unit PlistFile;

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, XMLRead;

type
  { TPListFile }
  TPListFile = class(TObject)
  private
    fFileName : String;
    fDoc      : TXMLDocument;
    firstkey  : TDOMNode;
  protected
    procedure ReadValues;
    //todo: add "parent" for FindKeyNode
    function FindKeyNode(const keyName: string): TDOMNode;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    function GetStrValue(const Key: String): String;
  end;

implementation

{ TPListFile }

constructor TPListFile.Create(const AFileName: String);
begin
  fFileName := AFileName;
  inherited Create;
end;

destructor TPListFile.Destroy;
begin
  fDOC.Free;
  inherited Destroy;
end;

function TPListFile.GetStrValue(const Key: String): String;
var
  node  : TDOMNode;
begin
  Result:='';
  node:=FindKeyNode(Key);
  if not Assigned(node) then Exit;

  node:=node.NextSibling;
  if Assigned(node) and (node.NodeName='string') then
    Result:=node.TextContent
  else
    Result:='';
end;

procedure TPListFile.ReadValues;
var
  plist : TDOMNode;
begin
  firstkey:=nil;
  try
    ReadXMLFile(fDoc, fFileName);
  except
  end;

  if not Assigned(fDoc) then begin
    fDoc:=TXMLDocument.Create;
    Exit; // create an empty document
  end;

  try
    plist:=fDoc.FindNode('plist');
    if not Assigned(plist) then Exit;

    while Assigned(plist) do begin
      if (plist is TDOMElement) and (plist.NodeName='plist') and (plist.ChildNodes.Count>0) then
        Break;
      plist:=plist.NextSibling;
    end;

    firstkey:=plist.FindNode('dict');
    if Assigned(firstkey) then
      firstkey:=firstkey.FindNode('key');

  except
    firstkey:=nil;
  end;
end;

function TPListFile.FindKeyNode(const keyName: string): TDOMNode;
begin
  if not Assigned(fDoc) then ReadValues;
  if not Assigned(firstkey) then begin
    Result:=nil;
    Exit;
  end;
  Result:=firstkey;
  while Assigned(Result) do begin
    if (Result.NodeName='key') and (Result.TextContent=keyName) then Exit;
    Result:=Result.NextSibling;
  end;
end;


end.

