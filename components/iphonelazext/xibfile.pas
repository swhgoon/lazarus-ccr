unit xibfile;

{$mode objfpc}

interface

uses
  Classes, SysUtils, XMLRead, DOM;

type
  { TXibObject }

  TXibObject = class(TObject)
  private
    fXibNode      : TDOMNode;
    fNextObject   : TXibObject;
    fChildObject  : TXibObject;
  protected
    function GetBoolProp(const PropName: String):Boolean;
    function GetIntProp(const PropName: String):Integer;
    function GetStrProp(const PropName: String):String;
    function FindProperty(const PropName: String): TDOMNode;
    function GetName: String;
    function GetXibClass: String;
  public
    constructor Create(AXibNode: TDOMNode);
    destructor Destroy; override;
    procedure GetUnnamedStrProps(list: TStrings);
    property NextObject: TXibObject read fNextObject;
    property ChildObject: TXibObject read fChildObject;
    property BoolProp[const PropName: String]: Boolean read GetBoolProp;
    property StrProp[const PropName: String]: String read GetStrProp;
    property IntProp[const PropName: String]: Integer read GetIntProp;
    property Name: String read GetName;
    property XibClass: String read GetXibClass;
  end;

  { TXibFile }

  TXibFile = class(TObject)
  private
    fDoc          : TXMLDocument;
    fFirstObject  : TXibObject;
  public
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: AnsiString);
    property FirstObject: TXibObject read fFirstObject;
  end;

procedure DoReadXibDoc(ADoc: TXMLDocument; var Obj: TXibObject);

function FindXibObject(root: TXibObject; const ObjName: String; Recursive: Boolean=False): TXibObject;

type
  TXibKeyValue = record
    Key   : AnsiString;
    Value : AnsiString;
  end;

  { TXibClassDescr }

  TXibClassDescr = class(TObject)
    Name    : AnsiString;
    Actions : array of TXibKeyValue;
    Outlets : array of TXibKeyValue;
    constructor Create(const AName: AnsiString);
  end;

procedure ListClassesDescr(root: TXibObject; DstList : TList); overload;
procedure ListClassesDescr(const FileName: AnsiString; DstList : TList); overload;

implementation

function Min(a,b: integer): Integer;
begin
  if a<b then Result:=a
  else Result:=b;
end;

procedure SetActions(names, types: TStrings; descr: TXibClassDescr);
var
  i : integer;
begin
  if not Assigned(names) or not Assigned(types) then Exit;

  SetLength(descr.Actions, Min(names.Count, types.Count));
  for i:=0 to length(descr.Actions)- 1 do begin
    descr.Actions[i].Key:=names[i];
    descr.Actions[i].Value:=types[i];
  end;
end;

procedure SetOutlets(names, types: TStrings; descr: TXibClassDescr);
var
  i : integer;
begin
  if not Assigned(names) or not Assigned(types) then Exit;

  SetLength(descr.Outlets, Min(names.Count, types.Count));
  for i:=0 to length(descr.Outlets)- 1 do begin
    descr.Outlets[i].Key:=names[i];
    descr.Outlets[i].Value:=types[i];
  end;
end;


procedure ListDictionary(dict: TXibObject; keys, values: TStrings);
var
  xibkeys : TXibObject;
  xibvals : TXibObject;
begin
  if Assigned(dict) then begin
    xibkeys:=FindXibObject(dict, 'dict.sortedKeys');
    xibvals:=FindXibObject(dict, 'dict.values');
    if Assigned(xibkeys) and Assigned(xibvals) then begin
      xibkeys.GetUnnamedStrProps(keys);
      xibvals.GetUnnamedStrProps(values);
    end;
  end;
end;

procedure ListClassesDescr(root: TXibObject; DstList : TList);
var
  obj   : TXibObject;
  act   : TXibObject;
  outs  : TXibObject;
  cls   : TXibClassDescr;
  names : TStringList;
  types : TStringList;
begin
  if not Assigned(DstList) then Exit;

  obj:=FindXibObject(root, 'IBDocument.Classes', true);
  if not Assigned(obj) then Exit;

  names := TStringList.Create;
  types := TStringList.Create;
  obj:=FindXibObject(obj, 'referencedPartialClassDescriptions', true);

  obj:=obj.ChildObject;
  while Assigned(obj) do begin

    if obj.XibClass<>'IBPartialClassDescription' then begin
      obj:=obj.NextObject;
      Continue;
    end;

    cls:=TXibClassDescr.Create(obj.StrProp['className']);
    act:=FindXibObject(obj, 'actions');
    if Assigned(act) then begin
      names.Clear; types.Clear;
      ListDictionary(act, names, types);
      SetActions(names, types, cls);
    end;

    outs:=FindXibObject(obj, 'outlets');
    if Assigned(outs) then begin
      names.Clear; types.Clear;
      ListDictionary(outs, names, types);
      SetOutlets(names, types, cls);
    end;
    DstList.Add(cls);

    obj:=obj.NextObject;
  end;
  names.Free;
  types.Free;
end;

function FindXibObject(root: TXibObject; const ObjName: String; Recursive: Boolean): TXibObject;
var
  obj : TXibObject;
begin
  obj:=root.ChildObject;
  while Assigned(obj) and (obj.Name<>ObjName) do begin
    if Recursive then begin
      Result:=FindXibObject(obj, ObjName, Recursive);
      if Assigned(Result) then Exit;
    end;
    obj:=obj.NextObject;
  end;
  Result:=obj;
end;

procedure DoReadXibDoc(ADoc: TXMLDocument; var Obj: TXibObject);
const
  DataNode  = 'data';
  XibObject = 'object';
var
  node  : TDOMNode;
  n     : TDOMNode;
  xib   : TXibObject;
  pending : TList;
begin
  Obj:=nil;
  writeln('ADoc = ', Integer(ADoc));
//  node:=ADoc.FindNode(DataNode);
  node:=ADoc.FindNode('archive');
  node:=node.FindNode('data');
  writeln('no data? ', Integer(node));
  if not Assigned(node) then Exit;

  xib:=TXibObject.Create(node);
  pending:=TList.Create;
  Obj:=xib;
  try
    while Assigned(xib) do begin
      node:=xib.fXibNode;
      n:=node.NextSibling;
      while Assigned(n) and (n.NodeName<>XibObject) do
        n:=n.NextSibling;
      if Assigned(n) then begin
        xib.fNextObject:=TXibObject.Create(n);
        pending.add(xib.NextObject);
      end;
      n:=node.FirstChild;
      while Assigned(n) and (n.NodeName<>XibObject) do
        n:=n.NextSibling;
      if Assigned(n) then begin
        xib.fChildObject:=TXibObject.Create(n);
        pending.add(xib.ChildObject);
      end;
      if pending.Count>0 then begin
        xib:=TXibObject(pending[0]);
        pending.Delete(0);
      end else
        xib:=nil;
    end;
  except
  end;
  pending.Free;
end;

{ TXibFile }

destructor TXibFile.Destroy;
begin
  fDoc.Free;
  fFirstObject.Free;
  inherited Destroy;
end;

procedure TXibFile.LoadFromStream(Stream:TStream);
begin
  fDoc.Free;
  fDoc:=nil;
  try
    ReadXMLFile(fDoc, Stream);
    DoReadXibDoc(fDoc, fFirstObject);
  except
  end;
end;

procedure TXibFile.LoadFromFile(const FileName:AnsiString);
var
  fs : TFileStream;
begin
  fs:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

{ TXibObject }

constructor TXibObject.Create(AXibNode:TDOMNode);
begin
  inherited Create;
  fXibNode:=AXibNode;
end;

destructor TXibObject.Destroy;
begin
  fNextObject.Free;
  fChildObject.Free;
  inherited Destroy;
end;

procedure TXibObject.GetUnnamedStrProps(list:TStrings);
var
  n : TDOMNode;
begin
  if not Assigned(list) then Exit;

  n:=fXibNode.FirstChild;
  while Assigned(n) do begin
    if (n.NodeName='string') and (TDOMElement(n).AttribStrings['key']='') then
      list.Add(n.TextContent);
    n:=n.NextSibling;
  end;
end;

function TXibObject.GetBoolProp(const PropName: String):Boolean;
var
  n : TDOMNode;
begin
  n:=FindProperty(PropName);
  Result:=Assigned(n) and (n.NodeName='bool') and (n.TextContent='YES');
end;

function TXibObject.GetIntProp(const PropName: String):Integer;
var
  n   : TDOMNode;
  err : Integer;
begin
  n:=FindProperty(PropName);
  if Assigned(n) and (n.NodeName='int') then begin
    Val(n.TextContent, Result, err);
    if err<>0 then Result:=0;
  end else
    Result:=0;
end;

function TXibObject.GetStrProp(const PropName: String):String;
var
  n : TDOMNode;
begin
  n:=FindProperty(PropName);
  if Assigned(n) and (n.NodeName='string') then Result:=n.TextContent
  else Result:='';
end;

function isKeyAttr(n: TDomNode; const KeyAttrVal: String): Boolean;
begin
  Result:=Assigned(n) and (n is TDOMElement) and (TDOMElement(n).AttribStrings['key']=KeyAttrVal)
end;

function TXibObject.FindProperty(const PropName:String):TDOMNode;
var
  n : TDOMNode;
begin
  n:=fXibNode.FirstChild;
  while Assigned(n) and (n.NodeName='object') and (not isKeyAttr(n, PropName)) do
    n:=n.NextSibling;
  Result:=n;
end;

function TXibObject.GetName:String;
begin
  if not (fXibNode is TDOMElement) then begin
    Result:='';
    Exit;
  end;
  Result:=TDOMElement(fXibNode).AttribStrings['key'];
end;

function TXibObject.GetXibClass:String;
begin
  if not (fXibNode is TDOMElement) then begin
    Result:='';
    Exit;
  end;
  Result:=TDOMElement(fXibNode).AttribStrings['class'];
end;

procedure ListClassesDescr(const FileName: AnsiString; DstList : TList); overload;
var
  xib : TXibFile;
begin
  xib := TXibFile.Create;
  try
    xib.LoadFromFile(FileName);
    ListClassesDescr(xib.FirstObject, DstList);
  finally
    xib.Free;
  end;
end;

{ TXibClassDescr }

constructor TXibClassDescr.Create(const AName:AnsiString);
begin
  inherited Create;
  Name:=AName;
end;

end.

