unit cmdlinecfguijson;

interface

{$mode delphi}

uses
  Classes, SysUtils, cmdlinecfgui, fpjson, jsonparser;


function CmdLineUIJSONReadFile(stream: TStream; lt: TCmdLineLayoutInfo): Boolean; overload;
function CmdLineUIJSONReadFile(const FileName: String; lt: TCmdLineLayoutInfo): Boolean; overload;
procedure CmdLineCfgUIJSONLoadFilesFromDir(const Dir: String; list: TList; const Mask : string = '*.coptui');

implementation

type
  { TSectionIterator }

  TSectionIterator = class(TObject)
  public
    lt  : TCmdLineLayoutInfo;
    sc  : TLayoutSection;
    constructor Create;
    destructor Destroy; override;
    procedure Iterate(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var DoContinue: Boolean);
  end;


function CmdLineUIJSONReadFile(stream: TStream; lt: TCmdLineLayoutInfo): Boolean;
var
  p : TJSONParser;
  d : TJSONData;
  core : TJSONObject;
  st   : TSectionIterator;
begin
  Result:=False;
  d:=nil;
  p:=TJSONParser.Create(stream);
  try
    d:=p.Parse;
    if d.JSONType<>jtObject then Exit;
    core:=TJSONObject(d);

    st:=TSectionIterator.Create;
    try
      st.lt:=lt;
      st.sc:=lt.RootElement;
      core.Iterate( st.Iterate, st)
    finally
      st.Free;
    end;
    Result:=true;
  finally
    d.Free;
    p.Free;
  end;
end;

function CmdLineUIJSONReadFile(const FileName: String; lt: TCmdLineLayoutInfo): Boolean;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result:=CmdLineUIJSONReadFile(fs, lt);
  finally
    fs.Free;
  end;
end;

procedure CmdLineCfgUIJSONLoadFilesFromDir(const Dir: String; list: TList; const Mask : string = '*.coptui');
var
  rslt : TSearchRec;
  res  : Integer;
  pth  : string;
  cfg  : TCmdLineLayoutInfo;
begin
  pth:=IncludeTrailingPathDelimiter(Dir);
  res:=FindFirst( pth+Mask, faAnyFile, rslt);
  try
    while res = 0 do begin
      if (rslt.Attr and faDirectory=0) and (rslt.Size>0) then begin
        cfg := TCmdLineLayoutInfo.Create;
        if not CmdLineUIJSONReadFile(pth+rslt.Name, cfg) then cfg.Free
        else list.Add(cfg);
      end;
      res:=FindNext(rslt);
    end;
  finally
    FindClose(rslt);
  end;

end;

{ TSectionIterator }

constructor TSectionIterator.Create;
begin
  inherited Create;
end;

destructor TSectionIterator.Destroy;
begin
  inherited Destroy;
end;

procedure TSectionIterator.Iterate(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var DoContinue: Boolean);
var
  l   : string;
  a   : TJSONArray;
  i   : Integer;
  st  : TSectionIterator;
  subnm : string;
begin
  l:=AnsiLowerCase(AName);
  if (l='switches') and (Item.JSONType=jtArray) then begin
    a:=TJSONArray(Item);
    for i:=0 to a.Count-1 do begin
      if (a.Items[i].JSONType=jtString) then
        sc.AddElement( TJSONString(a.Items[i]).AsString, letSwitch );
    end;
  end else if (l='display') and (Item.JSONType=jtString) then begin
    sc.Display:=Item.AsString;
  end else if (l='hint') and (Item.JSONType=jtString) then begin
    sc.GUIHint:=Item.AsString;
  end else if (item.JSONType=jtObject) then begin
    // sub section
    st:=TSectionIterator.Create;
    try
      st.sc:=Self.sc.AddElement(AName, letSection);
      st.lt:=Self.lt;
      TJSONObject(Item).Iterate(st.Iterate, st);
    finally
      st.Free;
    end;
  end;
end;

end.
