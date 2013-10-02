unit cmdlinecfgjson;

{$mode delphi}

interface

uses
  Classes, SysUtils, cmdlinecfg, fpjson, jsonparser;

function CmdLineCfgJSONReadFile(stream: TStream; cfg: TCmdLineCfg): Boolean; overload;
function CmdLineCfgJSONReadFile(const FileName: String; cfg: TCmdLineCfg): Boolean; overload;
procedure CmdLineCfgJSONLoadFilesFromDir(const Dir: String; list: TList; const Mask : string = '*.copt');

implementation

// ugh... no better way to do it anyway. ... see TValueIterator below
type
  { TCfgIterator }
  TCfgIterator = class(TObject)
    class procedure Iterate(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var DoContinue: Boolean);
    class procedure IterateOption(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var DoContinue: Boolean);
    class procedure IterateValue(const AName: TJSONStringType; Item: TJSONdata; Data: TObject; var DoContinue: Boolean);
  end;

type
  TIterateValue = class(TObject)
    opt : TCmdLineCfgOption;
    idx : Integer;
  end;

class procedure TCfgIterator.IterateOption(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var DoContinue: Boolean);
var
  opt : TCmdLineCfgOption;
  nm  : String;
  ja  : TJSONArray;
  i   : Integer;
  iv  : TIterateValue;
begin
  opt:=TCmdLineCfgOption(Data);
  nm:=lowerCase(AName);
  if nm='section' then opt.Section:=Item.AsString
  else if nm='subsection' then opt.SubSection:=Item.AsString
  else if nm='type' then opt.OptType:=Item.AsString
  else if nm='name' then opt.Name:=Item.AsString
  else if nm='key' then opt.Key:=Item.AsString
  else if nm='masterkey' then opt.MasterKey:=Item.AsString
  else if nm='display' then opt.Display:=Item.AsString
  else if (nm='condition') or (nm='cond') then opt.Condition:=Item.AsString
  else if (nm='value') then opt.SetValue(Item.AsString)
  else if (nm='alias') then opt.AliasToKey:=Item.AsString
  else if (nm='multiple') then opt.isMultiple:=(Item.JSONType=jtBoolean) and (Item.AsBoolean)
  else if (nm='options') then begin
    ja:=TJSONArray(Item);
    if ja.Count>0 then begin
      iv:=TIterateValue.Create;
      try
        iv.opt:=opt;
        for i:=0 to ja.Count-1 do begin
          if ja.Items[i].JSONType = jtObject then begin
            iv.idx:=opt.ValCount;
            TJSONObject(ja.Items[i]).Iterate ( TCfgIterator.IterateValue, iv);
          end;
        end;
      finally
        iv.Free;
      end;
    end;
  end
end;

class procedure TCfgIterator.IterateValue(const AName: TJSONStringType;
  Item: TJSONdata; Data: TObject; var DoContinue: Boolean);
var
  opt : TCmdLineCfgOption;
  idx : Integer;
  nm  : String;
begin
  idx:=TIterateValue(Data).idx;
  opt:=TIterateValue(Data).opt;
  nm:=lowerCase(AName);
  if nm='display' then opt.SetValDisplay(Item.AsString, idx)
  else if nm='value' then opt.SetValue(Item.AsString, idx)
  else if nm='condition' then opt.SetCondition(Item.AsString, idx);
end;

class procedure TCfgIterator.Iterate(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var DoContinue: Boolean);
var
  cfg : TCmdLineCfg;
  nm  : string;
  ja  : TJSONArray;
  j   : TJSONData;
  i   : Integer;
  opt : TCmdLineCfgOption;

begin
  cfg:=TCmdLineCfg(data);
  nm:=lowerCase(AName);
  if nm='options' then begin
    if Item.JSONType<>jtArray then Exit; // options must be an array of options
    ja:=TJSONArray(Item);
    for i:=0 to ja.Count-1 do begin
      j:=ja.Items[i];
      if j.JSONType<>jtObject then Continue;

      opt:=TCmdLineCfgOption.Create;
      TJSONObject(j).Iterate(TCfgIterator.IterateOption, opt);
      if (opt.Key='') then begin
        opt.Free
      end else begin
        CmdLineOptionNormalize(opt);
        cfg.Options.Add(opt);
      end;
    end;

  end else begin
    if Item.JSONType<>jtString then Exit;
    if nm='executable' then cfg.Executable:=Item.AsString
    else if nm='version' then cfg.Version:=Item.AsString
    else if nm='fromversion' then cfg.FromVersion:=Item.AsString
    else if nm='testvalue' then cfg.TestValue:=Item.AsString
    else if nm='testkey' then cfg.TestKey:=Item.AsString
  end;

end;

function CmdLineCfgJSONReadFile(stream: TStream; cfg: TCmdLineCfg): Boolean;
var
  p : TJSONParser;
  d : TJSONData;
  core : TJSONObject;
begin
  Result:=False;
  d:=nil;
  p:=TJSONParser.Create(stream);
  try
    d:=p.Parse;
    if d.JSONType<>jtObject then Exit;
    core:=TJSONObject(d);
    core.Iterate(TCfgIterator.Iterate, cfg);
    Result:=cfg.Executable<>'';
  finally
    d.Free;
    p.Free;
  end;
end;

function CmdLineCfgJSONReadFile(const FileName: String; cfg: TCmdLineCfg): Boolean;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result:=CmdLineCfgJSONReadFile(fs, cfg);
  finally
    fs.Free;
  end;
end;

procedure CmdLineCfgJSONLoadFilesFromDir(const Dir: String; list: TList; const Mask: string);
var
  rslt : TSearchRec;
  res  : Integer;
  pth  : string;
  cfg  : TCmdLineCfg;
begin
  pth:=IncludeTrailingPathDelimiter(Dir);
  res:=FindFirst( pth+Mask, faAnyFile, rslt);
  try
    while res = 0 do begin
      if (rslt.Attr and faDirectory=0) and (rslt.Size>0) then begin
        cfg := TCmdLineCfg.Create;
        if not CmdLineCfgJSONReadFile(pth+rslt.Name, cfg) then cfg.Free
        else list.Add(cfg);
      end;
      res:=FindNext(rslt);
    end;
  finally
    FindClose(rslt);
  end;
end;

end.

