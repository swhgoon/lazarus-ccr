unit cmdlinecfgutils;

interface

{$mode delphi}

uses
  SysUtils, Classes, cmdlinecfg, process;

function CmdLineCfgCombine(const ancestor, child: TCmdLineCfg; DoDeleteDashTypes: Boolean = true): Boolean;
procedure CmdLineCfgRemoveUnused(cfg: TCmdLineCfg);
function CmdLineCfgDetect(listofcfg: TList {of TCmdLineCfg}; const Dir, FullPathExec: string): TCmdLineCfg;

function ReadOutput(const Dir, ExecCommand: String): string;

// make the Value to be comand-line friendly, by handling CommandLineINvalidChars
// quotes would be added, if white-space characters are found
// todo: command lines replacement, should be system specific!
function CmdLineNormalizeParam(const Value: String): String;

// parses a command line into a list of arguments
// to be compatbile with RTL: ParamStr, ParamCount
procedure CmdLineParse(const cmdline: string; arguments : TStrings);
function CmdLineToExecutable(const cmdline: String; var Executable: string; Args: TStrings): Boolean;

procedure CmdLineAllocMultiValues(opt: TCmdLineCfgOption; const SrcValue: string; Delim: Char; dst: TList);

implementation

function OverrideIfEmpty(const existingValue, ReplacingValue: string): string;
begin
  if existingValue='' then Result:=ReplacingValue else Result:=existingValue;
end;

function CmdLineCfgOptionCopy(const opt: TCmdLineCfgOption): TCmdLineCfgOption;
var
  i : Integer;
begin
  Result:=TCmdLineCfgOption.Create;
  Result.Section:=opt.Section;
  Result.SubSection:=opt.SubSection;
  Result.Name:=opt.Name;
  Result.OptType:=opt.OptType;
  Result.Key:=opt.Key;
  Result.Display:=opt.Display;
  Result.Condition:=opt.Condition;
  Result.ValCount:=opt.ValCount;
  SetLength(Result.Values, Result.ValCount);
  for i:=0 to Result.ValCount-1 do begin
    Result.Values[i].Condition:=opt.Values[i].Condition;
    Result.Values[i].DisplayName:=opt.Values[i].DisplayName;
    Result.Values[i].CmdLineValue:=opt.Values[i].DisplayName;
  end;
end;

function SortByName(p1,p2: Pointer): Integer;
var
  o1, o2: TCmdLineCfgOption;
begin
  o1:=TCmdLineCfgOption(p1);
  o2:=TCmdLineCfgOption(p2);
  Result:=CompareStr(o1.Name, o2.Name);
end;

function CmdLineCfgCombine(const ancestor, child: TCmdLineCfg; DoDeleteDashTypes: Boolean = true): Boolean;
var
  i, j  : integer;
  l1,l2 : TList;
  opt   : TCmdLineCfgOption;
begin
  Result:=Assigned(ancestor) and Assigned(child)
          and (ancestor.Version=child.FromVersion) and (ancestor.Executable=child.Executable);
  if not Result then Exit;
  // executable
  // version
  // testValue
  // fromVersion are not inheritable
  child.TestKey:=OverrideIfEmpty(child.TestKey, ancestor.TestKey);
  ancestor.Options.Sort(@SortByName);
  child.Options.Sort(@SortByName);
  i:=0;
  j:=0;
  for i:=0 to ancestor.Options.Count-1 do begin
    opt:=TCmdLineCfgOption(ancestor.Options[i]);
    while (j<child.Options.Count) and (CompareStr(opt.Name, TCmdLineCfgOption(child.Options[j]).Name)>0) do
      inc(j);
    if (j<child.Options.Count) and (CompareStr(opt.Name, TCmdLineCfgOption(child.Options[j]).Name)<0) then begin
      child.Options.Add ( CmdLineCfgOptionCopy (opt));
    end;
  end;
  if DoDeleteDashTypes then CmdLineCfgRemoveUnused(child);
end;

procedure CmdLineCfgRemoveUnused(cfg: TCmdLineCfg);
var
  i : Integer;
begin
  for i:=0 to cfg.Options.Count-1 do
    if TCmdLineCfgOption(cfg.Options[i]).OptType='-' then begin
      TCmdLineCfgOption(cfg.Options[i]).Free;
      cfg.Options[i]:=nil;
    end;
  cfg.Options.Pack;
end;

function ReadOutput(const Dir, ExecCommand: String): string;
var
  p: TProcess;
  m: TMemoryStream;
  BytesRead : Integer;
  n: INteger;
  exe : string;
const
  READ_BYTES = 1024;
begin
  Result:='';
  BytesRead:=0;
  m:=TMemoryStream.Create;
  p:=TProcess.Create(nil);
  try
    exe:='';
    if not CmdLineToExecutable(ExecCommand, exe, p.Parameters) then Exit;
    p.Executable:=exe;
    p.CurrentDirectory:=Dir;
    p.Options:=[poUsePipes, poStdErrToOutput];
    p.Execute;
    while P.Running do begin
      if P.Output.NumBytesAvailable>0 then begin
        if M.Size-M.Position<READ_BYTES then begin
          M.SetSize(BytesRead + READ_BYTES);
        end;
        n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
        if n > 0 then Inc(BytesRead, n) else Sleep(1);
      end;
    end;
    repeat
      M.SetSize(BytesRead + READ_BYTES);
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then Inc(BytesRead, n);
    until n <= 0;
    if BytesRead > 0 then M.SetSize(BytesRead);
    M.Position:=0;
    SetLength(Result, M.Size);
    if length(Result)>0 then
      M.Read(Result[1], M.Size);
  finally
    p.Free;
  end;
end;

function SortByTestKey(c1, c2: TCmdLineCfg {these are actually Pointers in here!}): Integer;
begin
  Result:=CompareStr(c1.TestKey, c2.TestKey);
end;

function CmdLineCfgDetect(listofcfg: TList {of TCmdLineCfg}; const Dir, FullPathExec: string): TCmdLineCfg;
var
  i   : integer;
  cfg : TCmdLineCfg;
  tk  : String;
  tv  : String;
  search : TList;
begin
  Result:=nil;
  search:=TList.Create;
  try
    tk:='';
    search.Assign(listofcfg);
    search.Sort(@SortByTestKey);
    for i:=0 to listofcfg.Count-1 do begin
      cfg := TCmdLineCfg(listofcfg[i]);
      if cfg.TestKey<>tk then begin
        tk:=cfg.TestKey;
        tv:=trim(ReadOutput(dir, FullPathExec+' '+tk));
      end;
      if cfg.TestValue=tv then begin
        Result:=cfg;
        Exit;
      end;
    end;
  finally
    search.Free;
  end;
end;

function CmdLineNormalizeParam(const Value: String): String;
var
  i : Integer;
const
  CommandLineInvalidChars : set of Char = ['/','\',':','"','''','?','<','>',' '];
begin
  for i:=0 to length(Value) do
    if Value[i] in CommandLineInvalidChars then begin
      //todo!
      Result:='"'+Result+'"';
      Exit;
    end;
  Result:=Value;
end;


function CmdLineToExecutable(const cmdline: String; var Executable: string;
  Args: TStrings): Boolean;
var
  a : TStringList;
begin
  a:=TStringList.Create;
  try
    CmdLineParse(cmdline, a);
    Result:=a.Count>0;
    if Result then begin
      Executable:=a[0];
      a.Delete(0);
      Args.Assign(a);
    end;
  finally
    a.Free;
  end;
end;

procedure CmdLineParse(const cmdline: string; arguments : TStrings);
var
  i : integer;
  j : integer;
  isprm : Boolean;
  p : string;
const
  WhiteSpace : set of char = [#32,#9,#8,#13,#10];
  QuoteChar  = '"'; // yeah! be academic!
begin
  if not Assigned(arguments) then eXit;
  j:=1;
  i:=1;
  isprm:=false;
  p:='';
  while i<=length(cmdline) do begin
    if not (cmdline[i] in WhiteSpace) then begin
      if not isprm then j:=i;
      if cmdline[i]=QuoteChar then begin
        p:=p+Copy(cmdline, j, i-j);
        inc(i);
        j:=i;
        while (i<=length(cmdline)) and (cmdline[i]<>'"') do
          inc(i);
        p:=p+Copy(cmdline, j, i-j);
        j:=i+1;
      end;
      isprm:=true;
    end else if isprm then begin
      arguments.Add(p+Copy(cmdline, j, i-j));
      isprm:=false;
      p:='';
    end;
    inc(i);
  end;
  if isprm then arguments.Add(p+Copy(cmdline, j, i-j));
end;

procedure CmdLineAllocMultiValues(opt: TCmdLineCfgOption; const SrcValue: string; Delim: Char; dst: TList);
var
  i, j : Integer;
  vl   : TCmdLineOptionValue;
  v    : string;
begin
  if not Assigned(opt) or not Assigned(dst) or (SrcValue='') then Exit;
  i:=1; j:=1;
  while i<=length(SrcValue) do begin
    if SrcValue[i]=Delim then begin
      v:=Trim(Copy(SrcValue, j, i-j));
      j:=i+1;
      if v<>'' then dst.Add( TCmdLineOptionValue.Create(opt, v));
    end;
    inc(i);
  end;
  if j<i then begin
    v:=Trim(Copy(SrcValue, j, i-j));
    j:=i+1;
    if v<>'' then dst.Add( TCmdLineOptionValue.Create(opt, v));
  end;
end;

end.
