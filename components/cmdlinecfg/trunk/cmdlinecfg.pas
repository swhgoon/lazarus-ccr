unit cmdlinecfg;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TCmdLineCfgValues = record
    CmdLineValue : String; // the actual command that goes to the cmd line
    DisplayName  : String; // the default display name (in English)
    Condition    : String; // condition for the value of the option
  end;

  { TCmdLineCfgOption }

  TCmdLineCfgOption = class(TObject)
  private
    procedure AssureSizeForIndex(AIndex: Integer);
  public
    Section     : String;  // the secion of the option
    SubSection  : String;  // logical sub-section of the option
    Name        : String;  // the "code" of the option, to be saved into project settings (to be backward compatible)
    OptType     : String;  // option type - free form type option options
    Key         : String;  // the key that needs to go
    MasterKey   : String;  // the key values will be combined into a single Key, prefixed with the MasterKey
                           // example: two options -Ct -Co will be combined into -Cto, if both have -C as master key.
    AliasToKey  : string;  // the key is deprecated and it's alias to a newer and better key
    Display     : String;  // the default description of the option
    Condition   : String;  // the condition for the option (in general)
    Values      : array of TCmdLineCfgValues; // cmd line value used with the key
    ValCount    : Integer; // the total number of values
    isMultiple  : Boolean;
    constructor Create;
    procedure SetValue(const AValue: string; Index: Integer = 0);
    procedure SetValDisplay(const DispName: string; Index: Integer = 0);
    procedure SetCondition(const Condition: string; Index: Integer = 0);
  end;

  { TCmdLineCfg }

  TCmdLineCfg = class(TObject)
  private
    fHash       : TFPHashObjectList;
    isValid     : Boolean;
  public
    Options      : TList;
    Executable   : String; // the executable code. Doesn't have to be the actual command-line executable name
    Version      : String; // human-readable version name
    FromVersion  : String; // the previous version of configuration
    TestKey      : String; // the command that should return the TestValue
    TestValue    : String; // expected test value to confirm the version.
    constructor Create;
    destructor Destroy; override;
    function FindOption(const name: string): TCmdLineCfgOption;
  end;

  { TCmdLineOptionValue }

  TCmdLineOptionValue = class(TObject)
    Option : TCmdLineCfgOption;
    Value  : String;
    constructor Create(AOption: TCmdLineCfgOption=nil; const AValue: string = '');
  end;

procedure CmdLineDebug(cfg: TCmdLineCfg);
procedure CmdLineDebugOption(opt: TCmdLineCfgOption);
function CmdLineMakeOptions(values: TList {of TCmdLineOptionValue}): string;

// returns the substring for thr command-line, by replacing %value% from the "Key" param
// is ValueType is switch, simply returns the key, if Value is not an empty string
// Example #1
//   Key       = -Ck%value%
//   ValueType = int
//   Value     = 5000
//   Result    = -Ck5000
// Example #2
//   Key       = -Fu%value%
//   ValueType = filename
//   Value     = /usr/bin/my files/test.pas
//   Result    = -Fu"/usr/bin/my files/test.pas"
function CmdLineCollectValue(const Key, ValueType, Value: string): string;

function CmdLineGenerateName(const Key,Name: String): String;
// Automatically sets the name based by CmdLineGenerateName
// Empty type is not allow, so defaults to "switch"
// Chagnes type from "switch" to either "string" ot "select"
// if the Key has a value (string), or there's a list of options given (select)
procedure CmdLineOptionNormalize(opt: TCmdLineCfgOption);

implementation

procedure CmdLineOptionNormalize(opt: TCmdLineCfgOption);
var
  tp: string;
begin
  if not Assigned(opt) then Exit;
  opt.Name:=CmdLineGenerateName(opt.Key, opt.Name);
  if opt.OptType='' then opt.OptType:='switch';
  tp:=AnsiLowerCase(opt.OptType);
  if (pos('%value%', AnsiLowercase(opt.Key))>0) and (tp='switch')then begin
    if opt.ValCount>1 then opt.OptType:='select'
    else opt.OptType:='string';
  end;;

end;

function CmdLineGenerateName(const Key,Name: String): String;
begin
  Result:=Name;
  if Name='' then Result:=StringReplace(Key, '%value%', '', [rfIgnoreCase,rfReplaceAll]);
end;

{ TCmdLineOptionValue }

constructor TCmdLineOptionValue.Create(AOption: TCmdLineCfgOption;
  const AValue: string);
begin
  inherited Create;
  Option:=AOption;
  Value:=AValue;
end;


{ TCmdLineCfgOption }

procedure TCmdLineCfgOption.AssureSizeForIndex(AIndex: Integer);
begin
  while length(Values)<=AIndex do begin
    if length(Values)=0 then SetLength(Values, 4)
    else SetLength(Values, length(Values)*2);
  end;
  if ValCount<=AIndex then ValCount:=AIndex+1;
end;

constructor TCmdLineCfgOption.Create;
begin
  inherited Create;
end;

procedure TCmdLineCfgOption.SetValue(const AValue: string; Index: Integer);
begin
  AssureSizeForIndex(Index);
  Values[Index].CmdLineValue:=AValue;
end;

procedure TCmdLineCfgOption.SetValDisplay(const DispName: string; Index: Integer);
begin
  AssureSizeForIndex(Index);
  Values[Index].DisplayName:=DispName;
end;

procedure TCmdLineCfgOption.SetCondition(const Condition: string; Index: Integer
  );
begin
  AssureSizeForIndex(Index);
  Values[Index].Condition:=Condition;
end;

{ TCmdLineCfg }

constructor TCmdLineCfg.Create;
begin
  Options:=TList.Create;
  fHash:=TFPHashObjectList.Create(false);
end;

destructor TCmdLineCfg.Destroy;
var
  i : integer;
begin
  for i:=0 to Options.Count-1 do TCmdLineCfgOption(Options[i]).Free;
  Options.Free;
  fHash.Free;
  inherited Destroy;
end;

function TCmdLineCfg.FindOption(const name: string): TCmdLineCfgOption;
var
  i   : integer;
  l   : string;
  opt : TCmdLineCfgOption;
begin
  if not isValid then begin
    for i:=0 to Options.Count-1 do begin
      opt := TCmdLineCfgOPtion(Options[i]);
      fHash.Add( opt.Name, opt);
    end;
    isValid:=true;
  end;
  Result:=TCmdLineCfgOption(fHash.Find(name));
end;

procedure CmdLineDebugOption(opt: TCmdLineCfgOption);
var
  i : integer;
begin
  if (opt.Section<>'') or (opt.SubSection<>'') then
    writeln(opt.Name, ' [', opt.Section,'/',opt.SubSection,']');
  writeln('key:  ', opt.key,' (',opt.Display,')');
  writeln('type: ', opt.OptType);
  if opt.isMultiple then writeln('multiple values allowed');
  if opt.MasterKey<>'' then writeln('masterkey: ', opt.MasterKey);
  for i:=0 to opt.ValCount-1 do begin
    writeln('  value: ', opt.Values[i].CmdLineValue,' ', opt.Values[i].DisplayName );
    if opt.Values[i].Condition<>'' then
      writeln('  condition: ', opt.Values[i].Condition);
  end;
end;

procedure CmdLineDebug(cfg: TCmdLineCfg);
var
  i : integer;
begin
  writeln('executable: ', cfg.Executable);
  writeln('version:    ', cfg.Version);
  writeln('test key:   ', cfg.TestKey);
  writeln('test value: ', cfg.TestValue);
  writeln('total options: ', cfg.Options.Count);
  writeln;
  for i:=0 to cfg.Options.Count-1 do begin
    CmdLineDebugOption(TCmdLineCfgOption(cfg.Options[i]));
    writeln;
  end;
end;

function CheckQuotes(const v: string): string;
var
  i : integer;
begin
  Result:=v;
  for i:=1 to length(v) do
    if (v[i] in [' ','<','>',#39]) then begin
      //todo: how to handle quotes in parameter value?
      Result:='"'+v+'"';
      Exit;
    end;
end;

function CmdLineCollectValue(const Key, ValueType, Value: string): string;
var
  l : string;
  j : Integer;
  vl : string;
const
  ValueParam = '%value%';
begin
  if Value='' then begin
    Result:='';
    Exit;
  end;

  l:=LowerCase(ValueType);
  if l='switch' then begin
    Result:=Key // no values expected
  end else begin
    vl:=CheckQuotes(Value);
    Result:=Key;
    j:=Pos(ValueParam, LowerCase(Result));
    if j>0 then begin
      //%value% is present in key declaration
      Delete(Result, j, length(ValueParam));
      // replacing any %% with %
      Result:=StringReplace(Result, '%%', '%', [rfIgnoreCase, rfReplaceAll]);
      Insert(vl, Result, j);
    end else
      //%value% is not present in key declaration, so just attach it to the key
      Result:=Key+StringReplace(Key, '%%', '%', [rfIgnoreCase, rfReplaceAll])+vl;
  end;
end;

function CmdLineMakeOptions(values: TList {of TCmdLineOption}): string;
var
  i       : Integer;
  j       : Integer;
  masters : TStringList;
  vl      : TCmdLineOptionValue;
  v       : string;
  mk      : string;
begin
  Result:='';
  masters := TStringList.Create;
  try
    for i:=0 to values.Count-1 do begin
      vl:=TCmdLineOptionValue(values[i]);
      if vl.Option = nil then Continue;

      v:=CmdLineCollectValue(vl.Option.Key, vl.Option.OptType, vl.Value);
      if v='' then Continue;

      mk:=vl.Option.MasterKey;
      if mk<>'' then begin
        j:=masters.IndexOfName(mk);
        v:=Copy(v, length(mk)+1, length(v));
        if j<0 then
          masters.Values[mk]:=v
        else
          masters.ValueFromIndex[j]:=masters.ValueFromIndex[j]+v;
      end else begin
        if Result='' then Result:=v
        else Result:=Result+' '+v;
      end;
    end;
    for i:=0 to masters.Count-1 do begin
      v:=masters.Names[i]+masters.ValueFromIndex[i];
      if Result='' then Result:=v
      else Result:=Result+' '+v;
    end;
  finally
    masters.Free;
  end;

end;

end.

