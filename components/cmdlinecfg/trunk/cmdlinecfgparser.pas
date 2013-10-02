unit cmdlinecfgparser;

interface

{$mode delphi}

uses
  Classes, SysUtils, cmdlinecfg, cmdlinecfgutils;

type

  { TCmdLineOptionParse }

  TOptionParse = class(TObject)
    key : string;
    opt : TCmdLineCfgOption;
    isDelimited : Boolean; // the value comes with a white-space after the key
    isParameter : Boolean;
    constructor Create(aopt: TCmdLineCfgOption; const akey: string; AisDelim: Boolean; AisParam: Boolean);
  end;

  { TCmdLineArgsParser }

  TCmdLineArgsParser = class(TObject)
  private
    fKeyPrefix   : TStrings;
    fCfg         : TCmdLineCfg;
    fisValid     : Boolean;
    fMasters     : TStringList;
    MaxMasterLen : Integer;
    MinMasterLen : Integer;
    fOptions     : TStringList;
    MaxKeyLen    : Integer;
    MinKeyLen    : Integer;
  protected
    procedure SetCfg(ACfg: TCmdLineCfg);
    procedure PrepareConfig;
    function FindMasterKey(const arg: string): string;
    function FindParseOption(const arg: string): TOptionParse;
    function CreateMasterKeyValues(const ArgValue, MasterKey: string; Vals: TList): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    // if an argumant doesn't have a corresponding cmd line option;
    // .Option field will be set to nil. The next "unkey" option will be set as a value;
    //   KeyPrefix - would be used to find values that are not part of arguments only;
    // the method will create TCmdLineOptionValue objects and add them to values list
    // it doesn't check for their existance, just adds them!
    function Parse(Args: TStrings; Vals: TList {of TCmdLineOptionValue}): Boolean;
    property CmdLineCfg:  TCmdLineCfg read fCfg write SetCfg;
    property KeyPrefix : TStrings read fKeyPrefix;
  end;

// note that KeyPrefix defaults to unix keys = "-". On Windows, many MS commandlines
// are using "/" as the key prefix
procedure CmdLineMatchArgsToOpts(CmdLineCfg: TCmdLineCfg; Args: TStrings; Vals: TList {of TCmdLineOptionValue}; const KeyPrefix: string = '-'); overload;
procedure CmdLineMatchArgsToOpts(CmdLineCfg: TCmdLineCfg; const CmdLine: string; Vals: TList {of TCmdLineOptionValue}; const KeyPrefix: string = '-'); overload;

implementation

procedure CmdLineMatchArgsToOpts(CmdLineCfg: TCmdLineCfg; Args: TStrings; Vals: TList;
  const KeyPrefix: string);
var
  parser : TCmdLineArgsParser;
begin
  parser := TCmdLineArgsParser.Create;
  try
    parser.CmdLineCfg:=CmdLineCfg;
    parser.KeyPrefix.Add(KeyPrefix);
    parser.Parse(Args, Vals);
  finally
    parser.Free;
  end;
end;

procedure CmdLineMatchArgsToOpts(CmdLineCfg: TCmdLineCfg; const CmdLine: string;
  Vals: TList; const KeyPrefix: string);
var
  args : TStringList;
begin
  args:=TstringList.Create;
  try
    CmdLineParse(cmdLine, args);
    CmdLineMatchArgsToOpts(cmdlinecfg, args, vals, KeyPrefix);
  finally
    args.Free;
  end;
end;

{ TOptionParse }

constructor TOptionParse.Create(aopt: TCmdLineCfgOption; const akey: string; AisDelim: Boolean; AisParam: Boolean);
begin
  inherited Create;
  opt:=Aopt;
  key:=akey;
  isDelimited:=AisDelim;
  isParameter:=AisParam;
end;

{ TCmdLineArgsParse }

procedure TCmdLineArgsParser.SetCfg(ACfg: TCmdLineCfg);
begin
  if fCfg<>ACfg then begin
    fisValid:=false;
    fCfg:=ACfg;
  end;
end;

procedure MaxMinOptionLen(options: TStrings; var minlen, maxlen: Integer);
var
  i  : Integer;
  j  : Integer;
  ln : Integer;
  k  : string;
begin
  maxlen:=0;
  minlen:=0;
  if options.Count=0 then Exit;

  for i:=0 to options.Count-1 do begin
    ln:=length(options[i]);
    if ln>maxlen then maxlen:=ln;
  end;

  j:=0;
  repeat
    inc(minlen);
    k:=Copy(options[0],1,minlen);
    for i:=0 to options.Count-1 do
      if Pos(k, options[i])<>1 then begin
        inc(j);
        break;
      end;
  until (j<>0) or (minlen>maxlen);
  dec(minlen);
end;

procedure TCmdLineArgsParser.PrepareConfig;
var
  i  : integer;
  ov : TCmdLineCfgOption;
  k  : string;
  j  : integer;
  y  : integer;
begin
  if not Assigned(fCfg) then Exit;
  fMasters.Clear;
  fOptions.Clear;
  fMasters.Duplicates:=dupIgnore;
  for i:=0 to fCfg.Options.Count-1 do begin
    ov:=TCmdLineCfgOption(fCfg.Options[i]);
    if not Assigned(ov) then Continue;
    k:=Trim(ov.Key);
    if ov.MasterKey<>'' then fMasters.Add(ov.MasterKey);
    // preparing keys for values with parameters, like -Cp%test% or -Cp %test%
    j:=Pos('%', k);
    y:=Pos(' ', k);
    if (y>0) and ((y<j) or (j=0)) then  k:=Copy(k, 1, y-1)
    else if j>0 then k:=Copy(k, 1, j-1);
    fOptions.AddObject(k, TOptionParse.Create(ov, k, y>0, j>0)  );
  end;
  MaxMinOptionLen(fMasters, MinMasterLen, MaxMasterLen);
  MaxMinOptionLen(fOptions, MinKeyLen, MaxKeyLen);
  fOptions.Sort;
  fisValid:=true;
end;

function TCmdLineArgsParser.FindMasterKey(const arg: string): string;
var
  i : integer;
  t : string;
  j : integer;
begin
  for i:=MinMasterLen to MaxMasterLen do begin
    t:=Copy(arg, 1, i);
    j:=fMasters.IndexOf(t);
    if j>=0 then begin
      Result:=fMasters[j];
      Exit;
    end;
  end;
  Result:='';
end;

function TCmdLineArgsParser.FindParseOption(const arg: string): TOptionParse;
var
  k : string;
  j : Integer;
  i : Integer;
begin
  for i:=MinKeyLen to MaxKeyLen do begin
    k:=Copy(arg, 1, i);
    j:=fOptions.IndexOf(k);
    if j>=0 then begin
      Result:=TOptionParse( fOptions.Objects[j] );
      Exit;
    end;
  end;
  Result:=nil;
end;

function TCmdLineArgsParser.CreateMasterKeyValues(const ArgValue, MasterKey: string; Vals: TList): Boolean;
var
  i : Integer;
  k : string;
  j : Integer;
  op : TOptionParse;
begin
  Result:=False;
  for i:=length(MasterKey)+1 to length(ArgValue) do begin
    k:=MasterKey + ArgValue[i];
    j:=fOptions.IndexOf(k);
    if j>=0 then begin
      Result:=True;
      op:=TOptionParse(fOptions.Objects[j]);
      Vals.Add( TCmdLineOptionValue.Create ( op.opt, '1'))
    end else begin
      Vals.Add( TCmdLineOptionValue.Create ( nil, k));
    end;
  end;
end;

constructor TCmdLineArgsParser.Create;
begin
  fKeyPrefix := TStringList.Create;
  TStringList(fKeyPrefix).CaseSensitive:=true;

  fMasters := TStringList.Create;
  TStringList(fMasters).CaseSensitive:=true;

  fOptions := TStringList.Create;
  TStringList(fOptions).CaseSensitive:=true;
  TStringList(fOptions).OwnsObjects:=true;
end;

destructor TCmdLineArgsParser.Destroy;
begin
  fMasters.Free;
  fOptions.Clear;
  fOptions.Free;
  fKeyPrefix.Free;
  inherited Destroy;
end;

function TCmdLineArgsParser.Parse(Args: TStrings; Vals: TList): Boolean;
var
  i  : integer;
  v  : string;
  mk : string;
  op : TOptionParse;
begin
  Result:=Assigned(fCfg);
  if not Result then Exit;
  if not fisValid then PrepareConfig;
  i:=0;
  while i<Args.Count do begin
    v:=Args[i];
    mk:=FindMasterKey(v);
    if mk<>'' then begin
      // todo: test if there's any known value among keys!
      CreateMasterKeyValues(v, mk, Vals);
    end else begin
      op:=FindParseOption(v);
      if not Assigned(op) then
        Vals.Add ( TCmdLineOptionValue.Create(nil, v))
      else begin
        if op.isParameter then begin
          if op.isDelimited then begin
            inc(i);
            if i<Args.Count then v:=args[i] else v:='';
          end else
            v:=Copy(v, length(op.key)+1, length(v));
        end else
          v:='1'; // is switch, is enabled!
        Vals.Add( TCmdLineOptionValue.Create(op.opt, v));
      end;
    end;
    inc(i);
  end;
end;


end.
