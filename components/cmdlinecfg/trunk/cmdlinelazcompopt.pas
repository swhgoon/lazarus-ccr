unit cmdlinelazcompopt;

{$mode delphi}{$H+}

interface

// this unit depends on IDEIntf package (CompOptsIntf.pas) !

uses
  Classes, SysUtils, CompOptsIntf, cmdlinecfg, cmdlinecfgutils, cmdlinecfgparser, contnrs;

{ Either procedures depends on a certain names/keys used for the FPC options.
  This has to be caferully maintained in the configuration file.              }
procedure LazCompOptToVals(opt: TLazCompilerOptions; cfg: TCmdLineCfg; list: TList {of TCmdLineValueOpts});
procedure ValsToLazCompOpt(list: TList {of TCmdLineValueOpts}; opt: TLazCompilerOptions);

implementation

// hash all values - make their copies and "join" multi values (if available)
function AllocLookup(list: TList): TFPHashObjectList;
var
  i  : integer;
  v  : TCmdLineOptionValue;
  lv : TCmdLineOptionValue;
  d  : string;
begin
  Result:=TFPHashObjectList.Create(true);
  for i:=0 to list.Count-1 do begin
    v:=TCmdLineOptionValue(list[i]);
    if not Assigned(v) or not Assigned(v.Option) then Continue;
    lv:=TCmdLineOptionValue(Result.Find(v.Option.Name));
    if v.Option.isMultiple then begin
      if not Assigned(lv) then begin
        lv:=TCmdLineOptionValue.Create(v.Option, v.Value);
        Result.Add(v.Option.Name, lv);
      end else begin
        if (v.Option.OptType='filepath') or (v.Option.OptType='dirpath') then d:=';' else d:=' ';
        if lv.Value='' then lv.Value:=v.Value else lv.Value:=lv.Value+d+v.Value;
      end;
    end else begin
      if not Assigned(lv) then begin
        lv:=TCmdLineOptionValue.Create(v.Option, v.Value);
        Result.Add(v.Option.Name, lv);
      end else
        lv.Value:=v.Value;
    end;
  end;
end;

function LookupStr(lp: TFPHashObjectList; const Name: string; const Default: string = ''; Remove: Boolean = true): string;
var
  v: TCmdLineOptionValue;
  i: integer;
begin
  i:=lp.FindIndexOf(Name);
  if i>=0 then begin
    v:=TCmdLineOptionValue(lp.Items[i]);
    Result:=v.Value;
    if Remove then lp.Delete(i); // frees the object
  end else
    Result:=Default;
end;

function LookupBool(lp: TFPHashObjectList; const Name: string; const Default: Boolean = false; Remove: Boolean = true): Boolean;
var
  v: TCmdLineOptionValue;
  i: integer;
begin
  i:=lp.FindIndexOf(Name);
  if i>=0 then begin
    v:=TCmdLineOptionValue(lp.Items[i]);
    Result:=v.Value<>'';
    if Remove then lp.Delete(i);  // frees the object
  end else
    Result:=Default;
end;

function LookupInt(lp: TFPHashObjectList; const Name: string; const Default: Integer = 0; Remove: Boolean = true): Integer;
var
  v: TCmdLineOptionValue;
  i: integer;
begin
  i:=lp.FindIndexOf(Name);
  if i>=0 then begin
    v:=TCmdLineOptionValue(lp.Items[i]);
    Result:=StrToIntDef(v.Value,Default);
    if Remove then lp.Delete(i);   // frees the object
  end else
    Result:=Default;
end;

function StrToDbgSymbolType(const vals: string): TCompilerDbgSymbolType;
var
  v : string;
begin
  v:=AnsiLowerCase(vals);
  if v='s' then Result:=dsStabs
  else if v='w2' then Result:=dsDwarf2
  else if v='w'  then Result:=dsDwarf2Set // ???
  else if v='w3' then Result:=dsDwarf3;
end;

procedure ValsToLazCompOpt(list: TList {of TCmdLineValueOpts}; opt: TLazCompilerOptions);
var
  lookup : TFPHashObjectList;
  i      : Integer;
  l      : TList;
begin
  lookup:=AllocLookup(list);
  try
    // search paths:
    opt.IncludePath:=LookupStr(lookup, '-Fi');
    opt.Libraries:=LookupStr(lookup, '-Fl');
    opt.ObjectPath:=LookupStr(lookup, '-Fo');
    opt.OtherUnitFiles:=LookupStr(lookup, '-Fu');
    //opt.SrcPath  (not in compiler options)
    //opt.DebugPath
    opt.UnitOutputDirectory:=LookupStr(lookup, '-FU');

    // target:
    opt.TargetFilename:=LookupStr(lookup, '-o');
    //opt.TargetFilenameApplyConventions

    // parsing:
    opt.SyntaxMode:=LookupStr(lookup, '-M');
    //property AssemblerStyle: Integer read fAssemblerStyle write SetAssemblerStyle;
    opt.CStyleOperators:=LookupBool(lookup, '-Sc');
    opt.IncludeAssertionCode:=LookupBool(lookup, '-Sa');
    opt.AllowLabel:=LookupBool(lookup,'-Sg');
    opt.UseAnsiStrings:=LookupBool(lookup,'-Sh');
    opt.CPPInline:=LookupBool(lookup,'-Si');
    opt.CStyleMacros:=LookupBool(lookup,'-Sm');
    opt.InitConstructor:=LookupBool(lookup,'-Ss');

    // -St is obsolete option ... so shouldn't be available
    opt.StaticKeyword:=LookupBool(lookup,'-St');

    // code generation:
    opt.IOChecks:=LookupBool(lookup,'-Ci');
    opt.RangeChecks:=LookupBool(lookup,'-Cr');
    opt.OverflowChecks:=LookupBool(lookup,'-Co');
    opt.StackChecks:=LookupBool(lookup,'-Ct');
    opt.SmartLinkUnit:=LookupBool(lookup,'-CX');
    opt.RelocatableUnit:=LookupBool(lookup,'-WR');
    opt.EmulatedFloatOpcodes:=LookupBool(lookup,'-Ce');

    opt.HeapSize:=LookupInt(lookup, '-Ch');
    opt.StackSize:=LookupInt(lookup, '-Cs');
    opt.VerifyObjMethodCall:=LookupBool(lookup,'-CR');

    opt.SmallerCode :=LookupBool(lookup, '-Os');
    opt.TargetCPU   :=LookupStr(lookup, '-P');
    opt.TargetProcessor:=LookupStr(lookup, '-Op');
    opt.TargetOS:=LookupStr(lookup, '-T');
    opt.VariablesInRegisters:=LookupBool(lookup, '-Or');
    opt.UncertainOptimizations:=LookupBool(lookup, '-Ou');
    opt.OptimizationLevel:=StrToIntDef(LookupStr(lookup, '-O'),0);

    // linking:
    opt.GenerateDebugInfo:=LookupBool(lookup, '-g');

    opt.DebugInfoType:=StrToDbgSymbolType(LookupStr(lookup, '-g'));
    //opt.DebugInfoTypeStr: String read GetDebugInfoTypeStr;

    opt.UseLineInfoUnit:=LookupBool(lookup, '-gl');
    opt.UseHeaptrc:=LookupBool(lookup, '-gh');
    opt.UseValgrind:=LookupBool(lookup, '-gv');
    opt.GenGProfCode:=LookupBool(lookup, '-pg');
    opt.StripSymbols:=LookupBool(lookup, '-Xs');
    opt.LinkSmart:=LookupBool(lookup, '-XX');

    opt.LinkerOptions:=LookupStr(lookup, '-k');
    opt.PassLinkerOptions:=opt.LinkerOptions<>''; //todo:!

    opt.Win32GraphicApp:=LookupBool(lookup, '-WG');
    //ExecutableType: TCompilationExecutableType   read FExecutableType write SetExecutableType;
    opt.UseExternalDbgSyms:=LookupBool(lookup, '-Xg');

    // messages:
    opt.ShowErrors:=LookupBool(lookup, '-ve');
    opt.ShowWarn:=LookupBool(lookup, '-vw');
    opt.ShowNotes:=LookupBool(lookup, '-vn');
    opt.ShowHints:=LookupBool(lookup, '-vh');
    opt.ShowGenInfo:=LookupBool(lookup, '-vi');
    opt.ShowLineNum:=LookupBool(lookup, '-vl');
    opt.ShowAll:=LookupBool(lookup, '-va');
    opt.ShowAllProcsOnError:=LookupBool(lookup, '-Xs');

    opt.ShowDebugInfo:=LookupBool(lookup, '-vd');
    opt.ShowUsedFiles:=LookupBool(lookup, '-vu');
    opt.ShowTriedFiles:=LookupBool(lookup, '-vt');
    opt.ShowCompProc:=LookupBool(lookup, '-vp');
    opt.ShowCond:=LookupBool(lookup, '-vc');
    opt.ShowExecInfo:=LookupBool(lookup, '-vx');
    opt.ShowNothing:=LookupBool(lookup, '-v0');
    //opt.ShowSummary
    //opt.ShowHintsForUnusedUnitsInMainSrc
    //opt.ShowHintsForSenderNotUsed
    opt.WriteFPCLogo:=LookupBool(lookup, '-l');
    opt.StopAfterErrCount:=LookupInt(lookup, '-Se');

    // other
    opt.DontUseConfigFile:=LookupBool(lookup, '-n');
    //opt.ConfigFilePath:=LookupStr(lookup, '@');
    //opt.CustomConfigFile:=opt.ConfigFilePath<>'';


    if lookup.Count>0 then begin
      l:=TList.Create;
      try
        for i:=0 to lookup.Count-1 do l.Add(lookup.Items[i]);
        opt.CustomOptions:=CmdLineMakeOptions(l);
      finally
        l.Free; // values, will be freed with lookup
      end;
    end;
  finally
    lookup.Free;
  end;
end;

procedure AddBoolValue(cfg: TCmdLineCfg; const Key: string; AVal: Boolean; list: TList; var Other: string);
var
  o  : TCmdLineCfgOption;
begin
  if not AVal then Exit;
  o:=cfg.FindOption(Key);
  if Assigned(o) then
    list.Add(TCmdLineOptionValue.Create(o, '1'));
end;

procedure AddStrValue(cfg: TCmdLineCfg; const Key, AVal: string; list: TList; var Other: string);
var
  o  : TCmdLineCfgOption;
begin
  if AVal='' then Exit;
  o:=cfg.FindOption(Key);
  if Assigned(o) then
    list.Add(TCmdLineOptionValue.Create(o, AVal));
end;

procedure AddIntValue(cfg: TCmdLineCfg; const Key: string; AVal: Integer; list: TList; var Other: string);
var
  o  : TCmdLineCfgOption;
begin
  if AVal<=0 then Exit;
  o:=cfg.FindOption(Key);
  if Assigned(o) then
    list.Add(TCmdLineOptionValue.Create(o, IntToStr(AVal)));
end;

procedure AddMultiStrValue(cfg: TCmdLineCfg; const Key, AVal, Delim: string; list: TList; var Other: string);
var
  o  : TCmdLineCfgOption;
  ch : Char;
begin
  if AVal='' then Exit;
  o:=cfg.FindOption(Key);
  if Assigned(o) then begin
    if length(DElim)>0 then ch:=Delim[1] else ch:=#0;
    CmdLineAllocMultiValues(o, AVal, ch, list);
  end;
end;


procedure LazCompOptToVals(opt: TLazCompilerOptions; cfg: TCmdLineCfg; list: TList {of TCmdLineValueOpts});
var
  other : string;
begin
  other := '';
  AddMultiStrValue(cfg, '-Fi', opt.IncludePath, ';', list, Other);
  AddMultiStrValue(cfg, '-Fl', opt.Libraries, ';', list, Other);
  AddMultiStrValue(cfg, '-Fo', opt.ObjectPath, ';', list, Other);
  AddMultiStrValue(cfg, '-Fu', opt.OtherUnitFiles, ';', list, Other);
  // opt.SrcPath   (not in compiler options) ?? -sources for Lazarus itself?

  //opt.DebugPath
  AddStrValue(cfg, '-FU', opt.UnitOutputDirectory, list, other);

  // target:
  AddStrValue(cfg, '-o', opt.TargetFilename, list, other);

  // parsing:
  AddStrValue(cfg, '-M', opt.UnitOutputDirectory, list, other);

  //property AssemblerStyle: Integer read fAssemblerStyle write SetAssemblerStyle;
  AddBoolValue(cfg, '-Sc', opt.CStyleOperators, list, other);

  AddBoolValue(cfg, '-Sa', opt.IncludeAssertionCode, list, other);
  AddBoolValue(cfg, '-Sg', opt.AllowLabel, list, other);
  AddBoolValue(cfg, '-Sh', opt.UseAnsiStrings, list, other);
  AddBoolValue(cfg, '-Si', opt.CPPInline, list, other);
  AddBoolValue(cfg, '-Sm', opt.CStyleMacros, list, other);
  AddBoolValue(cfg, '-Ss', opt.InitConstructor, list, other);

  // -St is obsolete option ... so shouldn't be available
  AddBoolValue(cfg, '-St', opt.StaticKeyword, list, other);

  // code generation:
  AddBoolValue(cfg, '-Ci', opt.IOChecks, list, other);
  AddBoolValue(cfg, '-Cr', opt.RangeChecks, list, other);
  AddBoolValue(cfg, '-Co', opt.OverflowChecks, list, other);
  AddBoolValue(cfg, '-Ct', opt.StackChecks, list, other);
  AddBoolValue(cfg, '-CX', opt.SmartLinkUnit, list, other);
  AddBoolValue(cfg, '-WR', opt.RelocatableUnit, list, other);
  AddBoolValue(cfg, '-Ce', opt.EmulatedFloatOpcodes, list, other);

  AddIntValue(cfg, '-Ch', opt.HeapSize, list, other);
  AddIntValue(cfg, '-Cs', opt.StackSize, list, other);
  AddBoolValue(cfg, '-CR', opt.VerifyObjMethodCall, list, other);

  AddBoolValue(cfg, '-CR', opt.SmallerCode, list, other);
  AddStrValue(cfg, '-P', opt.TargetCPU, list, other);
  AddStrValue(cfg, '-Op', opt.TargetProcessor, list, other);
  AddStrValue(cfg, '-T', opt.TargetOS, list, other);
  AddBoolValue(cfg, '-Or', opt.VariablesInRegisters, list, other);
  AddBoolValue(cfg, '-Ou', opt.UncertainOptimizations, list, other);
  AddStrValue(cfg,  '-O',  IntToStr(opt.OptimizationLevel), list, other);

  // linking:
  AddBoolValue(cfg, '-g', opt.GenerateDebugInfo, list, other);

  //todo: EPIC TODO
  //AddStrValue(cfg, '-g', opt.DebugInfoType, list, other);
  //opt.DebugInfoTypeStr: String read GetDebugInfoTypeStr;

  AddBoolValue(cfg, '-gl', opt.UseLineInfoUnit, list, other);
  AddBoolValue(cfg, '-gh', opt.UseHeaptrc, list, other);
  AddBoolValue(cfg, '-gv', opt.UseValgrind, list, other);
  AddBoolValue(cfg, '-pg', opt.GenGProfCode, list, other);
  AddBoolValue(cfg, '-Xs', opt.StripSymbols, list, other);
  AddBoolValue(cfg, '-XX', opt.LinkSmart, list, other);

  AddMultiStrValue(cfg, '-k', opt.LinkerOptions, ' ', list, other);
  {opt.LinkerOptions:=LookupStr(lookup, '-k');
  opt.PassLinkerOptions:=opt.LinkerOptions<>''; //todo:!}

  AddBoolValue(cfg, '-WG', opt.Win32GraphicApp, list, other);
  //ExecutableType: TCompilationExecutableType   read FExecutableType write SetExecutableType;
  AddBoolValue(cfg, '-Xg', opt.UseExternalDbgSyms, list, other);

  // messages:
  AddBoolValue(cfg, '-ve', opt.ShowErrors, list, other);
  AddBoolValue(cfg, '-vw', opt.ShowWarn, list, other);
  AddBoolValue(cfg, '-vn', opt.ShowNotes, list, other);
  AddBoolValue(cfg, '-vh', opt.ShowHints, list, other);
  AddBoolValue(cfg, '-vi', opt.ShowGenInfo, list, other);
  AddBoolValue(cfg, '-vl', opt.ShowLineNum, list, other);
  AddBoolValue(cfg, '-va', opt.ShowAll, list, other);
  AddBoolValue(cfg, '-Xs', opt.ShowAllProcsOnError, list, other);

  AddBoolValue(cfg, '-vd', opt.ShowDebugInfo, list, other);
  AddBoolValue(cfg, '-vu', opt.ShowUsedFiles, list, other);
  AddBoolValue(cfg, '-vt', opt.ShowTriedFiles, list, other);
  AddBoolValue(cfg, '-vp', opt.ShowCompProc, list, other);
  AddBoolValue(cfg, '-vc', opt.ShowCond, list, other);
  AddBoolValue(cfg, '-vx', opt.ShowExecInfo, list, other);
  AddBoolValue(cfg, '-v0', opt.ShowNothing, list, other);
  //opt.ShowSummary
  //opt.ShowHintsForUnusedUnitsInMainSrc
  //opt.ShowHintsForSenderNotUsed
  AddBoolValue(cfg, '-l' , opt.WriteFPCLogo, list, other);
  AddIntValue(cfg, '-Se', opt.StopAfterErrCount, list, other);

  // other
  AddBoolValue(cfg, '-n', opt.DontUseConfigFile, list, other);
  CmdLineMatchArgsToOpts(cfg, opt.CustomOptions, list);
end;

end.

