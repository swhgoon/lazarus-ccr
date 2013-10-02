unit cfgcompoptreg;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs
  , LazIDEIntf, MenuIntf
  , cmdlinecfg, cmdlinecfgutils
  , cmdlinecfgjson, cmdlinecfgui, cmdlinecfguijson
  , optviewform
  , XMLConf;

procedure Register;

var
  listOfOpt    : TList;
  listOfLayout : TList;

  CompOptVers  : string; // the version of fpc compiler used
  CompOptCfg   : TCmdLineCfg; // fpc compiler options
  CfgLayout    : TCmdLineLayoutInfo; // compiler options layout hints

implementation

resourcestring
  mnuViewCfgCompilerOpt = 'Review Compiler Options';

function GetCompilerPath: string;
var
  path : string;
  xml  : TXMLConfig;
begin
  //appears to be a hack, but is there a better way to do that?
  path := LazarusIDE.GetPrimaryConfigPath;
  xml := TXMLConfig.Create(nil);
  try
    xml.RootName:='CONFIG';
    xml.Filename:=IncludeTrailingPathDelimiter(path)+'environmentoptions.xml';
    Result:=xml.GetValue('EnvironmentOptions/CompilerFilename/Value', '');
  finally
    xml.Free;
  end;
end;

procedure ReviewCompOpt(Sender: TObject);
var
  cmp : string;
begin
  if not Assigned(CompOptCfg) then begin
    cmp:=GetCompilerPath;
    if cmp<>'' then
      CompOptCfg:=CmdLineCfgDetect(listOfOpt, ExtractFileDir(cmp), cmp);
  end;

  //todo: better selection of default options
  if not Assigned(CompOptCfg) and (listOfOpt.Count>0) then
    CompOptCfg:=TCmdLineCfg(listOfOpt[0]);

  if not Assigned(CompOptCfg) then begin
    ShowMessage('Unable to detect the compiler version.');
    Exit;
  end;

  if Assigned(LazarusIDE.ActiveProject) then begin
    if not Assigned(OptView) then OptView:=TOptView.Create(Application);
    ReviewOpts(CompOptCfg, CfgLayout);
  end;
end;

procedure DoRegister;
var
  cmd : TIDEMenuCommand;
begin
  cmd := RegisterIDEMenuCommand(itmProjectWindowSection, 'showCfgCompOpt',
    mnuViewCfgCompilerOpt, nil, ReviewCompOpt, nil, '');
end;

procedure ReadConfig;
var
  path : string;
begin
  path:=ExcludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath);
  if not DirectoryExists( path ) then  path:=ExcludeTrailingPathDelimiter(LazarusIDE.GetSecondaryConfigPath );
  CmdLineCfgJSONLoadFilesFromDir( path , listOfOpt );
  CmdLineCfgUIJSONLoadFilesFromDir( path , listOfLayout );


  //todo: make a smarter layout selection
  if listOfLayout.Count>0 then CfgLayout:=TCmdLineLayoutInfo(listOfLayout[0]);
end;

procedure Register;
begin
  try
    DoRegister;
    ReadConfig;
  except
  end;
end;

procedure Init;
begin
  listOfOpt := TList.Create;
  listOfLayout := TList.Create;
end;

procedure Release;
var
  i : integer;
begin
  for i:=0 to listOfOpt.Count-1 do TObject(listOfOpt[i]).Free;
  listOfOpt.Free;
  for i:=0 to listOfLayout.Count-1 do TObject(listOfLayout[i]).Free;
  listOfLayout.Free;
end;

initialization
  Init;

finalization
  Release;

end.

