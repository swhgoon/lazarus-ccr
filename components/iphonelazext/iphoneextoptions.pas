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

unit iPhoneExtOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, LazIDEIntf, ProjectIntf, iPhoneBundle, DOM, XMLRead, XMLConf, PlistFile;

type

  { TiPhoneProjectOptions }

  TiPhoneProjectOptions = class(TAbstractIDEProjectOptions)
  private
    fisiPhone   : Boolean;
    fAppID      : string;
    fSDK        : string;
    DataWritten : Boolean;
    fSpaceName  : string;
  public
    constructor Create;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function Load: Boolean;
    function Save: Boolean;
    procedure Reset;
    property isIPhoneApp: Boolean read fisIPhone write fisIPhone;
    property SDK: string read fSDK write fSDK;
    property AppID: string read fAppID write fAppID;
    property SpaceName: string read fSpaceName write fSpaceName;
  end;

  { TiPhoneEnvironmentOptions }

  TSDKInfo = class(TObject)
    devName : String;
    devPath : String;
    simName : String;
    simPath : String;
    options : String;
  end;

  TiPhoneEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    fPlatformsBaseDir : string;
    fCompilerPath     : string;
    fBaseRTLPath      : string;
    fCommonOpt        : string;
    fSimAppsPath      : string;
    fSimBundle        : string;
    fDefaultSDK       : string;

    fVersions  : TStringList;
  protected
    function XMLFileName: string;

    procedure ClearVersionsInfo;
    procedure FoundSDK(const Version, DevSDKName, DevSDKPath, SimSDKName, SimSDKPath: String);
    function GetSDKInfo(const Version: String): TSDKInfo;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;

    function Load: Boolean;
    function Save: Boolean;

    function GetSDKName(const SDKVer: string; simulator: Boolean): string;
    function GetSDKFullPath(const SDKVer: string; simulator: Boolean): string;

    procedure GetSDKVersions(Strings: TStrings);
    procedure RefreshVersions;

    property PlatformsBaseDir: string read fPlatformsBaseDir write fPlatformsBaseDir;
    property CompilerPath: string read fCompilerPath write fCompilerPath;
    property BaseRTLPath: string read fBaseRTLPath write fBaseRTLPath;
    property CommonOpt: string read fCommonOpt write fCommonOpt;

    property SimBundle: string read fSimBundle write fSimBundle;
    property SimAppsPath: string read fSimAppsPath write fSimAppsPath;

    property DefaultSDK: string read fDefaultSDK write fDefaultSDK;
  end;

function EnvOptions: TiPhoneEnvironmentOptions;
function ProjOptions: TiPhoneProjectOptions;


type
  TSDKFoundEvent = procedure (const Version: String;
    const DeviceSDKName, DeviceSDKPath, SimSDKName, SimSDKPath: string) of object;

function ScanForSDK(const PlatformDir: String; FoundProc: TSDKFoundEvent): Boolean;

var
  iPhoneEnvGroup : Integer;
  iPhonePrjGroup : Integer;

implementation

var
  fEnvOptions   : TiPhoneEnvironmentOptions = nil;
  fProjOptions  : TiPhoneProjectOptions = nil;

const
  DefaultXMLName = 'iphoneextconfig.xml';

  optisIphone   = 'iPhone/isiPhoneApp';
  optSDK        = 'iPhone/SDK';
  optAppID      = 'iPhone/AppID';
  optSpaceName  = 'iPhone/SimSpaceName';


function EnvOptions: TiPhoneEnvironmentOptions;
begin
  if not Assigned(fEnvOptions) then
    fEnvOptions:=TiPhoneEnvironmentOptions.Create;
  Result:=fEnvOptions;
end;

function ProjOptions: TiPhoneProjectOptions;
begin
  if not Assigned(fProjOptions) then
    fProjOptions:=TiPhoneProjectOptions.Create;
  Result:=fProjOptions;
end;

procedure InitOptions;
begin
  iPhoneEnvGroup := GetFreeIDEOptionsGroupIndex(GroupEnvironment);
  iPhonePrjGroup := GetFreeIDEOptionsGroupIndex(GroupProject);
  RegisterIDEOptionsGroup(iPhoneEnvGroup, TiPhoneEnvironmentOptions);
  RegisterIDEOptionsGroup(iPhonePrjGroup, TiPhoneProjectOptions);
end;

procedure FreeOptions;
begin
  fEnvOptions.Free;
end;

{ TiPhoneEnvironmentOptions }

class function TiPhoneEnvironmentOptions.GetGroupCaption: string;
begin
  Result:='iPhone Environment';
end;

class function TiPhoneEnvironmentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=EnvOptions;
end;

function TiPhoneEnvironmentOptions.XMLFileName: String;
begin
  Result:=IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)+DefaultXMLName;
end;

procedure TiPhoneEnvironmentOptions.ClearVersionsInfo;
var
  i : Integer;
begin
  for i:=0 to fVersions.Count-1 do begin
    fVersions.Objects[i].Free;
    fVersions.Objects[i]:=nil;
  end;
  fVersions.Clear;
end;

procedure TiPhoneEnvironmentOptions.FoundSDK(const Version, DevSDKName,
  DevSDKPath, SimSDKName, SimSDKPath: String);
var
  info: TSDKInfo;
begin
  info:=TSDKInfo.Create;
  info.devName:=DevSDKName;
  info.devPath:=DevSDKPath;
  info.simName:=SimSDKName;
  info.simPath:=SimSDKPath;
  fVersions.AddObject(Version, info);
end;

function TiPhoneEnvironmentOptions.GetSDKInfo(const Version: String): TSDKInfo;
var
  i : Integer;
begin
  i:=fVersions.IndexOf(Version);
  if i<0 then Result:=nil
  else Result:=TSDKInfo(fVersions.Objects[i]);
end;

function GetDefaultPlatformPath: WideString;
begin
  //todo:
  Result:='/Developer/Platforms';
end;

function GetDefaultSimBundlePath: WideString;
begin
  //todo:
  Result:=IncludeTrailingPathDelimiter(GetDefaultPlatformPath)+
          'iPhoneSimulator.platform/Developer/Applications/iPhone Simulator.app';
end;

function GetDefaultSimAppPath: WideSTring;
begin
  //todo:
  Result:=IncludeTrailingPathDelimiter(GetUserDir)+
          'Library/Applications Support/iPhone Simulator/User/Applications/';
end;

constructor TiPhoneEnvironmentOptions.Create;
begin
  inherited Create;
  fPlatformsBaseDir := GetDefaultPlatformPath;
  fSimAppsPath := GetDefaultSimAppPath;
  fSimBundle := GetDefaultSimBundlePath;
  fCompilerPath := '/usr/local/bin/fpc';
  fVersions:=TStringList.Create;
end;

destructor TiPhoneEnvironmentOptions.Destroy;
begin
  ClearVersionsInfo;
  fVersions.Free;
  inherited Destroy;
end;


function TiPhoneEnvironmentOptions.Load: Boolean;
var
  xmlcfg : TXMLConfig;
begin
  Result:=true;
  try
    xmlcfg := TXMLConfig.Create(nil);
    try
      xmlcfg.RootName:='config';
      xmlcfg.Filename:=XMLFileName;
      fPlatformsBaseDir := UTF8Encode(xmlcfg.GetValue('Platforms', fPlatformsBaseDir ));
      fCompilerPath := UTF8Encode(xmlcfg.GetValue('Compiler', fCompilerPath));
      fBaseRTLPath  := UTF8Encode(xmlcfg.GetValue('RTLPath', fBaseRTLPath));
      fCommonOpt    := UTF8Encode(xmlcfg.GetValue('CompilerOptions', fCommonOpt));
      fSimBundle    := UTF8Encode(xmlcfg.GetValue('SimBundle', fSimBundle));
      fSimAppsPath  := UTF8Encode(xmlcfg.GetValue('SimAppPath', fSimAppsPath));
      fDefaultSDK := UTF8Encode(xmlcfg.GetValue('DefaultSDK', fDefaultSDK));

      RefreshVersions;
      if (fDefaultSDK = '') and (fVersions.Count>0) then
        fDefaultSDK:=fVersions[0];
    finally
      xmlcfg.Free;
    end;
  except
    Result:=false;
  end;
end;

function TiPhoneEnvironmentOptions.Save: Boolean;
var
  xmlcfg : TXMLConfig;
begin
  Result:=true;
  try
    xmlcfg := TXMLConfig.Create(nil);
    try
      xmlcfg.RootName:='config';
      xmlcfg.Filename:=XMLFileName;
      xmlcfg.SetValue('Platforms', UTF8Decode(fPlatformsBaseDir));
      xmlcfg.SetValue('Compiler', UTF8Decode(fCompilerPath));
      xmlcfg.SetValue('RTLPath', UTF8Decode(fBaseRTLPath));
      xmlcfg.SetValue('CompilerOptions', UTF8Decode(fCommonOpt));
      xmlcfg.SetValue('SimBundle', UTF8Decode(fSimBundle));
      xmlcfg.SetValue('SimAppPath', UTF8Decode(fSimAppsPath));
      xmlcfg.SetValue('DefaultSDK', UTF8Decode(fDefaultSDK));
    finally
      xmlcfg.Free;
    end;
  except
    Result:=false;
  end;

end;

function TiPhoneEnvironmentOptions.GetSDKName(const SDKVer: string; simulator: Boolean): string;
var
  info : TSDKInfo;
begin
  info:=GetSDKInfo(SDKVer);
  if not Assigned(info) then Result:=''
  else begin
    if simulator then Result:=info.devName
    else Result:=info.simName;
  end;
end;

function TiPhoneEnvironmentOptions.GetSDKFullPath(const SDKVer: string; simulator: Boolean): string;
var
  info : TSDKInfo;
begin
  info:=GetSDKInfo(SDKVer);
  if not Assigned(info) then Result:=''
  else begin
    if simulator then Result:=info.devPath
    else Result:=info.simPath;
  end;
end;

procedure TiPhoneEnvironmentOptions.GetSDKVersions(Strings: TStrings);
var
  i : Integer;
begin
  for i:=0 to fVersions.Count-1 do
    Strings.Add( fVersions[i] );
end;

procedure TiPhoneEnvironmentOptions.RefreshVersions;
begin
  ClearVersionsInfo;
  ScanForSDK(EnvOptions.PlatformsBaseDir, @FoundSDK);
end;

{ TiPhoneProjectOptions }

procedure TiPhoneProjectOptions.Reset;
begin
  fisiPhone:=false;
  fSDK:='iPhone 2.0';
  fAppID:='com.mycompany.myapplication';
  fSpaceName:='';
  DataWritten:=false;
end;

constructor TiPhoneProjectOptions.Create;
begin
  inherited Create;
  Reset;
end;

class function TiPhoneProjectOptions.GetGroupCaption: string;
begin
  Result:='iPhone';
end;

class function TiPhoneProjectOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=ProjOptions;
end;

function TiPhoneProjectOptions.Load: Boolean;
begin
  Result:=True;
  with LazarusIDE.ActiveProject do begin
    DataWritten:=CustomData.Contains(optisIphone);
    fisiPhone:=(DataWritten) and (CustomData.Values[optisIphone] = 'true');
    if CustomData.Contains(optSDK) then fSDK:=CustomData.Values[optSDK];
    if CustomData.Contains(optAppID) then fAppID:=CustomData.Values[optAppID];
    fSpaceName:=CustomData.Values[optSpaceName];
    if fSpaceName='' then fSpaceName:=RandomSpaceName;
  end;
end;

function TiPhoneProjectOptions.Save: Boolean;
const
  BoolStr : array[Boolean] of string = ('false', 'true');
begin
  Result:=True;
  {do not write iPhone related info to non-iPhone projects}
  if DataWritten or fisiPhone then
    with LazarusIDE.ActiveProject do begin
      CustomData.Values[optisIPhone] := BoolStr[fisiPhone];
      CustomData.Values[optSDK]:=fSDK;
      CustomData.Values[optAppID]:=fAppID;
      CustomData.Values[optSpaceName]:=fSpaceName;
    end;
end;

type
  TSDKDescription = record
    FullPath  : String;  {full SDK path}
    Name      : String;
    Alternate : String; {alternate SDK -> iphonesimulator for iphoneos}
    Version   : String;
    isDevice  : Boolean; {true for real iPhoneOS, false for iPhoneSimulator}
  end;

// todo: implement reading .plist via OSX functions! (in case a .plist format changes)
function ReadSDKSettings(const FileName: string; var Descr: TSDKDescription): Boolean;
var
  plist : TPListFile;
begin
  Result:=False;
  plist:=TPListFile.Create(FileName);

  Descr.Name:=plist.GetStrValue('CanonicalName');
  Descr.Alternate:=plist.GetStrValue('AlternateSDK');
  Descr.Version:=plist.GetStrValue('Version');

  plist.Free;
end;

function isSDKDir(const SDKDir: String; var d: TSDKDescription): Boolean;
var
  plist : string;
begin
  plist := IncludeTrailingPathDelimiter(SDKDir)+'SDKSettings.plist';
  Result:=FileExists(plist);
  if not Result then Exit;
  ReadSDKSettings(plist, d);
  d.FullPath:=SDKDir;
end;

function ScanForSDK(const PlatformDir: String; FoundProc: TSDKFoundEvent): Boolean;
const
  PlatformName: array [Boolean] of string = ('iPhoneOS.platform','iPhoneSimulator.platform');
  SDKSubDir = PathDelim+'Developer'+PathDelim+'SDKs'+PathDelim;
var
  isdev : Boolean;
  dir   : string;
  sr    : TSearchRec;
  sdks  : array of TSDKDescription;
  descr : TSDKDescription;
  cnt   : Integer;
  simname : string;
  simpath : string;
  i,j     : Integer;

  procedure AddDescription(const d: TSDKDescription);
  begin
    if cnt = length(sdks) then begin
      if cnt = 0 then SetLength(sdks, 16)
      else SetLength(sdks, cnt*2);
    end;
    sdks[cnt]:=d;
    inc(cnt);
  end;

begin
  writeln('scan for SDK');
  Result:=Assigned(FoundProc);
  if not Result then Exit;

  cnt:=0;

  for isdev:=false to true do begin
    dir := IncludeTrailingPathDelimiter(PlatformDir) + PlatformName[isdev] + SDKSubDir;
    if FindFirst(dir+'*', faAnyFile, sr)=0 then begin
      repeat
        if (sr.Attr and faDirectory>0) and (ExtractFileExt(sr.Name) = '.sdk') then
          if isSDKDir( dir + sr.Name, descr) then begin
            descr.isDevice:=isdev;
            AddDescription(descr);
          end;
      until FindNext(sr)<>0;
      FindClose(sr);
    end;
  end;

  for i:=0 to cnt-1 do
    if sdks[i].isDevice then begin
      simname:='';
      simpath:='';
      for j:=0 to cnt-1 do
        if (not sdks[j].isDevice) and (sdks[i].Alternate=sdks[j].Name) then begin
          simname:=sdks[j].Name;
          simpath:=sdks[j].FullPath;
        end;
      FoundProc(sdks[i].Version, sdks[i].Name, sdks[i].FullPath, simname, simpath);
    end;

  Result:=True;
end;

initialization
  InitOptions;

finalization
  FreeOptions;

end.

