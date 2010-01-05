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
  Classes, SysUtils, IDEOptionsIntf, LazIDEIntf, ProjectIntf, iPhoneBundle, XMLConf;

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

  TiPhoneEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    fPlatformsBaseDir : string;
    fCompilerPath     : string;
    fBaseRTLPath      : string;
    fCommonOpt        : string;
    fSimAppsPath      : string;
    fSimBundle        : string;
  protected
    function XMLFileName: string;
  public
    constructor Create;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;

    function Load: Boolean;
    function Save: Boolean;

    function GetSimSDK(const ProjSDK: string; var SDKName, SDKFullPath: string): Boolean;
    function GetDeviceSDK(const ProjSDK: string; var SDKName, SDKFullPath: string): Boolean;

    function GetSDKName(const SDKVer: string; simulator: Boolean): string;
    function GetSDKFullPath(const SDKVer: string; simulator: Boolean): string;

    procedure GetSDKVersions(Strings: TStringList);


    property PlatformsBaseDir: string read fPlatformsBaseDir write fPlatformsBaseDir;
    property CompilerPath: string read fCompilerPath write fCompilerPath;
    property BaseRTLPath: string read fBaseRTLPath write fBaseRTLPath;
    property CommonOpt: string read fCommonOpt write fCommonOpt;

    property SimBundle: string read fSimBundle write fSimBundle;
    property SimAppsPath: string read fSimAppsPath write fSimAppsPath;
  end;

function EnvOptions: TiPhoneEnvironmentOptions;
function ProjOptions: TiPhoneProjectOptions;

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
    finally
      xmlcfg.Free;
    end;
  except
    Result:=false;
  end;

end;

function TiPhoneEnvironmentOptions.GetSimSDK(const ProjSDK: string; var SDKName, SDKFullPath: string): Boolean;
const
  iPhoneSimSDKDir = 'iPhoneSimulator2.0.sdk';
begin
  SDKName:=sdk_iPhoneSim2_0;
  SDKFullPath:=IncludeTrailingPathDelimiter(fPlatformsBaseDir)+'iPhoneSimulator.platform/Developer/SDKs/'+iPhoneSimSDKDir;
  Result:=true;
end;

function TiPhoneEnvironmentOptions.GetDeviceSDK(const ProjSDK: string; var SDKName, SDKFullPath: string): Boolean;
begin
  SDKName:='iphoneos2.0';
  SDKFullPath:='';
  Result:=false;
end;

function TiPhoneEnvironmentOptions.GetSDKName(const SDKVer: string;
  simulator: Boolean): string;
begin
  if simulator then Result:='iphonesimulator2.0'
  else Result:='iphoneos2.0';
end;

function TiPhoneEnvironmentOptions.GetSDKFullPath(const SDKVer: string; simulator: Boolean): string;
begin
  if simulator then
    Result := IncludeTrailingPathDelimiter(fPlatformsBaseDir)+'iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator2.0.sdk'
  else
    Result := IncludeTrailingPathDelimiter(fPlatformsBaseDir)+'iPhoneOS.platform/Developer/SDKs/iPhoneOS2.0.sdk';
end;

procedure TiPhoneEnvironmentOptions.GetSDKVersions(Strings: TStringList);
begin
  //todo:
  Strings.Add('iPhone OS 2.0');
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

initialization
  InitOptions;

finalization
  FreeOptions;

end.

