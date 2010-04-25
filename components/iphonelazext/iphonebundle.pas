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
unit iPhoneBundle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  platform_iPhoneSim = 'iphonesimulator';
  sdk_iPhoneSim2_0   = 'iphonesimulator2.0';
  sdk_iPhoneSim2_1   = 'iphonesimulator2.1';
  sdk_iPhoneSim2_2   = 'iphonesimulator2.2';
  sdk_iPhoneSim3_0   = 'iphonesimulator3.0';
  sdk_iPhoneSim3_1   = 'iphonesimulator3.1';

type
  TiPhoneBundleInfo = record
    DisplayName : WideString; {if DisplayName='' then DisplayName=BundleName}
    iPlatform   : WideString;
    SDKVersion  : WideString;
    AppID       : WideString;
    MainNib     : WideString;
  end;

function GetUserHomeDir: WideString;

function GetiPhoneSimUserPath(const UserHomeDir: WideString=''): WideString;

procedure MakeSimSpaceStruct(const BundleName: WideString; var BundleAppDir: WideString);
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, BundleName: WideString; var BundleAppDir: WideString);
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, SpaceName, BundleName: WideString; var BundleAppDir: WideString);

function GetBundleExeName(const BundleAppDir, ExeName: WideString): WideString;

procedure WritePkgFile(const FileName: WideString);
function WriteDefInfoList(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;

procedure CreateBundle(const BundleName, ExeName: WideString; const Info: TiPhoneBundleInfo; var FullBundlePath, FullExeName: WideString);
procedure CreateBundle(const BundleName, SpaceName, ExeName: WideString; const Info: TiPhoneBundleInfo; var RealSpace, FullBundlePath, FullExeName: WideString);
function GetBundleFullDir(const BundleName, SpaceName: WideString): WideString;
function GetSandBoxDir(const SpaceName: WideString): WideString;

function AddPathDelim(const w: WideString): WideString;

function RandomSpaceName: WideString;

implementation

function RandomSpaceName: WideString;
var
  g   : TGUID;
  id  : String;
begin
  CreateGUID(g);
  id:=GUIDToString(g);
  id:=Copy(id, 2, length(id)-2);
  Result:=id;
end;


procedure CreateBundle(const BundleName, SpaceName, ExeName: WideString; const Info: TiPhoneBundleInfo; var RealSpace, FullBundlePath, FullExeName: WideString);
var
  appdir : WideString;
begin
  if SpaceName='' then
    RealSpace:=RandomSpaceName
  else
    RealSpace:=SpaceName;

  MakeSimSpaceStruct(GetiPhoneSimUserPath, RealSpace, BundleName, appdir);
  FullBundlePath:=appdir;
  appdir:=AddPathDelim(appdir);
  FullExeName:=appdir+ExeName;
  WritePkgFile(appdir+'PkgInfo');
  WriteDefInfoList(appdir+'Info.plist', BundleName, ExeName, Info);
end;

function GetBundleFullDir(const BundleName, SpaceName: WideString): WideString;
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(GetiPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  Result:=UTF8Decode(p+UTF8Encode(BundleName)+'.app');
end;

function GetSandBoxDir(const SpaceName: WideString): WideString;
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(GetiPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  Result:=UTF8Decode(p);
end;

procedure CreateBundle(const BundleName, ExeName: WideString; const Info: TiPhoneBundleInfo; var FullBundlePath, FullExeName: WideString);
var
  sp : WideString;
begin
  CreateBundle(BundleName, '', ExeName, Info, sp, FullBundlePath, FullExeName);
end;

function AddPathDelim(const w: WideString): WideString;
begin
  if w='' then
    Result:=PathDelim
  else if w[length(w)]<>PathDelim then
    Result:=w+PathDelim;
end;

function GetUserHomeDir: WideString;
begin
  Result:=UTF8Decode(GetUserDir);
end;

function GetiPhoneSimUserPath(const UserHomeDir: WideString=''): WideString;
var
  nm : WideString;
begin
  if UserHomeDir = '' then nm:=GetUserHomeDir
  else nm:=UserHomeDir;

  if nm='' then Exit;
  if nm[length(nm)]<>'/' then nm:=nm+'/';
  Result:=nm+'Library/Application Support/iPhone Simulator/User/Applications/';
end;

{
~/Library/Application Support/iPhone Simulator/Users/%SPACENAME%

%SPACENAME%/Applications
%SPACENAME%/Applications/%AppBundle.app%
%SPACENAME%/Documents
%SPACENAME%/tmp
}
procedure MakeSimSpaceStruct(const iPhoneSimUserPath, SpaceName, BundleName: WideString; var BundleAppDir: WideString);
var
  path8   : String;
  space8  : String;
  p : string;
begin
  path8:=UTF8Encode(iPhoneSimUserPath);
  space8:=UTF8Encode(SpaceName);

  p:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(path8)+space8);
  BundleAppDir:=UTF8Decode(p+UTF8Encode(BundleName)+'.app');
  ForceDirectories(UTF8Encode(BundleAppDir));
  ForceDirectories(p+'Documents');
  ForceDirectories(p+'tmp');

end;

procedure MakeSimSpaceStruct(const iPhoneSimUserPath, BundleName: WideString; var BundleAppDir: WideString);
begin
  MakeSimSpaceStruct(iPhoneSimUserPath, RandomSpaceName, BundleName, BundleAppDir);
end;

procedure MakeSimSpaceStruct(const BundleName: WideString; var BundleAppDir: WideString);
begin
  MakeSimSpaceStruct( GetiPhoneSimUserPath, BundleName, BundleAppDir);
end;

function GetBundleExeName(const BundleAppDir, ExeName: WideString): WideString;
begin
  Result:=AddPathDelim(BundleAppDir)+ExeName;
end;

procedure WritePkgFile(const FileName: WideString);
var
  fs : TFileStream;
  s  : String;
begin
  fs:=TFileStream.Create( UTF8Encode(FileName), fmCreate);
  s:='APPL????';
  fs.Write(s[1], length(s));
  fs.Free;
end;

function WriteDefInfoList(const InfoFileName, BundleName, ExeName: WideString; const info: TiPhoneBundleInfo): Boolean;
const
  BundleFormat : AnsiString =
    '<?xml version="1.0" encoding="UTF-8"?>'#10+
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">'#10+
    '<plist version="1.0">'#10+
    '<dict>'#10+
    '	<key>CFBundleDevelopmentRegion</key>'#10+     '	<string>English</string>'#10+
    '	<key>CFBundleDisplayName</key>'#10+           '	<string>%s</string>'#10+    {display name}
    '	<key>CFBundleExecutable</key>'#10+            '	<string>%s</string>'#10+    {exe name}
    '	<key>CFBundleIdentifier</key>'#10+            '	<string>%s</string>'#10+ {company + bundle name}
    '	<key>CFBundleInfoDictionaryVersion</key>'#10+ '	<string>6.0</string>'#10+
    '	<key>CFBundleName</key>'#10+                  '	<string>%s</string>'#10+    {bundle name}
    '	<key>CFBundlePackageType</key>'#10+           '	<string>APPL</string>'#10+
    '	<key>CFBundleSignature</key>'#10+             '	<string>????</string>'#10+
    '	<key>CFBundleSupportedPlatforms</key>'#10+    '	<array>'#10+'		<string>%s</string>'#10+'	</array>'#10+ {platform}
    '%s'+ // optional MainNib name
    '	<key>CFBundleVersion</key>'#10+               '	<string>1.0</string>'#10+
    '	<key>DTPlatformName</key>'#10+                '	<string>%s</string>'#10+     {platform}
    '	<key>DTSDKName</key>'#10+                     '	<string>%s</string>'#10+     {sdk version}
    '	<key>LSRequiresIPhoneOS</key>'#10+            '	<true/>'#10+
    '</dict>'#10+
    '</plist>';

  function MainNibString(const NibName: WideString): AnsiString;
  begin
    if NibName='' then Result:=''
    else Result:='<key>NSMainNibFile</key><string>'+UTF8Encode(NibName)+'</string>'#10;
  end;

var
  dispName : WideString;
  s        : String;
  fs       : TFileStream;
begin
  Result:=false;
  if BundleName='' then Exit;

  dispName:=info.DisplayName;
  if dispName='' then dispName:=BundleName;

  with info do
    s:=Format( BundleFormat,
      [ UTF8Encode(dispName),
        UTF8Encode(ExeName),
        UTF8Encode(AppID),
        UTF8Encode(BundleName),
        UTF8Encode(iPlatform),
        MainNibString(info.MainNib),
        UTF8Encode(iPlatform),
        UTF8Encode(SDKVersion)
      ]);
  if FileExists(InfoFileName) then DeleteFile(InfoFileName);

  fs:=TFileStream.Create(InfoFileName, fmCreate or fmOpenWrite);
  try
    if s<>'' then fs.Write(s[1], length(s));
  finally
    fs.Free;
  end;
end;

end.

