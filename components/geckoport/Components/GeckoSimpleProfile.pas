(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GeckoComponents for Delphi.
 *
 * The Initial Developer of the Original Code is Takanori Ito.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)
unit GeckoSimpleProfile;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TGeckoSimpleProfile = class(TComponent)
  private
    { Private éŒ¾ }
    procedure SetProfileBaseDirectory(Value: String);
    function GetProfileBaseDirectory: String;
  protected
    { Protected éŒ¾ }
  public
    { Public éŒ¾ }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published éŒ¾ }
    property ProfileBaseDirectory: String
        read GetProfileBaseDirectory
        write SetProfileBaseDirectory;
  end;

procedure Register;

implementation

uses
  nsXPCOM, nsXPCOMGlue, nsError, nsGeckoStrings, nsProfile, nsTypes, nsInit,
  GeckoInit;

procedure Register;
begin
  RegisterComponents('Gecko', [TGeckoSimpleProfile]);
end;

var
  sProfileDirServiceProvider: nsProfileDirServiceProvider;
  sProfileBaseDirectory: String;
  sRefCnt: Integer = 0;

procedure ChangeProfileBaseDirectory; forward;
procedure SavePreferences; forward;
procedure LoadPreferences; forward;

constructor TGeckoSimpleProfile.Create(AOwner: TComponent);
var
  provider: nsProfileDirServiceProvider;
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    GeckoComponentsStartup;

    if not Assigned(sProfileDirServiceProvider) then
    begin
      provider := NS_NewProfileDirServiceProvider(True);

      sProfileDirServiceProvider := provider;

      //if Length(sProfileBaseDirectory)>0 then
      ChangeProfileBaseDirectory;
      sProfileDirServiceProvider.Register;
    end;
    Inc(sRefCnt);
  end;
end;

destructor TGeckoSimpleProfile.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    SavePreferences;

    Dec(sRefCnt);
    if sRefCnt=0 then
      sProfileDirServiceProvider := nil;

    GeckoComponentsShutdown;
  end;

  inherited;
end;

procedure TGeckoSimpleProfile.SetProfileBaseDirectory(Value: String);
begin
  if Value = sProfileBaseDirectory then Exit;

  sProfileBaseDirectory := Value;

  if not (csDesigning in ComponentState) then
  begin
    ChangeProfileBaseDirectory;
    LoadPreferences;
  end;
end;

function TGeckoSimpleProfile.GetProfileBaseDirectory: String;
begin
  Result := sProfileBaseDirectory;
end;

procedure ChangeProfileBaseDirectory;
var
  rv: nsresult;
  localFile: nsILocalFile;
{$ifndef UNICODE}
  localFileStr: IInterfacedCString;
{$else}
  localFileStr: IInterfacedString;
{$endif}
  modulePath: array[0..MAX_PATH] of Char;
  targetPath, relativePath: String;
begin
  GetModuleFileName(0, modulePath, MAX_PATH);

  if (Length(sProfileBaseDirectory)>0) and
     (sProfileBaseDirectory[1] = '\') then
  begin
    // case of Path from root or network path
    if (Length(sProfileBaseDirectory)>=2) and
       (sProfileBaseDirectory[2] <> '\') then
    begin
      // case of Path from root
      targetPath := ExtractFileDrive(modulePath) + sProfileBaseDirectory;
    end else
    begin
      // case of network path
      targetPath := sProfileBaseDirectory;
    end;
  end else
  if (Length(sProfileBaseDirectory)<3) or
     (sProfileBaseDirectory[2] <> ':') then
  begin
    // case of relative path
    targetPath := ExtractFileDir(modulePath);
    if Length(sProfileBaseDirectory)>0 then
      relativePath := sProfileBaseDirectory;
  end else
  begin
    // case of full path
    targetPath := sProfileBaseDirectory;
  end;

  try
{$ifndef UNICODE}
    localFileStr := NewCString(targetPath);
{$else}
    localFileStr := NewString(targetPath);
{$endif}
  except
    Exit;
  end;
{$ifndef UNICODE}
  rv := NS_NewNativeLocalFile(localFileStr.ACString, True, localFile);
{$else}
  rv := NS_NewLocalFile(localFileStr.AString, True, localFile);
{$endif}
  if NS_FAILED(rv) then Exit;

  if Length(relativePath)>0 then
  begin
    localFileStr.Assign(relativePath);
{$ifndef UNICODE}
    localFile.AppendRelativeNativePath(localFileStr.ACString);
{$else}
    localFile.AppendRelativePath(localFileStr.AString);
{$endif}
  end;

  sProfileDirServiceProvider.SetProfileDir(localFile);
end;

procedure SavePreferences;
var
  prefSrv: nsIPrefService;
  prefFile: nsIFile;
begin
  NS_GetService(NS_PREFSERVICE_CID, nsIPrefService, prefSrv);
  prefFile := NS_GetSpecialDirectory(NS_APP_PREFS_50_FILE);
  prefSrv.SavePrefFile(prefFile);
end;

procedure LoadPreferences;
var
  prefSrv: nsIPrefService;
  prefFile, prefDir: nsIFile;
begin
  NS_GetService(NS_PREFSERVICE_CID, nsIPrefService, prefSrv);
  prefDir := NS_GetSpecialDirectory(NS_APP_PREFS_50_DIR);
  if not prefDir.Exists then
  begin
    prefDir.Create(NS_IFILE_DIRECTORY_TYPE, 7*64+5*8+5);
  end;
  prefFile := NS_GetSpecialDirectory(NS_APP_PREFS_50_FILE);
  prefSrv.ResetPrefs;
  if prefFile.Exists then
  begin
    prefSrv.ReadUserPrefs(prefFile);
  end;
end;

end.
