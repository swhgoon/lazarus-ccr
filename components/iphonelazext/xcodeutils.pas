unit XcodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PlistFile, LazFilesUtils;

const
  iPhoneOSplatform = 'iPhoneOS.platform';

// file names should utf8 encoded


// Scanning Xcode platform for available SDKs

type
  TSDKFoundEvent = procedure (const Version: String;
    const DeviceSDKName, DeviceSDKPath, SimSDKName, SimSDKPath: String) of object;

function ScanForSDK(const PlatformDir: String; FoundProc: TSDKFoundEvent): Boolean;

// Scanning for Templates

function XibTemplateDir(const PlatformDir: AnsiString): AnsiString;

type
  TScanTemplateProc = procedure ( const TemplateName, XibFileName,
    Description, IconFileName: AnsiString) of object;

procedure ScanForXibTemplates(const TemplateDir: AnsiString; Callback: TScanTemplateProc);

implementation

type
  TSDKDescription = record
    FullPath  : String;  {full SDK path}
    Name      : String;
    Alternate : String; {alternate SDK -> iphonesimulator for iphoneos}
    Version   : String;
    isSim     : Boolean; {true for real iPhoneOS, false for iPhoneSimulator}
  end;

function ReadSDKSettings(const FileName: String; var Descr: TSDKDescription): Boolean;
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
  plist : String;
begin
  plist := IncludeTrailingPathDelimiter(SDKDir)+'SDKSettings.plist';
  Result:=FileExists(plist);
  if not Result then Exit;
  ReadSDKSettings(plist, d);
  d.FullPath:=SDKDir;
end;

function ScanForSDK(const PlatformDir: String; FoundProc: TSDKFoundEvent): Boolean;
const
  PlatformName: array [Boolean] of String = ('iPhoneOS.platform','iPhoneSimulator.platform');
  SDKSubDir = PathDelim+'Developer'+PathDelim+'SDKs'+PathDelim;
var
  isSim   : Boolean;
  dir     : String;
  sr      : TSearchRec;
  sdks    : array of TSDKDescription;
  descr   : TSDKDescription;
  cnt     : Integer;
  simname : String;
  simpath : String;
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
  Result:=Assigned(FoundProc);
  if not Result then Exit;

  cnt:=0;

  for isSim:=false to true do begin
    dir := IncludeTrailingPathDelimiter(PlatformDir) + PlatformName[isSim] + SDKSubDir;
    if FindFirst(dir+'*', faAnyFile, sr)=0 then begin
      repeat
        if (sr.Attr and faDirectory>0) and (ExtractFileExt(sr.Name) = '.sdk') then
          if isSDKDir( dir + sr.Name, descr) then begin
            descr.isSim:=isSim;
            AddDescription(descr);
          end;
      until FindNext(sr)<>0;
      FindClose(sr);
    end;
  end;

  for i:=0 to cnt-1 do
    if not sdks[i].isSim then begin
      simname:='';
      simpath:='';
      for j:=0 to cnt-1 do
        if (sdks[j].isSim) and (sdks[i].Alternate=sdks[j].Name) then begin
          simname:=sdks[j].Name;
          simpath:=sdks[j].FullPath;
        end;
      FoundProc(sdks[i].Version, sdks[i].Name, sdks[i].FullPath, simname, simpath);
    end;

  Result:=True;
end;


function XibTemplateDir(const PlatformDir: AnsiString): AnsiString;
const
  TemplatePath = 'Developer/Library/Xcode/File Templates/User Interface';
begin
  Result:=IncludeTrailingPathDelimiter(PlatformDir)+TemplatePath;
end;

procedure ScanForXibTemplates(const TemplateDir: AnsiString; Callback: TScanTemplateProc);
var
  dirs   : TStringList;
  files  : TStringList;
  i,j    : Integer;
  plist  : TPListFile;

  xib     : AnsiString;
  name    : AnsiString;
  descr   : AnsiString;
const
  XibTemplateMask = '*.pbfiletemplate';
  IconFile = 'TemplateIcon.tiff';
begin
  if not Assigned(Callback) or not DirectoryExists(TemplateDir) then Exit;

  dirs:=TStringList.Create;
  files:=TStringList.Create;
  try
    EnumFilesAtDir( TemplateDir, XibTemplateMask, dirs );
    for i:=0 to dirs.Count-1 do begin
      if DirectoryExists(dirs[i]) then begin
        files.Clear;
        EnumFilesAtDir(dirs[i], files);

        xib:='';
        for j:=0 to files.Count-1 do
          if AnsiLowerCase(ExtractFileExt(files[j]))='.plist' then begin
            plist:=TPListFile.Create(files[j]);
            xib:=plist.GetStrValue('MainTemplateFile');
            descr:=plist.GetStrValue('Description');
            name:=ChangeFileExt(xib, '');
            Break;
          end;

        if xib<>'' then begin
          xib:=IncludeTrailingPathDelimiter(dirs[i])+xib;
          Callback(name, xib, descr, IncludeTrailingPathDelimiter(dirs[i])+IconFile);
        end;
      end;
    end;
  finally
    dirs.Free;
    files.Free;
  end;
end;

end.

