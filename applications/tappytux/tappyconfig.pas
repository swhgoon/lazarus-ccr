unit tappyconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics;

type

  { TTappyTuxConfig }

  TTappyTuxConfig = class
  public
    function GetResourcesDir: string;
    function GetCurrentSkinDir: string;
  end;

const
  STR_LINUX_RESOURCES_FOLDER = '/usr/share/tappytux/';

  ID_ENGLISH = 0;
  ID_PORTUGUESE = 1;

var
  vTappyTuxConfig: TTappyTuxConfig;

implementation

{$ifdef Darwin}
uses
  MacOSAll;
{$endif}

const
  BundleResourcesDirectory = '/Contents/Resources/';

{ TChessConfig }

function TTappyTuxConfig.GetResourcesDir: string;
{$ifdef Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := STR_LINUX_RESOURCES_FOLDER;
{$endif}
{$endif}

{$ifdef Windows}
  Result := ExtractFilePath(Application.EXEName);
{$endif}
end;

function TTappyTuxConfig.GetCurrentSkinDir: string;
begin
  Result := GetResourcesDir() + 'skins' + PathDelim + 'classic' + PathDelim;
end;

initialization

vTappyTuxConfig := TTappyTuxConfig.Create;

finalization

vTappyTuxConfig.Free;

end.
