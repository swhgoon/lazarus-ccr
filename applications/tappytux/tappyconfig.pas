unit tappyconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, LCLProc, FileUtil;

type

  { TTappyTuxConfig }

  TTappyTuxConfig = class
  public
    function GetResourcesDir: utf8string;
  end;

const
  STR_LINUX_RESOURCES_FOLDER = '/home/felipe/Programas/lazarus-ccr/applications/tappytux/'; // Temporary debug path
  //STR_LINUX_RESOURCES_FOLDER = '/usr/share/tappytux/'; // Real path

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

function TTappyTuxConfig.GetResourcesDir: utf8string;
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
  CFStringGetPascalString(pathCFStr, @pathStr, 255, kCFStringEncodingUTF8);
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := STR_LINUX_RESOURCES_FOLDER;
{$endif}
{$endif}

{$ifdef Windows}
  Result := SysToUTF8(ExtractFilePath(Application.EXEName));
{$endif}
end;

initialization

vTappyTuxConfig := TTappyTuxConfig.Create;

finalization

vTappyTuxConfig.Free;

end.
