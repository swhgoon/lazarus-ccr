unit chessconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TChessConfig }

  TChessConfig = class
  public
    function GetResourcesDir: string;
    function GetCurrentSkinDir: string;
  end;

var
  vChessConfig: TChessConfig;

implementation

{$ifdef Darwin}
uses
  MacOSAll;
{$endif}

const
  BundleResourcesDirectory = '/Contents/Resources/';

{ TChessConfig }

function TChessConfig.GetResourcesDir: string;
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
  Result := '';
{$endif}
{$endif}

{$ifdef Windows}
  Result := ExtractFilePath(Application.EXEName);
{$endif}
end;

function TChessConfig.GetCurrentSkinDir: string;
begin
  Result := GetResourcesDir() + 'skins' + PathDelim + 'classic' + PathDelim;
end;

initialization

vChessConfig := TChessConfig.Create;

finalization

vChessConfig.Free;

end.

