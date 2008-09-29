unit model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MacOSAll, objc, appkit, foundation;

type

  { TMyModel }

  TMyModel = class
  private
    procedure LoadImages;
    function GetResourcesDir: string;
  public
    { Routines and variables for program resources }
    imgOpen, imgSave, imgClose: NSImage;
    ResourcesDir: string;
    constructor Create;
  end;

var
  myModel: TMyModel;

implementation

procedure TMyModel.LoadImages;
var
  ImagePath: CFStringRef;
begin
  ImagePath := CFStringCreateWithPascalString(nil, ResourcesDir + 'imgOpen.png', kCFStringEncodingUTF8);
  imgOpen := NSImage.initWithContentsOfFile(ImagePath);

  ImagePath := CFStringCreateWithPascalString(nil, ResourcesDir + 'imgSave.png', kCFStringEncodingUTF8);
  imgSave := NSImage.initWithContentsOfFile(ImagePath);

  ImagePath := CFStringCreateWithPascalString(nil, ResourcesDir + 'imgClose.png', kCFStringEncodingUTF8);
  imgClose := NSImage.initWithContentsOfFile(ImagePath);
end;

function TMyModel.GetResourcesDir: string;
const
  BundleResourcesDirectory = '/Contents/Resources/';
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
begin
  // Under Mac OS X we need to get the location of the bundle
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
end;

constructor TMyModel.Create;
begin
  LoadImages;

  ResourcesDir := GetResourcesDir;
end;

end.

