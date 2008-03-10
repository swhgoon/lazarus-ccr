{
 actions.pas

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
unit actions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, foundation, objc, appkit, FPCMacOSAll;

type

  { TMyActionList }

  TMyActionList = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    procedure AddMethods;
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
  end;

{ Objective-c Methods }
procedure doShowStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;
procedure doHideStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;

var
  actionList: TMyActionList;

{ Other helper functions }
function GetResourcesDir: string;
function CreateButton(AView: NSView; ATitle: shortstring;
 AX, AY, AWidth, AHeight: Double;
 ACallbackName: string; ACallbackClass: NSObject): NSButton;

implementation

{ TMyActionList }

{ Adds methods to the class }
procedure TMyActionList.AddMethods;
begin
  { Parameters string:
  
    The first parameter is the result (v = void),
    followed by self and _cmd (@ = id and : = SEL),
    and on the end "sender" (@ = id) }

  AddMethod('doShowStatusitem:', 'v@:@', @doShowStatusitem);
  AddMethod('doHideStatusitem:', 'v@:@', @doHideStatusitem);
end;

constructor TMyActionList.Create;
var
  fileName: CFStringRef;
begin
  inherited Create;

  AddMethods();

  bar := NSStatusBar.systemStatusBar();
  
  fileName := CFStringCreateWithPascalString(nil,
   GetResourcesDir + 'icon.ico', kCFStringEncodingUTF8);
  image := NSImage.initWithContentsOfFile(fileName);
end;

{ Objective-c Methods }

procedure doShowStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;
begin
  if actionList.item <> nil then Exit;

  actionList.item := actionList.bar.statusItemWithLength(NSSquareStatusItemLength);
  actionList.item.retain();
  actionList.item.setImage(actionList.image);
end;

procedure doHideStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;
begin
  if actionList.item = nil then Exit;

  actionList.item.Free;
  actionList.item := nil;
end;

{ Other helper functions }

function GetResourcesDir: string;
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

function CreateButton(AView: NSView; ATitle: shortstring;
 AX, AY, AWidth, AHeight: Double;
 ACallbackName: string; ACallbackClass: NSObject): NSButton;
var
  CFButtonText: CFStringRef;
  ButtonRect: NSRect;
begin
  CFButtonText := CFStringCreateWithPascalString(nil, ATitle, kCFStringEncodingUTF8);
  ButtonRect.origin.x := AX;
  ButtonRect.origin.y := AY;
  ButtonRect.size.width := AWidth;
  ButtonRect.size.height := AHeight;
  Result := NSButton.initWithFrame(ButtonRect);
  Result.setTitle(CFButtonText);
  Result.setBezelStyle(NSRoundedBezelStyle);
  Result.setAction(sel_registerName(PChar(ACallbackName)));
  Result.setTarget(ACallbackClass.Handle);
  AView.addSubview(Result);
end;

end.

