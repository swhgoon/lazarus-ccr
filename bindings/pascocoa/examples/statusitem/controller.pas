{
 controller.pas

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
unit controller;

{$mode delphi}

interface

uses
  Classes, SysUtils, foundation, objc, appkit, FPCMacOSAll;

type

  { TMyController }

  TMyController = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    procedure AddMethods;
    { Objective-c Methods }
    class procedure doShowStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
    class procedure doHideStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
    class function  applicationShouldTerminateAfterLastWindowClosed(_self: objc.id;
     _cmd: SEL; theApplication: objc.id): cbool; cdecl;
    { Other helper functions }
    function GetResourcesDir: string;
    function CreateButton(AView: NSView; ATitle: shortstring;
     AX, AY, AWidth, AHeight: Double;
     ACallbackName: string; ACallbackClass: NSObject): NSButton;
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
  end;

const
  Str_doShowStatusitem = 'doShowStatusitem:';
  Str_doHideStatusitem = 'doHideStatusitem:';
  Str_applicationShouldTerminateAfterLastWindowClosed = 'applicationShouldTerminateAfterLastWindowClosed:';

var
  myController: TMyController;

implementation

{ TMyController }

{@@
  Adds methods to the class

  Details of the parameters string:

  The first parameter is the result (v = void),
  followed by self and _cmd (@ = id and : = SEL),
  and on the end "sender" (@ = id)
}
procedure TMyController.AddMethods;
begin
  AddMethod(Str_doShowStatusItem, 'v@:@', Pointer(doShowStatusitem));
  AddMethod(Str_doHideStatusitem, 'v@:@', Pointer(doHideStatusitem));
  AddMethod(Str_applicationShouldTerminateAfterLastWindowClosed, 'b@:@',
   Pointer(applicationShouldTerminateAfterLastWindowClosed));
end;

constructor TMyController.Create;
var
  fileName: CFStringRef;
begin
  inherited Create;

  AddMethods();

  bar := NSStatusBar.systemStatusBar();
  
  fileName := CFStringCreateWithPascalString(nil, GetResourcesDir + 'icon.ico', kCFStringEncodingUTF8);
  image := NSImage.initWithContentsOfFile(fileName);
end;

{ Objective-c Methods }

class procedure TMyController.doShowStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  if myController.item <> nil then Exit;

  myController.item := myController.bar.statusItemWithLength(NSSquareStatusItemLength);
  myController.item.retain();
  myController.item.setImage(myController.image);
end;

class procedure TMyController.doHideStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  if myController.item = nil then Exit;

  myController.item.Free;
  myController.item := nil;
end;

class function TMyController.applicationShouldTerminateAfterLastWindowClosed(_self: objc.id;
 _cmd: SEL; theApplication: objc.id): cbool; cdecl;
begin
  Result := objc.YES;
end;

{ Other helper functions }

function TMyController.GetResourcesDir: string;
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

function TMyController.CreateButton(AView: NSView; ATitle: shortstring;
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

