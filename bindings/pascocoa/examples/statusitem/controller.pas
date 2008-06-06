{
 controller.pas

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
unit controller;

{$mode delphi}{$STATIC ON}

interface

uses
  Classes, SysUtils, foundation, objc, appkit, FPCMacOSAll;

type

  { TMyController }

  TMyController = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    class function getClass: objc.id; override;
    procedure AddMethods; override;
    { Objective-c Methods }
    class procedure doShowStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; static;
    class procedure doHideStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; static;
    class procedure doClose(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; static;
    class function  applicationShouldTerminateAfterLastWindowClosed(_self: objc.id;
     _cmd: SEL; theApplication: objc.id): cbool; cdecl; static;
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
  Str_TMyController = 'TMyController';

  Str_doShowStatusitem = 'doShowStatusitem:';
  Str_doHideStatusitem = 'doHideStatusitem:';
  Str_doClose = 'doClose:';
  Str_applicationShouldTerminateAfterLastWindowClosed = 'applicationShouldTerminateAfterLastWindowClosed:';

var
  myController: TMyController;

  { classes }
  pool: NSAutoreleasePool;
  MainWindow: NSWindow;
  MainWindowView: NSView;
  { strings and sizes}
  CFTitle: CFStringRef;
  MainWindowRect: NSRect;

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
  AddMethod(Str_doClose, 'v@:@', Pointer(doClose));
  AddMethod(Str_applicationShouldTerminateAfterLastWindowClosed, 'b@:@',
   Pointer(applicationShouldTerminateAfterLastWindowClosed));
end;

constructor TMyController.Create;
var
  fileName: CFStringRef;
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  if not CreateClassDefinition(Str_TMyController, Str_NSObject) then WriteLn('Failed to create objc class');

  inherited Create;

  AddMethods();

  bar := NSStatusBar.systemStatusBar();
  
  fileName := CFStringCreateWithPascalString(nil, GetResourcesDir + 'icon.ico', kCFStringEncodingUTF8);
  image := NSImage.initWithContentsOfFile(fileName);
end;

class function TMyController.getClass: objc.id;
begin
  Result := objc_getClass(Str_TMyController);
end;

{ Objective-c Methods }

class procedure TMyController.doShowStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  if myController.item <> nil then Exit;

  myController.item := NSStatusItem.CreateWithHandle(myController.bar.statusItemWithLength(NSSquareStatusItemLength));
  myController.item.retain();
  myController.item.setImage(myController.image);
end;

class procedure TMyController.doHideStatusitem(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  if myController.item = nil then Exit;

  myController.item.Free;
  myController.item := nil;
end;

class procedure TMyController.doClose(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  MainWindow.close;
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

