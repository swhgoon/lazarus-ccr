{
 statusitem.pas

 This example shows how to create a window with several buttons which will
 control a NSStatusItem (a menubar extras icon).

 Compilation of this example requires the following options:
 -k-framework -kcocoa -k-lobjc

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
program statusitem;

{$mode delphi}

uses
  objc, ctypes, FPCMacOSAll, AppKit, Foundation;

const
  Str_Window_Title = 'StatusItem example project';
  Str_Show_Button = 'Show StatusItem';
var
  { classes }
  pool: NSAutoreleasePool;
  MainWindow: NSWindow;
  MainWindowView: NSView;
  MyButton: NSButton;
  bar: NSStatusBar;
  item: NSStatusItem;
  { strings }
  CFTitle, CFButtonText: CFStringRef;
  { sizes }
  MainWindowRect, ButtonRect: NSRect;

type
  TMyWindow = class(NSWindow)
  public
    { Extra binding functions }
    function getClass: objc.id; override;
  end;

procedure doShowStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;
begin
  item := bar.statusItemWithLength(NSSquareStatusItemLength);
end;

function TMyWindow.getClass: objc.id;
var
  method_list: Pobjc_method_list;
begin
  Result := objc_getClass(Str_NSWindow);
  
  { Adds methods to the class }
  
  method_list := GetMem(SizeOf(objc_method_list)); { We can't free this until the last instance is freed }
  
  method_list.method_count := 1;
  method_list.method_list[0].method_name := sel_registerName('doShowStatusitem:');
  { The first parameter is the result (v = void),
    followed by self and _cmd (@ = id and : = SEL),
    and on the end "sender" (@ = id) }
  method_list.method_list[0].method_types := 'v@:@';
  method_list.method_list[0].method_imp := IMP(@doShowStatusitem);
  class_addMethods(Result, method_list);
end;

begin
  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool.Create;

  { Creates the application NSApp object }
  NSApp := NSApplication.sharedApplication;

  { Creates a simple window }
  
  MainWindowRect.origin.x := 300.0;
  MainWindowRect.origin.y := 300.0;
  MainWindowRect.size.width := 300.0;
  MainWindowRect.size.height := 500.0;

  MainWindow := TMyWindow.initWithContentRect(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, NO);
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);

  CFTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);
  
  { Puts some buttons on the window }

  CFButtonText := CFStringCreateWithPascalString(nil, Str_Show_Button, kCFStringEncodingUTF8);
  ButtonRect.origin.x := 50.0;
  ButtonRect.origin.y := MainWindowRect.size.height - 50.0;
  ButtonRect.size.width := 100.0;
  ButtonRect.size.height := 25.0;
  MyButton := NSButton.initWithFrame(ButtonRect);
  MyButton.setStringValue(CFButtonText);
  MyButton.setBezelStyle(NSRoundedBezelStyle);
  MyButton.setAction(sel_registerName('doShowStatusitem:'));
  MyButton.setTarget(MainWindow.Handle);
  MainWindowView.addSubview(MyButton);

  { Initializes the StatusBar object }

  bar := NSStatusBar.systemStatusBar();

  { Enters main message loop }

  MainWindow.orderFrontRegardless;

  NSApp.run;

  {  Releases the AutoreleasePool }
  pool.Free;
end.

