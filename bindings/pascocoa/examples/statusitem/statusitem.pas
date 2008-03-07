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
  objc, ctypes, FPCMacOSAll, AppKit, Foundation, actions;

const
  Str_Window_Title = 'StatusItem example project';
  Str_Show_Button = 'Show StatusItem';
var
  { classes }
  pool: NSAutoreleasePool;
  MainWindow: NSWindow;
  MainWindowView: NSView;
  MyButton: NSButton;
  { strings }
  CFTitle, CFButtonText: CFStringRef;
  { sizes }
  MainWindowRect, ButtonRect: NSRect;
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

  MainWindow := NSWindow.initWithContentRect(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, NO);
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);

  CFTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);
  
  { Initializes the action responding object }

  actionList := TMyActionList.Create();
  
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
  MyButton.setTarget(actionList.Handle);
  MainWindowView.addSubview(MyButton);

  { Enters main message loop }

  MainWindow.orderFrontRegardless;

  NSApp.run;

  {  Releases the AutoreleasePool }
  pool.Free;
end.

