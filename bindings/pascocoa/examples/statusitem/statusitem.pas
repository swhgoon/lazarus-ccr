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

{$linkframework Cocoa}
{$linklib objc}

uses
  objc, ctypes, MacOSAll, AppKit, Foundation, controller, cocoa_pkg;

const
  Str_Window_Title = 'StatusItem example project';
  Str_Show_Button = 'Show StatusItem';
  Str_Hide_Button = 'Hide StatusItem';
  Str_Close_Button = 'Exit Program';
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

  MainWindow := NSWindow.initWithContentRect_styleMask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, LongBool(NO));
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);

  CFTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);
  
  { Initializes the controller object }

  myController := TMyController.Create();
  
  { Adds the buttons }

  myController.CreateButton(MainWindowView, Str_Show_Button,
   50.0, MainWindowRect.size.height - 50.0, 200.0, 25.0,
   Str_doShowStatusItem, myController);

  myController.CreateButton(MainWindowView, Str_Hide_Button,
   50.0, MainWindowRect.size.height - 100.0, 200.0, 25.0,
   Str_doHideStatusItem, myController);

  myController.CreateButton(MainWindowView, Str_Close_Button,
   50.0, MainWindowRect.size.height - 150.0, 200.0, 25.0,
   Str_doClose, myController);

  { Enters main message loop }

  MainWindow.orderFrontRegardless;

  NSApp.setDelegate(myController.Handle);

  NSApp.run;

  {  Releases the AutoreleasePool }
  pool.Free;
end.

