{
 simpleform.pas

 This example shows how to use the PasCocoa bindings to create a
 NSAutoreleasePool, initialize the application global variable, create
 a simple window without contents and attach a close handler to it that
 exits the application.

 Compilation of this example requires the following options:
 -k-framework -kcocoa -k-lobjc

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
program simplewindow;

{$mode delphi}

uses
  objc, ctypes, FPCMacOSAll, AppKit, Foundation;

const
  Str_Panel_Title = 'This is the title';
  Str_Panel_Message = 'This is the message';
var
  { classes }
  pool: NSAutoreleasePool;
  MainWindow: NSWindow;
  { strings }
  CFTitle, CFMessage: CFStringRef;
  { sizes }
  MainWindowRect: NSRect;
begin
  {  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init]; }
  pool := NSAutoreleasePool.Create;

  // Creates the application NSApp object
  NSApp := NSApplication.sharedApplication;

  // Creates a simple window
  MainWindowRect.origin.x := 300.0;
  MainWindowRect.origin.y := 300.0;
  MainWindowRect.size.width := 300.0;
  MainWindowRect.size.height := 500.0;

  MainWindow := NSWindow.initWithContentRect(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, NO);
    
  MainWindow.orderFrontRegardless;

  { Enters main message loop }

  NSApp.run;

  {  [pool release]; }
  pool.Free;
end.

