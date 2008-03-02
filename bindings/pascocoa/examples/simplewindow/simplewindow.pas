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
  Str_Window_Title = 'This is the title';
  Str_Window_Message = 'This is the message';
var
  { classes }
  pool: NSAutoreleasePool;
  MainWindow: NSWindow;
  MainWindowView: NSView;
  TextField: NSTextField;
  { strings }
  CFTitle, CFMessage: CFStringRef;
  { sizes }
  MainWindowRect, TextFieldRect: NSRect;
begin
  {  Creates a AutoreleasePool for this thread. Every thread must have one }
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

  { Initializes the title of the window }

  CFTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);

  { Adds a NSTextField with a string }
  TextFieldRect.origin.x := 0.0;
  TextFieldRect.origin.y := 0.0;
  TextFieldRect.size.width := 300.0;
  TextFieldRect.size.height := 500.0;
  TextField := NSTextField.initWithFrame(TextFieldRect);
  TextField.setStringValue(CFTitle);
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);
  MainWindowView.addSubview(TextField);

  { Put's the window on the front z-order }

  MainWindow.orderFrontRegardless;

  { Enters main message loop }

  NSApp.run;

  { Releases the AutoreleasePool for this thread }
  pool.Free;
end.

