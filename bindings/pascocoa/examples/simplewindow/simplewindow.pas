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

{$ifdef fpc}{$mode delphi}{$endif}

uses
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  objc, ctypes, AppKit, Foundation, cocoa_pkg;

const
  Str_Window_Title = 'Pascal Cocoa Example';
  Str_Window_Message = 'NSTextField Control';
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
  MainWindowRect.size.height := 100.0;

  MainWindow := NSWindow.initWithContentRect_styleMask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, LongBool(NO));

  { Initializes the title of the window }

  CFTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);

  { Adds a NSTextField with a string }
  
  CFMessage := CFStringCreateWithPascalString(nil, Str_Window_Message, kCFStringEncodingUTF8);
  TextFieldRect.origin.x := 50.0;
  TextFieldRect.origin.y := 50.0;
  TextFieldRect.size.width := 200.0;
  TextFieldRect.size.height := 25.0;
  TextField := NSTextField.initWithFrame(TextFieldRect);
  TextField.setBordered(LongBool(NO));
  TextField.setDrawsBackground(LongBool(NO));
  TextField.setStringValue(CFMessage);
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);
  MainWindowView.addSubview(TextField.Handle);

  { Put's the window on the front z-order }

  MainWindow.orderFrontRegardless;

  { Enters main message loop }

  NSApp.run;

  { Releases the AutoreleasePool for this thread }
  pool.Free;
end.

