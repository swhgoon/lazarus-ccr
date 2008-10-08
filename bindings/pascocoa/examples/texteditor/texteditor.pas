{
 texteditor.pas

 This example shows how to create a simple text editor with PasCocoa

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
program texteditor;

{$mode delphi}

{$linkframework Cocoa}
{$linklib objc}

uses
  Math,
  objc, ctypes, MacOSAll, AppKit, Foundation,
  {$ifndef WITHOUT_PKG}cocoa_pkg,{$endif}
  controller, model, view, mytoolbar;

var
  pool: NSAutoreleasePool;
begin
  { Avoids arithmetic exceptions in Objective-C code }

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool.Create;

  { Creates the application NSApp object }
  NSApp := NSApplication.sharedApplication;

//  NSBundleNib.loadNibNamed_owner(CFStringCreateWithPascalString(nil, 'applemenu', kCFStringEncodingUTF8), NSApp.Handle);

  { Initializes the controller and the model. Must be before the view to
    attach the events (controller) and guarantee that resources are loaded (model) }

  myController := TMyController.Create();
  myModel := TMyModel.Create();

  { Initializes the view object }

  myView := TMyView.Create;
  myView.CreateUserInterface;

  { Enters main message loop }

  myView.MainWindow.orderFrontRegardless;

  NSApp.setDelegate(myController.Handle);

  NSApp.run;

  { Releases all objects }

  myController.Free;
  myModel.Free;
  myView.Free;

  {  Releases the AutoreleasePool }
  pool.Free;
end.

