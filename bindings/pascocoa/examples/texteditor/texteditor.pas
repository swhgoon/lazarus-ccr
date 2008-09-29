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
  controller, model, view,
  cocoa_pkg, mytoolbar;

var
  pool: NSAutoreleasePool;
begin
  { Avoids arithmetic exceptions in Objective-C code }

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool.Create;

  { Creates the application NSApp object }
  NSApp := NSApplication.sharedApplication;

  { Initializes the controller, view and model objects }

  myController := TMyController.Create();
  myModel := TMyModel.Create();
  myView := TMyView.Create();

  { Creates the user interface }

  myView.CreateUserInterface();

  myView.MainWindow.orderFrontRegardless;

  { Enters main message loop }

  NSApp.setDelegate(myController.Handle);

  NSApp.run;

  { Releases all objects }

  myController.Free;
  myModel.Free;
  myView.Free;

  {  Releases the AutoreleasePool }
  pool.Free;
end.

