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

  { Initializes the model object,
    must be after the view so that the resources are loaded }

  myModel := TMyModel.Create();

  { Initializes the view object }

  myView := TMyView.Create;
  myView.CreateUserInterface;

  { The controller needs to be the last to be creates for an unknown reason,
    and we can only attach the events after the controller is created }

  myController := TMyController.Create();

  myView.AttachEventHandlers();
  myToolbarController.AttachEventHandlers();

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

