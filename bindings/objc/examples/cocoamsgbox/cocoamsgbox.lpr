{
 cocoamsgbox.pas

 This example shows how to use the objective-c runtime headers to call
 initialization and finalization code for an objective-c class (in this case
 NSAutoreleasePool), and also shows a message box using minimal AppKit
 bindings to demonstrate that this can be used to build Cocoa applications.
 
 Compilation of this example requires the following options:
 -k-framework -kcocoa -k-lobjc

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
program cocoamsgbox;

{$mode objfpc}{$H+}

uses
  objc, ctypes, FPCMacOSAll;

{ Very limited appkit bindings, just to run this example independently
  of the Cocoa bindings }

{ From AppKit/NSApplication.inc }
function NSApplicationLoad(): CBOOL; cdecl; external;

{ from AppKit/NSPanel.inc }
function NSRunAlertPanel(title, msg, defaultButton, alternateButton, otherButton: CFStringRef; others: array of const): cint; cdecl; external;

const
  Str_NSAutoreleasePool = 'NSAutoreleasePool';
  Str_alloc = 'alloc';
  Str_init = 'init';
  Str_release = 'release';
  Str_Panel_Title = 'This is the title';
  Str_Panel_Message = 'This is the message';
var
  { classes }
  NSAutoreleasePoolId: objc.id;
  { objects }
  allocbuf, pool: objc.id;
  { strings }
  CFTitle, CFMessage: CFStringRef;
begin
  {  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init]; }
  NSAutoreleasePoolId := objc_getClass(PChar(Str_NSAutoreleasePool));
  allocbuf := objc_msgSend(NSAutoreleasePoolId, sel_registerName(PChar(Str_alloc)), []);
  pool := objc_msgSend(allocbuf, sel_registerName(PChar(Str_init)), []);

  NSApplicationLoad();

  CFTitle := CFStringCreateWithCString(nil, PChar(Str_Panel_Title), kCFStringEncodingUTF8);
  CFMessage := CFStringCreateWithCString(nil, PChar(Str_Panel_Message), kCFStringEncodingUTF8);

  { uses a default "OK" button and no alternate buttons }
  NSRunAlertPanel(CFTitle, CFMessage, nil, nil, nil, []);

  CFRelease(CFTitle);
  CFRelease(CFMessage);

  {  [pool release]; }
  objc_msgSend(pool, sel_registerName(PChar(Str_release)), []);
end.

