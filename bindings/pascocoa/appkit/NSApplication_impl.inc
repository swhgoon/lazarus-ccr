
constructor NSApplication.sharedApplication;
begin
  ClassId := objc_getClass(PChar(Str_NSApplication));
  Handle := objc_msgSend(ClassId, sel_registerName(PChar(Str_sharedApplication)), []);
end;

procedure NSApplication.run;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_run)), []);
end;

function NSApplication.runModalForWindow(theWindow: NSWindow): cint;
begin
  Result := cint(objc_msgSend(Handle, sel_registerName(PChar(Str_runModalForWindow)), [theWindow.Handle]));
end;

{@interface NSApplication(NSStandardAboutPanel)}

procedure NSApplication.orderFrontStandardAboutPanelWithOptions(optionsDictionary: Pointer);
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_orderFrontStandardAboutPanelWithOptions)), [optionsDictionary]);
end;

{@end}

