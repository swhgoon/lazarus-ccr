{%mainunit appkit.pas}

constructor NSWindow.initWithContentRect(
  contentRect: NSRect;
  aStyle: cuint;
  bufferingType: NSBackingStoreType;
  defer: CBOOL);
type
  initWithContentRect_t = function (param1: objc.id; param2: SEL;
   param3, param4, param5, param6: cfloat;
   param7: cuint;
   param8: NSBackingStoreType; param9: BOOL): objc.id; cdecl;
var
  vinit: initWithContentRect_t;
begin
{    NSWindow* MainWindow = [[NSWindow alloc] initWithContentRect: MainWindowRect
      styleMask: NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask | NSResizableWindowMask
        backing: NSBackingStoreBuffered
          defer: NO]; }
  ClassId := objc_getClass(PChar(Str_NSWindow));
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  vinit := initWithContentRect_t(@objc_msgSend);
  Handle := vinit(allocbuf, sel_registerName(PChar(Str_initWithContentRect)),
   contentRect.origin.x, contentRect.origin.y, contentRect.size.width, contentRect.size.height,
   aStyle, bufferingType, defer);
end;

procedure NSWindow.orderFrontRegardless;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_orderFrontRegardless)), []);
end;

