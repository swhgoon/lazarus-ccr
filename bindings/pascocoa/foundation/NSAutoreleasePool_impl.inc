
constructor NSAutoreleasePool.Create;
begin
  {  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init]; }
  ClassId := objc_getClass(PChar(Str_NSAutoreleasePool));
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  Handle := objc_msgSend(allocbuf, sel_registerName(PChar(Str_init)), []);
end;

