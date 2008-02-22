{	NSAutoreleasePool.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSAUTORELEASEPOOL_PAS_H}
{$define NSAUTORELEASEPOOL_PAS_H}

{$include NSObject.inc}

const
  Str_NSAutoreleasePool = 'NSAutoreleasePool';

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSAUTORELEASEPOOL_PAS_C}
{$define NSAUTORELEASEPOOL_PAS_C}

{$include NSObject.inc}

  NSAutoreleasePool = class(NSObject)
  public
    constructor Create; override;

//+ (void)addObject:(id)anObject;

//- (void)addObject:(id)anObject;

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
//- (void)drain;
//#endif

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

constructor NSAutoreleasePool.Create;
begin
  {  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init]; }
  ClassId := objc_getClass(PChar(Str_NSAutoreleasePool));
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  Handle := objc_msgSend(allocbuf, sel_registerName(PChar(Str_init)), []);
end;

{$endif}

