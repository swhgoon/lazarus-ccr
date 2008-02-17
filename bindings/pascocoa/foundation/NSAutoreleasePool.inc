{	NSAutoreleasePool.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
}

//#import <Foundation/NSObject.h>

const
  Str_NSAutoreleasePool = 'NSAutoreleasePool';

type
  NSAutoreleasePool = class(NSObject)
  public
    constructor Create; override;

//+ (void)addObject:(id)anObject;

//- (void)addObject:(id)anObject;

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
//- (void)drain;
//#endif

  end;

