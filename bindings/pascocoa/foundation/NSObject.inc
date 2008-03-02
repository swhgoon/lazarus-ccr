{	NSObject.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSOBJECT_PAS_H}
{$define NSOBJECT_PAS_H}

//#import <Foundation/NSObjCRuntime.h>
//#import <Foundation/NSZone.h>

//@class NSInvocation, NSMethodSignature, NSCoder, NSString, NSEnumerator;
//@class Protocol;

{***************	Basic protocols		***************}

{@protocol NSObject

- (BOOL)isEqual:(id)object;
- (unsigned)hash;

- (Class)superclass;
- (Class)class;
- (id)self;
- (NSZone *)zone;

- (id)performSelector:(SEL)aSelector;
- (id)performSelector:(SEL)aSelector withObject:(id)object;
- (id)performSelector:(SEL)aSelector withObject:(id)object1 withObject:(id)object2;

- (BOOL)isProxy;

- (BOOL)isKindOfClass:(Class)aClass;
- (BOOL)isMemberOfClass:(Class)aClass;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;

- (BOOL)respondsToSelector:(SEL)aSelector;

- (id)retain;
- (oneway void)release;
- (id)autorelease;
- (unsigned)retainCount;

- (NSString *)description;

@end

@protocol NSCopying

- (id)copyWithZone:(NSZone *)zone;

@end

@protocol NSMutableCopying

- (id)mutableCopyWithZone:(NSZone *)zone;

@end

@protocol NSCoding

- (void)encodeWithCoder:(NSCoder *)aCoder;
- (id)initWithCoder:(NSCoder *)aDecoder;

@end     }

{***********	Base class		***********}

const
  Str_alloc = 'alloc';
  Str_init = 'init';
  Str_release = 'release';

{***********	Object Allocation / Deallocation		*******}

{FOUNDATION_EXPORT id <NSObject> NSAllocateObject(Class aClass, unsigned extraBytes, NSZone *zone);

FOUNDATION_EXPORT void NSDeallocateObject(id <NSObject>object);

FOUNDATION_EXPORT id <NSObject> NSCopyObject(id <NSObject>object, unsigned extraBytes, NSZone *zone);

FOUNDATION_EXPORT BOOL NSShouldRetainWithZone(id <NSObject> anObject, NSZone *requestedZone);

FOUNDATION_EXPORT void NSIncrementExtraRefCount(id object);

FOUNDATION_EXPORT BOOL NSDecrementExtraRefCountWasZero(id object);

FOUNDATION_EXPORT unsigned NSExtraRefCount(id object);}

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSOBJECT_PAS_C}
{$define NSOBJECT_PAS_C}

  NSObject = class
  public
    { class id }
    ClassId: objc.id;
    { object references }
    allocbuf, Handle: objc.id;
    { Constructor / Destructor }
    constructor Create; virtual;
    constructor CreateWithHandle(aHandle: objc.id);
    destructor Destroy; override;

{+ (void)load;

+ (void)initialize;
- (id)init;

+ (id)new;
+ (id)allocWithZone:(NSZone *)zone;
+ (id)alloc;
- (void)dealloc;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
- (void)finalize;
#endif

- (id)copy;
- (id)mutableCopy;

+ (id)copyWithZone:(NSZone *)zone;
+ (id)mutableCopyWithZone:(NSZone *)zone;

+ (Class)superclass;
+ (Class)class;
+ (void)poseAsClass:(Class)aClass;
+ (BOOL)instancesRespondToSelector:(SEL)aSelector;
+ (BOOL)conformsToProtocol:(Protocol *)protocol;
- (IMP)methodForSelector:(SEL)aSelector;
+ (IMP)instanceMethodForSelector:(SEL)aSelector;
+ (int)version;
+ (void)setVersion:(int)aVersion;
- (void)doesNotRecognizeSelector:(SEL)aSelector;
- (void)forwardInvocation:(NSInvocation *)anInvocation;
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector;

+ (NSMethodSignature *)instanceMethodSignatureForSelector:(SEL)aSelector;

#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED
+ (BOOL)isSubclassOfClass:(Class)aClass;
#endif

+ (NSString *)description;

- (Class)classForCoder;
- (id)replacementObjectForCoder:(NSCoder *)aCoder;
- (id)awakeAfterUsingCoder:(NSCoder *)aDecoder;}

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

constructor NSObject.Create;
begin

end;

constructor NSObject.CreateWithHandle(aHandle: objc.id);
begin
  Handle := aHandle;
  
  inherited Create;
end;

destructor NSObject.Destroy;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_release)), []);
end;

{$endif}

