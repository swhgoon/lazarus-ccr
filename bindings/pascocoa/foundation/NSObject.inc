{	NSObject.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSOBJECT_PAS_H}
{$define NSOBJECT_PAS_H}

//#import <Foundation/NSObjCRuntime.h>
//#import <Foundation/NSZone.h>

{ Class and method name strings }
const
  {***********	Base class		***********}

  Str_NSObject = 'NSObject';

  Str_alloc = 'alloc';
  Str_init = 'init';

  {***************	Basic protocols		***************}

  Str_retain = 'retain';
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

{***********	Base class		***********}

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
    { Extra binding functions }
    function getClass: objc.id; virtual;
    { Class creation methods }
    procedure AddMethod(aName, aParameters: string; aPointer: Pointer);
  public
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

- (BOOL)respondsToSelector:(SEL)aSelector;}

    function retain: objc.id;
    procedure release;
{- (id)autorelease;
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

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

{***********	Base class		***********}

constructor NSObject.Create;
begin
  ClassId := getClass();
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  Handle := objc_msgSend(allocbuf, sel_registerName(PChar(Str_init)), []);
end;

constructor NSObject.CreateWithHandle(aHandle: objc.id);
begin
  ClassId := getClass();
  Handle := aHandle;
end;

destructor NSObject.Destroy;
begin
  release;
end;

function NSObject.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSObject);
end;

procedure NSObject.AddMethod(aName, aParameters: string; aPointer: Pointer);
var
  method_list: Pobjc_method_list;
begin
  method_list := GetMem(SizeOf(objc_method_list)); { We can't free this until the last instance is freed }

  method_list^.method_count := 1;
  method_list^.method_list[0].method_name := sel_registerName(PChar(aName));
  method_list^.method_list[0].method_types := PChar(aParameters);
  method_list^.method_list[0].method_imp := IMP(aPointer);
  class_addMethods(ClassId, method_list);
end;

{***************	Basic protocols		***************}

function NSObject.retain: objc.id;
begin
  Result := objc_msgSend(Handle, sel_registerName(PChar(Str_retain)), []);
end;

procedure NSObject.release;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_release)), []);
end;

{$endif}

