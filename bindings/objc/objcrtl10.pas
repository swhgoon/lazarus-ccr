{
 objcrtl10.pas

 Copyright (C) 2009 Dmitry Boyarintsev

 This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 1.0
 headers included with XCode 3.1.2
 The original copyright note of is kept on each include file
}

unit objcrtl10;

{$mode objfpc}{$H+}

interface

//todo: *10 WRAPPERS!

{
  Mac OS X Version 10.5 Delta
  ---------------------------

  The low-level Objective-C runtime API is significantly updated
 in Mac OS X version 10.5. Many functions and all existing data structures
 are replaced with new functions. This document describes the differences
 between the 10.5 version and previous versions.

  http://developer.apple.com/documentation/Cocoa/Reference/ObjCRuntimeRef/Articles/ocr10_5delta.html#//apple_ref/doc/uid/TP40002981-TPXREF101
}


uses
  objcrtl, dynlibs;

function InitializeObjCRtl10(const ObjCLibName: AnsiString): Boolean;

implementation

function object_getClass10(obj:id): _Class; cdecl;
begin
  Result := nil;
end;

function object_setClass10(obj:id; cls: _Class):_Class; cdecl;
begin
  Result := nil;
end;

function object_getIvar10(obj:id; ivar:Ivar):id; cdecl;
begin
  Result := 0;
end;

procedure object_setIvar10(obj:id; ivar:Ivar; value:id); cdecl;
begin
end;

function class_getName10(cls:_Class):PChar; cdecl;
begin
  Result := nil;
end;

function class_getSuperclass10(cls:_Class):_Class; cdecl;
begin
  Result := nil;
end;

function class_isMetaClass10(cls:_Class):BOOL; cdecl;
begin
  Result := false;
end;

function class_copyMethodList10(cls:_Class; outCount:pdword):PMethod; cdecl;
begin
  Result := nil;
end;

function class_getMethodImplementation10(cls:_Class; name:SEL):IMP; cdecl;
begin
  Result := nil;
end;

function class_respondsToSelector10(cls:_Class; sel:SEL):BOOL; cdecl;
begin
  Result := false;
end;

function class_conformsToProtocol10(cls:_Class; var protocol: Protocol):BOOL; cdecl;
begin
  Result := false;
end;

function class_copyProtocolList10(cls:_Class; var outCount: dword):PArrayPProtocol; cdecl;
begin
  Result := nil;
end;

function class_copyIvarList10(cls:_Class; outCount:pdword):PIvar; cdecl;
begin
  Result := nil;
end;

function class_getMethodImplementation_stret10(cls:_Class; name:SEL):IMP; cdecl;
begin
  Result := nil;
end;


function objc_allocateClassPair10(superclass:_Class; name:pchar; extraBytes:size_t):_Class; cdecl;
begin
  Result := nil;
end;

procedure objc_registerClassPair10(cls:_Class); cdecl;
begin

end;

function objc_duplicateClass10(original:_Class; name:pchar; extraBytes:size_t):_Class; cdecl;
begin
  Result := nil;
end;

procedure objc_disposeClassPair10(cls:_Class); cdecl;
begin

end;

function class_addMethod10(cls:_Class; name:SEL; imp:IMP; types:pchar):BOOL; cdecl;
begin
  Result := false;
end;

function class_addIvar10(cls:_Class; name:pchar; size:size_t; alignment:uint8_t; types:pchar):BOOL; cdecl;
begin
  Result := false;
end;

function class_addProtocol10(cls:_Class; protocol:pProtocol):BOOL; cdecl;
begin
  Result := false;
end;


function method_getName10(m:Method):SEL; cdecl;
begin
  Result := nil;
end;

function method_getImplementation10(m:Method):IMP; cdecl;
begin
  Result := nil;
end;

function method_getTypeEncoding10(m:Method):Pchar; cdecl;
begin
  Result := nil;
end;

function method_copyReturnType10(m:Method):Pchar; cdecl;
begin
  Result := nil;
end;

function method_copyArgumentType10(m:Method; index:dword):Pchar; cdecl;
begin
  Result := nil;
end;

function method_setImplementation10(m:Method; imp:IMP):IMP; cdecl;
begin
  Result := nil;
end;

function sel_getName10(sel: SEL ): PChar; cdecl;
begin
  Result := nil;
end;

function sel_registerName10(str: PChar): SEL; cdecl;
begin
  Result := nil;
end;

function sel_getUid10(const str: PChar): SEL; cdecl;
begin
  Result := nil;
end;

function ivar_getName10(v:Ivar):Pchar; cdecl;
begin
  Result := nil;
end;

function ivar_getTypeEncoding10(v:Ivar):Pchar; cdecl;
begin
  Result := nil;
end;

function ivar_getOffset10(v:Ivar):ptrdiff_t; cdecl;
begin
  Result := nil;
end;

function sel_isEqual10(lhs:SEL; rhs:SEL):BOOL; cdecl;
begin
  Result := false;
end;

function objc_getProtocol10(name:pchar): PProtocol; cdecl;
begin
  Result := nil;
end;

function objc_copyProtocolList10(outCount:pdword):PArrayPProtocol; cdecl;
begin
  Result := nil;
end;

function InitializeObjCRtl10(const ObjCLibName: AnsiString): Boolean;
var
  hnd : TLibHandle;
begin
  hnd := LoadLibrary(ObjCLibName);
  Result := hnd <> 0;
  if not Result then Exit;

  //Exceptions - are unchanged:
  LoadDefaultObjCExepction(hnd);
  //Synchronization - unchanged:
  LoadDefaultObjCSync(hnd);

  // Instances

  // The following functions are unchanged:
  Pointer(object_dispose) := GetProcedureAddress(hnd, 'object_dispose');
  Pointer(object_getClassName) := GetProcedureAddress(hnd, 'object_getClassName');
  Pointer(object_getIndexedIvars) := GetProcedureAddress(hnd, 'object_getIndexedIvars');
  Pointer(object_setInstanceVariable) := GetProcedureAddress(hnd, 'object_setInstanceVariable');
  Pointer(object_getInstanceVariable) := GetProcedureAddress(hnd, 'object_getInstanceVariable');

  //The following function is modified:
  // needs wrapper?
  // object_copy (The nBytes parameter is changed from unsigned to size_t.)
  Pointer(object_copy)    := GetProcedureAddress(hnd, 'object_copy');

  //The following functions are added:
  object_getClass := @object_getClass10;
  object_setClass := @object_setClass10;
  object_getIvar := @object_getIvar10;
  object_setIvar := @object_setIVar10;

  // The following functions are deprecated:
  //object_copyFromZone: deprecated in favor of object_copy
  //object_realloc
  //object_reallocFromZone: no substitute
  //_alloc: no substitute
  //_copy: no substitute
  //_realloc: no substitute
  //_dealloc: no substitute
  //_zoneAlloc: no substitute
  //_zoneRealloc: no substitute
  //_zoneCopy: no substitute
  //_error: no substitute


  //Class Inspection

  //The following functions are unchanged:
  Pointer(objc_getClassList) := GetProcedureAddress(hnd, 'objc_getClassList');
  Pointer(objc_lookUpClass) := GetProcedureAddress(hnd, 'objc_lookUpClass');
  Pointer(objc_getClass)    := GetProcedureAddress(hnd, 'objc_getClass');
  Pointer(objc_getMetaClass) := GetProcedureAddress(hnd, 'objc_getMetaClass');
  Pointer(class_getVersion) := GetProcedureAddress(hnd, 'class_getVersion');
  Pointer(class_getInstanceVariable) := GetProcedureAddress(hnd, 'class_getInstanceVariable');
  Pointer(class_getInstanceMethod) := GetProcedureAddress(hnd, 'class_getInstanceMethod');
  Pointer(class_getClassMethod) := GetProcedureAddress(hnd, 'class_getClassMethod');

  // The following function is modified:
  // needs wrapper?
  // class_createInstance: idxIvars parameter Changed from unsigned to size_t
  Pointer(class_createInstance) := GetProcedureAddress(hnd, 'class_createInstance');

  // The following functions are added:
  class_getName:=@class_getName10;
  class_getSuperclass:=@class_getSuperclass10;
  class_isMetaClass:=@class_isMetaClass10;
  class_copyMethodList:=@class_copyMethodList10;
  class_getMethodImplementation:=@class_getMethodImplementation10;
  class_getMethodImplementation_stret:=@class_getMethodImplementation_stret10;
  class_respondsToSelector:=@class_respondsToSelector10;
  class_conformsToProtocol:=@class_conformsToProtocol10;
  class_copyProtocolList:=@class_copyProtocolList10;
  class_copyIvarList:=@class_copyIvarList10;

  //The following functions are deprecated:
  //objc_getClasses: deprecated in favor of objc_getClassList
  //class_createInstanceFromZone: deprecated in favor of class_createInstance
  //class_nextMethodList: deprecated in favor of new class_copyMethodList
  //class_lookupMethod: deprecated in favor of class_getMethodImplementation
  //class_respondsToMethod: deprecated in favor of class_respondsToSelector

  //The following function is used only by ZeroLink:
  //objc_getRequiredClass

  // Class Manipulation

  //The following function is unchanged:
  Pointer(class_setVersion) := GetProcedureAddress(hnd, 'class_setVersion');

  //The following functions are added:
  objc_allocateClassPair := @objc_allocateClassPair10;
  objc_registerClassPair := @objc_registerClassPair10;
  objc_duplicateClass := @objc_duplicateClass10;
  class_addMethod := @class_addMethod10;
  class_addIvar := @class_addIvar10;
  class_addProtocol := @class_addProtocol10;

  //The following functions are deprecated:
  //objc_addClass: deprecated in favor of objc_allocateClassPair and objc_registerClassPair
  //class_addMethods: deprecated in favor of new class_addMethod
  //class_removeMethods: deprecated with no substitute
  //class_poseAs: deprecated in favor of categories and method_setImplementation


  //Methods

  //The following function is unchanged:
  Pointer(method_getNumberOfArguments) := GetProcedureAddress(hnd, 'method_getNumberOfArguments');

  //The following functions are added:
  method_getName            := @method_getName10;
  method_getImplementation  := @method_getImplementation10;
  method_getTypeEncoding    := @method_getTypeEncoding10;
  method_copyReturnType     := @method_copyReturnType10;
  method_copyArgumentType   := @method_copyArgumentType10;
  method_setImplementation  := @method_setImplementation10;

  //The following functions are deprecated:
  //method_getArgumentInfo
  //method_getSizeOfArguments


  //Instance Variables

  //The following functions are added:
  ivar_getName := @ivar_getName10;
  ivar_getTypeEncoding := @ivar_getTypeEncoding10;
  ivar_getOffset := @ivar_getOffset10;

  //Selectors
  //The following functions are unchanged:
  Pointer(sel_getName) := GetProcedureAddress(hnd, 'sel_getName');
  Pointer(sel_registerName) := GetProcedureAddress(hnd, 'sel_registerName');
  Pointer(sel_getUid) := GetProcedureAddress(hnd, 'sel_getUid');

  //The following function is added:
  sel_isEqual := @sel_isEqual10;

  //The following function is deprecated:
  //sel_isMapped: deprecated with no substitute


  //Runtime
  //The following functions are deprecated favor of dyld:
  //objc_loadModules
  //objc_loadModule
  //objc_unloadModules

  //The following functions are deprecated:
  //objc_setClassHandler: deprecated with no substitute
  //objc_setMultithreaded: deprecated with no substitute

  //The following previously undocumented functions are deprecated with no substitute:
  //objc_getOrigClass, _objc_create_zone, _objc_error, _objc_flush_caches,
  //_objc_resolve_categories_for_class, _objc_setClassLoader,_ objc_setNilReceiver,
  //_objc_getNilReceiver,_ objcInit

  //The following undocumented functions are unchanged:
  //_objc_getFreedObjectClass, instrumentObjcMessageSends, _objc_debug_class_hash
  //_class_printDuplicateCacheEntries, _class_printMethodCaches, _class_printMethodCacheStatistics


  //Messaging
  //The following functions are unchanged:
  Pointer(objc_msgSend) := GetProcedureAddress(hnd, 'objc_msgSend');
  Pointer(objc_msgSend_stret) := GetProcedureAddress(hnd, 'objc_msgSend_stret');
  Pointer(objc_msgSendSuper) := GetProcedureAddress(hnd, 'objc_msgSendSuper');
  Pointer(objc_msgSendSuper_stret) := GetProcedureAddress(hnd, 'objc_msgSendSuper_stret');
  //todo:
  Pointer(objc_msgSend_fpret) := GetProcedureAddress(hnd, 'objc_msgSend_fpret');

  //The following functions are removed:objc_msgSendv	Given an argument list, send a message with a simple return value.
  //objc_msgSendv_stret	Given an argument list, send a message with a data-structure return value.
  //objc_msgSendv_fpret	Given an argument list, send a message with a floating point return value.

  //Protocols
  //The following functions are added:
  objc_getProtocol := @objc_getProtocol10;
  objc_copyProtocolList := @objc_copyProtocolList10;
end;

end.
