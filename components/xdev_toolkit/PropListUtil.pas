unit PropListUtil;

{

  Class for working with property list (for example, app bundle's
   Info.plist).

  Author:    Phil Hess.
  Copyright: Copyright (C) 2010 Phil Hess. All rights reserved.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.

}

{$MODE Delphi}

interface

uses
  MacOSAll,
  CFHelpers;  {Handy routines for use with Core Foundation}
  
type  {Note: Not all CF object types are supported yet by this class}
  TCFPropertyList = class(TObject)
  private
    propertyList : CFPropertyListRef;
    function GetValue(const KeyName : string) : UnivPtr;
  public
    destructor Destroy; override;
    function LoadFromFile(const FileName : string) : Boolean;
    function GetString(const KeyName : string) : string;
    function GetBoolean(const KeyName : string) : Boolean;
    end;

function GetInfoPlistString(const KeyName : string) : string;


implementation

destructor TCFPropertyList.Destroy;
begin
  FreeCFRef(propertyList);
end;


function TCFPropertyList.LoadFromFile(const FileName : string) : Boolean;
 {Adapted from example ObjC code given here: 
  http://developer.apple.com/library/mac/documentation/Cocoa/
   Conceptual/PropertyLists/SerializePlist/SerializePlist.html#//
   apple_ref/doc/uid/10000048i-CH7-SW5}
var
  plistFileName : CFStringRef;
  fileURL       : CFURLRef;
  resourceData  : CFDataRef;
  errorCode     : SInt32;
  errorString   : CFStringRef;
begin
  Result := False;
  
  FreeAndNilCFRef(propertyList);  {In case something previously loaded}

  AnsiStrToCFStr(FileName, plistFileName);

  fileURL := CFURLCreateWithFileSystemPath(
              kCFAllocatorDefault,
              plistFileName,
              kCFURLPOSIXPathStyle,  {Interpret as POSIX path}
              False);                {Not a directory}
   {Note that if file name is not absolute, treated relative
     to working directory.}

  FreeCFRef(plistFileName);

   {Read the XML file.
    Note getting resource data, not specified properties.}
  try
    if not CFURLCreateDataAndPropertiesFromResource(
            kCFAllocatorDefault,
            fileURL,
            @resourceData,  {Place to put XML file's data}
            nil,
            nil,
            errorCode) then
      Exit;
       {Description of function suggests resourceData might
         be non-null even if failure, so release below.}

     {Reconstitute the dictionary using the XML data.}
    propertyList := CFPropertyListCreateFromXMLData(
                     kCFAllocatorDefault,
                     resourceData,
                     kCFPropertyListImmutable,
                     @errorString);

    if Assigned(propertyList) then
      Result := True
    else
      FreeCFRef(errorString);  //return this too?
  finally
    FreeCFRef(fileURL);
    FreeCFRef(resourceData);  
    end;
end;  {TCFPropertyList.LoadFromFile}


function TCFPropertyList.GetValue(const KeyName : string) : UnivPtr;
 {Retrieve key's CF value from property list.}
var
  KeyRef : CFStringRef;
begin
  Result := nil;
  if not Assigned(propertyList) then  {Error - list not loaded?}
    Exit;
  if CFGetTypeID(propertyList) <> CFDictionaryGetTypeID then  {Not valid?}
    Exit;
  AnsiStrToCFStr(KeyName, KeyRef);
  Result := CFDictionaryGetValue(propertyList, KeyRef);
  FreeCFRef(KeyRef);
end;


function TCFPropertyList.GetString(const KeyName : string) : string;
 {Retrieve key's string value from property list.}
var
  Value : UnivPtr;
begin
  Result := '';
  Value := GetValue(KeyName);
  if not Assigned(Value) then  {Key not found?}
    Exit;
  if CFGetTypeID(Value) = CFStringGetTypeID then  {Value is a string?}
    Result := CFStrToAnsiStr(Value);
end;


function TCFPropertyList.GetBoolean(const KeyName : string) : Boolean;
 {Retrieve key's Boolean value from property list.}
var
  Value : UnivPtr;
begin
  Result := False;
  Value := GetValue(KeyName);
  if not Assigned(Value) then  {Key not found?}
    Exit;
  if CFGetTypeID(Value) = CFBooleanGetTypeID then  {Value is a Boolean?}
    Result := CFBooleanGetValue(Value);
end; 



function GetInfoPlistString(const KeyName : string) : string;
 {Retrieve key's string value from app bundle's Info.plist file.}
var
  BundleRef : CFBundleRef;
  KeyRef    : CFStringRef;
  ValueRef  : CFTypeRef;
begin
  Result := '';
  BundleRef := CFBundleGetMainBundle;
  if BundleRef = nil then  {Executable not in an app bundle?}
    Exit;
  AnsiStrToCFStr(KeyName, KeyRef);
  try
    ValueRef := CFBundleGetValueForInfoDictionaryKey(BundleRef, KeyRef);
    if CFGetTypeID(ValueRef) <> CFStringGetTypeID then  {Value not a string?}
      Exit;
    Result := CFStrToAnsiStr(ValueRef);
  finally
    FreeCFRef(KeyRef);
    end;
end;  {GetInfoPlistString}


end.
