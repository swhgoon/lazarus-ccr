unit PrefsUtil;

{

  Class for working with application preferences.

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
  TCFPreferences = class(TObject)
  private
    function GetAppValue(const KeyName : string) : CFPropertyListRef;
  public
    destructor Destroy; override;
    function AppHasKey(const KeyName : string) : Boolean;
    function GetAppString(const KeyName : string) : string;
    function GetAppStringDef(const KeyName : string;
                             const Default : string) : string;
    procedure SetAppString(const KeyName : string;
                           const Value   : string);
    function GetAppBoolean(const KeyName : string) : Boolean;
    function GetAppBooleanDef(const KeyName : string;
                                    Default : Boolean) : Boolean;
    procedure SetAppBoolean(const KeyName : string;
                                  Value   : Boolean);
    procedure DeleteAppKey(const KeyName : string);
    end;


implementation

destructor TCFPreferences.Destroy;
 {Write any changes to preferences file.}
begin
  CFPreferencesAppSynchronize(kCFPreferencesCurrentApplication);
  inherited Destroy;
end;


function TCFPreferences.GetAppValue(const KeyName : string) : CFPropertyListRef;
 {Get key's value for preference domain "Current User, Current 
   Application, Any Host."}
var
  KeyRef : CFStringRef;
begin
  AnsiStrToCFStr(KeyName, KeyRef);
  try
    Result := 
     CFPreferencesCopyAppValue(KeyRef, kCFPreferencesCurrentApplication);
  finally
    FreeCFRef(KeyRef);
    end;
end;


function TCFPreferences.AppHasKey(const KeyName : string) : Boolean;
 {Return True if key exists in preference domain "Current User, Current 
   Application, Any Host."}
var
  ValueRef : CFPropertyListRef;
begin
  Result := False;
  try
    ValueRef := GetAppValue(KeyName);
    if Assigned(ValueRef) then
      Result := True;
  finally
    FreeCFRef(ValueRef);
    end;
end;


function TCFPreferences.GetAppString(const KeyName : string) : string;
 {Get key's string value for preference domain "Current User, Current 
   Application, Any Host."
  If key does not exist, returns blank string.}
var
  ValueRef : CFPropertyListRef;
begin
  Result := '';
  try
    ValueRef := GetAppValue(KeyName);
    if Assigned(ValueRef) and 
       (CFGetTypeID(ValueRef) = CFStringGetTypeID) then  {Value is a string?}
      Result := CFStrToAnsiStr(ValueRef);
  finally
    FreeCFRef(ValueRef);
    end;
end;


function TCFPreferences.GetAppStringDef(const KeyName : string;
                                        const Default : string) : string;
 {Get key's string value for preference domain "Current User, Current 
   Application, Any Host."
  If key does not exist, returns Default.}
begin
  if AppHasKey(KeyName) then
    Result := GetAppString(KeyName)
  else
    Result := Default;
end;


procedure TCFPreferences.SetAppString(const KeyName : string;
                                      const Value   : string);
 {Set key's string value in preference domain "Current User, Current 
   Application, Any Host."}
var
  KeyRef   : CFStringRef;
  ValueRef : CFPropertyListRef;
begin
  AnsiStrToCFStr(KeyName, KeyRef);
  AnsiStrToCFStr(Value, ValueRef);
  try
    CFPreferencesSetAppValue(KeyRef, ValueRef, kCFPreferencesCurrentApplication);
  finally
    FreeCFRef(KeyRef);
    FreeCFRef(ValueRef);
    end;  
end;


function TCFPreferences.GetAppBoolean(const KeyName : string) : Boolean;
 {Get key's Boolean value for preference domain "Current User, Current 
   Application, Any Host."
  If key does not exist, returns False.}
var
  ValueRef : CFPropertyListRef;
begin
  Result := False;
  try
    ValueRef := GetAppValue(KeyName);
    if Assigned(ValueRef) and
       (CFGetTypeID(ValueRef) = CFBooleanGetTypeID) then  {Value is a Boolean?}
      Result := CFBooleanGetValue(ValueRef);
  finally
    FreeCFRef(ValueRef);
    end;
end;


function TCFPreferences.GetAppBooleanDef(const KeyName : string;
                                               Default : Boolean) : Boolean;
 {Get key's Boolean value for preference domain "Current User, Current 
   Application, Any Host."
  If key does not exist, returns Default.}
begin
  if AppHasKey(KeyName) then
    Result := GetAppBoolean(KeyName)
  else
    Result := Default;
end;


procedure TCFPreferences.SetAppBoolean(const KeyName : string;
                                             Value   : Boolean);
 {Set key's Boolean value in preference domain "Current User, Current 
   Application, Any Host."}
var
  KeyRef   : CFStringRef;
  ValueRef : CFBooleanRef;
begin
  AnsiStrToCFStr(KeyName, KeyRef);
  if Value then
    ValueRef := kCFBooleanTrue
  else
    ValueRef := kCFBooleanFalse;
  try
    CFPreferencesSetAppValue(KeyRef, ValueRef, kCFPreferencesCurrentApplication);
  finally
    FreeCFRef(KeyRef);
    end;  
end;


procedure TCFPreferences.DeleteAppKey(const KeyName : string);
 {Delete key from preference domain "Current User, Current 
   Application, Any Host."}
var
  KeyRef : CFStringRef;
begin
  AnsiStrToCFStr(KeyName, KeyRef);
  try
    CFPreferencesSetAppValue(KeyRef, nil, kCFPreferencesCurrentApplication);
  finally
    FreeCFRef(KeyRef);
    end;  
end;


end.
