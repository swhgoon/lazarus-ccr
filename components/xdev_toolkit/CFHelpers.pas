unit CFHelpers;

{
  Unit of handy routines for use with Core Foundation.

  CFStrToAnsiStr was adapted from the Lazarus CarbonProc unit's
   CFStringToStr function.
  License: Modified LGPL.
  
  Note that objects returned by functions with "Create" or "Copy"
   in the function name need to be released by the calling code.
   For example, CFStringCreateWithCString is called in AnsiStrToCFStr,
   meaning this applies to code that calls AnsiStrToCFStr as well.
  FreeCFRef and FreeAndNilCFRef are convenience routines provided 
   for that purpose.
  See Apple docs for more information on the so-called Create Rule 
   and Get Rule: 
  https://developer.apple.com/library/mac/#documentation/CoreFoundation/
          Conceptual/CFMemoryMgmt/Concepts/Ownership.html
}

{$MODE Delphi}

interface

uses
  MacOSAll;
  
function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);

procedure FreeCFRef(var cfRef: CFTypeRef);

procedure FreeAndNilCFRef(var cfRef : CFTypeRef);


implementation

function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
 {Convert CFString to AnsiString.
  If encoding is not specified, use CP1252 by default.}
var
  StrPtr   : Pointer;
  StrRange : CFRange;
  StrSize  : CFIndex;
begin
  if cfStr = nil then
    begin
    Result := '';
    Exit;
    end;

   {First try the optimized function}
  StrPtr := CFStringGetCStringPtr(cfStr, encoding);
  if StrPtr <> nil then  {Succeeded?}
    Result := PChar(StrPtr)
  else  {Use slower approach - see comments in CFString.pas}
    begin
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(cfStr);

     {Determine how long resulting string will be}
    CFStringGetBytes(cfStr, StrRange, encoding, Ord('?'),
                     False, nil, 0, StrSize);
    SetLength(Result, StrSize);  {Expand string to needed length}

    if StrSize > 0 then  {Convert string?}
      CFStringGetBytes(cfStr, StrRange, encoding, Ord('?'),
                       False, @Result[1], StrSize, StrSize);
    end;
end;  {CFStrToAnsiStr}


procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
 {Create CFString from AnsiString.
  If encoding is not specified, use CP1252 by default.
  Note: Calling code is responsible for calling CFRelease on 
   returned CFString. Presumably that's the reason why CarbonProc 
   unit's CreateCFString is a procedure, so you don't use it in 
   an expression and leave the CFString dangling.}
begin
  cfStr := CFStringCreateWithCString(nil, Pointer(PChar(aStr)), encoding);
end;


procedure FreeCFRef(var cfRef : CFTypeRef);
 {Convenience routine to free a CF reference so you don't have
   to check if it's nil.}
begin
  if Assigned(cfRef) then
    CFRelease(cfRef);
end;


procedure FreeAndNilCFRef(var cfRef : CFTypeRef);
 {Convenience routine to free a CF reference and set it to nil.}
begin
  FreeCFRef(cfRef);
  cfRef := nil;
end;


end.
