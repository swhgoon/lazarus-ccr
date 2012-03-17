unit nsTypes;

{$mode objfpc}{$H+}

interface

uses
  nsGeckoStrings;


type

  // idl-types
  idllong   = longint;
  idlulong  = longword;
  idlshort  = smallint;
  idlushort = word;
  idlfloat  = single;


  PRUint8 = Byte;
  PRUint16 = Word;
  PRUint32 = Longword;
  PRUint64 = Int64;
  PRTime = Int64;
  PRInt8 = Shortint;
  PRInt16 = Smallint;
  PRInt32 = Longint;
  PRInt64 = Int64;

  nsIIDRef = PGuid;
  nsCIDRef = PGUID;
  nsCIDPtr = PGUID;
  nsIID = TGuid;
  nsIDRef = PGuid;
  nsID = TGuid;
  nsQIResult = interface end;
  DOMString = nsAString;
  AUTF8String = nsACString;
  ACString = nsACString;
  jsval = pointer;
  voidPtr = pointer;
  DOMTimeStamp = PRUint64;
  charPtr = PAnsiChar;

  nsLoadFlags = idlulong;

  nsresult = idlulong;
  nsrefcnt = idlulong;

  nsCoord = idllong;
  nsRect = record
    left, top, right, bottom: nsCoord;
  end;

  AString = WideString; //?
  nsIIDPtr = PGuid; //?

  nsDocShellEditorDataPtr = pointer;

  PRFileDescStar = type Pointer;
  PRLibraryStar = type Pointer;
  PFile = type Pointer;

  GREVersionRange = record
    lower: PAnsiChar;
    lowerInclusive: longbool;
    upper: PAnsiChar;
    upperInclusive: longbool;
  end;
  PGREVersionRangeArray = ^GREVersionRangeArray;
  GREVersionRangeArray = array [0..SizeOf(GREVersionRange)] of GREVersionRange;

  GREProperty = record
    property_: PAnsiChar;
    value: PAnsiChar;
  end;
  PGREPropertyArray = ^GREPropertyArray;
  GREPropertyArray = array[0..SizeOf(GREProperty)] of GREProperty;

  PRUint8Array = ^PRUint8;

  size_t=SizeUInt;


implementation

end.

