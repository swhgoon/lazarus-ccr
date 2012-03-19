type

  nsIDataType = interface(nsISupports)
  ['{4d12e540-83d7-11d5-90ed-0010a4e73d9a}']

  end;

const
  nsIDataType_VTYPE_INT8=0;
  nsIDataType_VTYPE_INT16=1;
  nsIDataType_VTYPE_INT32=2;
  nsIDataType_VTYPE_INT64=3;
  nsIDataType_VTYPE_UINT8=4;
  nsIDataType_VTYPE_UINT16=5;
  nsIDataType_VTYPE_UINT32=6;
  nsIDataType_VTYPE_UINT64=7;
  nsIDataType_VTYPE_FLOAT=8;
  nsIDataType_VTYPE_DOUBLE=9;
  nsIDataType_VTYPE_BOOL=10;
  nsIDataType_VTYPE_CHAR=11;
  nsIDataType_VTYPE_WCHAR=12;
  nsIDataType_VTYPE_VOID=13;
  nsIDataType_VTYPE_ID=14;
  nsIDataType_VTYPE_DOMSTRING=15;
  nsIDataType_VTYPE_CHAR_STR=16;
  nsIDataType_VTYPE_WCHAR_STR=17;
  nsIDataType_VTYPE_INTERFACE=18;
  nsIDataType_VTYPE_INTERFACE_IS=19;
  nsIDataType_VTYPE_ARRAY=20;
  nsIDataType_VTYPE_STRING_SIZE_IS=21;
  nsIDataType_VTYPE_WSTRING_SIZE_IS=22;
  nsIDataType_VTYPE_UTF8STRING=23;
  nsIDataType_VTYPE_CSTRING=24;
  nsIDataType_VTYPE_ASTRING=25;
  nsIDataType_VTYPE_EMPTY_ARRAY=254;
  nsIDataType_VTYPE_EMPTY=255;

type
  nsIVariant = interface(nsISupports)
  ['{81e4c2de-acac-4ad6-901a-b5fb1b851a0d}']
    function GetdataType(): PRUint16; safecall;
    property dataType : PRUint16 read GetdataType;
    function getAsInt8() : PRUint8; safecall;
    function getAsInt16() : PRInt16; safecall;
    function getAsInt32() : PRInt32; safecall;
    function getAsInt64() : PRInt64; safecall;
    function getAsUint8() : PRUint8; safecall;
    function getAsUint16() : PRUint16; safecall;
    function getAsUint32() : PRUint32; safecall;
    function getAsUint64() : PRUint64; safecall;
    function getAsFloat() : idlfloat; safecall;
    function getAsDouble() : double; safecall;
    function getAsBool() : longbool; safecall;
    function getAsChar() : char; safecall;
    function getAsWChar() : wchar; safecall;
    function getAsID(out retval: nsID) : nsresult; safecall;
    function getAsAString() : AString; safecall;
    procedure getAsDOMString( result_: DOMString); safecall;
    procedure getAsACString( result_: ACString); safecall;
    procedure getAsAUTF8String( result_: AUTF8String); safecall;
    function getAsString() : PAnsiChar; safecall;
    function getAsWString() : PWideChar; safecall;
    function getAsISupports() : nsISupports; safecall;
    function getAsJSVal() : jsval; safecall;
    procedure getAsInterface(out iid: nsIIDPtr; out iface); safecall;
    function getAsArray(out atype: PRUint16; out iid: nsIID; out count: PRUint32; out ptr: voidPtr) : nsresult; safecall;
    procedure getAsStringWithSize(out size: PRUint32; out str: PAnsiChar); safecall;
    procedure getAsWStringWithSize(out size: PRUint32; out str: PWideChar); safecall;

  end;


  nsIWritableVariant = interface(nsIVariant)
  ['{5586a590-8c82-11d5-90f3-0010a4e73d9a}']
    function Getwritable(): longbool; safecall;
    procedure Setwritable(awritable: longbool); safecall;
    property writable : longbool read Getwritable write Setwritable;
    procedure setAsInt8(aValue: PRUint8); safecall;
    procedure setAsInt16(aValue: PRInt16); safecall;
    procedure setAsInt32(aValue: PRInt32); safecall;
    procedure setAsInt64(aValue: PRInt64); safecall;
    procedure setAsUint8(aValue: PRUint8); safecall;
    procedure setAsUint16(aValue: PRUint16); safecall;
    procedure setAsUint32(aValue: PRUint32); safecall;
    procedure setAsUint64(aValue: PRUint64); safecall;
    procedure setAsFloat(aValue: idlfloat); safecall;
    procedure setAsDouble(aValue: double); safecall;
    procedure setAsBool(aValue: longbool); safecall;
    procedure setAsChar(aValue: char); safecall;
    procedure setAsWChar(aValue: wchar); safecall;
    procedure setAsID(aValue: nsIDRef); safecall;
    procedure setAsAString(aValue: AString); safecall;
    procedure setAsDOMString(aValue: DOMString); safecall;
    procedure setAsACString(aValue: ACString); safecall;
    procedure setAsAUTF8String(aValue: AUTF8String); safecall;
    procedure setAsString(aValue: PAnsiChar); safecall;
    procedure setAsWString(aValue: PWideChar); safecall;
    procedure setAsISupports(aValue: nsISupports); safecall;
    procedure setAsInterface(constref iid: TGuid; iface: nsQIResult); safecall;
    procedure setAsArray(atype: PRUint16; iid: nsIIDPtr; count: PRUint32; ptr: voidPtr); safecall;
    procedure setAsStringWithSize(size: PRUint32; str: PAnsiChar); safecall;
    procedure setAsWStringWithSize(size: PRUint32; str: PWideChar); safecall;
    procedure setAsVoid(); safecall;
    procedure setAsEmpty(); safecall;
    procedure setAsEmptyArray(); safecall;
    procedure setFromVariant(aValue: nsIVariant); safecall;

  end;

