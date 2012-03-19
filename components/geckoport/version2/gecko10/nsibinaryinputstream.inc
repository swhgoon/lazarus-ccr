type

  nsIBinaryInputStream = interface(nsIInputStream)
  ['{7b456cb0-8772-11d3-90cf-0040056a906e}']
    procedure setInputStream(aInputStream: nsIInputStream); safecall;
    function readBoolean() : longbool; safecall;
    function read8() : PRUint8; safecall;
    function read16() : PRUint16; safecall;
    function read32() : PRUint32; safecall;
    function read64() : PRUint64; safecall;
    function readFloat() : idlfloat; safecall;
    function readDouble() : double; safecall;
    procedure readCString( result_: ACString); safecall;
    function readString() : AString; safecall;
    procedure readBytes(aLength: PRUint32; out aString: PAnsiChar); safecall;
    procedure readByteArray(aLength: PRUint32; out aBytes: PRUint8); safecall;

  end;

