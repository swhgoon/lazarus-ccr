type

  nsIBinaryOutputStream = interface(nsIOutputStream)
  ['{204ee610-8765-11d3-90cf-0040056a906e}']
    procedure setOutputStream(aOutputStream: nsIOutputStream); safecall;
    procedure writeBoolean(aBoolean: longbool); safecall;
    procedure write8(aByte: PRUint8); safecall;
    procedure write16(a16: PRUint16); safecall;
    procedure write32(a32: PRUint32); safecall;
    procedure write64(a64: PRUint64); safecall;
    procedure writeFloat(aFloat: idlfloat); safecall;
    procedure writeDouble(aDouble: double); safecall;
    procedure writeStringZ(aString: PAnsiChar); safecall;
    procedure writeWStringZ(aString: PWideChar); safecall;
    procedure writeUtf8Z(aString: PWideChar); safecall;
    procedure writeBytes(aString: PAnsiChar; aLength: PRUint32); safecall;
    procedure writeByteArray(aBytes: PRUint8; aLength: PRUint32); safecall;

  end;

