type
  nsReadSegmentFun = function (aOutStream: nsIOutputStream;
    aClosure: Pointer;
    aToSegment: Pointer;
    aFromOffset: PRUint32;
    aCount: PRUint32;
    out aReadCount: PRUint32): nsresult; extdecl;

type

  nsIOutputStream = interface(nsISupports)
  ['{0d0acd2a-61b4-11d4-9877-00c04fa0cf4a}']
    procedure close(); safecall;
    procedure flush(); safecall;
    function write(aBuf: PAnsiChar; aCount: idlulong) : idlulong; safecall;
    function writeFrom(aFromStream: nsIInputStream; aCount: idlulong) : idlulong; safecall;
    function writeSegments(aReader: nsReadSegmentFun; aClosure: voidPtr; aCount: idlulong) : idlulong; safecall;
    function isNonBlocking() : longbool; safecall;

  end;

