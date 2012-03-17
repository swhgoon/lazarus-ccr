type
  nsWriteSegmentFun = function (aInStream: nsIInputStream;
                                aClosure: Pointer;
                                const aFromSegment: Pointer;
                                aToOffset: PRUint32;
                                aCount: PRUint32;
                                out aWriteCount: PRUint32): nsresult; extdecl;

type

  nsIInputStream = interface(nsISupports)
  ['{fa9c7f6c-61b3-11d4-9877-00c04fa0cf4a}']
    procedure close(); safecall;
    function available() : idlulong; safecall;
    function read(aBuf: charPtr; aCount: idlulong) : idlulong; safecall;
    function readSegments(aWriter: nsWriteSegmentFun; aClosure: voidPtr; aCount: idlulong) : idlulong; safecall;
    function isNonBlocking() : longbool; safecall;

  end;

