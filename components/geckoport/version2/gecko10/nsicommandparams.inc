type

  nsICommandParams = interface(nsISupports)
  ['{83f892cf-7ed3-490e-967a-62640f3158e1}']
    function getValueType(name: PAnsiChar) : idlshort; safecall;
    function getBooleanValue(name: PAnsiChar) : longbool; safecall;
    function getLongValue(name: PAnsiChar) : idllong; safecall;
    function getDoubleValue(name: PAnsiChar) : double; safecall;
    function getStringValue(name: PAnsiChar) : AString; safecall;
    function getCStringValue(name: PAnsiChar) : PAnsiChar; safecall;
    function getISupportsValue(name: PAnsiChar) : nsISupports; safecall;
    procedure setBooleanValue(name: PAnsiChar; value: longbool); safecall;
    procedure setLongValue(name: PAnsiChar; value: idllong); safecall;
    procedure setDoubleValue(name: PAnsiChar; value: double); safecall;
    procedure setStringValue(name: PAnsiChar; value: AString); safecall;
    procedure setCStringValue(name: PAnsiChar; value: PAnsiChar); safecall;
    procedure setISupportsValue(name: PAnsiChar; value: nsISupports); safecall;
    procedure removeValue(name: PAnsiChar); safecall;
    function hasMoreElements() : longbool; safecall;
    procedure first(); safecall;
    function getNext() : PAnsiChar; safecall;

  end;

const
  nsICommandParams_eNoType=0;
  nsICommandParams_eBooleanType=1;
  nsICommandParams_eLongType=2;
  nsICommandParams_eDoubleType=3;
  nsICommandParams_eWStringType=4;
  nsICommandParams_eISupportsType=5;
  nsICommandParams_eStringType=6;

