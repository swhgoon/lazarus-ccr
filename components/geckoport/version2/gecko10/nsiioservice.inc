type

  nsIIOService = interface(nsISupports)
  ['{bddeda3f-9020-4d12-8c70-984ee9f7935e}']
    function getProtocolHandler(aScheme: PAnsiChar) : nsIProtocolHandler; safecall;
    function getProtocolFlags(aScheme: PAnsiChar) : idlulong; safecall;
    function newURI(aSpec: AUTF8String; aOriginCharset: PAnsiChar; aBaseURI: nsIURI) : nsIURI; safecall;
    function newFileURI(aFile: nsIFile) : nsIURI; safecall;
    function newChannelFromURI(aURI: nsIURI) : nsIChannel; safecall;
    function newChannel(aSpec: AUTF8String; aOriginCharset: PAnsiChar; aBaseURI: nsIURI) : nsIChannel; safecall;
    function Getoffline(): longbool; safecall;
    procedure Setoffline(aoffline: longbool); safecall;
    property offline : longbool read Getoffline write Setoffline;
    function allowPort(aPort: idllong; aScheme: PAnsiChar) : longbool; safecall;
    procedure extractScheme(urlString: AUTF8String; result_: ACString); safecall;

  end;

