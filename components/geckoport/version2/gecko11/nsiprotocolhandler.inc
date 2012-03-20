type

  nsIProtocolHandler = interface(nsISupports)
  ['{15fd6940-8ea7-11d3-93ad-00104ba0fd40}']
    procedure Getscheme( result_: ACString); safecall;
    function GetdefaultPort(): idllong; safecall;
    property defaultPort : idllong read GetdefaultPort;
    function GetprotocolFlags(): idlulong; safecall;
    property protocolFlags : idlulong read GetprotocolFlags;
    function newURI(aSpec: AUTF8String; aOriginCharset: PAnsiChar; aBaseURI: nsIURI) : nsIURI; safecall;
    function newChannel(aURI: nsIURI) : nsIChannel; safecall;
    function allowPort(port: idllong; ascheme: PAnsiChar) : longbool; safecall;

  end;

const
  nsIProtocolHandler_URI_STD=0;
  nsIProtocolHandler_URI_NORELATIVE=(1<<0);
  nsIProtocolHandler_URI_NOAUTH=(1<<1);
  nsIProtocolHandler_URI_INHERITS_SECURITY_CONTEXT=(1<<4);
  nsIProtocolHandler_URI_FORBIDS_AUTOMATIC_DOCUMENT_REPLACEMENT=(1<<5);
  nsIProtocolHandler_URI_LOADABLE_BY_ANYONE=(1<<6);
  nsIProtocolHandler_URI_DANGEROUS_TO_LOAD=(1<<7);
  nsIProtocolHandler_URI_IS_UI_RESOURCE=(1<<8);
  nsIProtocolHandler_URI_IS_LOCAL_FILE=(1<<9);
  nsIProtocolHandler_URI_LOADABLE_BY_SUBSUMERS=(1<<14);
  nsIProtocolHandler_URI_NON_PERSISTABLE=(1<<10);
  nsIProtocolHandler_URI_DOES_NOT_RETURN_DATA=(1<<11);
  nsIProtocolHandler_URI_IS_LOCAL_RESOURCE=(1<<12);
  nsIProtocolHandler_URI_OPENING_EXECUTES_SCRIPT=(1<<13);
  nsIProtocolHandler_ALLOWS_PROXY=(1<<2);
  nsIProtocolHandler_ALLOWS_PROXY_HTTP=(1<<3);
  nsIProtocolHandler_URI_FORBIDS_COOKIE_ACCESS=(1<<15);

