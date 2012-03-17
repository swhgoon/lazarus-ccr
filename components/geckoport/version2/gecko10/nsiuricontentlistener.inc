type

  nsIURIContentListener = interface(nsISupports)
  ['{94928AB3-8B63-11d3-989D-001083010E9B}']
    function onStartURIOpen(aURI: nsIURI) : longbool; safecall;
    function doContent(aContentType: PAnsiChar; aIsContentPreferred: longbool; aRequest: nsIRequest; out aContentHandler: nsIStreamListener) : longbool; safecall;
    function isPreferred(aContentType: PAnsiChar; out aDesiredContentType: PAnsiChar) : longbool; safecall;
    function canHandleContent(aContentType: PAnsiChar; aIsContentPreferred: longbool; out aDesiredContentType: PAnsiChar) : longbool; safecall;
    function GetloadCookie(): nsISupports; safecall;
    procedure SetloadCookie(aloadCookie: nsISupports); safecall;
    property loadCookie : nsISupports read GetloadCookie write SetloadCookie;
    function GetparentContentListener(): nsIURIContentListener; safecall;
    procedure SetparentContentListener(aparentContentListener: nsIURIContentListener); safecall;
    property parentContentListener : nsIURIContentListener read GetparentContentListener write SetparentContentListener;

  end;

