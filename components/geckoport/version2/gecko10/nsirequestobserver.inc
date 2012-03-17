type

  nsIRequestObserver = interface(nsISupports)
  ['{fd91e2e0-1481-11d3-9333-00104ba0fd40}']
    procedure onStartRequest(aRequest: nsIRequest; aContext: nsISupports); safecall;
    procedure onStopRequest(aRequest: nsIRequest; aContext: nsISupports; aStatusCode: nsresult); safecall;

  end;

