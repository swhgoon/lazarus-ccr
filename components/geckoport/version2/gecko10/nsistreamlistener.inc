type

  nsIStreamListener = interface(nsIRequestObserver)
  ['{1a637020-1482-11d3-9333-00104ba0fd40}']
    procedure onDataAvailable(aRequest: nsIRequest; aContext: nsISupports; aInputStream: nsIInputStream; aOffset: idlulong; aCount: idlulong); safecall;

  end;

