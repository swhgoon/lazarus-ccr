type

  nsIObserver = interface(nsISupports)
  ['{DB242E01-E4D9-11d2-9DDE-000064657374}']
    procedure observe(aSubject: nsISupports; aTopic: PAnsiChar; aData: PWideChar); safecall;

  end;

