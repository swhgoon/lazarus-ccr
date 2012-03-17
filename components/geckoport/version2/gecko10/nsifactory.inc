type

  nsIFactory = interface(nsISupports)
  ['{00000001-0000-0000-c000-000000000046}']
    procedure createInstance(aOuter: nsISupports; iid: nsIIDRef; out result); safecall;
    procedure lockFactory(lock: longbool); safecall;

  end;

