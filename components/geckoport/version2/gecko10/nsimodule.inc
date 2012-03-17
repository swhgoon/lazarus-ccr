type

  nsIModule = interface(nsISupports)
  ['{7392D032-5371-11d3-994E-00805FD26FEE}']
    procedure getClassObject(aCompMgr: nsIComponentManager; aClass: nsCIDRef; aIID: nsIIDRef; out aResult: nsQIResult); safecall;
    procedure registerSelf(aCompMgr: nsIComponentManager; aLocation: nsIFile; aLoaderStr: PAnsiChar; aType: PAnsiChar); safecall;
    procedure unregisterSelf(aCompMgr: nsIComponentManager; aLocation: nsIFile; aLoaderStr: PAnsiChar); safecall;
    function canUnload(aCompMgr: nsIComponentManager) : longbool; safecall;

  end;

