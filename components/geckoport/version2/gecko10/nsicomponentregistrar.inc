type

  nsIComponentRegistrar = interface(nsISupports)
  ['{2417cbfe-65ad-48a6-b4b6-eb84db174392}']
    procedure autoRegister(aSpec: nsIFile); safecall;
    procedure autoUnregister(aSpec: nsIFile); safecall;
    procedure registerFactory(aClass: nsCIDRef; aClassName: PAnsiChar; aContractID: PAnsiChar; aFactory: nsIFactory); safecall;
    procedure unregisterFactory(aClass: nsCIDRef; aFactory: nsIFactory); safecall;
    procedure registerFactoryLocation(aClass: nsCIDRef; aClassName: PAnsiChar; aContractID: PAnsiChar; aFile: nsIFile; aLoaderStr: PAnsiChar; aType: PAnsiChar); safecall;
    procedure unregisterFactoryLocation(aClass: nsCIDRef; aFile: nsIFile); safecall;
    function isCIDRegistered(aClass: nsCIDRef) : longbool; safecall;
    function isContractIDRegistered(aContractID: PAnsiChar) : longbool; safecall;
    function enumerateCIDs() : nsISimpleEnumerator; safecall;
    function enumerateContractIDs() : nsISimpleEnumerator; safecall;
    function CIDToContractID(aClass: nsCIDRef) : PAnsiChar; safecall;
    function contractIDToCID(aContractID: PAnsiChar) : nsCIDPtr; safecall;

  end;

