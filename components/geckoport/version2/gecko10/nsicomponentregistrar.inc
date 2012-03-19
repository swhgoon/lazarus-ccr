type

  nsIComponentRegistrar = interface(nsISupports)
  ['{2417cbfe-65ad-48a6-b4b6-eb84db174392}']
    procedure autoRegister(aSpec: nsIFile); safecall;
    procedure autoUnregister(aSpec: nsIFile); safecall;
    procedure registerFactory(constref aClass: TGuid; aClassName: PAnsiChar; aContractID: PAnsiChar; aFactory: nsIFactory); safecall;
    procedure unregisterFactory(constref aClass: TGuid; aFactory: nsIFactory); safecall;
    procedure registerFactoryLocation(constref aClass: TGuid; aClassName: PAnsiChar; aContractID: PAnsiChar; aFile: nsIFile; aLoaderStr: PAnsiChar; aType: PAnsiChar); safecall;
    procedure unregisterFactoryLocation(constref aClass: TGuid; aFile: nsIFile); safecall;
    function isCIDRegistered(constref aClass: TGuid) : longbool; safecall;
    function isContractIDRegistered(aContractID: PAnsiChar) : longbool; safecall;
    function enumerateCIDs() : nsISimpleEnumerator; safecall;
    function enumerateContractIDs() : nsISimpleEnumerator; safecall;
    function CIDToContractID(constref aClass: TGuid) : PAnsiChar; safecall;
    function contractIDToCID(aContractID: PAnsiChar) : nsCIDPtr; safecall;

  end;

