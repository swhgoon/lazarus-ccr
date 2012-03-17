type

  nsIServiceManager = interface(nsISupports)
  ['{8bb35ed9-e332-462d-9155-4a002ab5c958}']
  // aIID and aClass parameters are manually converted to constref/TGuid, so
  // that it is possible to assign an interface to this parameter.
    procedure getService(constref aClass: TGuid; constref aIID: TGuid; out result); safecall;
    procedure getServiceByContractID(aContractID: PAnsiChar; aIID: nsIIDRef; out result); safecall;
    function isServiceInstantiated(aClass: nsCIDRef; aIID: nsIIDRef) : longbool; safecall;
    function isServiceInstantiatedByContractID(aContractID: PAnsiChar; aIID: nsIIDRef) : longbool; safecall;

  end;

