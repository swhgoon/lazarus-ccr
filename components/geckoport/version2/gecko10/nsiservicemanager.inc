type

  nsIServiceManager = interface(nsISupports)
  ['{8bb35ed9-e332-462d-9155-4a002ab5c958}']
    procedure getService(constref aClass: TGuid; constref aIID: TGuid; out result); safecall;
    procedure getServiceByContractID(aContractID: PAnsiChar; constref aIID: TGuid; out result); safecall;
    function isServiceInstantiated(constref aClass: TGuid; constref aIID: TGuid) : longbool; safecall;
    function isServiceInstantiatedByContractID(aContractID: PAnsiChar; constref aIID: TGuid) : longbool; safecall;

  end;

