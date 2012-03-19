type

  nsIComponentManager = interface(nsISupports)
  ['{1d940426-5fe5-42c3-84ae-a300f2d9ebd5}']
    procedure getClassObject(constref aClass: TGuid; constref aIID: TGuid; out result); safecall;
    procedure getClassObjectByContractID(aContractID: PAnsiChar; constref aIID: TGuid; out result); safecall;
    procedure createInstance(constref aClass: TGuid; aDelegate: nsISupports; constref aIID: TGuid; out result); safecall;
    procedure createInstanceByContractID(aContractID: PAnsiChar; aDelegate: nsISupports; constref aIID: TGuid; out result); safecall;
    procedure addBootstrappedManifestLocation(aLocation: nsILocalFile); safecall;
    procedure removeBootstrappedManifestLocation(aLocation: nsILocalFile); safecall;

  end;

