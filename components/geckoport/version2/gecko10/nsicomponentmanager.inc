type

  nsIComponentManager = interface(nsISupports)
  ['{1d940426-5fe5-42c3-84ae-a300f2d9ebd5}']
    procedure getClassObject(aClass: nsCIDRef; aIID: nsIIDRef; out result); safecall;
    procedure getClassObjectByContractID(aContractID: PAnsiChar; aIID: nsIIDRef; out result); safecall;
    procedure createInstance(aClass: nsCIDRef; aDelegate: nsISupports; aIID: nsIIDRef; out result); safecall;
    procedure createInstanceByContractID(aContractID: PAnsiChar; aDelegate: nsISupports; aIID: nsIIDRef; out result); safecall;
    procedure addBootstrappedManifestLocation(aLocation: nsILocalFile); safecall;
    procedure removeBootstrappedManifestLocation(aLocation: nsILocalFile); safecall;

  end;

