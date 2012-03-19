type

  nsIInterfaceRequestor = interface(nsISupports)
  ['{033A1470-8B2A-11d3-AF88-00A024FFC08C}']
    procedure getInterface(constref uuid: TGuid; out result); safecall;

  end;

