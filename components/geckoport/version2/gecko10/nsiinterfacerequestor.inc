type

  nsIInterfaceRequestor = interface(nsISupports)
  ['{033A1470-8B2A-11d3-AF88-00A024FFC08C}']
  // uuid parameter is manually converted to constref/TGuid, so
  // that it is possible to assign an interface to this parameter.
    procedure getInterface(constref uuid: TGuid; out result); safecall;

  end;

