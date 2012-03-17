type

  nsIWeakReference = interface(nsISupports)
  ['{9188bc85-f92e-11d2-81ef-0060083a0bcf}']
  // uuid parameter is manually converted to constref/TGuid, so
  // that it is possible to assign an interface to this parameter.
    procedure QueryReferent(constref uuid: TGuid; out result); safecall;

  end;


  nsISupportsWeakReference = interface(nsISupports)
  ['{9188bc86-f92e-11d2-81ef-0060083a0bcf}']
    function GetWeakReference() : nsIWeakReference; safecall;

  end;

