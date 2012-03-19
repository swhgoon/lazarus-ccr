type

  nsIWeakReference = interface(nsISupports)
  ['{9188bc85-f92e-11d2-81ef-0060083a0bcf}']
    procedure QueryReferent(constref uuid: TGuid; out result); safecall;

  end;


  nsISupportsWeakReference = interface(nsISupports)
  ['{9188bc86-f92e-11d2-81ef-0060083a0bcf}']
    function GetWeakReference() : nsIWeakReference; safecall;

  end;

