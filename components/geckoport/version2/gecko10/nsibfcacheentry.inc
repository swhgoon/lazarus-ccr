type

  nsIBFCacheEntry = interface(nsISupports)
  ['{a576060e-c7df-4d81-aa8c-5b52bd6ad25d}']
    procedure RemoveFromBFCacheSync(); safecall;
    procedure RemoveFromBFCacheAsync(); safecall;
    function GetID(): idlulong; safecall;
    property ID : idlulong read GetID;

  end;

