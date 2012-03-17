type

  nsIDOMEventListener = interface(nsISupports)
  ['{df31c120-ded6-11d1-bd85-00805f8ae3f4}']
    procedure handleEvent(event: nsIDOMEvent); safecall;

  end;

