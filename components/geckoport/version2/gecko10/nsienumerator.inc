type

  nsIEnumerator = interface(nsISupports)
  ['{ad385286-cbc4-11d2-8cca-0060b0fc14a3}']
    procedure first(); safecall;
    procedure next(); safecall;
    function currentItem() : nsISupports; safecall;
    procedure isDone(); safecall;

  end;


  nsIBidirectionalEnumerator = interface(nsIEnumerator)
  ['{75f158a0-cadd-11d2-8cca-0060b0fc14a3}']
    procedure last(); safecall;
    procedure prev(); safecall;

  end;

