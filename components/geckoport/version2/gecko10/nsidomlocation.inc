type

  nsIDOMLocation = interface(nsISupports)
  ['{a6cf906d-15b3-11d2-932e-00805f8add32}']
    procedure Gethash( result_: DOMString); safecall;
    procedure Sethash(ahash: DOMString); safecall;
    procedure Gethost( result_: DOMString); safecall;
    procedure Sethost(ahost: DOMString); safecall;
    procedure Gethostname( result_: DOMString); safecall;
    procedure Sethostname(ahostname: DOMString); safecall;
    procedure Gethref( result_: DOMString); safecall;
    procedure Sethref(ahref: DOMString); safecall;
    procedure Getpathname( result_: DOMString); safecall;
    procedure Setpathname(apathname: DOMString); safecall;
    procedure Getport( result_: DOMString); safecall;
    procedure Setport(aport: DOMString); safecall;
    procedure Getprotocol( result_: DOMString); safecall;
    procedure Setprotocol(aprotocol: DOMString); safecall;
    procedure Getsearch( result_: DOMString); safecall;
    procedure Setsearch(asearch: DOMString); safecall;
    procedure reload(forceget: longbool); safecall;
    procedure replace(url: DOMString); safecall;
    procedure assign(url: DOMString); safecall;
    procedure toString( result_: DOMString); safecall;

  end;

