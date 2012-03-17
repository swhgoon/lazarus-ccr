type

  nsIDOMLocation = interface(nsISupports)
  ['{a6cf906d-15b3-11d2-932e-00805f8add32}']
    function Gethash(): DOMString; safecall;
    procedure Sethash(ahash: DOMString); safecall;
    property hash : DOMString read Gethash write Sethash;
    function Gethost(): DOMString; safecall;
    procedure Sethost(ahost: DOMString); safecall;
    property host : DOMString read Gethost write Sethost;
    function Gethostname(): DOMString; safecall;
    procedure Sethostname(ahostname: DOMString); safecall;
    property hostname : DOMString read Gethostname write Sethostname;
    function Gethref(): DOMString; safecall;
    procedure Sethref(ahref: DOMString); safecall;
    property href : DOMString read Gethref write Sethref;
    function Getpathname(): DOMString; safecall;
    procedure Setpathname(apathname: DOMString); safecall;
    property pathname : DOMString read Getpathname write Setpathname;
    function Getport(): DOMString; safecall;
    procedure Setport(aport: DOMString); safecall;
    property port : DOMString read Getport write Setport;
    function Getprotocol(): DOMString; safecall;
    procedure Setprotocol(aprotocol: DOMString); safecall;
    property protocol : DOMString read Getprotocol write Setprotocol;
    function Getsearch(): DOMString; safecall;
    procedure Setsearch(asearch: DOMString); safecall;
    property search : DOMString read Getsearch write Setsearch;
    procedure reload(forceget: longbool); safecall;
    procedure replace(url: DOMString); safecall;
    procedure assign(url: DOMString); safecall;
    function toString() : DOMString; safecall;

  end;

