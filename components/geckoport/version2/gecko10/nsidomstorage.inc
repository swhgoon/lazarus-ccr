type

  nsIDOMStorage = interface(nsISupports)
  ['{43E5EDAD-1E02-42c4-9D99-C3D9DEE22A20}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    procedure key(index: idlulong; result_: DOMString); safecall;
    procedure getItem(akey: DOMString; result_: DOMString); safecall;
    procedure setItem(akey: DOMString; data: DOMString); safecall;
    procedure removeItem(akey: DOMString); safecall;
    procedure clear(); safecall;

  end;

