type

  nsIDOMDOMTokenList = interface(nsISupports)
  ['{c6f1e160-eeeb-404a-98b0-6f1246520b6e}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    procedure item(index: idlulong; result_: DOMString); safecall;
    function contains(token: DOMString) : longbool; safecall;
    procedure add(token: DOMString); safecall;
    procedure remove(token: DOMString); safecall;
    function toggle(token: DOMString) : longbool; safecall;
    procedure toString( result_: DOMString); safecall;

  end;

