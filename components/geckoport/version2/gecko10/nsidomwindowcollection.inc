type

  nsIDOMWindowCollection = interface(nsISupports)
  ['{a6cf906f-15b3-11d2-932e-00805f8add32}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMWindow; safecall;
    function namedItem(name: DOMString) : nsIDOMWindow; safecall;

  end;

