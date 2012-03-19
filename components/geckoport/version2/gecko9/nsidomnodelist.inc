type

  nsIDOMNodeList = interface(nsISupports)
  ['{a6cf907d-15b3-11d2-932e-00805f8add32}']
    function item(index: idlulong) : nsIDOMNode; safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;

  end;

