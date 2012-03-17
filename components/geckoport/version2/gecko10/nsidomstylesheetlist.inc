type

  nsIDOMStyleSheetList = interface(nsISupports)
  ['{a6cf9081-15b3-11d2-932e-00805f8add32}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMStyleSheet; safecall;

  end;

