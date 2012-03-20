type

  nsIDOMClientRectList = interface(nsISupports)
  ['{917da19d-62f5-441d-b47e-9e35f05639c9}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMClientRect; safecall;

  end;

