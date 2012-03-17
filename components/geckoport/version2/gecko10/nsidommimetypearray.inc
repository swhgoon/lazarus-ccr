type

  nsIDOMMimeTypeArray = interface(nsISupports)
  ['{f6134683-f28b-11d2-8360-c90899049c3c}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMMimeType; safecall;
    function namedItem(name: DOMString) : nsIDOMMimeType; safecall;

  end;

