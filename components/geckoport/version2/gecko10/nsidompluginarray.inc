type

  nsIDOMPluginArray = interface(nsISupports)
  ['{f6134680-f28b-11d2-8360-c90899049c3c}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMPlugin; safecall;
    function namedItem(name: DOMString) : nsIDOMPlugin; safecall;
    procedure refresh(reloadDocuments: longbool); safecall;

  end;

