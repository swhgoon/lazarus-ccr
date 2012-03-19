type

  nsIDOMStorageObsolete = interface(nsISupports)
  ['{18013CF9-B104-49cf-9484-C2A7A845457E}']
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    procedure key(index: idlulong; result_: DOMString); safecall;
    function getItem(akey: DOMString) : nsIDOMStorageItem; safecall;
    procedure setItem(akey: DOMString; data: DOMString); safecall;
    procedure removeItem(akey: DOMString); safecall;

  end;

