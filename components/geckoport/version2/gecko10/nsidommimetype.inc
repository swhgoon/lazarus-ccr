type

  nsIDOMMimeType = interface(nsISupports)
  ['{f6134682-f28b-11d2-8360-c90899049c3c}']
    procedure Getdescription( result_: DOMString); safecall;
    function GetenabledPlugin(): nsIDOMPlugin; safecall;
    property enabledPlugin : nsIDOMPlugin read GetenabledPlugin;
    procedure Getsuffixes( result_: DOMString); safecall;
    procedure Gettype( result_: DOMString); safecall;

  end;

