type

  nsIDOMMimeType = interface(nsISupports)
  ['{f6134682-f28b-11d2-8360-c90899049c3c}']
    function Getdescription(): DOMString; safecall;
    property description : DOMString read Getdescription;
    function GetenabledPlugin(): nsIDOMPlugin; safecall;
    property enabledPlugin : nsIDOMPlugin read GetenabledPlugin;
    function Getsuffixes(): DOMString; safecall;
    property suffixes : DOMString read Getsuffixes;
    function Gettype(): DOMString; safecall;
    property atype : DOMString read Gettype;

  end;

