type

  nsIDOMPlugin = interface(nsISupports)
  ['{ff69a292-c74d-4ef5-9d0f-86112fff2d5b}']
    function Getdescription(): DOMString; safecall;
    property description : DOMString read Getdescription;
    function Getfilename(): DOMString; safecall;
    property filename : DOMString read Getfilename;
    function Getversion(): DOMString; safecall;
    property version : DOMString read Getversion;
    function Getname(): DOMString; safecall;
    property name : DOMString read Getname;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMMimeType; safecall;
    function namedItem(aname: DOMString) : nsIDOMMimeType; safecall;

  end;

