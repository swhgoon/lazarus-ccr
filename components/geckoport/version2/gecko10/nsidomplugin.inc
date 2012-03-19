type

  nsIDOMPlugin = interface(nsISupports)
  ['{ff69a292-c74d-4ef5-9d0f-86112fff2d5b}']
    procedure Getdescription( result_: DOMString); safecall;
    procedure Getfilename( result_: DOMString); safecall;
    procedure Getversion( result_: DOMString); safecall;
    procedure Getname( result_: DOMString); safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : nsIDOMMimeType; safecall;
    function namedItem(aname: DOMString) : nsIDOMMimeType; safecall;

  end;

