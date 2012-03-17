type

  nsIDOMStyleSheet = interface(nsISupports)
  ['{a6cf9080-15b3-11d2-932e-00805f8add32}']
    function Gettype(): DOMString; safecall;
    property atype : DOMString read Gettype;
    function Getdisabled(): longbool; safecall;
    procedure Setdisabled(adisabled: longbool); safecall;
    property disabled : longbool read Getdisabled write Setdisabled;
    function GetownerNode(): nsIDOMNode; safecall;
    property ownerNode : nsIDOMNode read GetownerNode;
    function GetparentStyleSheet(): nsIDOMStyleSheet; safecall;
    property parentStyleSheet : nsIDOMStyleSheet read GetparentStyleSheet;
    function Gethref(): DOMString; safecall;
    property href : DOMString read Gethref;
    function Gettitle(): DOMString; safecall;
    property title : DOMString read Gettitle;
    function Getmedia(): nsIDOMMediaList; safecall;
    property media : nsIDOMMediaList read Getmedia;

  end;

