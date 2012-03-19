type

  nsIDOMStyleSheet = interface(nsISupports)
  ['{a6cf9080-15b3-11d2-932e-00805f8add32}']
    procedure Gettype( result_: DOMString); safecall;
    function Getdisabled(): longbool; safecall;
    procedure Setdisabled(adisabled: longbool); safecall;
    property disabled : longbool read Getdisabled write Setdisabled;
    function GetownerNode(): nsIDOMNode; safecall;
    property ownerNode : nsIDOMNode read GetownerNode;
    function GetparentStyleSheet(): nsIDOMStyleSheet; safecall;
    property parentStyleSheet : nsIDOMStyleSheet read GetparentStyleSheet;
    procedure Gethref( result_: DOMString); safecall;
    procedure Gettitle( result_: DOMString); safecall;
    function Getmedia(): nsIDOMMediaList; safecall;
    property media : nsIDOMMediaList read Getmedia;

  end;

