type

  nsIDOMAttr = interface(nsIDOMNode)
  ['{03da4bc9-1b9a-41dc-a1a4-32414d48d704}']
    procedure Getname( result_: DOMString); safecall;
    function Getspecified(): longbool; safecall;
    property specified : longbool read Getspecified;
    procedure Getvalue( result_: DOMString); safecall;
    procedure Setvalue(avalue: DOMString); safecall;
    function GetownerElement(): nsIDOMElement; safecall;
    property ownerElement : nsIDOMElement read GetownerElement;
    function GetisId(): longbool; safecall;
    property isId : longbool read GetisId;

  end;

