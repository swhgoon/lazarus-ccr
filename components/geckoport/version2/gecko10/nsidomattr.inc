type

  nsIDOMAttr = interface(nsIDOMNode)
  ['{03da4bc9-1b9a-41dc-a1a4-32414d48d704}']
    function Getname(): DOMString; safecall;
    property name : DOMString read Getname;
    function Getspecified(): longbool; safecall;
    property specified : longbool read Getspecified;
    function Getvalue(): DOMString; safecall;
    procedure Setvalue(avalue: DOMString); safecall;
    property value : DOMString read Getvalue write Setvalue;
    function GetownerElement(): nsIDOMElement; safecall;
    property ownerElement : nsIDOMElement read GetownerElement;
    function GetisId(): longbool; safecall;
    property isId : longbool read GetisId;

  end;

