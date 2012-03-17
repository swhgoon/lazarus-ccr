type

  nsISelection = interface(nsISupports)
  ['{5ac0cd5d-3c08-4c4c-8e70-230c433f5d5c}']
    function GetanchorNode(): nsIDOMNode; safecall;
    property anchorNode : nsIDOMNode read GetanchorNode;
    function GetanchorOffset(): idllong; safecall;
    property anchorOffset : idllong read GetanchorOffset;
    function GetfocusNode(): nsIDOMNode; safecall;
    property focusNode : nsIDOMNode read GetfocusNode;
    function GetfocusOffset(): idllong; safecall;
    property focusOffset : idllong read GetfocusOffset;
    function GetisCollapsed(): longbool; safecall;
    property isCollapsed : longbool read GetisCollapsed;
    function GetrangeCount(): idllong; safecall;
    property rangeCount : idllong read GetrangeCount;
    function getRangeAt(index: idllong) : nsIDOMRange; safecall;
    procedure collapse(parentNode: nsIDOMNode; offset: idllong); safecall;
    procedure extend(parentNode: nsIDOMNode; offset: idllong); safecall;
    procedure collapseToStart(); safecall;
    procedure collapseToEnd(); safecall;
    function containsNode(node: nsIDOMNode; partlyContained: longbool) : longbool; safecall;
    procedure selectAllChildren(parentNode: nsIDOMNode); safecall;
    procedure addRange(range: nsIDOMRange); safecall;
    procedure removeRange(range: nsIDOMRange); safecall;
    procedure removeAllRanges(); safecall;
    procedure deleteFromDocument(); safecall;
    procedure selectionLanguageChange(langRTL: longbool); safecall;
    function toString() : PWideChar; safecall;
    procedure modify(alter: DOMString; direction: DOMString; granularity: DOMString); safecall;

  end;

