type

  nsIDOMNodeIterator = interface(nsISupports)
  ['{5af83f50-c8d5-4824-be29-1aa9d640bacb}']
    function Getroot(): nsIDOMNode; safecall;
    property root : nsIDOMNode read Getroot;
    function GetwhatToShow(): idlulong; safecall;
    property whatToShow : idlulong read GetwhatToShow;
    function Getfilter(): nsIDOMNodeFilter; safecall;
    property filter : nsIDOMNodeFilter read Getfilter;
    function GetexpandEntityReferences(): longbool; safecall;
    property expandEntityReferences : longbool read GetexpandEntityReferences;
    function nextNode() : nsIDOMNode; safecall;
    function previousNode() : nsIDOMNode; safecall;
    procedure detach(); safecall;
    function GetreferenceNode(): nsIDOMNode; safecall;
    property referenceNode : nsIDOMNode read GetreferenceNode;
    function GetpointerBeforeReferenceNode(): longbool; safecall;
    property pointerBeforeReferenceNode : longbool read GetpointerBeforeReferenceNode;

  end;

