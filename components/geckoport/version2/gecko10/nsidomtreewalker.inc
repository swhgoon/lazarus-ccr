type

  nsIDOMTreeWalker = interface(nsISupports)
  ['{400af3ca-1dd2-11b2-a50a-887ecca2e63a}']
    function Getroot(): nsIDOMNode; safecall;
    property root : nsIDOMNode read Getroot;
    function GetwhatToShow(): idlulong; safecall;
    property whatToShow : idlulong read GetwhatToShow;
    function Getfilter(): nsIDOMNodeFilter; safecall;
    property filter : nsIDOMNodeFilter read Getfilter;
    function GetexpandEntityReferences(): longbool; safecall;
    property expandEntityReferences : longbool read GetexpandEntityReferences;
    function GetcurrentNode(): nsIDOMNode; safecall;
    procedure SetcurrentNode(acurrentNode: nsIDOMNode); safecall;
    property currentNode : nsIDOMNode read GetcurrentNode write SetcurrentNode;
    function parentNode() : nsIDOMNode; safecall;
    function firstChild() : nsIDOMNode; safecall;
    function lastChild() : nsIDOMNode; safecall;
    function previousSibling() : nsIDOMNode; safecall;
    function nextSibling() : nsIDOMNode; safecall;
    function previousNode() : nsIDOMNode; safecall;
    function nextNode() : nsIDOMNode; safecall;

  end;

