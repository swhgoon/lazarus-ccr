type

  nsIDOMElement = interface(nsIDOMNode)
  ['{56aaaf03-f8f1-4c06-9cb5-f3e33a39e5c3}']
    procedure GettagName( result_: DOMString); safecall;
    procedure getAttribute(aname: DOMString; result_: DOMString); safecall;
    procedure setAttribute(aname: DOMString; avalue: DOMString); safecall;
    procedure removeAttribute(aname: DOMString); safecall;
    function getAttributeNode(aname: DOMString) : nsIDOMAttr; safecall;
    function setAttributeNode(anewAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function removeAttributeNode(anoldAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function getElementsByTagName(aname: DOMString) : nsIDOMNodeList; safecall;
    procedure getAttributeNS(anamespaceURI: DOMString; alocalName: DOMString; result_: DOMString); safecall;
    procedure setAttributeNS(anamespaceURI: DOMString; aqualifiedName: DOMString; avalue: DOMString); safecall;
    procedure removeAttributeNS(anamespaceURI: DOMString; alocalName: DOMString); safecall;
    function getAttributeNodeNS(anamespaceURI: DOMString; alocalName: DOMString) : nsIDOMAttr; safecall;
    function setAttributeNodeNS(anewAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function getElementsByTagNameNS(anamespaceURI: DOMString; alocalName: DOMString) : nsIDOMNodeList; safecall;
    function hasAttribute(aname: DOMString) : longbool; safecall;
    function hasAttributeNS(anamespaceURI: DOMString; alocalName: DOMString) : longbool; safecall;

  end;

