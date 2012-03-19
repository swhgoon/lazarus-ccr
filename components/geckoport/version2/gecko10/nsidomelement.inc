type

  nsIDOMElement = interface(nsIDOMNode)
  ['{1f249e8b-7b41-44c0-a8d5-15298c1198cd}']
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
    function Getonmouseenter(): jsval; safecall;
    procedure Setonmouseenter(aonmouseenter: jsval); safecall;
    property onmouseenter : jsval read Getonmouseenter write Setonmouseenter;
    function Getonmouseleave(): jsval; safecall;
    procedure Setonmouseleave(aonmouseleave: jsval); safecall;
    property onmouseleave : jsval read Getonmouseleave write Setonmouseleave;

  end;

