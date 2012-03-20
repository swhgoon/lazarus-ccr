type

  nsIDOMElement = interface(nsIDOMNode)
  ['{f561753a-1d4f-40c1-b147-ea955fc6fd94}']
    procedure GettagName( result_: DOMString); safecall;
    function GetclassList(): nsIDOMDOMTokenList; safecall;
    property classList : nsIDOMDOMTokenList read GetclassList;
    procedure getAttribute(aname: DOMString; result_: DOMString); safecall;
    procedure getAttributeNS(anamespaceURI: DOMString; alocalName: DOMString; result_: DOMString); safecall;
    procedure setAttribute(aname: DOMString; avalue: DOMString); safecall;
    procedure setAttributeNS(anamespaceURI: DOMString; aqualifiedName: DOMString; avalue: DOMString); safecall;
    procedure removeAttribute(aname: DOMString); safecall;
    procedure removeAttributeNS(anamespaceURI: DOMString; alocalName: DOMString); safecall;
    function hasAttribute(aname: DOMString) : longbool; safecall;
    function hasAttributeNS(anamespaceURI: DOMString; alocalName: DOMString) : longbool; safecall;
    function getAttributeNode(aname: DOMString) : nsIDOMAttr; safecall;
    function setAttributeNode(anewAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function removeAttributeNode(anoldAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function getAttributeNodeNS(anamespaceURI: DOMString; alocalName: DOMString) : nsIDOMAttr; safecall;
    function setAttributeNodeNS(anewAttr: nsIDOMAttr) : nsIDOMAttr; safecall;
    function getElementsByTagName(aname: DOMString) : nsIDOMNodeList; safecall;
    function getElementsByTagNameNS(anamespaceURI: DOMString; alocalName: DOMString) : nsIDOMNodeList; safecall;
    function getElementsByClassName(aclasses: DOMString) : nsIDOMNodeList; safecall;
    function Getchildren(): nsIDOMNodeList; safecall;
    property children : nsIDOMNodeList read Getchildren;
    function GetfirstElementChild(): nsIDOMElement; safecall;
    property firstElementChild : nsIDOMElement read GetfirstElementChild;
    function GetlastElementChild(): nsIDOMElement; safecall;
    property lastElementChild : nsIDOMElement read GetlastElementChild;
    function GetpreviousElementSibling(): nsIDOMElement; safecall;
    property previousElementSibling : nsIDOMElement read GetpreviousElementSibling;
    function GetnextElementSibling(): nsIDOMElement; safecall;
    property nextElementSibling : nsIDOMElement read GetnextElementSibling;
    function GetchildElementCount(): idlulong; safecall;
    property childElementCount : idlulong read GetchildElementCount;
    function Getonmouseenter(): jsval; safecall;
    procedure Setonmouseenter(aonmouseenter: jsval); safecall;
    property onmouseenter : jsval read Getonmouseenter write Setonmouseenter;
    function Getonmouseleave(): jsval; safecall;
    procedure Setonmouseleave(aonmouseleave: jsval); safecall;
    property onmouseleave : jsval read Getonmouseleave write Setonmouseleave;
    function getClientRects() : nsIDOMClientRectList; safecall;
    function getBoundingClientRect() : nsIDOMClientRect; safecall;
    function GetscrollTop(): idllong; safecall;
    procedure SetscrollTop(ascrollTop: idllong); safecall;
    property scrollTop : idllong read GetscrollTop write SetscrollTop;
    function GetscrollLeft(): idllong; safecall;
    procedure SetscrollLeft(ascrollLeft: idllong); safecall;
    property scrollLeft : idllong read GetscrollLeft write SetscrollLeft;
    function GetscrollWidth(): idllong; safecall;
    property scrollWidth : idllong read GetscrollWidth;
    function GetscrollHeight(): idllong; safecall;
    property scrollHeight : idllong read GetscrollHeight;
    function GetclientTop(): idllong; safecall;
    property clientTop : idllong read GetclientTop;
    function GetclientLeft(): idllong; safecall;
    property clientLeft : idllong read GetclientLeft;
    function GetclientWidth(): idllong; safecall;
    property clientWidth : idllong read GetclientWidth;
    function GetclientHeight(): idllong; safecall;
    property clientHeight : idllong read GetclientHeight;
    function mozMatchesSelector(aselector: DOMString) : longbool; safecall;
    procedure setCapture(aretargetToElement: longbool); safecall;
    procedure releaseCapture(); safecall;

  end;

