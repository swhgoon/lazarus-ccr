type

  nsIDOMNode = interface(nsISupports)
  ['{c8ac3f81-63e1-4c31-8543-70a656642789}']
    procedure GetnodeName( result_: DOMString); safecall;
    procedure GetnodeValue( result_: DOMString); safecall;
    procedure SetnodeValue(anodeValue: DOMString); safecall;
    function GetnodeType(): idlushort; safecall;
    property nodeType : idlushort read GetnodeType;
    function GetparentNode(): nsIDOMNode; safecall;
    property parentNode : nsIDOMNode read GetparentNode;
    function GetparentElement(): nsIDOMElement; safecall;
    property parentElement : nsIDOMElement read GetparentElement;
    function GetchildNodes(): nsIDOMNodeList; safecall;
    property childNodes : nsIDOMNodeList read GetchildNodes;
    function GetfirstChild(): nsIDOMNode; safecall;
    property firstChild : nsIDOMNode read GetfirstChild;
    function GetlastChild(): nsIDOMNode; safecall;
    property lastChild : nsIDOMNode read GetlastChild;
    function GetpreviousSibling(): nsIDOMNode; safecall;
    property previousSibling : nsIDOMNode read GetpreviousSibling;
    function GetnextSibling(): nsIDOMNode; safecall;
    property nextSibling : nsIDOMNode read GetnextSibling;
    function Getattributes(): nsIDOMNamedNodeMap; safecall;
    property attributes : nsIDOMNamedNodeMap read Getattributes;
    function GetownerDocument(): nsIDOMDocument; safecall;
    property ownerDocument : nsIDOMDocument read GetownerDocument;
    function insertBefore(newChild: nsIDOMNode; refChild: nsIDOMNode) : nsIDOMNode; safecall;
    function replaceChild(newChild: nsIDOMNode; oldChild: nsIDOMNode) : nsIDOMNode; safecall;
    function removeChild(oldChild: nsIDOMNode) : nsIDOMNode; safecall;
    function appendChild(newChild: nsIDOMNode) : nsIDOMNode; safecall;
    function hasChildNodes() : longbool; safecall;
    function cloneNode(deep: longbool) : nsIDOMNode; safecall;
    procedure normalize(); safecall;
    function isSupported(feature: DOMString; version: DOMString) : longbool; safecall;
    procedure GetnamespaceURI( result_: DOMString); safecall;
    procedure Getprefix( result_: DOMString); safecall;
    procedure GetlocalName( result_: DOMString); safecall;
    function hasAttributes() : longbool; safecall;
    procedure GetbaseURI( result_: DOMString); safecall;
    function compareDocumentPosition(other: nsIDOMNode) : idlushort; safecall;
    procedure GettextContent( result_: DOMString); safecall;
    procedure SettextContent(atextContent: DOMString); safecall;
    function isSameNode(other: nsIDOMNode) : longbool; safecall;
    procedure lookupPrefix(anamespaceURI: DOMString; result_: DOMString); safecall;
    function isDefaultNamespace(anamespaceURI: DOMString) : longbool; safecall;
    procedure lookupNamespaceURI(aprefix: DOMString; result_: DOMString); safecall;
    function isEqualNode(arg: nsIDOMNode) : longbool; safecall;
    function setUserData(key: DOMString; data: nsIVariant; handler: nsIDOMUserDataHandler) : nsIVariant; safecall;
    function getUserData(key: DOMString) : nsIVariant; safecall;
    function contains(aOther: nsIDOMNode) : longbool; safecall;

  end;

const
  nsIDOMNode_ELEMENT_NODE=1;
  nsIDOMNode_ATTRIBUTE_NODE=2;
  nsIDOMNode_TEXT_NODE=3;
  nsIDOMNode_CDATA_SECTION_NODE=4;
  nsIDOMNode_ENTITY_REFERENCE_NODE=5;
  nsIDOMNode_ENTITY_NODE=6;
  nsIDOMNode_PROCESSING_INSTRUCTION_NODE=7;
  nsIDOMNode_COMMENT_NODE=8;
  nsIDOMNode_DOCUMENT_NODE=9;
  nsIDOMNode_DOCUMENT_TYPE_NODE=10;
  nsIDOMNode_DOCUMENT_FRAGMENT_NODE=11;
  nsIDOMNode_NOTATION_NODE=12;
  nsIDOMNode_DOCUMENT_POSITION_DISCONNECTED=$01;
  nsIDOMNode_DOCUMENT_POSITION_PRECEDING=$02;
  nsIDOMNode_DOCUMENT_POSITION_FOLLOWING=$04;
  nsIDOMNode_DOCUMENT_POSITION_CONTAINS=$08;
  nsIDOMNode_DOCUMENT_POSITION_CONTAINED_BY=$10;
  nsIDOMNode_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC=$20;

