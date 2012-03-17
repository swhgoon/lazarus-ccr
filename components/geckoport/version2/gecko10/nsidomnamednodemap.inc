type

  nsIDOMNamedNodeMap = interface(nsISupports)
  ['{a6cf907b-15b3-11d2-932e-00805f8add32}']
    function getNamedItem(name: DOMString) : nsIDOMNode; safecall;
    function setNamedItem(arg: nsIDOMNode) : nsIDOMNode; safecall;
    function removeNamedItem(name: DOMString) : nsIDOMNode; safecall;
    function item(index: idlulong) : nsIDOMNode; safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function getNamedItemNS(namespaceURI: DOMString; localName: DOMString) : nsIDOMNode; safecall;
    function setNamedItemNS(arg: nsIDOMNode) : nsIDOMNode; safecall;
    function removeNamedItemNS(namespaceURI: DOMString; localName: DOMString) : nsIDOMNode; safecall;

  end;

