type

  nsIDOMDOMImplementation = interface(nsISupports)
  ['{03a6f574-99ec-42f8-9e6c-812a4a9bcbf7}']
    function hasFeature(feature: DOMString; version: DOMString) : longbool; safecall;
    function createDocumentType(qualifiedName: DOMString; publicId: DOMString; systemId: DOMString) : nsIDOMDocumentType; safecall;
    function createDocument(namespaceURI: DOMString; qualifiedName: DOMString; doctype: nsIDOMDocumentType) : nsIDOMDocument; safecall;
    function createHTMLDocument(title: DOMString) : nsIDOMDocument; safecall;

  end;

