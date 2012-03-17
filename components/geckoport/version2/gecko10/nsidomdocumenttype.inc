type

  nsIDOMDocumentType = interface(nsIDOMNode)
  ['{ac5fd4c5-6c5d-4dfc-878c-7d661aa676de}']
    function Getname(): DOMString; safecall;
    property name : DOMString read Getname;
    function GetpublicId(): DOMString; safecall;
    property publicId : DOMString read GetpublicId;
    function GetsystemId(): DOMString; safecall;
    property systemId : DOMString read GetsystemId;
    function GetinternalSubset(): DOMString; safecall;
    property internalSubset : DOMString read GetinternalSubset;

  end;

