type

  nsIDOMDocumentType = interface(nsIDOMNode)
  ['{ac5fd4c5-6c5d-4dfc-878c-7d661aa676de}']
    procedure Getname( result_: DOMString); safecall;
    procedure GetpublicId( result_: DOMString); safecall;
    procedure GetsystemId( result_: DOMString); safecall;
    procedure GetinternalSubset( result_: DOMString); safecall;

  end;

