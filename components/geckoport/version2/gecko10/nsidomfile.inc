type

  nsIDOMBlob = interface(nsISupports)
  ['{d5237f31-443a-460b-9e42-449a135346f0}']
    function Getsize(): idlulong; safecall;
    property size : idlulong read Getsize;
    function Gettype(): DOMString; safecall;
    property atype : DOMString read Gettype;
    function GetinternalStream(): nsIInputStream; safecall;
    property internalStream : nsIInputStream read GetinternalStream;
    function getInternalUrl(principal: nsIPrincipal) : DOMString; safecall;
    function mozSlice(start: idllong; anend: idllong; contentType: DOMString) : nsIDOMBlob; safecall;

  end;


  nsIDOMFile = interface(nsIDOMBlob)
  ['{b096ef67-7b77-47f8-8e70-5d8ee36416bf}']
    function Getname(): DOMString; safecall;
    property name : DOMString read Getname;
    function GetmozFullPath(): DOMString; safecall;
    property mozFullPath : DOMString read GetmozFullPath;
    function GetmozFullPathInternal(): DOMString; safecall;
    property mozFullPathInternal : DOMString read GetmozFullPathInternal;

  end;


  nsIDOMMozBlobBuilder = interface(nsISupports)
  ['{006d2cde-ec18-41d4-acc3-43682dd418e2}']
    function getBlob(contentType: DOMString) : nsIDOMBlob; safecall;
    function getFile(name: DOMString; contentType: DOMString) : nsIDOMFile; safecall;
    procedure append(data: jsval); safecall;

  end;

