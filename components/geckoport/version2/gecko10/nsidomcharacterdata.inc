type

  nsIDOMCharacterData = interface(nsIDOMNode)
  ['{cb75c251-afc7-444f-b2d6-b9635555f3ed}']
    function Getdata(): DOMString; safecall;
    procedure Setdata(adata: DOMString); safecall;
    property data : DOMString read Getdata write Setdata;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function substringData(offset: idlulong; count: idlulong) : DOMString; safecall;
    procedure appendData(arg: DOMString); safecall;
    procedure insertData(offset: idlulong; arg: DOMString); safecall;
    procedure deleteData(offset: idlulong; count: idlulong); safecall;
    procedure replaceData(offset: idlulong; count: idlulong; arg: DOMString); safecall;

  end;

