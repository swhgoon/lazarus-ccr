type

  nsIDOMCharacterData = interface(nsIDOMNode)
  ['{cb75c251-afc7-444f-b2d6-b9635555f3ed}']
    procedure Getdata( result_: DOMString); safecall;
    procedure Setdata(adata: DOMString); safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    procedure substringData(offset: idlulong; count: idlulong; result_: DOMString); safecall;
    procedure appendData(arg: DOMString); safecall;
    procedure insertData(offset: idlulong; arg: DOMString); safecall;
    procedure deleteData(offset: idlulong; count: idlulong); safecall;
    procedure replaceData(offset: idlulong; count: idlulong; arg: DOMString); safecall;

  end;

