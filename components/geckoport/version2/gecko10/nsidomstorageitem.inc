type

  nsIDOMStorageItem = interface(nsISupports)
  ['{0CC37C78-4C5F-48E1-ADFC-7480B8FE9DC4}']
    function Getsecure(): longbool; safecall;
    procedure Setsecure(asecure: longbool); safecall;
    property secure : longbool read Getsecure write Setsecure;
    function Getvalue(): DOMString; safecall;
    procedure Setvalue(avalue: DOMString); safecall;
    property value : DOMString read Getvalue write Setvalue;

  end;

