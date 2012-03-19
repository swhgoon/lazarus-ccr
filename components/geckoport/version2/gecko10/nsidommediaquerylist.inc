type

  nsIDOMMediaQueryList = interface(nsISupports)
  ['{e0e49c52-915b-40f9-9cba-6026305cdf3e}']
    procedure Getmedia( result_: DOMString); safecall;
    function Getmatches(): longbool; safecall;
    property matches : longbool read Getmatches;
    procedure addListener(listener: nsIDOMMediaQueryListListener); safecall;
    procedure removeListener(listener: nsIDOMMediaQueryListListener); safecall;

  end;


  nsIDOMMediaQueryListListener = interface(nsISupports)
  ['{279a5cbd-5c15-475d-847b-e0de1624eb77}']
    procedure handleChange(mql: nsIDOMMediaQueryList); safecall;

  end;

