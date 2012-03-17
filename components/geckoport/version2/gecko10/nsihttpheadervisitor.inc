type

  nsIHttpHeaderVisitor = interface(nsISupports)
  ['{35412859-b9d9-423c-8866-2d4559fdd2be}']
    procedure visitHeader(aHeader: ACString; aValue: ACString); safecall;

  end;

