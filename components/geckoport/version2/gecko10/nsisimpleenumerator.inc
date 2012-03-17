type

  nsISimpleEnumerator = interface(nsISupports)
  ['{D1899240-F9D2-11D2-BDD6-000064657374}']
    function hasMoreElements() : longbool; safecall;
    function getNext() : nsISupports; safecall;

  end;

