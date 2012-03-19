type

  nsIAtom = interface(nsISupports)
  ['{1f341018-521a-49de-b806-1bef5c9a00b0}']
    function toString() : AString; safecall;
    procedure toUTF8String( result_: AUTF8String); safecall;
    function equals(aString: AString) : longbool; safecall;
    function equalsUTF8(aString: AUTF8String) : longbool; safecall;
    function isStaticAtom() : longbool; safecall;

  end;

