type

  nsISHistoryListener = interface(nsISupports)
  ['{3b07f591-e8e1-11d4-9882-00c04fa02f40}']
    procedure OnHistoryNewEntry(aNewURI: nsIURI); safecall;
    function OnHistoryGoBack(aBackURI: nsIURI) : longbool; safecall;
    function OnHistoryGoForward(aForwardURI: nsIURI) : longbool; safecall;
    function OnHistoryReload(aReloadURI: nsIURI; aReloadFlags: idlulong) : longbool; safecall;
    function OnHistoryGotoIndex(aIndex: idllong; aGotoURI: nsIURI) : longbool; safecall;
    function OnHistoryPurge(aNumEntries: idllong) : longbool; safecall;

  end;

