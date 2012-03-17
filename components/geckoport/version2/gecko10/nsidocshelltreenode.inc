type

  nsIDocShellTreeNode = interface(nsISupports)
  ['{37f1ab73-f224-44b1-82f0-d2834ab1cec0}']
    function GetchildCount(): idllong; safecall;
    property childCount : idllong read GetchildCount;
    procedure addChild(child: nsIDocShellTreeItem); safecall;
    procedure removeChild(child: nsIDocShellTreeItem); safecall;
    function getChildAt(index: idllong) : nsIDocShellTreeItem; safecall;
    function findChildWithName(aName: PWideChar; aRecurse: longbool; aSameType: longbool; aRequestor: nsIDocShellTreeItem; aOriginalRequestor: nsIDocShellTreeItem) : nsIDocShellTreeItem; safecall;

  end;

