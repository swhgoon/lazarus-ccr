type

  nsIDocShellTreeOwner = interface(nsISupports)
  ['{932f9f93-8e21-4728-a527-cafc64b4d831}']
    function findItemWithName(name: PWideChar; aRequestor: nsIDocShellTreeItem; aOriginalRequestor: nsIDocShellTreeItem) : nsIDocShellTreeItem; safecall;
    procedure contentShellAdded(aContentShell: nsIDocShellTreeItem; aPrimary: longbool; aTargetable: longbool; aID: AString); safecall;
    procedure contentShellRemoved(aContentShell: nsIDocShellTreeItem); safecall;
    function GetprimaryContentShell(): nsIDocShellTreeItem; safecall;
    property primaryContentShell : nsIDocShellTreeItem read GetprimaryContentShell;
    procedure sizeShellTo(shell: nsIDocShellTreeItem; cx: idllong; cy: idllong); safecall;
    procedure setPersistence(aPersistPosition: longbool; aPersistSize: longbool; aPersistSizeMode: longbool); safecall;
    procedure getPersistence(out aPersistPosition: longbool; out aPersistSize: longbool; out aPersistSizeMode: longbool); safecall;
    function GettargetableShellCount(): idlulong; safecall;
    property targetableShellCount : idlulong read GettargetableShellCount;

  end;

