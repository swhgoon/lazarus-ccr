type

  nsIDocShellTreeItem = interface(nsIDocShellTreeNode)
  ['{09b54ec1-d98a-49a9-bc95-3219e8b55089}']
    function Getname(): PWideChar; safecall;
    procedure Setname(aname: PWideChar); safecall;
    property name : PWideChar read Getname write Setname;
    function nameEquals(aname: PWideChar) : longbool; safecall;
    function GetitemType(): idllong; safecall;
    procedure SetitemType(aitemType: idllong); safecall;
    property itemType : idllong read GetitemType write SetitemType;
    function Getparent(): nsIDocShellTreeItem; safecall;
    property parent : nsIDocShellTreeItem read Getparent;
    function GetsameTypeParent(): nsIDocShellTreeItem; safecall;
    property sameTypeParent : nsIDocShellTreeItem read GetsameTypeParent;
    function GetrootTreeItem(): nsIDocShellTreeItem; safecall;
    property rootTreeItem : nsIDocShellTreeItem read GetrootTreeItem;
    function GetsameTypeRootTreeItem(): nsIDocShellTreeItem; safecall;
    property sameTypeRootTreeItem : nsIDocShellTreeItem read GetsameTypeRootTreeItem;
    function findItemWithName(aname: PWideChar; aRequestor: nsISupports; aOriginalRequestor: nsIDocShellTreeItem) : nsIDocShellTreeItem; safecall;
    function GettreeOwner(): nsIDocShellTreeOwner; safecall;
    property treeOwner : nsIDocShellTreeOwner read GettreeOwner;
    procedure setTreeOwner(atreeOwner: nsIDocShellTreeOwner); safecall;

  end;

const
  nsIDocShellTreeItem_typeChrome=0;
  nsIDocShellTreeItem_typeContent=1;
  nsIDocShellTreeItem_typeContentWrapper=2;
  nsIDocShellTreeItem_typeChromeWrapper=3;
  nsIDocShellTreeItem_typeAll=$7FFFFFFF;

