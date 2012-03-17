type

  nsISHEntry = interface(nsIHistoryEntry)
  ['{6443FD72-A50F-4B8B-BB82-BB1FA04CB15D}']
    procedure setURI(anaURI: nsIURI); safecall;
    function GetreferrerURI(): nsIURI; safecall;
    procedure SetreferrerURI(areferrerURI: nsIURI); safecall;
    property referrerURI : nsIURI read GetreferrerURI write SetreferrerURI;
    function GetcontentViewer(): nsIContentViewer; safecall;
    procedure SetcontentViewer(acontentViewer: nsIContentViewer); safecall;
    property contentViewer : nsIContentViewer read GetcontentViewer write SetcontentViewer;
    function Getsticky(): longbool; safecall;
    procedure Setsticky(asticky: longbool); safecall;
    property sticky : longbool read Getsticky write Setsticky;
    function GetwindowState(): nsISupports; safecall;
    procedure SetwindowState(awindowState: nsISupports); safecall;
    property windowState : nsISupports read GetwindowState write SetwindowState;
    procedure getViewerBounds(abounds: nsIntRect); safecall;
    procedure setViewerBounds(abounds: nsIntRect); safecall;
    procedure addChildShell(ashell: nsIDocShellTreeItem); safecall;
    function childShellAt(anindex: idllong) : nsIDocShellTreeItem; safecall;
    procedure clearChildShells(); safecall;
    function GetrefreshURIList(): nsISupportsArray; safecall;
    procedure SetrefreshURIList(arefreshURIList: nsISupportsArray); safecall;
    property refreshURIList : nsISupportsArray read GetrefreshURIList write SetrefreshURIList;
    procedure syncPresentationState(); safecall;
    procedure setTitle(anaTitle: AString); safecall;
    function GetpostData(): nsIInputStream; safecall;
    procedure SetpostData(apostData: nsIInputStream); safecall;
    property postData : nsIInputStream read GetpostData write SetpostData;
    function GetlayoutHistoryState(): nsILayoutHistoryState; safecall;
    procedure SetlayoutHistoryState(alayoutHistoryState: nsILayoutHistoryState); safecall;
    property layoutHistoryState : nsILayoutHistoryState read GetlayoutHistoryState write SetlayoutHistoryState;
    function Getparent(): nsISHEntry; safecall;
    procedure Setparent(aparent: nsISHEntry); safecall;
    property parent : nsISHEntry read Getparent write Setparent;
    function GetloadType(): idlulong; safecall;
    procedure SetloadType(aloadType: idlulong); safecall;
    property loadType : idlulong read GetloadType write SetloadType;
    function GetID(): idlulong; safecall;
    procedure SetID(aID: idlulong); safecall;
    property ID : idlulong read GetID write SetID;
    function GetcacheKey(): nsISupports; safecall;
    procedure SetcacheKey(acacheKey: nsISupports); safecall;
    property cacheKey : nsISupports read GetcacheKey write SetcacheKey;
    function GetsaveLayoutStateFlag(): longbool; safecall;
    procedure SetsaveLayoutStateFlag(asaveLayoutStateFlag: longbool); safecall;
    property saveLayoutStateFlag : longbool read GetsaveLayoutStateFlag write SetsaveLayoutStateFlag;
    function GetexpirationStatus(): longbool; safecall;
    procedure SetexpirationStatus(aexpirationStatus: longbool); safecall;
    property expirationStatus : longbool read GetexpirationStatus write SetexpirationStatus;
    function GetcontentType(): ACString; safecall;
    procedure SetcontentType(acontentType: ACString); safecall;
    property contentType : ACString read GetcontentType write SetcontentType;
    function GetURIWasModified(): longbool; safecall;
    procedure SetURIWasModified(aURIWasModified: longbool); safecall;
    property URIWasModified : longbool read GetURIWasModified write SetURIWasModified;
    procedure setScrollPosition(ax: idllong; ay: idllong); safecall;
    procedure getScrollPosition(out ax: idllong; out ay: idllong); safecall;
    procedure create(aURI: nsIURI; atitle: AString; aninputStream: nsIInputStream; alayoutHistoryState: nsILayoutHistoryState; acacheKey: nsISupports; acontentType: ACString; anowner: nsISupports; adocshellID: idlulong; adynamicCreation: longbool); safecall;
    function clone() : nsISHEntry; safecall;
    procedure setIsSubFrame(anaFlag: longbool); safecall;
    function getAnyContentViewer(out anownerEntry: nsISHEntry) : nsIContentViewer; safecall;
    function Getowner(): nsISupports; safecall;
    procedure Setowner(aowner: nsISupports); safecall;
    property owner : nsISupports read Getowner write Setowner;
    function GetstateData(): nsIStructuredCloneContainer; safecall;
    procedure SetstateData(astateData: nsIStructuredCloneContainer); safecall;
    property stateData : nsIStructuredCloneContainer read GetstateData write SetstateData;
    function forgetEditorData() : nsDocShellEditorDataPtr; safecall;
    procedure setEditorData(anaData: nsDocShellEditorDataPtr); safecall;
    function hasDetachedEditor() : longbool; safecall;
    function isDynamicallyAdded() : longbool; safecall;
    function hasDynamicallyAddedChild() : longbool; safecall;
    function GetdocshellID(): idlulong; safecall;
    procedure SetdocshellID(adocshellID: idlulong); safecall;
    property docshellID : idlulong read GetdocshellID write SetdocshellID;
    function GetBFCacheEntry(): nsIBFCacheEntry; safecall;
    property BFCacheEntry : nsIBFCacheEntry read GetBFCacheEntry;
    function hasBFCacheEntry(anaEntry: nsIBFCacheEntry) : longbool; safecall;
    procedure adoptBFCacheEntry(anaEntry: nsISHEntry); safecall;
    procedure abandonBFCacheEntry(); safecall;
    function sharesDocumentWith(anaEntry: nsISHEntry) : longbool; safecall;

  end;


  nsISHEntryInternal = interface(nsISupports)
  ['{bb66ac35-253b-471f-a317-3ece940f04c5}']
    procedure RemoveFromBFCacheAsync(); safecall;
    procedure RemoveFromBFCacheSync(); safecall;
    function GetlastTouched(): idlulong; safecall;
    procedure SetlastTouched(alastTouched: idlulong); safecall;
    property lastTouched : idlulong read GetlastTouched write SetlastTouched;
    function getSharedState() : nsSHEntryShared; safecall;

  end;

