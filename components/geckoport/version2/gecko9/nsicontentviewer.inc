type
  nsIntRect = record
  end;

type


  nsIContentViewer = interface(nsISupports)
  ['{75306a89-e3ad-4a2b-9daf-ac4de06661a4}']
{    procedure init(aParentWidget: nsIWidgetPtr; aBounds: nsIntRectRef); safecall;
    function Getcontainer(): nsISupports; safecall;
    procedure Setcontainer(acontainer: nsISupports); safecall;
    property container : nsISupports read Getcontainer write Setcontainer;
    procedure loadStart(aDoc: nsISupports); safecall;
    procedure loadComplete(aStatus: idlulong); safecall;
    function permitUnload(aCallerClosesWindow: longbool) : longbool; safecall;
    procedure resetCloseWindow(); safecall;
    procedure pageHide(isUnload: longbool); safecall;
    procedure close(ahistoryEntry: nsISHEntry); safecall;
    procedure destroy(); safecall;
    procedure stop(); safecall;
    function GetDOMDocument(): nsIDOMDocument; safecall;
    procedure SetDOMDocument(aDOMDocument: nsIDOMDocument); safecall;
    property DOMDocument : nsIDOMDocument read GetDOMDocument write SetDOMDocument;
    function getDocument() : nsIDocumentPtr; safecall;
    procedure getBounds(aBounds: nsIntRectRef); safecall;
    procedure setBounds(aBounds: nsIntRectRef); safecall;
    function GetpreviousViewer(): nsIContentViewer; safecall;
    procedure SetpreviousViewer(apreviousViewer: nsIContentViewer); safecall;
    property previousViewer : nsIContentViewer read GetpreviousViewer write SetpreviousViewer;
    procedure move(aX: idllong; aY: idllong); safecall;
    procedure show(); safecall;
    procedure hide(); safecall;
    function Getsticky(): longbool; safecall;
    procedure Setsticky(asticky: longbool); safecall;
    property sticky : longbool read Getsticky write Setsticky;
    function requestWindowClose() : longbool; safecall;
    procedure open(aState: nsISupports; aSHEntry: nsISHEntry); safecall;
    procedure clearHistoryEntry(); safecall;
    procedure setPageMode(aPageMode: longbool; aPrintSettings: nsIPrintSettings); safecall;
    function GethistoryEntry(): nsISHEntry; safecall;
    property historyEntry : nsISHEntry read GethistoryEntry;
    function GetisTabModalPromptAllowed(): longbool; safecall;
    property isTabModalPromptAllowed : longbool read GetisTabModalPromptAllowed;
}
  end;

