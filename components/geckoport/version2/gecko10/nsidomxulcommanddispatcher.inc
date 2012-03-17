type

  nsIDOMXULCommandDispatcher = interface(nsISupports)
  ['{f3c50361-14fe-11d3-bf87-00105a1b0627}']
    function GetfocusedElement(): nsIDOMElement; safecall;
    procedure SetfocusedElement(afocusedElement: nsIDOMElement); safecall;
    property focusedElement : nsIDOMElement read GetfocusedElement write SetfocusedElement;
    function GetfocusedWindow(): nsIDOMWindow; safecall;
    procedure SetfocusedWindow(afocusedWindow: nsIDOMWindow); safecall;
    property focusedWindow : nsIDOMWindow read GetfocusedWindow write SetfocusedWindow;
    procedure addCommandUpdater(updater: nsIDOMElement; events: DOMString; targets: DOMString); safecall;
    procedure removeCommandUpdater(updater: nsIDOMElement); safecall;
    procedure updateCommands(eventName: DOMString); safecall;
    function getControllerForCommand(command: PAnsiChar) : nsIController; safecall;
    function getControllers() : nsIControllers; safecall;
    procedure advanceFocus(); safecall;
    procedure rewindFocus(); safecall;
    procedure advanceFocusIntoSubtree(elt: nsIDOMElement); safecall;
    function GetsuppressFocusScroll(): longbool; safecall;
    procedure SetsuppressFocusScroll(asuppressFocusScroll: longbool); safecall;
    property suppressFocusScroll : longbool read GetsuppressFocusScroll write SetsuppressFocusScroll;

  end;

