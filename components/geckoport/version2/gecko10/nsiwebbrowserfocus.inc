type

  nsIWebBrowserFocus = interface(nsISupports)
  ['{9c5d3c58-1dd1-11b2-a1c9-f3699284657a}']
    procedure activate(); safecall;
    procedure deactivate(); safecall;
    procedure setFocusAtFirstElement(); safecall;
    procedure setFocusAtLastElement(); safecall;
    function GetfocusedWindow(): nsIDOMWindow; safecall;
    procedure SetfocusedWindow(afocusedWindow: nsIDOMWindow); safecall;
    property focusedWindow : nsIDOMWindow read GetfocusedWindow write SetfocusedWindow;
    function GetfocusedElement(): nsIDOMElement; safecall;
    procedure SetfocusedElement(afocusedElement: nsIDOMElement); safecall;
    property focusedElement : nsIDOMElement read GetfocusedElement write SetfocusedElement;

  end;

