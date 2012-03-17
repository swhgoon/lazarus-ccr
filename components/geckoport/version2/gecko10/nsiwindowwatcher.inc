type

  nsIWindowWatcher = interface(nsISupports)
  ['{002286a8-494b-43b3-8ddd-49e3fc50622b}']
    function openWindow(aParent: nsIDOMWindow; aUrl: PAnsiChar; aName: PAnsiChar; aFeatures: PAnsiChar; aArguments: nsISupports) : nsIDOMWindow; safecall;
    procedure registerNotification(aObserver: nsIObserver); safecall;
    procedure unregisterNotification(aObserver: nsIObserver); safecall;
    function getWindowEnumerator() : nsISimpleEnumerator; safecall;
    function getNewPrompter(aParent: nsIDOMWindow) : nsIPrompt; safecall;
    function getNewAuthPrompter(aParent: nsIDOMWindow) : nsIAuthPrompt; safecall;
    procedure setWindowCreator(creator: nsIWindowCreator); safecall;
    function getChromeForWindow(aWindow: nsIDOMWindow) : nsIWebBrowserChrome; safecall;
    function getWindowByName(aTargetName: PWideChar; aCurrentWindow: nsIDOMWindow) : nsIDOMWindow; safecall;
    function GetactiveWindow(): nsIDOMWindow; safecall;
    procedure SetactiveWindow(aactiveWindow: nsIDOMWindow); safecall;
    property activeWindow : nsIDOMWindow read GetactiveWindow write SetactiveWindow;

  end;

