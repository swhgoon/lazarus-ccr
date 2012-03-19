type

  nsIWebBrowser = interface(nsISupports)
  ['{33e9d001-caab-4ba9-8961-54902f197202}']
    procedure addWebBrowserListener(aListener: nsIWeakReference; constref aIID: TGuid); safecall;
    procedure removeWebBrowserListener(aListener: nsIWeakReference; constref aIID: TGuid); safecall;
    function GetcontainerWindow(): nsIWebBrowserChrome; safecall;
    procedure SetcontainerWindow(acontainerWindow: nsIWebBrowserChrome); safecall;
    property containerWindow : nsIWebBrowserChrome read GetcontainerWindow write SetcontainerWindow;
    function GetparentURIContentListener(): nsIURIContentListener; safecall;
    procedure SetparentURIContentListener(aparentURIContentListener: nsIURIContentListener); safecall;
    property parentURIContentListener : nsIURIContentListener read GetparentURIContentListener write SetparentURIContentListener;
    function GetcontentDOMWindow(): nsIDOMWindow; safecall;
    property contentDOMWindow : nsIDOMWindow read GetcontentDOMWindow;
    function GetisActive(): longbool; safecall;
    procedure SetisActive(aisActive: longbool); safecall;
    property isActive : longbool read GetisActive write SetisActive;

  end;

