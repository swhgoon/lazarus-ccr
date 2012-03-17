type

  nsIWebProgress = interface(nsISupports)
  ['{570F39D0-EFD0-11d3-B093-00A024FFC08C}']
    procedure addProgressListener(aListener: nsIWebProgressListener; aNotifyMask: idlulong); safecall;
    procedure removeProgressListener(aListener: nsIWebProgressListener); safecall;
    function GetDOMWindow(): nsIDOMWindow; safecall;
    property DOMWindow : nsIDOMWindow read GetDOMWindow;
    function GetisLoadingDocument(): longbool; safecall;
    property isLoadingDocument : longbool read GetisLoadingDocument;

  end;

const
  nsIWebProgress_NOTIFY_STATE_REQUEST=$00000001;
  nsIWebProgress_NOTIFY_STATE_DOCUMENT=$00000002;
  nsIWebProgress_NOTIFY_STATE_NETWORK=$00000004;
  nsIWebProgress_NOTIFY_STATE_WINDOW=$00000008;
  nsIWebProgress_NOTIFY_STATE_ALL=$0000000f;
  nsIWebProgress_NOTIFY_PROGRESS=$00000010;
  nsIWebProgress_NOTIFY_STATUS=$00000020;
  nsIWebProgress_NOTIFY_SECURITY=$00000040;
  nsIWebProgress_NOTIFY_LOCATION=$00000080;
  nsIWebProgress_NOTIFY_REFRESH=$00000100;
  nsIWebProgress_NOTIFY_ALL=$000001ff;

