type

  nsIWebBrowserChrome = interface(nsISupports)
  ['{BA434C60-9D52-11d3-AFB0-00A024FFC08C}']
    procedure setStatus(statusType: idlulong; status: PWideChar); safecall;
    function GetwebBrowser(): nsIWebBrowser; safecall;
    procedure SetwebBrowser(awebBrowser: nsIWebBrowser); safecall;
    property webBrowser : nsIWebBrowser read GetwebBrowser write SetwebBrowser;
    function GetchromeFlags(): idlulong; safecall;
    procedure SetchromeFlags(achromeFlags: idlulong); safecall;
    property chromeFlags : idlulong read GetchromeFlags write SetchromeFlags;
    procedure destroyBrowserWindow(); safecall;
    procedure sizeBrowserTo(aCX: idllong; aCY: idllong); safecall;
    procedure showAsModal(); safecall;
    function isWindowModal() : longbool; safecall;
    procedure exitModalEventLoop(aStatus: nsresult); safecall;

  end;

const
  nsIWebBrowserChrome_STATUS_SCRIPT=$00000001;
  nsIWebBrowserChrome_STATUS_SCRIPT_DEFAULT=$00000002;
  nsIWebBrowserChrome_STATUS_LINK=$00000003;
  nsIWebBrowserChrome_CHROME_DEFAULT=$00000001;
  nsIWebBrowserChrome_CHROME_WINDOW_BORDERS=$00000002;
  nsIWebBrowserChrome_CHROME_WINDOW_CLOSE=$00000004;
  nsIWebBrowserChrome_CHROME_WINDOW_RESIZE=$00000008;
  nsIWebBrowserChrome_CHROME_MENUBAR=$00000010;
  nsIWebBrowserChrome_CHROME_TOOLBAR=$00000020;
  nsIWebBrowserChrome_CHROME_LOCATIONBAR=$00000040;
  nsIWebBrowserChrome_CHROME_STATUSBAR=$00000080;
  nsIWebBrowserChrome_CHROME_PERSONAL_TOOLBAR=$00000100;
  nsIWebBrowserChrome_CHROME_SCROLLBARS=$00000200;
  nsIWebBrowserChrome_CHROME_TITLEBAR=$00000400;
  nsIWebBrowserChrome_CHROME_EXTRA=$00000800;
  nsIWebBrowserChrome_CHROME_WITH_SIZE=$00001000;
  nsIWebBrowserChrome_CHROME_WITH_POSITION=$00002000;
  nsIWebBrowserChrome_CHROME_WINDOW_MIN=$00004000;
  nsIWebBrowserChrome_CHROME_WINDOW_POPUP=$00008000;
  nsIWebBrowserChrome_CHROME_WINDOW_RAISED=$02000000;
  nsIWebBrowserChrome_CHROME_WINDOW_LOWERED=$04000000;
  nsIWebBrowserChrome_CHROME_CENTER_SCREEN=$08000000;
  nsIWebBrowserChrome_CHROME_DEPENDENT=$10000000;
  nsIWebBrowserChrome_CHROME_MODAL=$20000000;
  nsIWebBrowserChrome_CHROME_OPENAS_DIALOG=$40000000;
  nsIWebBrowserChrome_CHROME_OPENAS_CHROME=$80000000;
  nsIWebBrowserChrome_CHROME_ALL=$00000ffe;

