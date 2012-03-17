type

  nsIWebBrowserSetup = interface(nsISupports)
  ['{F15398A0-8018-11d3-AF70-00A024FFC08C}']
    procedure setProperty(aId: idlulong; aValue: idlulong); safecall;

  end;

const
  nsIWebBrowserSetup_SETUP_ALLOW_PLUGINS=1;
  nsIWebBrowserSetup_SETUP_ALLOW_JAVASCRIPT=2;
  nsIWebBrowserSetup_SETUP_ALLOW_META_REDIRECTS=3;
  nsIWebBrowserSetup_SETUP_ALLOW_SUBFRAMES=4;
  nsIWebBrowserSetup_SETUP_ALLOW_IMAGES=5;
  nsIWebBrowserSetup_SETUP_FOCUS_DOC_BEFORE_CONTENT=6;
  nsIWebBrowserSetup_SETUP_USE_GLOBAL_HISTORY=256;
  nsIWebBrowserSetup_SETUP_IS_CHROME_WRAPPER=7;
  nsIWebBrowserSetup_SETUP_ALLOW_DNS_PREFETCH=8;

