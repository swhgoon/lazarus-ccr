type

  nsIWindowCreator = interface(nsISupports)
  ['{30465632-A777-44cc-90F9-8145475EF999}']
    function createChromeWindow(parent: nsIWebBrowserChrome; chromeFlags: PRUint32) : nsIWebBrowserChrome; safecall;

  end;

