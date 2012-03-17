type

  nsIWebBrowserChromeFocus = interface(nsISupports)
  ['{d2206418-1dd1-11b2-8e55-acddcd2bcfb8}']
    procedure focusNextElement(); safecall;
    procedure focusPrevElement(); safecall;

  end;

