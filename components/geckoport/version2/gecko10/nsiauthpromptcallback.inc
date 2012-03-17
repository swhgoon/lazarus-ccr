type

  nsIAuthPromptCallback = interface(nsISupports)
  ['{bdc387d7-2d29-4cac-92f1-dd75d786631d}']
    procedure onAuthAvailable(aContext: nsISupports; aAuthInfo: nsIAuthInformation); safecall;
    procedure onAuthCancelled(aContext: nsISupports; userCancel: longbool); safecall;

  end;

