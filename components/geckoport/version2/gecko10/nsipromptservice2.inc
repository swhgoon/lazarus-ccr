type

  nsIPromptService2 = interface(nsIPromptService)
  ['{cf86d196-dbee-4482-9dfa-3477aa128319}']
    function promptAuth(aParent: nsIDOMWindow; aChannel: nsIChannel; level: PRUint32; authInfo: nsIAuthInformation; checkboxLabel: PWideChar; var checkValue: longbool) : longbool; safecall;
    function asyncPromptAuth(aParent: nsIDOMWindow; aChannel: nsIChannel; aCallback: nsIAuthPromptCallback; aContext: nsISupports; level: PRUint32; authInfo: nsIAuthInformation; checkboxLabel: PWideChar; var checkValue: longbool) : nsICancelable; safecall;

  end;

