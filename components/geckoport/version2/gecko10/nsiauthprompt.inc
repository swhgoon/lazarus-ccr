type

  nsIAuthPrompt = interface(nsISupports)
  ['{358089f9-ee4b-4711-82fd-bcd07fc62061}']
    function prompt(dialogTitle: PWideChar; text: PWideChar; passwordRealm: PWideChar; savePassword: PRUint32; defaultText: PWideChar; out result: PWideChar) : longbool; safecall;
    function promptUsernameAndPassword(dialogTitle: PWideChar; text: PWideChar; passwordRealm: PWideChar; savePassword: PRUint32; var user: PWideChar; var pwd: PWideChar) : longbool; safecall;
    function promptPassword(dialogTitle: PWideChar; text: PWideChar; passwordRealm: PWideChar; savePassword: PRUint32; var pwd: PWideChar) : longbool; safecall;

  end;

const
  nsIAuthPrompt_SAVE_PASSWORD_NEVER=0;
  nsIAuthPrompt_SAVE_PASSWORD_FOR_SESSION=1;
  nsIAuthPrompt_SAVE_PASSWORD_PERMANENTLY=2;

