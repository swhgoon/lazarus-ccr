type

  nsIPromptService = interface(nsISupports)
  ['{1630C61A-325E-49ca-8759-A31B16C47AA5}']
    procedure alert(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar); safecall;
    procedure alertCheck(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool); safecall;
    function confirm(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar) : longbool; safecall;
    function confirmCheck(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool) : longbool; safecall;
    function confirmEx(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; aButtonFlags: idlulong; aButton0Title: PWideChar; aButton1Title: PWideChar; aButton2Title: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool) : PRInt32; safecall;
    function prompt(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; var aValue: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool) : longbool; safecall;
    function promptUsernameAndPassword(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; var aUsername: PWideChar; var aPassword: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool) : longbool; safecall;
    function promptPassword(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; var aPassword: PWideChar; aCheckMsg: PWideChar; var aCheckState: longbool) : longbool; safecall;
    function select(aParent: nsIDOMWindow; aDialogTitle: PWideChar; aText: PWideChar; aCount: PRUint32; aSelectList: PWideChar; out aOutSelection: idllong) : longbool; safecall;

  end;

const
  nsIPromptService_BUTTON_POS_0=1;
  nsIPromptService_BUTTON_POS_1=1 << 8;
  nsIPromptService_BUTTON_POS_2=1 << 16;
  nsIPromptService_BUTTON_TITLE_OK=1;
  nsIPromptService_BUTTON_TITLE_CANCEL=2;
  nsIPromptService_BUTTON_TITLE_YES=3;
  nsIPromptService_BUTTON_TITLE_NO=4;
  nsIPromptService_BUTTON_TITLE_SAVE=5;
  nsIPromptService_BUTTON_TITLE_DONT_SAVE=6;
  nsIPromptService_BUTTON_TITLE_REVERT=7;
  nsIPromptService_BUTTON_TITLE_IS_STRING=127;
  nsIPromptService_BUTTON_POS_0_DEFAULT=0;
  nsIPromptService_BUTTON_POS_1_DEFAULT=1 << 24;
  nsIPromptService_BUTTON_POS_2_DEFAULT=1 << 25;
  nsIPromptService_BUTTON_DELAY_ENABLE=1 << 26;
  nsIPromptService_STD_OK_CANCEL_BUTTONS=(nsIPromptService_BUTTON_TITLE_OK     * nsIPromptService_BUTTON_POS_0) +
                                                   (nsIPromptService_BUTTON_TITLE_CANCEL * nsIPromptService_BUTTON_POS_1);
  nsIPromptService_STD_YES_NO_BUTTONS=(nsIPromptService_BUTTON_TITLE_YES * nsIPromptService_BUTTON_POS_0) +
                                                   (nsIPromptService_BUTTON_TITLE_NO  * nsIPromptService_BUTTON_POS_1);

