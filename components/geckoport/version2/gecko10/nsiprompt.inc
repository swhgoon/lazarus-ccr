type

  nsIPrompt = interface(nsISupports)
  ['{a63f70c0-148b-11d3-9333-00104ba0fd40}']
    procedure alert(dialogTitle: PWideChar; text: PWideChar); safecall;
    procedure alertCheck(dialogTitle: PWideChar; text: PWideChar; checkMsg: PWideChar; var checkValue: longbool); safecall;
    function confirm(dialogTitle: PWideChar; text: PWideChar) : longbool; safecall;
    function confirmCheck(dialogTitle: PWideChar; text: PWideChar; checkMsg: PWideChar; var checkValue: longbool) : longbool; safecall;
    function confirmEx(dialogTitle: PWideChar; text: PWideChar; buttonFlags: idlulong; button0Title: PWideChar; button1Title: PWideChar; button2Title: PWideChar; checkMsg: PWideChar; var checkValue: longbool) : PRInt32; safecall;
    function prompt(dialogTitle: PWideChar; text: PWideChar; var value: PWideChar; checkMsg: PWideChar; var checkValue: longbool) : longbool; safecall;
    function promptPassword(dialogTitle: PWideChar; text: PWideChar; var password: PWideChar; checkMsg: PWideChar; var checkValue: longbool) : longbool; safecall;
    function promptUsernameAndPassword(dialogTitle: PWideChar; text: PWideChar; var username: PWideChar; var password: PWideChar; checkMsg: PWideChar; var checkValue: longbool) : longbool; safecall;
    function select(dialogTitle: PWideChar; text: PWideChar; count: PRUint32; selectList: PWideChar; out outSelection: idllong) : longbool; safecall;

  end;

const
  nsIPrompt_BUTTON_POS_0=1;
  nsIPrompt_BUTTON_POS_1=1 << 8;
  nsIPrompt_BUTTON_POS_2=1 << 16;
  nsIPrompt_BUTTON_TITLE_OK=1;
  nsIPrompt_BUTTON_TITLE_CANCEL=2;
  nsIPrompt_BUTTON_TITLE_YES=3;
  nsIPrompt_BUTTON_TITLE_NO=4;
  nsIPrompt_BUTTON_TITLE_SAVE=5;
  nsIPrompt_BUTTON_TITLE_DONT_SAVE=6;
  nsIPrompt_BUTTON_TITLE_REVERT=7;
  nsIPrompt_BUTTON_TITLE_IS_STRING=127;
  nsIPrompt_BUTTON_POS_0_DEFAULT=0 << 24;
  nsIPrompt_BUTTON_POS_1_DEFAULT=1 << 24;
  nsIPrompt_BUTTON_POS_2_DEFAULT=2 << 24;
  nsIPrompt_BUTTON_DELAY_ENABLE=1 << 26;
  nsIPrompt_STD_OK_CANCEL_BUTTONS=(nsIPrompt_BUTTON_TITLE_OK * nsIPrompt_BUTTON_POS_0) +
                                                    (nsIPrompt_BUTTON_TITLE_CANCEL * nsIPrompt_BUTTON_POS_1);
  nsIPrompt_STD_YES_NO_BUTTONS=(nsIPrompt_BUTTON_TITLE_YES * nsIPrompt_BUTTON_POS_0) +
                                                    (nsIPrompt_BUTTON_TITLE_NO * nsIPrompt_BUTTON_POS_1);

