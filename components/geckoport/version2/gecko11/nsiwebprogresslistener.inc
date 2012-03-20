type

  nsIWebProgressListener = interface(nsISupports)
  ['{a0cda7e4-c6ca-11e0-b6a5-001320257da5}']
    procedure onStateChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStateFlags: idlulong; aStatus: nsresult); safecall;
    procedure onProgressChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aCurSelfProgress: idllong; aMaxSelfProgress: idllong; aCurTotalProgress: idllong; aMaxTotalProgress: idllong); safecall;
    procedure onLocationChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aLocation: nsIURI; aFlags: idlulong); safecall;
    procedure onStatusChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStatus: nsresult; aMessage: PWideChar); safecall;
    procedure onSecurityChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aState: idlulong); safecall;

  end;

const
  nsIWebProgressListener_STATE_START=$00000001;
  nsIWebProgressListener_STATE_REDIRECTING=$00000002;
  nsIWebProgressListener_STATE_TRANSFERRING=$00000004;
  nsIWebProgressListener_STATE_NEGOTIATING=$00000008;
  nsIWebProgressListener_STATE_STOP=$00000010;
  nsIWebProgressListener_STATE_IS_REQUEST=$00010000;
  nsIWebProgressListener_STATE_IS_DOCUMENT=$00020000;
  nsIWebProgressListener_STATE_IS_NETWORK=$00040000;
  nsIWebProgressListener_STATE_IS_WINDOW=$00080000;
  nsIWebProgressListener_STATE_RESTORING=$01000000;
  nsIWebProgressListener_STATE_IS_INSECURE=$00000004;
  nsIWebProgressListener_STATE_IS_BROKEN=$00000001;
  nsIWebProgressListener_STATE_IS_SECURE=$00000002;
  nsIWebProgressListener_STATE_SECURE_HIGH=$00040000;
  nsIWebProgressListener_STATE_SECURE_MED=$00010000;
  nsIWebProgressListener_STATE_SECURE_LOW=$00020000;
  nsIWebProgressListener_STATE_IDENTITY_EV_TOPLEVEL=$00100000;
  nsIWebProgressListener_LOCATION_CHANGE_SAME_DOCUMENT=$00000001;

