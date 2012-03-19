type

  nsIDOMCrypto = interface(nsISupports)
  ['{12b6d899-2aed-4ea9-8c02-2223ab7ab592}']
    procedure Getversion( result_: DOMString); safecall;
    function GetenableSmartCardEvents(): longbool; safecall;
    procedure SetenableSmartCardEvents(aenableSmartCardEvents: longbool); safecall;
    property enableSmartCardEvents : longbool read GetenableSmartCardEvents write SetenableSmartCardEvents;
    function generateCRMFRequest() : nsIDOMCRMFObject; safecall;
    procedure importUserCertificates(nickname: DOMString; cmmfResponse: DOMString; doForcedBackup: longbool; result_: DOMString); safecall;
    procedure popChallengeResponse(challenge: DOMString; result_: DOMString); safecall;
    procedure random(numBytes: idllong; result_: DOMString); safecall;
    procedure signText(stringToSign: DOMString; caOption: DOMString; result_: DOMString); safecall;
    procedure logout(); safecall;
    procedure disableRightClick(); safecall;

  end;

