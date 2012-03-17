type

  nsIDOMCrypto = interface(nsISupports)
  ['{12b6d899-2aed-4ea9-8c02-2223ab7ab592}']
    function Getversion(): DOMString; safecall;
    property version : DOMString read Getversion;
    function GetenableSmartCardEvents(): longbool; safecall;
    procedure SetenableSmartCardEvents(aenableSmartCardEvents: longbool); safecall;
    property enableSmartCardEvents : longbool read GetenableSmartCardEvents write SetenableSmartCardEvents;
    function generateCRMFRequest() : nsIDOMCRMFObject; safecall;
    function importUserCertificates(nickname: DOMString; cmmfResponse: DOMString; doForcedBackup: longbool) : DOMString; safecall;
    function popChallengeResponse(challenge: DOMString) : DOMString; safecall;
    function random(numBytes: idllong) : DOMString; safecall;
    function signText(stringToSign: DOMString; caOption: DOMString) : DOMString; safecall;
    procedure logout(); safecall;
    procedure disableRightClick(); safecall;

  end;

