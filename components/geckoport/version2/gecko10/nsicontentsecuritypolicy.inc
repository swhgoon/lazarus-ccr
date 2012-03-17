type

  nsIContentSecurityPolicy = interface(nsISupports)
  ['{AB36A2BF-CB32-4AA6-AB41-6B4E4444A221}']
    function GetisInitialized(): longbool; safecall;
    procedure SetisInitialized(aisInitialized: longbool); safecall;
    property isInitialized : longbool read GetisInitialized write SetisInitialized;
    function GetreportOnlyMode(): longbool; safecall;
    procedure SetreportOnlyMode(areportOnlyMode: longbool); safecall;
    property reportOnlyMode : longbool read GetreportOnlyMode write SetreportOnlyMode;
    function Getpolicy(): AString; safecall;
    property policy : AString read Getpolicy;
    function GetallowsInlineScript(): longbool; safecall;
    property allowsInlineScript : longbool read GetallowsInlineScript;
    function GetallowsEval(): longbool; safecall;
    property allowsEval : longbool read GetallowsEval;
    procedure logViolationDetails(violationType: idlushort; sourceFile: AString; scriptSample: AString; lineNum: PRInt32); safecall;
    procedure sendReports(blockedURI: AString; violatedDirective: AString; scriptSample: AString; lineNum: PRInt32); safecall;
    procedure scanRequestData(aChannel: nsIHttpChannel); safecall;
    procedure refinePolicy(policyString: AString; selfURI: nsIURI); safecall;
    function permitsAncestry(docShell: nsIDocShell) : longbool; safecall;
    function shouldLoad(aContentType: idlulong; aContentLocation: nsIURI; aRequestOrigin: nsIURI; aContext: nsISupports; aMimeTypeGuess: ACString; aExtra: nsISupports) : idlshort; safecall;
    function shouldProcess(aContentType: idlulong; aContentLocation: nsIURI; aRequestOrigin: nsIURI; aContext: nsISupports; aMimeType: ACString; aExtra: nsISupports) : idlshort; safecall;

  end;

const
  nsIContentSecurityPolicy_VIOLATION_TYPE_INLINE_SCRIPT=1;
  nsIContentSecurityPolicy_VIOLATION_TYPE_EVAL=2;

