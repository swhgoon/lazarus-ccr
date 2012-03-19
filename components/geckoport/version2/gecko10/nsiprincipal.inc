type

  nsIPrincipal = interface(nsISerializable)
  ['{B406A2DB-E547-4C95-B8E2-AD09ECB54CE0}']
    procedure getPreferences(out prefBranch: PAnsiChar; out id: PAnsiChar; out asubjectName: PAnsiChar; out grantedList: PAnsiChar; out deniedList: PAnsiChar; out isTrusted: longbool); safecall;
    function equals(other: nsIPrincipal) : longbool; safecall;
    function equalsIgnoringDomain(other: nsIPrincipal) : longbool; safecall;
    function GethashValue(): idlulong; safecall;
    property hashValue : idlulong read GethashValue;
    function getJSPrincipals(cx: JSContext) : JSPrincipals; safecall;
    function GetsecurityPolicy(): voidPtr; safecall;
    procedure SetsecurityPolicy(asecurityPolicy: voidPtr); safecall;
    property securityPolicy : voidPtr read GetsecurityPolicy write SetsecurityPolicy;
    function canEnableCapability(capability: PAnsiChar) : idlshort; safecall;
    procedure setCanEnableCapability(capability: PAnsiChar; canEnable: idlshort); safecall;
    function isCapabilityEnabled(capability: PAnsiChar; annotation: voidPtr) : longbool; safecall;
    procedure enableCapability(capability: PAnsiChar; var annotation: voidPtr); safecall;
    procedure revertCapability(capability: PAnsiChar; var annotation: voidPtr); safecall;
    procedure disableCapability(capability: PAnsiChar; var annotation: voidPtr); safecall;
    function GetURI(): nsIURI; safecall;
    property URI : nsIURI read GetURI;
    function Getdomain(): nsIURI; safecall;
    procedure Setdomain(adomain: nsIURI); safecall;
    property domain : nsIURI read Getdomain write Setdomain;
    function Getorigin(): PAnsiChar; safecall;
    property origin : PAnsiChar read Getorigin;
    function GethasCertificate(): longbool; safecall;
    property hasCertificate : longbool read GethasCertificate;
    procedure Getfingerprint( result_: AUTF8String); safecall;
    procedure GetprettyName( result_: AUTF8String); safecall;
    function subsumes(other: nsIPrincipal) : longbool; safecall;
    procedure checkMayLoad(anuri: nsIURI; report: longbool); safecall;
    procedure GetsubjectName( result_: AUTF8String); safecall;
    function Getcertificate(): nsISupports; safecall;
    property certificate : nsISupports read Getcertificate;
    function Getcsp(): nsIContentSecurityPolicy; safecall;
    procedure Setcsp(acsp: nsIContentSecurityPolicy); safecall;
    property csp : nsIContentSecurityPolicy read Getcsp write Setcsp;

  end;

const
  nsIPrincipal_ENABLE_DENIED=1;
  nsIPrincipal_ENABLE_UNKNOWN=2;
  nsIPrincipal_ENABLE_WITH_USER_PERMISSION=3;
  nsIPrincipal_ENABLE_GRANTED=4;

