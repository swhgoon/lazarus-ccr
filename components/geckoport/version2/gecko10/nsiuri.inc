type

  nsIURI = interface(nsISupports)
  ['{395fe045-7d18-4adb-a3fd-af98c8a1af11}']
    function Getspec(): AUTF8String; safecall;
    procedure Setspec(aspec: AUTF8String); safecall;
    property spec : AUTF8String read Getspec write Setspec;
    function GetprePath(): AUTF8String; safecall;
    property prePath : AUTF8String read GetprePath;
    function Getscheme(): ACString; safecall;
    procedure Setscheme(ascheme: ACString); safecall;
    property scheme : ACString read Getscheme write Setscheme;
    function GetuserPass(): AUTF8String; safecall;
    procedure SetuserPass(auserPass: AUTF8String); safecall;
    property userPass : AUTF8String read GetuserPass write SetuserPass;
    function Getusername(): AUTF8String; safecall;
    procedure Setusername(ausername: AUTF8String); safecall;
    property username : AUTF8String read Getusername write Setusername;
    function Getpassword(): AUTF8String; safecall;
    procedure Setpassword(apassword: AUTF8String); safecall;
    property password : AUTF8String read Getpassword write Setpassword;
    function GethostPort(): AUTF8String; safecall;
    procedure SethostPort(ahostPort: AUTF8String); safecall;
    property hostPort : AUTF8String read GethostPort write SethostPort;
    function Gethost(): AUTF8String; safecall;
    procedure Sethost(ahost: AUTF8String); safecall;
    property host : AUTF8String read Gethost write Sethost;
    function Getport(): idllong; safecall;
    procedure Setport(aport: idllong); safecall;
    property port : idllong read Getport write Setport;
    function Getpath(): AUTF8String; safecall;
    procedure Setpath(apath: AUTF8String); safecall;
    property path : AUTF8String read Getpath write Setpath;
    function equals(other: nsIURI) : longbool; safecall;
    function schemeIs(ascheme: PAnsiChar) : longbool; safecall;
    function clone() : nsIURI; safecall;
    function resolve(relativePath: AUTF8String) : AUTF8String; safecall;
    function GetasciiSpec(): ACString; safecall;
    property asciiSpec : ACString read GetasciiSpec;
    function GetasciiHost(): ACString; safecall;
    property asciiHost : ACString read GetasciiHost;
    function GetoriginCharset(): ACString; safecall;
    property originCharset : ACString read GetoriginCharset;
    function Getref(): AUTF8String; safecall;
    procedure Setref(aref: AUTF8String); safecall;
    property ref : AUTF8String read Getref write Setref;
    function equalsExceptRef(other: nsIURI) : longbool; safecall;
    function cloneIgnoringRef() : nsIURI; safecall;
    function GetspecIgnoringRef(): AUTF8String; safecall;
    property specIgnoringRef : AUTF8String read GetspecIgnoringRef;
    function GethasRef(): longbool; safecall;
    property hasRef : longbool read GethasRef;

  end;

