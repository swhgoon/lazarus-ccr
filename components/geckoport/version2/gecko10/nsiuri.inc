type

  nsIURI = interface(nsISupports)
  ['{395fe045-7d18-4adb-a3fd-af98c8a1af11}']
    procedure Getspec( result_: AUTF8String); safecall;
    procedure Setspec(aspec: AUTF8String); safecall;
    procedure GetprePath( result_: AUTF8String); safecall;
    procedure Getscheme( result_: ACString); safecall;
    procedure Setscheme(ascheme: ACString); safecall;
    procedure GetuserPass( result_: AUTF8String); safecall;
    procedure SetuserPass(auserPass: AUTF8String); safecall;
    procedure Getusername( result_: AUTF8String); safecall;
    procedure Setusername(ausername: AUTF8String); safecall;
    procedure Getpassword( result_: AUTF8String); safecall;
    procedure Setpassword(apassword: AUTF8String); safecall;
    procedure GethostPort( result_: AUTF8String); safecall;
    procedure SethostPort(ahostPort: AUTF8String); safecall;
    procedure Gethost( result_: AUTF8String); safecall;
    procedure Sethost(ahost: AUTF8String); safecall;
    function Getport(): idllong; safecall;
    procedure Setport(aport: idllong); safecall;
    property port : idllong read Getport write Setport;
    procedure Getpath( result_: AUTF8String); safecall;
    procedure Setpath(apath: AUTF8String); safecall;
    function equals(other: nsIURI) : longbool; safecall;
    function schemeIs(ascheme: PAnsiChar) : longbool; safecall;
    function clone() : nsIURI; safecall;
    procedure resolve(relativePath: AUTF8String; result_: AUTF8String); safecall;
    procedure GetasciiSpec( result_: ACString); safecall;
    procedure GetasciiHost( result_: ACString); safecall;
    procedure GetoriginCharset( result_: ACString); safecall;
    procedure Getref( result_: AUTF8String); safecall;
    procedure Setref(aref: AUTF8String); safecall;
    function equalsExceptRef(other: nsIURI) : longbool; safecall;
    function cloneIgnoringRef() : nsIURI; safecall;
    procedure GetspecIgnoringRef( result_: AUTF8String); safecall;
    function GethasRef(): longbool; safecall;
    property hasRef : longbool read GethasRef;

  end;

