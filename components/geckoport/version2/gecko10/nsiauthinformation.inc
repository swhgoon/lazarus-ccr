type

  nsIAuthInformation = interface(nsISupports)
  ['{0d73639c-2a92-4518-9f92-28f71fea5f20}']
    function Getflags(): idlulong; safecall;
    property flags : idlulong read Getflags;
    function Getrealm(): AString; safecall;
    property realm : AString read Getrealm;
    function GetauthenticationScheme(): AUTF8String; safecall;
    property authenticationScheme : AUTF8String read GetauthenticationScheme;
    function Getusername(): AString; safecall;
    procedure Setusername(ausername: AString); safecall;
    property username : AString read Getusername write Setusername;
    function Getpassword(): AString; safecall;
    procedure Setpassword(apassword: AString); safecall;
    property password : AString read Getpassword write Setpassword;
    function Getdomain(): AString; safecall;
    procedure Setdomain(adomain: AString); safecall;
    property domain : AString read Getdomain write Setdomain;

  end;

const
  nsIAuthInformation_AUTH_HOST=1;
  nsIAuthInformation_AUTH_PROXY=2;
  nsIAuthInformation_NEED_DOMAIN=4;
  nsIAuthInformation_ONLY_PASSWORD=8;
  nsIAuthInformation_PREVIOUS_FAILED=16;

