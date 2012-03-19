type

  nsIWebNavigation = interface(nsISupports)
  ['{F5D9E7B0-D930-11d3-B057-00A024FFC08C}']
    function GetcanGoBack(): longbool; safecall;
    property canGoBack : longbool read GetcanGoBack;
    function GetcanGoForward(): longbool; safecall;
    property canGoForward : longbool read GetcanGoForward;
    procedure goBack(); safecall;
    procedure goForward(); safecall;
    procedure gotoIndex(index: idllong); safecall;
    procedure loadURI(aURI: PWideChar; aLoadFlags: idlulong; aReferrer: nsIURI; aPostData: nsIInputStream; aHeaders: nsIInputStream); safecall;
    procedure reload(aReloadFlags: idlulong); safecall;
    procedure stop(aStopFlags: idlulong); safecall;
    function Getdocument(): nsIDOMDocument; safecall;
    property document : nsIDOMDocument read Getdocument;
    function GetcurrentURI(): nsIURI; safecall;
    property currentURI : nsIURI read GetcurrentURI;
    function GetreferringURI(): nsIURI; safecall;
    property referringURI : nsIURI read GetreferringURI;
    function GetsessionHistory(): nsISHistory; safecall;
    procedure SetsessionHistory(asessionHistory: nsISHistory); safecall;
    property sessionHistory : nsISHistory read GetsessionHistory write SetsessionHistory;

  end;

const
  nsIWebNavigation_LOAD_FLAGS_MASK=$ffff;
  nsIWebNavigation_LOAD_FLAGS_NONE=$0000;
  nsIWebNavigation_LOAD_FLAGS_IS_REFRESH=$0010;
  nsIWebNavigation_LOAD_FLAGS_IS_LINK=$0020;
  nsIWebNavigation_LOAD_FLAGS_BYPASS_HISTORY=$0040;
  nsIWebNavigation_LOAD_FLAGS_REPLACE_HISTORY=$0080;
  nsIWebNavigation_LOAD_FLAGS_BYPASS_CACHE=$0100;
  nsIWebNavigation_LOAD_FLAGS_BYPASS_PROXY=$0200;
  nsIWebNavigation_LOAD_FLAGS_CHARSET_CHANGE=$0400;
  nsIWebNavigation_LOAD_FLAGS_STOP_CONTENT=$0800;
  nsIWebNavigation_LOAD_FLAGS_FROM_EXTERNAL=$1000;
  nsIWebNavigation_LOAD_FLAGS_ALLOW_THIRD_PARTY_FIXUP=$2000;
  nsIWebNavigation_LOAD_FLAGS_FIRST_LOAD=$4000;
  nsIWebNavigation_LOAD_FLAGS_ALLOW_POPUPS=$8000;
  nsIWebNavigation_LOAD_FLAGS_BYPASS_CLASSIFIER=$10000;
  nsIWebNavigation_LOAD_FLAGS_FORCE_ALLOW_COOKIES=$20000;
  nsIWebNavigation_LOAD_FLAGS_DISALLOW_INHERIT_OWNER=$40000;
  nsIWebNavigation_STOP_NETWORK=$01;
  nsIWebNavigation_STOP_CONTENT=$02;
  nsIWebNavigation_STOP_ALL=$03;

