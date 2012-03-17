type

  nsIDOMNavigator = interface(nsISupports)
  ['{c5acd187-bf76-4b26-9329-41c798e47fb6}']
    function GetappCodeName(): DOMString; safecall;
    property appCodeName : DOMString read GetappCodeName;
    function GetappName(): DOMString; safecall;
    property appName : DOMString read GetappName;
    function GetappVersion(): DOMString; safecall;
    property appVersion : DOMString read GetappVersion;
    function Getlanguage(): DOMString; safecall;
    property language : DOMString read Getlanguage;
    function GetmimeTypes(): nsIDOMMimeTypeArray; safecall;
    property mimeTypes : nsIDOMMimeTypeArray read GetmimeTypes;
    function Getplatform(): DOMString; safecall;
    property platform : DOMString read Getplatform;
    function Getoscpu(): DOMString; safecall;
    property oscpu : DOMString read Getoscpu;
    function Getvendor(): DOMString; safecall;
    property vendor : DOMString read Getvendor;
    function GetvendorSub(): DOMString; safecall;
    property vendorSub : DOMString read GetvendorSub;
    function Getproduct(): DOMString; safecall;
    property product : DOMString read Getproduct;
    function GetproductSub(): DOMString; safecall;
    property productSub : DOMString read GetproductSub;
    function Getplugins(): nsIDOMPluginArray; safecall;
    property plugins : nsIDOMPluginArray read Getplugins;
    function GetuserAgent(): DOMString; safecall;
    property userAgent : DOMString read GetuserAgent;
    function GetcookieEnabled(): longbool; safecall;
    property cookieEnabled : longbool read GetcookieEnabled;
    function GetonLine(): longbool; safecall;
    property onLine : longbool read GetonLine;
    function GetbuildID(): DOMString; safecall;
    property buildID : DOMString read GetbuildID;
    function GetdoNotTrack(): DOMString; safecall;
    property doNotTrack : DOMString read GetdoNotTrack;
    function javaEnabled() : longbool; safecall;
    function taintEnabled() : longbool; safecall;

  end;

