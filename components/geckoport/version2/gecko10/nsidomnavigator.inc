type

  nsIDOMNavigator = interface(nsISupports)
  ['{c5acd187-bf76-4b26-9329-41c798e47fb6}']
    procedure GetappCodeName( result_: DOMString); safecall;
    procedure GetappName( result_: DOMString); safecall;
    procedure GetappVersion( result_: DOMString); safecall;
    procedure Getlanguage( result_: DOMString); safecall;
    function GetmimeTypes(): nsIDOMMimeTypeArray; safecall;
    property mimeTypes : nsIDOMMimeTypeArray read GetmimeTypes;
    procedure Getplatform( result_: DOMString); safecall;
    procedure Getoscpu( result_: DOMString); safecall;
    procedure Getvendor( result_: DOMString); safecall;
    procedure GetvendorSub( result_: DOMString); safecall;
    procedure Getproduct( result_: DOMString); safecall;
    procedure GetproductSub( result_: DOMString); safecall;
    function Getplugins(): nsIDOMPluginArray; safecall;
    property plugins : nsIDOMPluginArray read Getplugins;
    procedure GetuserAgent( result_: DOMString); safecall;
    function GetcookieEnabled(): longbool; safecall;
    property cookieEnabled : longbool read GetcookieEnabled;
    function GetonLine(): longbool; safecall;
    property onLine : longbool read GetonLine;
    procedure GetbuildID( result_: DOMString); safecall;
    procedure GetdoNotTrack( result_: DOMString); safecall;
    function javaEnabled() : longbool; safecall;
    function taintEnabled() : longbool; safecall;

  end;

