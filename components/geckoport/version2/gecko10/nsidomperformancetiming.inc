type

  nsIDOMPerformanceTiming = interface(nsISupports)
  ['{8e09c825-da49-4a39-876b-ce2ff767dbe1}']
    function GetnavigationStart(): DOMTimeMilliSec; safecall;
    property navigationStart : DOMTimeMilliSec read GetnavigationStart;
    function GetunloadEventStart(): DOMTimeMilliSec; safecall;
    property unloadEventStart : DOMTimeMilliSec read GetunloadEventStart;
    function GetunloadEventEnd(): DOMTimeMilliSec; safecall;
    property unloadEventEnd : DOMTimeMilliSec read GetunloadEventEnd;
    function GetredirectStart(): DOMTimeMilliSec; safecall;
    property redirectStart : DOMTimeMilliSec read GetredirectStart;
    function GetredirectEnd(): DOMTimeMilliSec; safecall;
    property redirectEnd : DOMTimeMilliSec read GetredirectEnd;
    function GetfetchStart(): DOMTimeMilliSec; safecall;
    property fetchStart : DOMTimeMilliSec read GetfetchStart;
    function GetdomainLookupStart(): DOMTimeMilliSec; safecall;
    property domainLookupStart : DOMTimeMilliSec read GetdomainLookupStart;
    function GetdomainLookupEnd(): DOMTimeMilliSec; safecall;
    property domainLookupEnd : DOMTimeMilliSec read GetdomainLookupEnd;
    function GetconnectStart(): DOMTimeMilliSec; safecall;
    property connectStart : DOMTimeMilliSec read GetconnectStart;
    function GetconnectEnd(): DOMTimeMilliSec; safecall;
    property connectEnd : DOMTimeMilliSec read GetconnectEnd;
    function GetrequestStart(): DOMTimeMilliSec; safecall;
    property requestStart : DOMTimeMilliSec read GetrequestStart;
    function GetresponseStart(): DOMTimeMilliSec; safecall;
    property responseStart : DOMTimeMilliSec read GetresponseStart;
    function GetresponseEnd(): DOMTimeMilliSec; safecall;
    property responseEnd : DOMTimeMilliSec read GetresponseEnd;
    function GetdomLoading(): DOMTimeMilliSec; safecall;
    property domLoading : DOMTimeMilliSec read GetdomLoading;
    function GetdomInteractive(): DOMTimeMilliSec; safecall;
    property domInteractive : DOMTimeMilliSec read GetdomInteractive;
    function GetdomContentLoadedEventStart(): DOMTimeMilliSec; safecall;
    property domContentLoadedEventStart : DOMTimeMilliSec read GetdomContentLoadedEventStart;
    function GetdomContentLoadedEventEnd(): DOMTimeMilliSec; safecall;
    property domContentLoadedEventEnd : DOMTimeMilliSec read GetdomContentLoadedEventEnd;
    function GetdomComplete(): DOMTimeMilliSec; safecall;
    property domComplete : DOMTimeMilliSec read GetdomComplete;
    function GetloadEventStart(): DOMTimeMilliSec; safecall;
    property loadEventStart : DOMTimeMilliSec read GetloadEventStart;
    function GetloadEventEnd(): DOMTimeMilliSec; safecall;
    property loadEventEnd : DOMTimeMilliSec read GetloadEventEnd;

  end;

