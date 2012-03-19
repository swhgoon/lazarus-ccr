type

  nsIChannel = interface(nsIRequest)
  ['{06f6ada3-7729-4e72-8d3f-bf8ba630ff9b}']
    function GetoriginalURI(): nsIURI; safecall;
    procedure SetoriginalURI(aoriginalURI: nsIURI); safecall;
    property originalURI : nsIURI read GetoriginalURI write SetoriginalURI;
    function GetURI(): nsIURI; safecall;
    property URI : nsIURI read GetURI;
    function Getowner(): nsISupports; safecall;
    procedure Setowner(aowner: nsISupports); safecall;
    property owner : nsISupports read Getowner write Setowner;
    function GetnotificationCallbacks(): nsIInterfaceRequestor; safecall;
    procedure SetnotificationCallbacks(anotificationCallbacks: nsIInterfaceRequestor); safecall;
    property notificationCallbacks : nsIInterfaceRequestor read GetnotificationCallbacks write SetnotificationCallbacks;
    function GetsecurityInfo(): nsISupports; safecall;
    property securityInfo : nsISupports read GetsecurityInfo;
    procedure GetcontentType( result_: ACString); safecall;
    procedure SetcontentType(acontentType: ACString); safecall;
    procedure GetcontentCharset( result_: ACString); safecall;
    procedure SetcontentCharset(acontentCharset: ACString); safecall;
    function GetcontentLength(): idllong; safecall;
    procedure SetcontentLength(acontentLength: idllong); safecall;
    property contentLength : idllong read GetcontentLength write SetcontentLength;
    function open() : nsIInputStream; safecall;
    procedure asyncOpen(aListener: nsIStreamListener; aContext: nsISupports); safecall;
    function GetcontentDisposition(): idlulong; safecall;
    property contentDisposition : idlulong read GetcontentDisposition;
    function GetcontentDispositionFilename(): AString; safecall;
    property contentDispositionFilename : AString read GetcontentDispositionFilename;
    procedure GetcontentDispositionHeader( result_: ACString); safecall;

  end;

const
  nsIChannel_LOAD_DOCUMENT_URI=1 << 16;
  nsIChannel_LOAD_RETARGETED_DOCUMENT_URI=1 << 17;
  nsIChannel_LOAD_REPLACE=1 << 18;
  nsIChannel_LOAD_INITIAL_DOCUMENT_URI=1 << 19;
  nsIChannel_LOAD_TARGETED=1 << 20;
  nsIChannel_LOAD_CALL_CONTENT_SNIFFERS=1 << 21;
  nsIChannel_LOAD_CLASSIFY_URI=1 << 22;
  nsIChannel_DISPOSITION_INLINE=0;
  nsIChannel_DISPOSITION_ATTACHMENT=1;

