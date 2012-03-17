type

  imgIRequest = interface(nsIRequest)
  ['{c3bf4e2a-f64b-4ac1-a84e-18631b1802ab}']
    function Getimage(): imgIContainer; safecall;
    property image : imgIContainer read Getimage;
    function GetimageStatus(): idlulong; safecall;
    property imageStatus : idlulong read GetimageStatus;
    function GetURI(): nsIURI; safecall;
    property URI : nsIURI read GetURI;
    function GetdecoderObserver(): imgIDecoderObserver; safecall;
    property decoderObserver : imgIDecoderObserver read GetdecoderObserver;
    function GetmimeType(): PAnsiChar; safecall;
    property mimeType : PAnsiChar read GetmimeType;
    function clone(aObserver: imgIDecoderObserver) : imgIRequest; safecall;
    function GetimagePrincipal(): nsIPrincipal; safecall;
    property imagePrincipal : nsIPrincipal read GetimagePrincipal;
    function GetCORSMode(): idllong; safecall;
    property CORSMode : idllong read GetCORSMode;
    procedure cancelAndForgetObserver(aStatus: nsresult); safecall;
    procedure requestDecode(); safecall;
    procedure lockImage(); safecall;
    procedure unlockImage(); safecall;
    function getStaticRequest() : imgIRequest; safecall;
    procedure incrementAnimationConsumers(); safecall;
    procedure decrementAnimationConsumers(); safecall;

  end;

const
  imgIRequest_STATUS_NONE=$0;
  imgIRequest_STATUS_SIZE_AVAILABLE=$1;
  imgIRequest_STATUS_LOAD_PARTIAL=$2;
  imgIRequest_STATUS_LOAD_COMPLETE=$4;
  imgIRequest_STATUS_ERROR=$8;
  imgIRequest_STATUS_FRAME_COMPLETE=$10;
  imgIRequest_STATUS_DECODE_COMPLETE=$20;
  imgIRequest_CORS_NONE=1;
  imgIRequest_CORS_ANONYMOUS=2;
  imgIRequest_CORS_USE_CREDENTIALS=3;

