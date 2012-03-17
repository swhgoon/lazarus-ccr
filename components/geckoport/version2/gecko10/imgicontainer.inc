type

  imgIContainer = interface(nsISupports)
  ['{239dfa70-2285-4d63-99cd-e9b7ff9555c7}']
    function Getwidth(): PRInt32; safecall;
    property width : PRInt32 read Getwidth;
    function Getheight(): PRInt32; safecall;
    property height : PRInt32 read Getheight;
    function Gettype(): idlushort; safecall;
    property atype : idlushort read Gettype;
{    function GetType() : PRUint16; safecall;
    function Getanimated(): longbool; safecall;
    property animated : longbool read Getanimated;
    function GetcurrentFrameIsOpaque(): longbool; safecall;
    property currentFrameIsOpaque : longbool read GetcurrentFrameIsOpaque;
    function getFrame(aWhichFrame: PRUint32; aFlags: PRUint32) : gfxASurface; safecall;
    function copyFrame(aWhichFrame: PRUint32; aFlags: PRUint32) : gfxImageSurface; safecall;
    function extractFrame(aWhichFrame: PRUint32; aRect: nsIntRect; aFlags: PRUint32) : imgIContainer; safecall;
    procedure draw(aContext: gfxContext; aFilter: gfxGraphicsFilter; aUserSpaceToImageSpace: gfxMatrix; aFill: gfxRect; aSubimage: nsIntRect; aViewportSize: nsIntSize; aFlags: PRUint32); safecall;
    function GetRootLayoutFrame() : nsIFrame; safecall;
    procedure requestDecode(); safecall;
    procedure lockImage(); safecall;
    procedure unlockImage(); safecall;
    function GetanimationMode(): idlushort; safecall;
    procedure SetanimationMode(aanimationMode: idlushort); safecall;
    property animationMode : idlushort read GetanimationMode write SetanimationMode;
    procedure resetAnimation(); safecall;
 }
  end;

const
  imgIContainer_TYPE_RASTER=0;
  imgIContainer_TYPE_VECTOR=1;
  imgIContainer_FLAG_NONE=$0;
  imgIContainer_FLAG_SYNC_DECODE=$1;
  imgIContainer_FLAG_DECODE_NO_PREMULTIPLY_ALPHA=$2;
  imgIContainer_FLAG_DECODE_NO_COLORSPACE_CONVERSION=$4;
  imgIContainer_FRAME_FIRST=0;
  imgIContainer_FRAME_CURRENT=1;
  imgIContainer_FRAME_MAX_VALUE=1;
  imgIContainer_kNormalAnimMode=0;
  imgIContainer_kDontAnimMode=1;
  imgIContainer_kLoopOnceAnimMode=2;

