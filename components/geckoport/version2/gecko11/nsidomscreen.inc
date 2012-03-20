type

  nsIDOMScreen = interface(nsISupports)
  ['{4507e43f-097c-452a-bfc4-dbb99748f6fd}']
    function Gettop(): idllong; safecall;
    property top : idllong read Gettop;
    function Getleft(): idllong; safecall;
    property left : idllong read Getleft;
    function Getwidth(): idllong; safecall;
    property width : idllong read Getwidth;
    function Getheight(): idllong; safecall;
    property height : idllong read Getheight;
    function GetpixelDepth(): idllong; safecall;
    property pixelDepth : idllong read GetpixelDepth;
    function GetcolorDepth(): idllong; safecall;
    property colorDepth : idllong read GetcolorDepth;
    function GetavailWidth(): idllong; safecall;
    property availWidth : idllong read GetavailWidth;
    function GetavailHeight(): idllong; safecall;
    property availHeight : idllong read GetavailHeight;
    function GetavailLeft(): idllong; safecall;
    property availLeft : idllong read GetavailLeft;
    function GetavailTop(): idllong; safecall;
    property availTop : idllong read GetavailTop;
    function GetmozEnabled(): longbool; safecall;
    procedure SetmozEnabled(amozEnabled: longbool); safecall;
    property mozEnabled : longbool read GetmozEnabled write SetmozEnabled;
    function GetmozBrightness(): double; safecall;
    procedure SetmozBrightness(amozBrightness: double); safecall;
    property mozBrightness : double read GetmozBrightness write SetmozBrightness;

  end;

