type
  nativeWindow = voidPtr;
  nsIWidget = pointer;

type


  nsIBaseWindow = interface(nsISupports)
  ['{046BC8A0-8015-11d3-AF70-00A024FFC08C}']
    procedure initWindow(AparentNativeWindow: nativeWindow; AparentWidget: nsIWidget; x: idllong; y: idllong; cx: idllong; cy: idllong); safecall;
    procedure create(); safecall;
    procedure destroy(); safecall;
    procedure setPosition(x: idllong; y: idllong); safecall;
    procedure getPosition(out x: idllong; out y: idllong); safecall;
    procedure setSize(cx: idllong; cy: idllong; fRepaint: longbool); safecall;
    procedure getSize(out cx: idllong; out cy: idllong); safecall;
    procedure setPositionAndSize(x: idllong; y: idllong; cx: idllong; cy: idllong; fRepaint: longbool); safecall;
    procedure getPositionAndSize(out x: idllong; out y: idllong; out cx: idllong; out cy: idllong); safecall;
    procedure repaint(force: longbool); safecall;
    function GetparentWidget(): nsIWidget; safecall;
    procedure SetparentWidget(aparentWidget: nsIWidget); safecall;
    property parentWidget : nsIWidget read GetparentWidget write SetparentWidget;
    function GetparentNativeWindow(): nativeWindow; safecall;
    procedure SetparentNativeWindow(aparentNativeWindow: nativeWindow); safecall;
    property parentNativeWindow : nativeWindow read GetparentNativeWindow write SetparentNativeWindow;
    function Getvisibility(): longbool; safecall;
    procedure Setvisibility(avisibility: longbool); safecall;
    property visibility : longbool read Getvisibility write Setvisibility;
    function Getenabled(): longbool; safecall;
    procedure Setenabled(aenabled: longbool); safecall;
    property enabled : longbool read Getenabled write Setenabled;
    function GetblurSuppression(): longbool; safecall;
    procedure SetblurSuppression(ablurSuppression: longbool); safecall;
    property blurSuppression : longbool read GetblurSuppression write SetblurSuppression;
    function GetmainWidget(): nsIWidget; safecall;
    property mainWidget : nsIWidget read GetmainWidget;
    procedure setFocus(); safecall;
    function Gettitle(): PWideChar; safecall;
    procedure Settitle(atitle: PWideChar); safecall;
    property title : PWideChar read Gettitle write Settitle;

  end;

