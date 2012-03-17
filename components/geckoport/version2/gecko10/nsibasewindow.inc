type
  nativeWindow = voidPtr;
  nsIWidget = pointer;

type

  nsIBaseWindow = interface(nsISupports)
  ['{7144AC8B-6702-4A4B-A73D-D1D4E9717E46}']
    procedure initWindow(aparentNativeWindow: nativeWindow; aparentWidget: nsIWidget; x: idllong; y: idllong; cx: idllong; cy: idllong); safecall;
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
    function GetmainWidget(): nsIWidget; safecall;
    property mainWidget : nsIWidget read GetmainWidget;
    procedure setFocus(); safecall;
    function Gettitle(): PWideChar; safecall;
    procedure Settitle(atitle: PWideChar); safecall;
    property title : PWideChar read Gettitle write Settitle;

  end;

