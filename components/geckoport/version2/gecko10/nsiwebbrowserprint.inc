type

  nsIWebBrowserPrint = interface(nsISupports)
  ['{9A7CA4B0-FBBA-11d4-A869-00105A183419}']
    function GetglobalPrintSettings(): nsIPrintSettings; safecall;
    property globalPrintSettings : nsIPrintSettings read GetglobalPrintSettings;
    function GetcurrentPrintSettings(): nsIPrintSettings; safecall;
    property currentPrintSettings : nsIPrintSettings read GetcurrentPrintSettings;
    function GetcurrentChildDOMWindow(): nsIDOMWindow; safecall;
    property currentChildDOMWindow : nsIDOMWindow read GetcurrentChildDOMWindow;
    function GetdoingPrint(): longbool; safecall;
    property doingPrint : longbool read GetdoingPrint;
    function GetdoingPrintPreview(): longbool; safecall;
    property doingPrintPreview : longbool read GetdoingPrintPreview;
    function GetisFramesetDocument(): longbool; safecall;
    property isFramesetDocument : longbool read GetisFramesetDocument;
    function GetisFramesetFrameSelected(): longbool; safecall;
    property isFramesetFrameSelected : longbool read GetisFramesetFrameSelected;
    function GetisIFrameSelected(): longbool; safecall;
    property isIFrameSelected : longbool read GetisIFrameSelected;
    function GetisRangeSelection(): longbool; safecall;
    property isRangeSelection : longbool read GetisRangeSelection;
    function GetprintPreviewNumPages(): idllong; safecall;
    property printPreviewNumPages : idllong read GetprintPreviewNumPages;
    procedure print(aThePrintSettings: nsIPrintSettings; aWPListener: nsIWebProgressListener); safecall;
    procedure printPreview(aThePrintSettings: nsIPrintSettings; aChildDOMWin: nsIDOMWindow; aWPListener: nsIWebProgressListener); safecall;
    procedure printPreviewNavigate(aNavType: idlshort; aPageNum: idllong); safecall;
    procedure cancel(); safecall;
    procedure enumerateDocumentNames(out aCount: PRUint32; out aResult: PWideChar); safecall;
    procedure exitPrintPreview(); safecall;

  end;

const
  nsIWebBrowserPrint_PRINTPREVIEW_GOTO_PAGENUM=0;
  nsIWebBrowserPrint_PRINTPREVIEW_PREV_PAGE=1;
  nsIWebBrowserPrint_PRINTPREVIEW_NEXT_PAGE=2;
  nsIWebBrowserPrint_PRINTPREVIEW_HOME=3;
  nsIWebBrowserPrint_PRINTPREVIEW_END=4;

