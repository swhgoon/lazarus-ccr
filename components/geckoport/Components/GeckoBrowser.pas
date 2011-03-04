(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GeckoComponents for Delphi.
 *
 * The Initial Developer of the Original Code is Takanori Ito.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)
unit GeckoBrowser;

{$MACRO on}

{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ELSE Windows}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}

{$IFNDEF FPC_HAS_CONSTREF}
  {$DEFINE constref:=const}
{$ENDIF}

{$IFDEF LCLCocoa}
  {$MODESWITCH ObjectiveC1}
{$ENDIF}

interface

uses
  LclIntf, LMessages, LclType, LResources, Graphics,
  SysUtils, Classes, Controls, nsXPCOM,
  nsGeckoStrings, nsTypes, CallbackInterfaces, nsXPCOMGlue, BrowserSupports,
  nsXPCOM_std19, GeckoPromptService
  {$IFDEF LCLCarbon}, CarbonPrivate {$ENDIF}
  {$IFDEF LCLCocoa}, CocoaPrivate, CocoaAll, CocoaUtils {$ENDIF}
  {$IFDEF LCLGtk2}, gtk2,
                    ExtCtrls {This is temporal for TTimer needed for event pooling forced}
  {$ENDIF}
  ;

resourcestring
  SGeckoBrowserInitError = 'Failed to initialize TGeckoBrowser.';
  SGeckoBrowserCannotGoBack = 'Failed to go back history.';
  SGeckoBrowserCannotGoForward = 'Failed to go forward history.';
  SGeckoBrowserLoadURIError = 'Failed to load URI ''%s.'' ';
  SGeckoBrowserCannotReload = 'Failed to reload page.';

const
  LOAD_FLAGS_NONE = 0;
  LOAD_FLAGS_IS_REFRESH = 16;
  LOAD_FLAGS_IS_LINK = 32;
  LOAD_FLAGS_BYPASS_HISTORY = 64;
  LOAD_FLAGS_REPLACE_HISTORY = 128;
  LOAD_FLAGS_BYPASS_CACHE = 256;
  LOAD_FLAGS_BYPASS_PROXY = 512;
  LOAD_FLAGS_CHARSET_CHANGE = 1024;

{$IFDEF LCL}
const
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_NEXTDLGCTL = $0028;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_SHOWWINDOW = LM_SHOWWINDOW;
  E_FAIL        = HRESULT($80004005);
type
  TMessage      = TLMessage;
  TWMGetDlgCode = TLMNoParams;
{$ENDIF}

type
  //TCtxMenuInfo = BrowserSupports.TCtxMenuInfo;
  //TCtxMenuFlags = BrowserSupports.TCtxMenuFlags;
  TGeckoDOMEventType = (
    etNone,
    etEvent,
    etCustomEvent,
    etUIEvent,
    etMouseEvent,
    etStorageEvent
  );

  TGeckoDOMEvent = record
    Name: String;
    EventType: TGeckoDOMEventType;
    event: nsIDOMEvent;
  end;

  TGeckoDOMEventRegister = record
    Name: String;
    eventType: TGeckoDOMEventType;
    propertyName: String;
  end;
  TGeckoDOMEventRegisterArray = array [0..99] of TGeckoDOMEventRegister;
  PGeckoDOMEventRegisterArray = ^TGeckoDOMEventRegisterArray;

  TCustomGeckoBrowser = class;
  TCustomGeckoBrowserChrome = class;
  TCustomGeckoBrowserListener = class;
  TGeckoBrowser = class;
  TGeckoBrowserChrome = class;
  TGeckoBrowserListener = class;

  TCtxMenuInfo = class;

  EGeckoBrowserError = class(EGeckoError)
  end;
  EGeckoBrowserNavigationError = class(EGeckoBrowserError)
  end;

  {$PUSH}{$HINTS OFF} //Redefinition to expose the interface
  IDirectoryServiceProvider=nsXPCOMGlue.IDirectoryServiceProvider;
  {$POP}

  TGeckoBrowserContextMenu = procedure (Sender: TObject; aInfo: TCtxMenuInfo) of object;
  TGeckoBrowserStatusChange = procedure (Sender: TObject; aMessage: WideString) of object;
  TGeckoBrowserNewWindow = procedure (Sender: TObject; aChromeFlags: Longword; var newWindow: TCustomGeckoBrowser) of object;
  TGeckoBrowserProgressChange = procedure (Sender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TGeckoBrowserTitleChange = procedure (Sender: TObject; const Text: WideString) of object;
  TGeckoBrowserVisibleChange = procedure (Sender: TObject; Vislble: Bool) of object;
  TGeckoBrowserLocationChange = procedure (Sender: TObject; const uri: AnsiString) of object;
  TGeckoBrowserDOMEventHandler = procedure (Sender: TObject; aEvent:TGeckoDOMEvent) of object;
  TGeckoBrowserHistoryMove = procedure (Sender: TObject; aURI: nsIURI; out aContinue: LongBool; var Handled: Boolean) of object;
  TGeckoBrowserHistoryGoTo = procedure (Sender: TObject; aIndex: Longint; aURI: nsIURI; out aContinue: LongBool; var Handled: Boolean) of object;
  TGeckoBrowserDirectoryService = procedure (Sender: TObject; const aDirectoryService: IDirectoryServiceProvider) of object;

  TGeckoBrowserHisoty = record
    URI: AnsiString;
    Title: WideString;
    IsSubFrame: Boolean;
  end;

    //TODO 2 -cTCustomGeckoBrowser: DocShell プャpティを追加

  { TCustomGeckoBrowser }

  TCustomGeckoBrowser = class(TCustomControl,
                              IGeckoCreateWindowTarget)
  private
    FWebBrowser: nsIWebBrowser;
    FListeners: TCustomGeckoBrowserListener;
    FChrome: TCustomGeckoBrowserChrome;

    // イベント
    // nsIWebProgressListener
    FOnStatusChange: TGeckoBrowserStatusChange;
    FOnProgressChange: TGeckoBrowserProgressChange;
    FOnLocationChange: TGeckoBrowserLocationChange;
    FOnDocumentBegin: TNotifyEvent;
    FOnDocumentComplete: TNotifyEvent;
    //FOnSecurityChange: TGeckoBrowserSecurityChange;
    // nsIEmbeddingSiteWindow
    FOnTitleChange: TGeckoBrowserTitleChange;
    FOnVisibleChange: TGeckoBrowserVisibleChange;
    // nsIContextMenuListener
    FOnContextMenu: TGeckoBrowserContextMenu;
    // nsISHistoryListener
    FOnGoBack: TGeckoBrowserHistoryMove;
    FOnGoForward: TGeckoBrowserHistoryMove;
    FOnGoToIndex: TGeckoBrowserHistoryGoTo;

    FOnNewWindow: TGeckoBrowserNewWindow;

    FOnSetupProperties: TNotifyEvent;
    FOnDirectoryService: TGeckoBrowserDirectoryService;

    FGeckoComponentsStartupSucceeded: boolean;

    //Linked event components
    FPromptService: TGeckoPrompt;

    //misc settings
    FDisableJavaScript: Boolean;
    FInitializationStarted: Boolean;
    FInitialized: Boolean;

    //Designtime graphic
    FDesignTimeLogo: TPortableNetworkGraphic;

    function GetDisableJavaScript: Boolean;
    procedure SetDisableJavascript(const AValue: Boolean);
    procedure ShutdownWebBrowser;
    procedure InnerLoadURI(uri: WideString; Flags: PRUint32;
      referer: nsIURI; postData, headers: TStream);

    procedure SetChrome(aChrome: TCustomGeckoBrowserChrome);
    procedure SetListener(aListener: TCustomGeckoBrowserListener);

    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    function GetContentDocument: nsIDOMDocument;
    function GetContentWindow: nsIDOMWindow;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetWebBrowserChrome: nsIWebBrowserChrome;
    function GetWebBrowserFind: nsIWebBrowserFind;
    function GetWebBrowserPrint: nsIWebBrowserPrint;
    function GetWebNavigation: nsIWebNavigation;
    function GetNativeWindow : nativeWindow;
    //function GetMarkupDocumentViewer: nsIMarkupDocumentViewer;
    //function GetDocShell: nsIDocShell;
    //function GetDocumentCharsetInfo: nsIDocumentCharsetInfo;
    procedure DoInitializationIfNeeded;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitWebBrowser;  //FPC port: moved from private to public
    procedure LoadURI(const uri: WideString); overload;
    procedure LoadURI(const uri: WideString; const referer: UTF8String);
      overload;
    procedure LoadURI(const uri: WideString; const referer: WideString);
      overload;
    procedure LoadURI(const uri: WideString; referer: nsIURI); overload;
    procedure LoadURIWithFlags(const uri: WideString; Flags: PRUint32);
      overload;
    procedure LoadURIWithFlags(const uri: WideString; Flags: PRUint32;
      const referer: UTF8String); overload;
    procedure LoadURIWithFlags(const uri: WideString; Flags: PRUint32;
      const referer: WideString); overload;
    procedure LoadURIWithFlags(Const uri: WideString; Flags: PRUint32;
      referer: nsIURI); overload;
    procedure GoBack;
    procedure GoForward;
    procedure Reload;
    procedure ReloadWithFlags(AFlags: PRUint32);
  protected
    function  DoCreateChromeWindow(chromeFlags: Longword): nsIWebBrowserChrome; virtual; abstract;
    procedure DoGeckoComponentsStartup;

    // TControl
    procedure Resize; override;

    procedure Loaded; override;

    //TWinControl
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  protected
    property Chrome: TCustomGeckoBrowserChrome
        read FChrome write SetChrome;
    property Listener: TCustomGeckoBrowserListener
        read FListeners write SetListener;

    property WebBrowser: nsIWebBrowser //begin plus7
        read FWebBrowser;
    property WebBrowserFind: nsIWebBrowserFind
        read GetWebBrowserFind;
    property WebBrowserPrint: nsIWebBrowserPrint
        read GetWebBrowserPrint;
    property WebNavigation: nsIWebNavigation
        read GetWebNavigation;
    //property MarkupDocumentViewer: nsIMarkupDocumentViewer
    //    read GetMarkupDocumentViewer;
    //property DocShell: nsIDocShell
    //      read GetDocShell;
    //property DocumentCharsetInfo: nsIDocumentCharsetInfo
    //    read GetDocumentCharsetInfo; //end plus7

    property ContentWindow: nsIDOMWindow
        read GetContentWindow;
    property ContentDocument: nsIDOMDocument
        read GetContentDocument;
    property CanGoBack: Boolean
        read GetCanGoBack;
    property CanGoForward: Boolean
        read GetCanGoForward;
    // イベント
    // nsIWebBrowserChrome
    property OnStatusChange: TGeckoBrowserStatusChange
        read FOnStatusChange write FOnStatusChange;
    property OnProgressChange: TGeckoBrowserProgressChange
        read FOnProgressChange write FOnProgressChange;
    property OnLocationChange: TGeckoBrowserLocationChange
        read FOnLocationChange write FOnLocationChange;
    property OnDocumentBegin: TNotifyEvent
        read FOnDocumentBegin write FOnDocumentBegin;
    property OnDocumentComplete: TNotifyEvent
        read FOnDocumentComplete write FOnDocumentComplete;
    // nsIEmbeddingSiteWindow
    property OnTitleChange: TGeckoBrowserTitleChange
      read FOnTitleChange write FOnTitleChange;
    property OnVisibleChange: TGeckoBrowserVisibleChange
      read FOnVisibleChange write FOnVisibleChange;
    // nsIContextMenuListener
    property OnContextMenu: TGeckoBrowserContextMenu
      read FOnContextMenu write FOnContextMenu;
    // nsISHistoryListener
    property OnGoBack:TGeckoBrowserHistoryMove
      read FOnGoBack write FOnGoBack;
    property OnGoForward:TGeckoBrowserHistoryMove
      read FOnGoForward write FOnGoForward;
    property OnGoToIndex:TGeckoBrowserHistoryGoTo
      read FOnGoToIndex write FOnGoToIndex;

    property OnNewWindow: TGeckoBrowserNewWindow
      read FOnNewWindow write FOnNewWindow;

    property OnSetupProperties: TNotifyEvent
      read FOnSetupProperties write FOnSetupProperties;
    property OnDirectoryService: TGeckoBrowserDirectoryService
      read FOnDirectoryService write FOnDirectoryService;
    // misc base settings
    property DisableJavaScript: Boolean
      read GetDisableJavaScript write SetDisableJavascript;
    property Initialized: Boolean read FInitialized;

    // Linked components set
    property Prompt: TGeckoPrompt
      read FPromptService write FPromptService;
  end;

  TCustomGeckoBrowserChrome = class(TInterfacedObject,
                                    nsIWebBrowserChrome,
                                    nsIWebBrowserChromeFocus,
                                    nsIEmbeddingSiteWindow,
                                    IGeckoBrowserChrome)
  public
    //constructor Create;
    //destructor Destroy;
  protected
    // nsIWebBrowser
    procedure SetStatus(statusType: PRUint32; const status: PWideChar); virtual; safecall; abstract;
    function GetWebBrowser(): nsIWebBrowser; virtual; safecall; abstract;
    procedure SetWebBrowser(aWebBrowser: nsIWebBrowser); virtual; safecall; abstract;
    function GetChromeFlags(): PRUint32; virtual; safecall; abstract;
    procedure SetChromeFlags(aChromeFlags: PRUint32); virtual; safecall; abstract;
    procedure DestroyBrowserWindow(); virtual; safecall; abstract;
    procedure SizeBrowserTo(aCX: PRInt32; aCY: PRInt32); virtual; safecall; abstract;
    procedure ShowAsModal(); virtual; safecall; abstract;
    function IsWindowModal(): PRBool; virtual; safecall; abstract;
    procedure ExitModalEventLoop(aStatus: nsresult); virtual; safecall; abstract;
    // nsIWebBrowserChromeFocus
    procedure FocusNextElement(); virtual; safecall; abstract;
    procedure FocusPrevElement(); virtual; safecall; abstract;
    // nsIEmbeddingSiteWindow
    procedure SetDimensions(flags: PRUint32; x: PRInt32; y: PRInt32; cx: PRInt32; cy: PRInt32); virtual; safecall; abstract;
    procedure GetDimensions(flags: PRUint32; out x: PRInt32; out y: PRInt32; out cx: PRInt32; out cy: PRInt32); virtual; safecall; abstract;
    procedure SetFocus(); virtual; safecall; abstract;
    function GetVisibility(): PRBool; virtual; safecall; abstract;
    procedure SetVisibility(aVisibility: PRBool); virtual; safecall; abstract;
    function GetTitle(): PWideChar; virtual; safecall; abstract;
    procedure SetTitle(const aTitle: PWideChar); virtual; safecall; abstract;
    function GetSiteWindow(): Pointer; virtual; safecall; abstract;

    // IGeckoBrowserChrome;
    function GetCreateWindowTarget: IGeckoCreateWindowTarget; virtual; abstract;
  public
    function SafeCallException(obj: TObject; addr: Pointer): HRESULT; override;
  end;

  TCustomGeckoBrowserListener = class(TSupportsWeakReference,
                                      nsIWebProgressListener,
                                      nsIDOMEventListener)
  private
    FBrowser: TCustomGeckoBrowser;
    FDOMEvents: PGeckoDOMEventRegisterArray;
  public
    constructor Create(ABrowser: TCustomGeckoBrowser);
    //destructor Destroy;
  protected
    procedure InitListener(browser: TCustomGeckoBrowser); virtual;
    procedure ShutdownListener(browser: TCustomGeckoBrowser); virtual;

    procedure AddWebBrowserListener(browser: nsIWebBrowser); safecall;
    procedure RemoveWebBrowserListener(browser: nsIWebBrowser); safecall;
    // nsIWebProgressListener
    procedure OnStateChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStateFlags: PRUint32; aStatus: nsresult); virtual; safecall; abstract;
    procedure OnProgressChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aCurSelfProgress: PRInt32; aMaxSelfProgress: PRInt32; aCurTotalProgress: PRInt32; aMaxTotalProgress: PRInt32); virtual; safecall; abstract;
    procedure OnLocationChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; location: nsIURI); virtual; safecall; abstract;
    procedure OnStatusChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStatus: nsresult; const aMessage: PWideChar); virtual; safecall; abstract;
    procedure OnSecurityChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; state: PRUint32); virtual; safecall; abstract;
    // nsIDOMEventListener
    procedure HandleEvent(aEvent: nsIDOMEvent); safecall;
  public
    function SafeCallException(Obj: TObject; Addr: Pointer): HRESULT; override;
  end;

  { TGeckoBrowser }

  TGeckoBrowser = class(TCustomGeckoBrowser)
  {$IFDEF LCLGTK2}
  private
    EventPool: TTimer;
    procedure EventPoolProc(Sender: TObject);
  {$ENDIF}
  protected
    FBrowser: nsIWebBrowser;
    FTitle: WideString;
    // Tooltip
{$IFNDEF LCL}
    FHint: THintWindow;
{$ENDIF}
   //DOM EventHandler
    FOnDOMLoad: TGeckoBrowserDOMEventHandler;
    FOnDOMClick: TGeckoBrowserDOMEventHandler;
    FOnDOMMouseUp: TGeckoBrowserDOMEventHandler;
    FOnDOMMouseDown: TGeckoBrowserDOMEventHandler;
    FOnDOMMouseMove: TGeckoBrowserDOMEventHandler;
    FOnDOMMouseScroll: TGeckoBrowserDOMEventHandler;
    FOnDOMKeyUp: TGeckoBrowserDOMEventHandler;
    FOnDOMKeyDown: TGeckoBrowserDOMEventHandler;
    FOnDOMKeyPress: TGeckoBrowserDOMEventHandler;
    FOnDOMLinkAdded: TGeckoBrowserDOMEventHandler;
    FOnDOMDragOver: TGeckoBrowserDOMEventHandler;
    FOnDOMDragGesture: TGeckoBrowserDOMEventHandler;
    FOnDOMDragDrop: TGeckoBrowserDOMEventHandler;
    FOnDOMDragExit: TGeckoBrowserDOMEventHandler;
    FOnDOMFocus: TGeckoBrowserDOMEventHandler;

    FOnCloseWindow: TNotifyEvent;

    // The Last focused element
    FLastFocused: nsIDOMElement;

    function DoCreateChromeWindow(
      chromeFlags: Longword): nsIWebBrowserChrome; override;

    function GetURIString: UTF8String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Title: WideString read FTitle;

    property URIString: UTF8String read GetURIString;

    class function GetGeckoBrowserWithDOMWindow(constref DOMWindow: nsIDOMWindow): TGeckoBrowser;

    procedure Print(const aShowPrinterSelectDialog: Boolean);

  published
    property OnDOMLoad: TGeckoBrowserDOMEventHandler
      read FOnDOMLoad write FOnDOMLoad;
    property OnDOMClick: TGeckoBrowserDOMEventHandler
      read FOnDOMClick write FOnDOMClick;
    property OnDOMMouseUp: TGeckoBrowserDOMEventHandler
      read FOnDOMMouseUp write FOnDOMMouseUp;
    property OnDOMMouseDown: TGeckoBrowserDOMEventHandler
      read FOnDOMMouseDown write FOnDOMMouseDown;
    property OnDOMMouseMove: TGeckoBrowserDOMEventHandler
      read FOnDOMMouseMove write FOnDOMMouseMove;
    property OnDOMKeyUp: TGeckoBrowserDOMEventHandler
      read FOnDOMKeyUp write FOnDOMKeyUp;
    property OnDOMKeyDown: TGeckoBrowserDOMEventHandler
      read FOnDOMKeyDown write FOnDOMKeyDown;
    property OnDOMKeyPress: TGeckoBrowserDOMEventHandler
      read FOnDOMKeyPress write FOnDOMKeyPress;
    property OnDOMMouseScroll: TGeckoBrowserDOMEventHandler
      read FOnDOMMouseScroll write FOnDOMMouseScroll;
    property OnDOMLinkAdded: TGeckoBrowserDOMEventHandler
      read FOnDOMLinkAdded write FOnDOMLinkAdded;
    property OnDOMDragOver: TGeckoBrowserDOMEventHandler
      read FOnDOMDragOver write FOnDOMDragOver;
    property OnDOMDragGesture: TGeckoBrowserDOMEventHandler
      read FOnDOMDragGesture write FOnDOMDragGesture;
    property OnDOMDragDrop: TGeckoBrowserDOMEventHandler
      read FOnDOMDragDrop write FOnDOMDragDrop;
    property OnDOMDragExit: TGeckoBrowserDOMEventHandler
      read FOnDOMDragExit write FOnDOMDragExit;
    property OnDOMFocus: TGeckoBrowserDOMEventHandler
      read FOnDOMFocus write FOnDOMFocus;
    property OnCloseWindow: TNotifyEvent
      read FOnCloseWindow write FOnCloseWindow;

  published
    // TWinControl
    property Align;
    property TabOrder;
    property TabStop default True;
{$IFNDEF LCL}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
{$ELSE}
    property Anchors;
    property BorderSpacing;
    property Constraints;
{$ENDIF}
    property BorderStyle;
    property BorderWidth;

    property OnLocationChange;
    property OnProgressChange;
    property OnStatusChange;
    property OnTitleChange;
    property OnVisibleChange;
    property OnContextMenu;
    property OnNewWindow;
    property OnDocumentBegin;
    property OnDocumentComplete;

    property OnGoBack;
    property OnGoForward;
    property OnGoToIndex;

    property OnSetupProperties;
    property OnDirectoryService;

    property DisableJavaScript;
    property Prompt;
  public
    property ContentDocument;
    property ContentWindow;
    property CanGoBack;
    property CanGoForward;

  end;

  { TGeckoBrowserChrome }

  TGeckoBrowserChrome = class(TCustomGeckoBrowserChrome,
                              nsIInterfaceRequestor_std19,
                              nsIContextMenuListener2,
                              nsITooltipListener)
  private
    FBrowser: TGeckoBrowser;
  protected
  public
    constructor Create(Browser: TGeckoBrowser);
    destructor Destroy; override;
  protected
    // nsIWebBrowserChrome
    procedure SetStatus(statusType: PRUint32; const status: PWideChar); override;
    function GetWebBrowser(): nsIWebBrowser; override;
    procedure SetWebBrowser(aWebBrowser: nsIWebBrowser); override;
    function GetChromeFlags(): PRUint32; override; {$IFDEF FPC} safecall; {$ENDIF}
    procedure SetChromeFlags(aChromeFlags: PRUint32); override;
    procedure DestroyBrowserWindow(); override;
    procedure SizeBrowserTo(aCX: PRInt32; aCY: PRInt32); override;
    procedure ShowAsModal(); override;
    function IsWindowModal(): PRBool; override; {$IFDEF FPC} safecall; {$ENDIF}
    procedure ExitModalEventLoop(aStatus: nsresult); override;
    // nsIWebBrowserChromeFocus
    procedure FocusNextElement(); override;
    procedure FocusPrevElement(); override;
    // nsIEmbeddingSiteWindow
    procedure SetDimensions(flags: PRUint32; x: PRInt32; y: PRInt32; cx: PRInt32; cy: PRInt32); override;
    procedure GetDimensions(flags: PRUint32; out x: PRInt32; out y: PRInt32; out cx: PRInt32; out cy: PRInt32); override;
    procedure SetFocus(); override;
    function GetVisibility(): PRBool; override; {$IFDEF FPC} safecall; {$ENDIF}
    procedure SetVisibility(aVisibility: PRBool); override;
    function GetTitle(): PWideChar; override; {$IFDEF FPC} safecall; {$ENDIF}
    procedure SetTitle(const aTitle: PWideChar); override;
    function GetSiteWindow(): Pointer; override; {$IFDEF FPC} safecall; {$ENDIF}
    // nsIInterfaceRequestor
    function NS_GetInterface(constref uuid: TGUID; out _result): nsresult; extdecl;
    function nsIInterfaceRequestor_std19.GetInterface = NS_GetInterface;
    // nsIContextMenuListener2
    procedure OnShowContextMenu(aContextFlags: PRUint32;
      aUtils: nsIContextMenuInfo); safecall;
    // nsITooltipListener
    procedure OnShowTooltip(aXCoords: PRInt32; aYCoords: PRInt32; const aTipText: PWideChar); safecall;
    procedure OnHideTooltip(); safecall;

    // IGeckoBrowserChrome;
    function GetCreateWindowTarget: IGeckoCreateWindowTarget; override;
  end;

    TGeckoBrowserListener = class(TCustomGeckoBrowserListener,
                                  nsIWebProgressListener,
                                  nsISHistoryListener,
                                  nsIDOMEventListener)
  protected
    // nsIWebProgressListener
    procedure OnStateChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStateFlags: PRUint32; aStatus: nsresult); override;
    procedure OnProgressChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aCurSelfProgress: PRInt32; aMaxSelfProgress: PRInt32; aCurTotalProgress: PRInt32; aMaxTotalProgress: PRInt32); override;
    procedure OnLocationChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; location: nsIURI); override;
    procedure OnStatusChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStatus: nsresult; const aMessage: PWideChar); override;
    procedure OnSecurityChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; state: PRUint32); override;
    // nsISHistoryListener
    procedure OnHistoryNewEntry(aNewURI: nsIURI); safecall;
    function OnHistoryGoBack(aBackURI: nsIURI): PRBool; safecall;
    function OnHistoryGoForward(aForwardURI: nsIURI): PRBool; safecall;
    function OnHistoryReload(aReloadURI: nsIURI; aReloadFlags: PRUint32): PRBool; safecall;
    function OnHistoryGotoIndex(aIndex: PRInt32; aGotoURI: nsIURI): PRBool; safecall;
    function OnHistoryPurge(aNumEntries: PRInt32): PRBool; safecall;
    // nsIDOMEventListener
    //procedure HandleEvent(aEvent: nsIDOMEvent); safecall;
  public
    constructor Create(browser: TGeckoBrowser);
  end;

  (*TGeckoBrowser = class(TCustomControl,
                        nsISHistoryListener)
  private
    { Private 骭ｾ }
    FWebBrowser: nsIWebBrowser;
    FDocTitle: WideString;

    // イベント
    FOnNewWindow: TGeckoBrowserNewWindow;

    // nsISHistoryListener
    function OnHistoryNewEntry(aNewURI: nsIURI): Longword; extdecl;
    function OnHistoryGoBack(aBackURI: nsIURI; out aContinue: LongBool): Longword; extdecl;
    function OnHistoryGoForward(aForwardURI: nsIURI; out aContinue: LongBool): Longword; extdecl;
    function OnHistoryReload(aReloadURI: nsIURI; aReloadFlags: Longword; out aContinue: LongBool): Longword; extdecl;
    function OnHistoryGotoIndex(aIndex: Longint; aGotoURI: nsIURI; out aContinue: LongBool): Longword; extdecl;
    function OnHistoryPurge(aNumEntries: Longint; out aContinue: LongBool): Longword; extdecl;

    function GetHistoryEntry(index: Integer): TGeckoBrowserHisoty;
    function GetHistoryPosition: Integer;
    function GetHistoryCount: Integer;
  protected
    { Protected 骭ｾ }
    // TControl
    procedure Resize; override;

  public
    { Public 骭ｾ }
    // ナビゲ[ション
    // nsIWebNavigation
    procedure GotoIndex(aIndex: Integer);

    property HistoryEntry[index: Integer]: TGeckoBrowserHisoty read GetHistoryEntry;
    property HistoryPosition: Integer read GetHistoryPosition;
    property HistoryCount: Integer read GetHistoryCount;
  published
    { Published 骭ｾ }
    // TWinControl
    property Align;
    property TabOrder;
    property TabStop default True;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    //property BorderWidth;

    property OnCanResize;
    //property OnCanMove;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

    property OnStatusTextChange: TGeckoBrowserStatusChange read FOnStatusChange write FOnStatusChange;
    property OnNewWindow: TGeckoBrowserNewWindow read FOnNewWindow write FOnNewWindow;
    property OnProgressChange: TGeckoBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnTitleChange: TGeckoBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnVisible: TGeckoBrowserVisibleChange read FOnVisibleChange write FOnVisibleChange;
    property OnLocationChange: TGeckoBrowserLocationChange read FOnLocationChange write FOnLocationChange;
  end;*)

  TCtxMenuFlags = set of (cmfLink,
                          cmfImage,
                          cmfDocument,
                          cmfText,
                          cmfInput,
                          cmfBGImage );

  TCtxMenuInfo = class(TObject)
  private
    FInfo: nsIContextMenuInfo;
    FFlags: TCtxMenuFlags;

    // function GetMouseEvent: nsIDOMEvent;
    // function GetTargetNode: nsIDOMNode;
    function GetAssociatedLink: WideString;
    // function GetImageContainer: imgContainer;
    // function GetImageSrc: nsIURI;
    function GetImageURL: UTF8String;
    // function GetBGImageContainer: imgIContainer;
    // function GetBGImageSrc: nsIURI;
    function GetBGImageURL: UTF8String;
    function GetMouseEvent: nsIDOMEvent;
    function GetTargetNode: nsIDOMNode;
    function GetImageContainer: imgIContainer;
    function GetImageSrc: nsIURI;
    function GetBGImageContainer: imgIContainer;
    function GetBGImageSrc: nsIURI;
  public
    constructor Create(flags: Longword; info: nsIContextMenuInfo);

    property Flags: TCtxMenuFlags read FFlags;
    property AssociatedLink: WideString read GetAssociatedLink;
    property ImageURL: UTF8String read GetImageURL;
    property BGImageURL: UTF8String read GetBGImageURL;
    property MouseEvent: nsIDOMEvent read GetMouseEvent;
    property TargetNode: nsIDOMNode read GetTargetNode;
    property ImageContainer: imgIContainer read GetImageContainer;
    property ImageSrc: nsIURI read GetImageSrc;
    property BGImageContainer: imgIContainer read GetBGImageContainer;
    property BGImageSrc: nsIURI read GetBGImageSrc;
    property ContextMenuInfo: nsIContextMenuInfo read FInfo;
  end;

procedure Register;

{$IFNDEF LCL}
{$R *.dcr}
{$ELSE}
{$IFNDEF DARWIN}
{$R geckoresources.rc}
{$ENDIF}
{$ENDIF}

implementation

uses
  nsError, nsStream, nsMemory, nsNetUtil, GeckoInit,
  Forms, TypInfo, Variants;

var
  GeckoListBrowsers: TFPList=nil;

procedure Register;
begin
  RegisterComponents('Gecko', [TGeckoBrowser]);
end;

{$PUSH}
{$HINTS OFF}
procedure UseParameter(var X);
begin
end;
{$POP}

(*
// nsISHistoryListener
function TGeckoBrowser.OnHistoryNewEntry(aNewURI: nsIURI): Longword;
begin
  Result := NS_OK;
end;

function TGeckoBrowser.OnHistoryGoBack(aBackURI: nsIURI; out aContinue: LongBool): Longword;
begin
  if @aContinue = nil then
  begin
    Result := NS_ERROR_FAILURE;
    Exit;
  end;

  if (HistoryPosition>0) then
    aContinue := True
  else
    aContinue := False;
  Result := NS_OK;
end;

function TGeckoBrowser.OnHistoryGoForward(aForwardURI: nsIURI; out aContinue: LongBool): Longword;
begin
  if @aContinue = nil then
  begin
    Result := NS_ERROR_FAILURE;
    Exit;
  end;

  if (HistoryPosition+1)<HistoryCount then
    aContinue := True
  else
    aContinue := False;

  Result := NS_OK;
end;

function TGeckoBrowser.OnHistoryReload(aReloadURI: nsIURI; aReloadFlags: Longword; out aContinue: LongBool): Longword;
begin
  if @aContinue = nil then
  begin
    Result := NS_ERROR_FAILURE;
    Exit;
  end;

  aContinue := True;
  Result := NS_OK;
end;

function TGeckoBrowser.OnHistoryGotoIndex(aIndex: Longint; aGotoURI: nsIURI; out aContinue: LongBool): Longword;
begin
  if @aContinue = nil then
  begin
    Result := NS_ERROR_FAILURE;
    Exit;
  end;

  if aIndex in [0..HistoryCount-1] then
    aContinue := True
  else
    aContinue := False;

  Result := NS_OK;
end;

function TGeckoBrowser.OnHistoryPurge(aNumEntries: Longint; out aContinue: LongBool): Longword;
begin
  if @aContinue = nil then
  begin
    Result := NS_ERROR_FAILURE;
    Exit;
  end;

  aContinue := False;
  Result := NS_OK;
end;

// TControl 継ｳ
procedure TGeckoBrowser.Resize;
var
  BaseWindow: nsIBaseWindow;
  rc: TRect;
begin
  inherited Resize;

  if not Assigned(FWebBrowser) then Exit;
  BaseWindow := FWebBrowser as nsIBaseWindow;
  rc := ClientRect;
  BaseWindow.SetPositionAndSize(rc.Left, rc.Top, rc.Right-rc.left, rc.Bottom-rc.Top, False);
end;

procedure TGeckoBrowser.GotoIndex(aIndex: Integer);
var
  nav: nsIWebNavigation;
begin
  if not Supports(FWebBrowser, nsIWebNavigation, nav) then Exit;
  nav.GotoIndex(aIndex);
end;

function TGeckoBrowser.GetHistoryEntry(index: Integer): TGeckoBrowserHisoty;
var
  rv: Cardinal;
  nav: nsIWebNavigation;
  history: nsISHistory;
  entry: nsIHistoryEntry;
  str: IInterfacedCString;
  wstr: PWideChar;
  uri: nsIURI;
  bool: LongBool;
begin
  Result.URI := '';
  Result.Title := '';
  Result.IsSubFrame := False;

  rv := FWebBrowser.QueryInterface(nsIWebNavigation, nav);
  if NS_FAILED(rv) then Exit;

  rv := nav.GetSessionHistory(history);
  if NS_FAILED(rv) then Exit;

  rv := history.GetEntryAtIndex(index, False, entry);
  if NS_FAILED(rv) then Exit;

  rv := entry.GetURI(uri);
  if NS_FAILED(rv) then Exit;

  str := NewCString;
  rv := uri.GetSpec(str.ACString);
  if NS_FAILED(rv) then Exit;

  rv := entry.GetTitle(wstr);
  if NS_FAILED(rv) then Exit;

  rv := entry.GetIsSubFrame(bool);
  if NS_FAILED(rv) then Exit;

  Result.URI := str.ToString;
  Result.Title := WideString(wstr);
  Result.IsSubFrame := bool;

  nsMemory.Free(wstr);
end;

function TGeckoBrowser.GetHistoryPosition: Integer;
var
  nav: nsIWebNavigation;
  history: nsISHistory;
begin
  Result := -1;
  if NS_FAILED(FWebBrowser.QueryInterface(nsIWebNavigation, nav)) then Exit;
  if NS_FAILED(nav.GetSessionHistory(history)) then Exit;
  history.GetIndex(Result);
end;

function TGeckoBrowser.GetHistoryCount: Integer;
var
  nav: nsIWebNavigation;
  history: nsISHistory;
begin
  Result := 0;
  if NS_FAILED(FWebBrowser.QueryInterface(nsIWebNavigation, nav)) then Exit;
  if NS_FAILED(nav.GetSessionHistory(history)) then Exit;
  history.GetCount(Result);
end;

*)

function TCustomGeckoBrowserChrome.SafeCallException(obj: TObject; addr: Pointer): HRESULT;
begin
  UseParameter(obj); UseParameter(Addr);
  Result := E_FAIL;
end;

constructor TCustomGeckoBrowserListener.Create(ABrowser: TCustomGeckoBrowser);
begin
  inherited Create;
  FBrowser := ABrowser;
end;

procedure TCustomGeckoBrowserListener.InitListener(browser: TCustomGeckoBrowser);
var
  I: Integer;
  domWin: nsIDOMWindow;
  target: nsIDOMEventTarget;
begin
  AddWebBrowserListener(browser.WebBrowser);

  if Assigned(FDOMEvents) then
  begin
    I := 0;
    domWin := browser.ContentWindow;
    target := (domWin as nsIDOMWindow2).WindowRoot;
    while FDOMEvents[I].eventType <> etNone do
    begin
      with FDOMEvents[I] do
      begin
        target.AddEventListener(NewString(Name).AString, Self, true);
      end;
      Inc(I);
    end;
  end;
end;

procedure TCustomGeckoBrowserListener.ShutdownListener(browser: TCustomGeckoBrowser);
var
  I: Integer;
  domWin: nsIDOMWindow;
  target: nsIDOMEventTarget;
begin
  RemoveWebBrowserListener(browser.WebBrowser);

  if Assigned(FDOMEvents) then
  begin
    I := 0;
    domWin := browser.ContentWindow;
    target := (domWin as nsIDOMWindow2).WindowRoot;
    while FDOMEvents[I].eventType <> etNone do
    begin
      with FDOMEvents[I] do
      begin
        target.AddEventListener(NewString(Name).AString, Self, False);
      end;
      Inc(I);
    end;
  end;
end;

procedure TCustomGeckoBrowserListener.AddWebBrowserListener(browser: nsIWebBrowser);
var
  weak: nsIWeakReference;
  table: PInterfaceTable;
  i: Integer;
begin
  weak := GetWeakReference;
  table := ClassType.GetInterfaceTable;
  if Assigned(table) then
    for i:=0 to table.EntryCount-1 do
{$IFNDEF FPC}
      browser.AddWebBrowserListener(weak, table.Entries[i].IID);
{$ELSE}
 {$PUSH}
  {$R-}
      browser.AddWebBrowserListener(weak, table.Entries[i].IID^);  //FPC Entries is only array[0..0]!
 {$POP}
{$ENDIF}
end;

procedure TCustomGeckoBrowserListener.RemoveWebBrowserListener(browser: nsIWebBrowser);
var
  weak: nsIWeakReference;
  table: PInterfaceTable;
  i: Integer;
begin
  weak := GetWeakReference;
  table := ClassType.GetInterfaceTable;
  if Assigned(table) then
    for i:=0 to table.EntryCount-1 do
{$IFNDEF FPC}
      browser.RemoveWebBrowserListener(weak, table.Entries[i].IID);
{$ELSE}
 {$PUSH}
  {$R-}
      browser.RemoveWebBrowserListener(weak, table.Entries[i].IID^);
 {$POP}
{$ENDIF}
end;

function TCustomGeckoBrowserListener.SafeCallException(
                Obj: TObject; Addr: Pointer): HResult;
begin
  UseParameter(obj); UseParameter(Addr);
  Result := HRESULT(NS_ERROR_FAILURE);
end;

procedure TCustomGeckoBrowserListener.HandleEvent(aEvent: nsIDOMEvent);
var
  i: Integer;
  eventType: String;
  str: IInterfacedString;
  method: TMethod;
  eventHandler: TGeckoBrowserDOMEventHandler;
  domEvent: TGeckoDOMEvent;
begin
  if Assigned(FDOMEvents) then
  begin
    str := NewString;
    aEvent.GetType(str.AString);
    eventType := str.ToString;
    I := 0;
    while FDOMEvents[I].eventType <>etNone do
    begin
      if FDOMEvents[I].Name = eventType then
      begin
        method := GetMethodProp(FBrowser, FDOMEvents[I].propertyName);
        eventHandler := TGeckoBrowserDOMEventHandler(method);
        if Assigned(eventHandler) then
        begin
          domEvent.Name := FDOMEvents[I].Name;
          domEvent.EventType := FDOMEvents[I].eventType;
          domEvent.event := aEvent;
          eventHandler(FBrowser, domEvent);
        end;
        Exit;
      end;
      Inc(I);
    end;
  end;
end;

constructor TCustomGeckoBrowser.Create(AOwner: TComponent);
var
  Logo: TResourceStream;
begin
  inherited;

  {$IFDEF DEBUG}
  OutputDebugString('TGeckoBrowser.Create');
  {$ENDIF}
  if not (csDesigning in ComponentState) then
  begin
  end else begin
    Logo:=TResourceStream.Create(HINSTANCE,'ID_GECKO_LOGO',pchar(RT_RCDATA));
    FDesignTimeLogo:=TPortableNetworkGraphic.Create;
    FDesignTimeLogo.LoadFromStream(Logo);
    Logo.Free;
  end;
end;

destructor TCustomGeckoBrowser.Destroy;
begin
  {$IFDEF DEBUG}
  OutputDebugString('TGeckoBrowser.Destroy');
  {$ENDIF}

  if assigned(FDesignTimeLogo) then
    FreeAndNil(FDesignTimeLogo);
  ShutdownWebBrowser;

  Chrome := nil;
  Listener := nil;

  if FGeckoComponentsStartupSucceeded then
    GeckoComponentsShutdown;

  inherited;
end;

// override methods from TControl
procedure TCustomGeckoBrowser.Resize;
var
  baseWin: nsIBaseWindow;
  rc: TRect;
begin
  inherited Resize;

  if not Assigned(FWebBrowser) then Exit;
  baseWin := FWebBrowser as nsIBaseWindow;
  rc := GetClientRect;
  baseWin.SetPositionAndSize(rc.Left, rc.Top, rc.Right - rc.Left, rc.Bottom - rc.Top, False);
end;

procedure TCustomGeckoBrowser.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    DoGeckoComponentsStartup;
  end;
  inherited Loaded;
  DoInitializationIfNeeded;
end;

procedure TCustomGeckoBrowser.CreateWnd;
begin
  {$IFDEF DEBUG}
  OutputDebugString('TGeckoBrowser.CreateWnd');
  {$ENDIF}
  inherited CreateWnd;
  if not (csDesigning in ComponentState) and not FGeckoComponentsStartupSucceeded and not FInitializationStarted then
  begin
    DoGeckoComponentsStartup;
    DoInitializationIfNeeded;
  end;
end;

procedure TCustomGeckoBrowser.DestroyWnd;
begin
  {$IFDEF DEBUG}
  OutputDebugString('TGeckoBrowser.DestroyWnd');
  {$ENDIF}
  inherited DestroyWnd;
  if not FGeckoComponentsStartupSucceeded then
    FreeAndNIL(GeckoEngineDirectoryService);
end;

procedure TCustomGeckoBrowser.GoBack;
begin
  try
    (FWebBrowser as nsIWebNavigation).GoBack;
  except
    raise EGeckoBrowserNavigationError.CreateRes(
      PResStringRec(@SGeckoBrowserCannotGoBack));
  end;
end;

procedure TCustomGeckoBrowser.GoForward;
begin
  try
    (FWebBrowser as nsIWebNavigation).GoForward;
  except
    raise EGeckoBrowserNavigationError.CreateRes(
      PResStringRec(@SGeckoBrowserCannotGoForward));
  end;
end;

procedure TCustomGeckoBrowser.InitWebBrowser;
var
  baseWin: nsIBaseWindow;
  focus: nsIWebBrowserFocus;
  rc: TRect;
begin

  // Initialize WindowCreator
  if not InitWindowCreator then
    raise EGeckoBrowserError.CreateRes(PResStringRec(@SGeckoBrowserInitError));

  // Create Browser Object
  NS_CreateInstance(NS_WEBBROWSER_CID, nsIWebBrowser, FWebBrowser);

  try
    // Initialize Browser
    FWebBrowser.ContainerWindow := FChrome;
    baseWin := FWebBrowser as nsIBaseWindow;

    rc := ClientRect;
    baseWin.InitWindow(getNativeWindow,
                       nil,
                       rc.Left,
                       rc.Top,
                       rc.Right-rc.Left,
                       rc.Bottom-rc.Top);
    baseWin.Create();

    // Register Listeners
    FListeners.InitListener(Self);

    // Show Browser
    baseWin.SetVisibility(True);
    // Activate Focus
    focus := FWebBrowser as nsIWebBrowserFocus;
    focus.Activate;
  except
    raise EGeckoBrowserError.CreateRes(PResStringRec(@SGeckoBrowserInitError));
  end;
end;

procedure TCustomGeckoBrowser.InnerLoadURI(uri: WideString;
  Flags: PRUint32; referer: nsIURI; postData, headers: TStream);
var
  nav: nsIWebNavigation;
  post: nsIInputStream;
  head: nsIInputStream;
begin
  try
    nav := FWebBrowser as nsIWebNavigation;
    if Assigned(postData) then
      post := NS_NewInputStreamFromTStream(postData, True);
    if Assigned(headers) then
      head := NS_NewInputStreamFromTStream(headers, True);
    nav.LoadURI(PWideChar(uri), Flags, referer, post, head);
  except
    raise EGeckoBrowserNavigationError.CreateResFmt(
      PResStringRec(@SGeckoBrowserLoadURIError),
      [String(uri)]);
  end;
end;

procedure TCustomGeckoBrowser.LoadURI(const uri: WideString);
begin
  if FGeckoComponentsStartupSucceeded then
    InnerLoadURI(uri, 0, nil, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURI(const uri: WideString;
  const referer: UTF8String);
var
  ref: nsIURI;
  refStr: IInterfacedUTF8String;
begin
  refStr := NewUTF8String(referer);
  ref := NS_NewURI(refStr.AUTF8String);
  InnerLoadURI(uri, 0, ref, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURI(const uri: WideString; const referer: WideString);
var
  ref: nsIURI;
  refStr: IInterfacedUTF8String;
begin
  refStr := NewUTF8String(UTF8String(referer));
  ref := NS_NewURI(refStr.AUTF8String);
  InnerLoadURI(uri, 0, ref, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURI(const uri: WideString; referer: nsIURI);
begin
  InnerLoadURI(uri, 0, referer, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURIWithFlags(const uri: WideString;
  Flags: PRUint32);
begin
  InnerLoadURI(uri, Flags, nil, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURIWithFlags(const uri: WideString;
  Flags: PRUint32; const referer: UTF8String);
var
  ref: nsIURI;
  refStr: IInterfacedUTF8String;
begin
  refStr := NewUTF8String(UTF8String(referer));
  ref := NS_NewURI(refStr.AUTF8String);
  InnerLoadURI(uri, Flags, ref, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURIWithFlags(const uri: WideString; Flags: PRUint32; const referer: WideString);
var
  ref: nsIURI;
  refStr: IInterfacedUTF8String;
begin
  refStr := NewUTF8String(UTF8String(referer));
  ref := NS_NewURI(refStr.AUTF8String);
  InnerLoadURI(uri, Flags, ref, nil, nil);
end;

procedure TCustomGeckoBrowser.LoadURIWithFlags(const uri: WideString; Flags: PRUint32; referer: nsIURI);
begin
  InnerLoadURI(uri, Flags, referer, nil, nil);
end;
procedure TCustomGeckoBrowser.Reload;
begin
  ReloadWithFlags(NS_IWEBNAVIGATION_LOAD_FLAGS_NONE);
end;

procedure TCustomGeckoBrowser.ReloadWithFlags(AFlags: PRUint32);
var
  nav: nsIWebNavigation;
begin
  try
    nav := FWebBrowser as nsIWebNavigation;
    nav.Reload(AFlags);
  except
    raise EGeckoBrowserNavigationError.CreateRes(
      PResStringRec(@SGeckoBrowserCannotReload));
  end;
end;

procedure TCustomGeckoBrowser.DoGeckoComponentsStartup;
begin
  if not Assigned(GeckoEngineDirectoryService) then begin
    //This interface must be created as soon as possible because it
    //will be callbacked when starting the XRE which happend just
    //after the GeckoBrowser is created but before it is ready to be
    //used. The setup of this component is a one time operation, called
    //by the FIRST instance of GeckoBrowser and not called by the next
    //ones; and its data persists while the program is running.
    GeckoEngineDirectoryService:=IDirectoryServiceProvider.Create;
  end;
  if Assigned(FOnDirectoryService) then
    FOnDirectoryService(Self,GeckoEngineDirectoryService);
  try
    GeckoComponentsStartup;
    FGeckoComponentsStartupSucceeded := true;
    //Create the prompt service and register
    RegisterPromptService;
  except
    FGeckoComponentsStartupSucceeded := false;
  end;
end;

procedure TCustomGeckoBrowser.ShutdownWebBrowser;
begin
  if Assigned(FWebBrowser) then
  begin
    //FListeners.RemoveWebBrowserListener(FWebBrowser);
    FListeners.ShutdownListener(Self);
    FWebBrowser.SetContainerWindow(nil);
    FWebBrowser := nil;
  end;
end;

function TCustomGeckoBrowser.GetDisableJavaScript: Boolean;
begin
  Result:=FDisableJavaScript;
end;

procedure TCustomGeckoBrowser.SetDisableJavascript(const AValue: Boolean);
var
  iWebSetup: nsIWebBrowserSetup;
begin
  try
    if FInitialized then begin
      iWebSetup:=Self.FWebBrowser as nsIWebBrowserSetup;
      iWebSetup.SetProperty(NS_IWEBBROWSERSETUP_SETUP_ALLOW_JAVASCRIPT,PRInt32(not AValue));
    end;
    FDisableJavaScript:=AValue;
  except
    try
      Raise EGeckoHint.Create('Unable to disable JavaScript at this moment. Gecko not created?');
    except
    end;
  end;
end;

procedure TCustomGeckoBrowser.SetChrome(aChrome: TCustomGeckoBrowserChrome);
var
  old: TCustomGeckoBrowserChrome;
begin
  old := FChrome;
  FChrome := aChrome;
  if Assigned(FChrome) then
    FChrome._AddRef;
  if Assigned(old) then old._Release;
end;

procedure TCustomGeckoBrowser.SetListener(aListener: TCustomGeckoBrowserListener);
var
  old: TCustomGeckoBrowserListener;
begin
  old := FListeners;
  FListeners := aListener;
  if Assigned(FListeners) then
    FListeners._AddRef;
  if Assigned(old) then old._Release;
end;

procedure TCustomGeckoBrowser.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTTAB;
end;

constructor TGeckoBrowserChrome.Create(Browser: TGeckoBrowser);
begin
  inherited Create;
  FBrowser := Browser;
end;

destructor TGeckoBrowserChrome.Destroy;
begin
  inherited Destroy;
end;

procedure TGeckoBrowserChrome.SetStatus(
                statusType: PRUint32;
                const status: PWideChar);
begin
  {$IFDEF DEBUG}
  {
  OutputDebugString(PChar(
    'GeckoBrowser.SetStatus('+status+')'
  ));
  }
  {$ENDIF}
  UseParameter(statusType);
  if Assigned(FBrowser.OnStatusChange) then
    FBrowser.OnStatusChange(FBrowser, status);
end;

function TGeckoBrowserChrome.GetWebBrowser()
                : nsIWebBrowser;
begin
  Result := FBrowser.FWebBrowser;
end;

procedure TGeckoBrowserChrome.SetWebBrowser(
                aWebBrowser: nsIWebBrowser);
begin
  FBrowser.FWebBrowser := aWebBrowser;
end;

function TGeckoBrowserChrome.GetChromeFlags()
                : PRUint32;
begin
  //TODO 2 -cTGeckoBrowserChrome: Chrome フラグの扱いをどうしようか
  Result := NS_IWEBBROWSERCHROME_CHROME_DEFAULT;
end;

procedure TGeckoBrowserChrome.SetChromeFlags(
                aChromeFlags: PRUint32);
begin
  UseParameter(aChromeFlags);
end;

procedure TGeckoBrowserChrome.DestroyBrowserWindow();
begin
  if Assigned(FBrowser.FOnCloseWindow) then
    FBrowser.FOnCloseWindow(FBrowser);
end;

procedure TGeckoBrowserChrome.SizeBrowserTo(
                aCX: PRInt32;
                aCY: PRInt32);
begin
  FBrowser.Width := aCX;
  FBrowser.Height:= aCY;
end;

procedure TGeckoBrowserChrome.ShowAsModal();
begin
end;

function TGeckoBrowserChrome.IsWindowModal()
                : PRBool;
begin
  Result := False;
end;

procedure TGeckoBrowserChrome.ExitModalEventLoop(
                aStatus: nsresult);
begin
  UseParameter(aStatus);
end;

procedure TGeckoBrowserChrome.SetDimensions(
                flags: PRUint32;
                x: PRInt32;
                y: PRInt32;
                cx: PRInt32;
                cy: PRInt32);
const
  FLAGS_POSITION   = ns_IEmbeddingSiteWindow_DIM_FLAGS_POSITION;
  FLAGS_SIZE_INNER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_INNER;
  FLAGS_SIZE_OUTER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_OUTER;
var
  bounds: TRect;
  clientrect: TRect;
  w, h: Integer;
begin
  bounds := FBrowser.BoundsRect;
  clientrect := FBrowser.ClientRect;
  w := bounds.Right - bounds.Left;
  h := bounds.Bottom - bounds.Top;
  if (flags and FLAGS_POSITION)<>0 then
  begin
    if (flags and FLAGS_SIZE_INNER)<>0 then
    begin
      SetRect(bounds, x, y, x+(w-clientrect.Right)+cx, y+(h-clientrect.Bottom)+cy);
    end else
    if (flags and FLAGS_SIZE_OUTER)<>0 then
    begin
      SetRect(bounds, x, y, x+cx, y+cy);
    end else
    begin
      SetRect(bounds, x, y, x+w, y+h);
    end;
  end else
  if (flags and FLAGS_SIZE_INNER)<>0 then
  begin
    bounds.Right := bounds.Left + x+(w-clientrect.Right)+cx;
    bounds.Bottom := bounds.Top + y+(h-clientrect.Bottom)+cy;
  end else
  if (flags and FLAGS_SIZE_OUTER)<>0 then
  begin
    bounds.Right := bounds.Left + cx;
    bounds.Bottom := bounds.Top + cy;
  end;
  FBrowser.BoundsRect := bounds;
end;

procedure TGeckoBrowserChrome.GetDimensions(
                flags: PRUint32;
                out x: PRInt32;
                out y: PRInt32;
                out cx: PRInt32;
                out cy: PRInt32);
const
  FLAGS_POSITION   = NS_IEMBEDDINGSITEWINDOW_DIM_FLAGS_POSITION;
  FLAGS_SIZE_INNER = NS_IEMBEDDINGSITEWINDOW_DIM_FLAGS_SIZE_INNER;
  FLAGS_SIZE_OUTER = NS_IEMBEDDINGSITEWINDOW_DIM_FLAGS_SIZE_OUTER;
begin
  if (flags and FLAGS_POSITION)<>0 then
  begin
    x := FBrowser.Left;
    y := FBrowser.Top;
  end;

  if (flags and FLAGS_SIZE_INNER)<>0 then
  begin
    cx := FBrowser.ClientWidth;
    cy := FBrowser.ClientHeight;
  end;
  if (flags and FLAGS_SIZE_OUTER)<>0 then
  begin
    cx := FBrowser.Width;
    cy := FBrowser.Height;
  end;
end;

procedure TGeckoBrowserChrome.SetFocus();
begin
  if Assigned(FBrowser.FOnVisibleChange) then begin
    //Give the browser a chance to become visible
    FBrowser.FOnVisibleChange(FBrowser,true);
  end;
  try
    FBrowser.SetFocus;
  except
    Raise EGeckoHint.Create('Unable to set focus to '+FBrowser.Name);
  end;
end;

function TGeckoBrowserChrome.GetVisibility(): PRBool;
begin
  // TODO 1 -cTGeckoBrowserChrome: TGeckoBrowserChrome.GetVisibility はどうすべきか
  Result := True;
end;

procedure TGeckoBrowserChrome.SetVisibility(
                aVisibility: PRBool);
begin
  UseParameter(aVisibility);
  //TODO 1 -cTGeckoBrowserChrome: TGeckoBrowserChrome.SetVisibility の実装
end;

function TGeckoBrowserChrome.GetTitle(): PWideChar;
var
  pstr: PWideChar;
  title: WideString;
  len: Integer;
begin
  title := FBrowser.FTitle;
  len := Length(title);
  pstr := PWideChar(title);
  Result := nsMemory.Clone(pstr, (len+1)*2);
  if not Assigned(Result) then
    OutOfMemoryError;
end;

procedure TGeckoBrowserChrome.SetTitle(
                const aTitle: PWideChar);
begin
  FBrowser.FTitle := aTitle;
  if Assigned(FBrowser.OnTitleChange) then
    FBrowser.OnTitleChange(FBrowser, FBrowser.FTitle);
end;

function TGeckoBrowserChrome.GetSiteWindow(): Pointer;
begin
{$PUSH}
{$HINTS OFF}
  Result := Pointer(FBrowser.Handle);
{$POP}
end;

constructor TGeckoBrowserListener.Create(browser: TGeckoBrowser);
const
  //Most usual events at the beginning to improve handling speed.
  events: array [0..15] of TGeckoDOMEventRegister = (
    (name:'mousemove';      eventType:etMouseEvent; propertyName:'OnDOMMouseMove' ),
    (name:'DOMMouseScroll'; eventType: etMouseEvent;propertyName:'OnDOMMouseScroll'),
    (name:'focus';          eventType:etEvent;      propertyName:'OnDOMFocus'),
    (name:'load';           eventType:etEvent;      propertyName:'OnDOMLoad' ),
    (name:'click';          eventType:etMouseEvent; propertyName:'OnDOMClick' ),
    (name:'mouseup';        eventType:etMouseEvent; propertyName:'OnDOMMouseUp' ),
    (name:'mousedown';      eventType:etMouseEvent; propertyName:'OnDOMMouseDown' ),
    (name:'keyup';          eventType:etEvent;      propertyName:'OnDOMKeyUp' ),
    (name:'keydown';        eventType:etEvent;      propertyName:'OnDOMKeyDown'),
    (name:'keypress';       eventType:etEvent;      propertyName:'OnDOMKeyPress'),
    (name:'DOMLinkAdded';   eventType: etEvent;     propertyName:'OnDOMLinkAdded'),
    (name:'dragover';       eventType:etEvent;      propertyName:'OnDOMDragOver'),
    (name:'draggesture';    eventType:etEvent;      propertyName:'OnDOMDragGesture'),
    (name:'dragdrop';       eventType:etEvent;      propertyName:'OnDOMDragDrop'),
    (name:'dragexit';       eventType:etEvent;      propertyName:'OnDOMDragExit'),
    (name:'';               eventType:etNone;       propertyName:'')
  );
begin
  inherited Create(browser);
  FDOMEvents := PGeckoDOMEventRegisterArray(@events);
end;

procedure TGeckoBrowserListener.OnStateChange(
                aWebProgress: nsIWebProgress;
                aRequest: nsIRequest;
                aStateFlags: PRUint32;
                aStatus: nsresult);
{$IFDEF DEBUG}
var
  uri: nsIURI;
  str: IInterfacedCString;
  channel: nsIChannel;
{$ENDIF}
const
  STATE_IS_DOCUMENT = NS_IWEBPROGRESSLISTENER_STATE_IS_DOCUMENT;
  STATE_IS_NETWORK  = NS_IWEBPROGRESSLISTENER_STATE_IS_NETWORK;
  STATE_START       = NS_IWEBPROGRESSLISTENER_STATE_START;
  STATE_STOP        = NS_IWEBPROGRESSLISTENER_STATE_STOP;
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aStatus);
  if (aStateFlags and STATE_IS_DOCUMENT)<>0 then
  begin
    // ﾔの変化はドキュャ塔gに対してである
    if (aStateFlags and STATE_START)<>0 then
    begin
      // ドキュャ塔gの読み桙ﾝが開始された
      {$IFDEF DEBUG}
      {
      OutputDebugString('GeckoBrowser.OnDocumentBegin');
      }
      {$ENDIF}
      if Assigned(FBrowser.OnDocumentBegin) then
        FBrowser.OnDocumentBegin(Self);
    end else
    if (aStateFlags and STATE_STOP)<>0 then
    begin
      // ドキュャ塔gの読み桙ﾝが完了した
      {$IFDEF DEBUG}
      {
      OutputDebugString('GeckoBrowser.OnDocumentComplete');
      }
      {$ENDIF}
      if Assigned(FBrowser.OnDocumentComplete) then
        FBrowser.OnDocumentComplete(Self);
      if Assigned(FBrowser.OnStatusChange) then
        FBrowser.OnStatusChange(FBrowser, '');
    end;
  end;
  if (aStateFlags and STATE_IS_NETWORK)<>0 then
  begin
    // ﾔの変化はネットゼクに対してである
    if (aStateFlags and STATE_START)<>0 then
    begin
      // ネットゼクの転送が開始された鼇
      {$IFDEF DEBUG}
      {
      str := NewCString;
      channel := aRequest as nsIChannel;
      uri := channel.URI;
      uri.GetSpec(str.ACString);
      OutputDebugStringA(
        PAnsiChar('GeckoBrowser.OnDownloadBegin('+str.ToString+')'));
      }
      {$ENDIF}
    end else
    if (aStateFlags and STATE_STOP)<>0 then
    begin
      // ネットゼクの転送がI了した鼇
      {$IFDEF DEBUG}
      {
      str := NewCString;
      channel := aRequest as nsIChannel;
      uri := channel.URI;
      uri.GetSpec(str.ACString);
      OutputDebugStringA(
        PAnsiChar('GeckoBrowser.OnDownloadComplete('+str.ToString+')'));
      }
      {$ENDIF}
      if Assigned(FBrowser.OnStatusChange) then
        FBrowser.OnStatusChange(FBrowser, '');
    end;
  end;
end;

procedure TGeckoBrowserListener.OnProgressChange(
                aWebProgress: nsIWebProgress;
                aRequest: nsIRequest;
                aCurSelfProgress: PRInt32;
                aMaxSelfProgress: PRInt32;
                aCurTotalProgress: PRInt32;
                aMaxTotalProgress: PRInt32);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aCurSelfProgress);
  UseParameter(aMaxSelfProgress);
  if Assigned(FBrowser.OnProgressChange) then
  begin
    {$IFDEF DEBUG}
    {
    OutputDebugString(PChar(
      'OnProgressListener('+IntToStr(aCurTotalProgress)+'/'+IntToStr(aMaxTotalProgress)+')'
    ));
    }
    {$ENDIF}
    FBrowser.OnProgressChange(FBrowser, aCurTotalProgress, aMaxTotalProgress);
  end;
end;

procedure TGeckoBrowserListener.OnLocationChange(
                aWebProgress: nsIWebProgress;
                aRequest: nsIRequest;
                location: nsIURI);
var
  str: IInterfacedCString;
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  str := NewCString;
  location.GetSpec(str.ACString);
  {$IFDEF DEBUG}
  {
  OutputDebugStringA(PAnsiChar(
    'GeckoBrowser.LocationChange('+str.ToString+')'
  ));
  }
  {$ENDIF}
  if Assigned(FBrowser.OnLocationChange) then
    FBrowser.OnLocationChange(FBrowser, str.ToString);
end;

procedure TGeckoBrowserListener.OnStatusChange(
                aWebProgress: nsIWebProgress;
                aRequest: nsIRequest;
                aStatus: nsresult;
                const aMessage: PWideChar);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aStatus);
{$IFDEF DEBUG}
{
  OutputDebugStringW(PWideChar(
    'GeckoBrowser.OnStatusChange('+aMessage+')'
  ));
}
{$ENDIF}
  if Assigned(FBrowser.OnStatusChange) then
    FBrowser.OnStatusChange(FBrowser, aMessage);
end;

procedure TGeckoBrowserListener.OnSecurityChange(
                aWebProgress: nsIWebProgress;
                aRequest: nsIRequest;
                state: PRUint32);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(State);
  //TODO 1 -cTGeckoBrowserListner: TGeckoBrowserListener.OnSecurityChange の記q
end;

constructor TGeckoBrowser.Create(AOwner: TComponent);
begin
  if not Assigned(GeckoListBrowsers) then GeckoListBrowsers:=TFPList.Create;
  GeckoListBrowsers.Add(Self);
  inherited;
  Chrome := TGeckoBrowserChrome.Create(Self);
  Listener := TGeckoBrowserListener.Create(Self);
  {$IFDEF LCLGTK2}
  //Creates a timer that forces the event queue to be pooled. This could be a
  //bug in the LCL or a problem with events coming for the XULRUNNER.
  //A timer of 100 ms creates the feel that everything is pooled as it comes and
  //it only have a visual impact.
  EventPool := TTimer.Create(Self);
  EventPool.Interval:=100;
  EventPool.Enabled:=true;
  EventPool.OnTimer:=EventPoolProc;
  {$ENDIF}
end;

destructor TGeckoBrowser.Destroy;
begin
  inherited Destroy;
  GeckoListBrowsers.Remove(Self);
  if GeckoListBrowsers.Count=0 then FreeAndNil(GeckoListBrowsers);
end;

class function TGeckoBrowser.GetGeckoBrowserWithDOMWindow(
  constref DOMWindow: nsIDOMWindow): TGeckoBrowser;
var
  ThisGecko: TGeckoBrowser;
  t1,t2: nsIDOMWindow;
  j: integer;
begin
  Result:=nil;
  if Assigned(GeckoListBrowsers) then begin
    for j := 0 to GeckoListBrowsers.Count-1 do begin
      ThisGecko:=TGeckoBrowser(GeckoListBrowsers[j]);
      t1:=ThisGecko.GetContentWindow.Parent;
      t2:=DOMWindow.Parent;
      if t1=t2 then begin
        Result:=ThisGecko;
        break;
      end;
    end;
  end;
end;

procedure TGeckoBrowser.Print(const aShowPrinterSelectDialog: Boolean);
var
  PrintInterface: nsIWebBrowserPrint;
  PrintSettings: nsIPrintSettings;
begin
  PrintInterface:=GetWebBrowserPrint;
  if Assigned(PrintInterface) then begin
    PrintSettings:=PrintInterface.GetGlobalPrintSettings;
    if Assigned(PrintSettings) then begin
      PrintSettings.ShowPrintProgress:=false; //true implies need of nsIWebProgressListener
      PrintSettings.PrintBGImages:=true;
      PrintSettings.SetPrintSilent(not aShowPrinterSelectDialog);
    end;
    PrintInterface.Print(PrintSettings,nil);
  end;
end;

function TGeckoBrowserChrome.NS_GetInterface(constref uuid: TGUID; out _result): nsresult;
begin
  if IsEqualGUID(uuid, nsIDOMWindow) then
  begin
    Result:= nsresult(FBrowser.GetContentWindow.QueryInterface(uuid, _result));
  end else
  begin
// FPC port: Result is PRUInt32, but QueryInterface returns Longint,
//  so cast to nsresult to prevent range check error.
//    Result := QueryInterface(uuid, _result);
    Result := nsresult(QueryInterface(uuid, _result));
  end;
end;

procedure TGeckoBrowserChrome.OnShowContextMenu(aContextFlags: PRUint32;
  aUtils: nsIContextMenuInfo);
(*
const
  CONTEXT_NONE             = NS_ICONTEXTMENULISTENER2_CONTEXT_NONE;
  CONTEXT_LINK             = NS_ICONTEXTMENULISTENER2_CONTEXT_LINK;
  CONTEXT_IMAGE            = NS_ICONTEXTMENULISTENER2_CONTEXT_IMAGE;
  CONTEXT_DOCUMENT         = NS_ICONTEXTMENULISTENER2_CONTEXT_DOCUMENT;
  CONTEXT_TEXT             = NS_ICONTEXTMENULISTENER2_CONTEXT_TEXT;
  CONTEXT_INPUT            = NS_ICONTEXTMENULISTENER2_CONTEXT_INPUT;
  CONTEXT_BACKGROUND_IMAGE = NS_ICONTEXTMENULISTENER2_CONTEXT_BACKGROUND_IMAGE;*)
var
  cmenu: TCtxMenuInfo;
begin
    if Assigned(FBrowser.OnContextMenu) then
  begin
    cmenu := TCtxMenuInfo.Create(aContextFlags, aUtils);
    try
      FBrowser.OnContextMenu(FBrowser, cmenu);
    finally
      cmenu.Free;
    end;
  end;
end;

procedure TGeckoBrowserChrome.OnShowTooltip(aXCoords: PRInt32; aYCoords: PRInt32; const aTipText: PWideChar); safecall;
{$IFNDEF FPC}
var
  r:TRect;
  p,ap:TPoint;
//  height:Integer;
{$ENDIF}
begin
{$IFNDEF LCL}
  if FBrowser.FHint = nil then
  FBrowser.FHint := THintWindow.Create(FBrowser);
  r := FBrowser.FHint.CalcHintRect(400,aTipText,nil);
//  height := r.Bottom;
  ap.X := aXCoords;
  ap.Y := aYCoords;
  p := FBrowser.ClientToScreen(ap);
  r.Left:=p.x;
  r.Top:=p.y-r.Bottom;
  r.Right:=r.Right +p.x;
  r.Bottom:=p.y;
  FBrowser.FHint.ActivateHint(r,aTipText);
{$ELSE}
  UseParameter(aXCoords);
  UseParameter(aYCoords);
  FBrowser.Hint:=aTiptext;
  FBrowser.ShowHint:=true;
{$ENDIF}
end;

procedure TGeckoBrowserChrome.OnHideTooltip(); safecall;
begin
{$IFNDEF LCL}
  FBrowser.FHint.ReleaseHandle;
{$ENDIF}
end;

{$IFDEF LCLGTK2}
procedure TGeckoBrowser.EventPoolProc(Sender: TObject);
begin
  //Do nothing. Just a hack.
end;
{$ENDIF}

function TGeckoBrowser.DoCreateChromeWindow(
  chromeFlags: Longword): nsIWebBrowserChrome;
var
  newWin: TCustomGeckoBrowser;
begin
  if Assigned(OnNewWindow) then
  begin
    newWin := nil;
    OnNewWindow(Self, chromeFlags, newWin);
    if Assigned(newWin) then
      Result := newWin.FChrome;
  end;
end;

function TCustomGeckoBrowser.GetWebBrowserChrome: nsIWebBrowserChrome;
begin
  Result := FChrome;
end;

function TCustomGeckoBrowser.GetContentDocument: nsIDOMDocument;
begin
  Result := FWebBrowser.ContentDOMWindow.Document;
end;

function TCustomGeckoBrowser.GetContentWindow: nsIDOMWindow;
begin
  Result := FWebBrowser.ContentDOMWindow;
end;

procedure TCustomGeckoBrowser.WMEraseBkGnd(var Msg: TMessage);
begin
  // Cancel erase background actions.
  Msg.Result := 0;
end;

procedure TCustomGeckoBrowser.Paint;
var
  rc: TRect;
  baseWin: nsIBaseWindow;
begin
  if csDesigning in ComponentState then
  begin
    rc := ClientRect;
    Canvas.Brush.Color:=clWhite;
    Canvas.FillRect(rc);
    if Assigned(FDesignTimeLogo) then
      Canvas.StretchDraw(rc,FDesignTimeLogo)
  end else
  begin
    if FGeckoComponentsStartupSucceeded then
    begin
      baseWin := FWebBrowser as nsIBaseWindow;
      baseWin.Repaint(true);
      {$IFDEF LCLGTK2}
      Self.AdjustSize;
      {$ENDIF}
    end
    else
    begin
      rc := ClientRect;
      Canvas.FillRect(rc);
      Canvas.TextOut(0,0,SGeckoBrowserInitError);
    end;
  end;
  inherited;
end;

function TCustomGeckoBrowser.GetCanGoBack: Boolean;
var
  nav: nsIWebNavigation;
  history: nsISHistory;
  index: PRInt32;
begin
  nav := FWebBrowser as nsIWebNavigation;
  history := nav.SessionHistory;
  index := history.Index;

  Result := (index > 0);
end;

function TCustomGeckoBrowser.GetCanGoForward: Boolean;
var
  nav: nsIWebNavigation;
  history: nsISHistory;
  count, index: PRInt32;
begin
  nav := FWebBrowser as nsIWebNavigation;
  history := nav.SessionHistory;
  count := history.Count;
  index := history.Index;

  Result := (index+1<count);
end;

function TCustomGeckoBrowser.GetWebBrowserFind: nsIWebBrowserFind;
begin
  Result := FWebBrowser as nsIWebBrowserFind;
end;

function TCustomGeckoBrowser.GetWebBrowserPrint: nsIWebBrowserPrint;
var
  ir:nsIInterfaceRequestor;
  wbp:nsIWebBrowserPrint;
begin
  ContentWindow.QueryInterface(nsIInterfaceRequestor,ir);
  ir.GetInterface(nsIWebBrowserPrint, wbp);
  Result := wbp;
end;

function TCustomGeckoBrowser.GetWebNavigation: nsIWebNavigation;
begin
  Result := FWebBrowser as nsIWebNavigation;
end;

function TCustomGeckoBrowser.GetNativeWindow: nativeWindow;
{$IFDEF LCLCocoa}
var
  ARect : NSRect;
  AView : NSView;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := Handle;
{$ENDIF}
{$IFDEF LCLCarbon}
  Result := nativeWindow(TCarbonWindow(Handle).Window);
{$ENDIF}
{$IFDEF LCLCocoa}
  ARect := NSView(TCocoaWindow(Handle).contentView).visibleRect;
  ARect.size.width := ARect.size.width - 30;
  ARect.size.height := ARect.size. height - 30;
  ARect.origin.x := 15;
  ARect.origin.y := 15;
  AView := NSView.alloc.initWithFrame(ARect);
  NSView(TCocoaWindow(Handle).contentView).addSubView(AView);
  //Maybe Result := nativeWindow(AView) ? for 64 bits ?
  Result := THANDLE(AView);
{$ENDIF}
{$IFDEF LCLGtk}
  Result := Handle;
{$ENDIF}
{$IFDEF LCLGtk2}
  Result := nativeWindow(PGtkWindow(Handle)^.default_widget);
{$ENDIF}
end;

procedure TCustomGeckoBrowser.DoInitializationIfNeeded;
begin
  if not FInitializationStarted and FGeckoComponentsStartupSucceeded then
    if not (csDesigning in ComponentState) then
    begin
      FInitializationStarted:=true;
      InitWebBrowser;
      LoadURI('about:blank');
      FInitialized:=true;
      if Assigned(FOnSetupProperties) then begin
        FOnSetupProperties(Self);
      end;
      //Set again the published properties
      SetDisableJavascript(FDisableJavaScript);
    end;
end;

{
function TCustomGeckoBrowser.GetMarkupDocumentViewer: nsIMarkupDocumentViewer;
var
  mdv:nsIMarkupDocumentViewer;
begin
  Result:=nil;
  (DocShell as nsIInterfaceRequestor).GetInterface(
    ns_IMarkupDocumentViewer_iid, mdv
  );
  Result:=mdv;
end;
function TCustomGeckoBrowser.GetDocShell: nsIDocShell;
var
  ds: nsIDocShell;
begin
  Result := nil;
  NS_GetInterface(FWebBrowser, NS_IDOCSHELL_IID, ds);
  Result := ds;
end;

function TCustomGeckoBrowser.GetDocumentCharsetInfo: nsIDocumentCharsetInfo;
begin
  Result := DocShell.GetDocumentCharsetInfo;
end;
}

function TGeckoBrowser.GetURIString: UTF8String;
var
  str: IInterfacedUTF8String;
begin
  Result:='';
  str :=NewUTF8String;
//URI
  if Self.WebNavigation <> nil then
    Self.WebNavigation.CurrentURI.GetSpec(str.AUTF8String);
  Result := str.ToString;
end;

procedure TGeckoBrowserChrome.FocusPrevElement();
var
  Ancestor: TWinControl;
begin
  Ancestor := FBrowser.Parent;
  while Assigned(Ancestor) and (not(Ancestor is TForm)) do
    Ancestor := Ancestor.Parent;
  if Assigned(Ancestor) then
    PostMessage(Ancestor.Handle, WM_NEXTDLGCTL, 1, 0);
end;

procedure TGeckoBrowserChrome.FocusNextElement();
var
  Ancestor: TWinControl;
begin
  Ancestor := FBrowser.Parent;
  while Assigned(Ancestor) and (not(Ancestor is TForm)) do
    Ancestor := Ancestor.Parent;
  if Ancestor <> nil then
    PostMessage(Ancestor.Handle, WM_NEXTDLGCTL, 0, 0);
end;

function TGeckoBrowserChrome.GetCreateWindowTarget: IGeckoCreateWindowTarget;
begin
  Supports(FBrowser, IGeckoCreateWindowTarget, Result);
end;

procedure TGeckoBrowserListener.OnHistoryNewEntry(aNewURI: nsIURI);
begin
  UseParameter(aNewURI);
end;

function TGeckoBrowserListener.OnHistoryGoBack(aBackURI: nsIURI): PRBool;
var
  Handled:Boolean;
  aContinue:PRBool;
begin
  Handled:=false;
  if Assigned(FBrowser.FOnGoBack) then
    FBrowser.FOnGoBack(Self,aBackURI,aContinue,Handled);
  if Handled then begin
    Result := aContinue;
  end
  else {if not Handled then }begin
    {if (HistoryPosition>0) then
      Result := True
    else
      Result := False;}
    Result := True;
  end;
end;

function TGeckoBrowserListener.OnHistoryGoForward(aForwardURI: nsIURI): PRBool;
var
  Handled:Boolean;
  aContinue:PRBool;
begin
  Handled:=false;
  if Assigned(FBrowser.FOnGoForward) then
    FBrowser.FOnGoForward(Self,aForwardURI,aContinue,Handled);
  if Handled then begin
    Result := aContinue;
  end
  else {if not Handled then} begin
    {if (HistoryPosition+1)<HistoryCount then
      Result := True
    else
      Result := False;}
    Result := True;
  end;
end;

function TGeckoBrowserListener.OnHistoryReload(aReloadURI: nsIURI; aReloadFlags: PRUint32): PRBool;
begin
  UseParameter(aReloadURI);
  UseParameter(aReloadFlags);
  Result := True;
end;

function TGeckoBrowserListener.OnHistoryGotoIndex(aIndex: PRInt32; aGotoURI: nsIURI): PRBool;
var
  Handled:Boolean;
  aContinue:PRBool;
begin
  Handled:=false;
  if Assigned(FBrowser.FOnGoToIndex) then
    FBrowser.FOnGoToIndex(Self,aIndex,aGotoURI,aContinue,Handled);

  if Handled then begin
    Result := aContinue;
  end
  else begin
    {if aIndex in [0..HistoryCount-1] then
      Result := True
    else
      Result := False;}
    Result := True;
  end;
end;

function TGeckoBrowserListener.OnHistoryPurge(aNumEntries: PRInt32): PRBool;
begin
  UseParameter(aNumEntries);
  Result := True;
end;

constructor TCtxMenuInfo.Create(flags: Longword; info: nsIContextMenuInfo);
begin
  inherited Create;

  FInfo := info;
  FFlags := [];

  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_LINK) then
    FFlags := FFlags + [cmfLink];
  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_IMAGE) then
    FFlags := FFlags + [cmfImage];
  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_DOCUMENT) then
    FFlags := FFlags + [cmfDocument];
  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_TEXT) then
    FFlags := FFlags + [cmfText];
  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_INPUT) then
    FFlags := FFlags + [cmfInput];
  if 0<>(flags and ns_IContextMenuListener2_CONTEXT_BACKGROUND_IMAGE) then
    FFlags := FFlags + [cmfBGImage];
end;

function TCtxMenuInfo.GetAssociatedLink: WideString;
var
  str: IInterfacedString;
begin
  try
    str := NewString;
    FInfo.GetAssociatedLink(str.AString);
    Result := str.ToString;
  except
  end;
end;

function TCtxMenuInfo.GetImageURL: UTF8String;
var
  str: IInterfacedUTF8String;
  uri: nsIURI;
begin
  try
    str := NewUTF8String;
    uri := FInfo.GetImageSrc();
    uri.GetSpec(str.AUTF8String);
    Result := str.ToString;
  except
  end;
end;

function TCtxMenuInfo.GetBGImageURL: UTF8String;
var
  str: IInterfacedUTF8String;
  uri: nsIURI;
begin
  try
    str := NewUTF8String;
    uri := FInfo.GetBackgroundImageSrc();
    uri.GetSpec(str.AUTF8String);
    Result := str.ToString;
  except
  end;
end;

function TCtxMenuInfo.GetMouseEvent: nsIDOMEvent;
begin
  Result := FInfo.MouseEvent;
end;

function TCtxMenuInfo.GetTargetNode: nsIDOMNode;
begin
  Result := FInfo.TargetNode;
end;

function TCtxMenuInfo.GetImageContainer: imgIContainer;
begin
  Result := FInfo.ImageContainer;
end;

function TCtxMenuInfo.GetImageSrc: nsIURI;
begin
  Result := FInfo.ImageSrc;
end;

function TCtxMenuInfo.GetBGImageContainer: imgIContainer;
begin
  Result := FInfo.BackgroundImageContainer;
end;

function TCtxMenuInfo.GetBGImageSrc: nsIURI;
begin
  Result := FInfo.BackgroundImageSrc;
end;

{$IFDEF LCL}
initialization
{$I GeckoBrowser.lrs}
{$ENDIF}

end.

