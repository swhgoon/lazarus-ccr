unit Brow10;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  nsXRE,
  nsXPCOM,
  nsXPCOMGlue,
  nsTypes,
  nsError,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls
  {$IFDEF LCLCarbon}, CarbonPrivate {$ENDIF}
  {$IFDEF LCLCocoa}, CocoaPrivate {$ENDIF};

type
  nsIWidget = interface end;
  nsIBaseWindow = interface(nsISupports)
  ['{046BC8A0-8015-11d3-AF70-00A024FFC08C}']
    procedure InitWindow(parentNativeWindow: Pointer;
                         parentWidget: nsIWidget;
                         x: PRInt32;
                         y: PRInt32;
                         cx: PRInt32;
                         cy: PRInt32); safecall;
    procedure Create(); safecall;
    procedure Destroy(); safecall;
    procedure SetPosition(x: PRInt32;
                          y: PRInt32); safecall;
    procedure GetPosition(out x: PRInt32;
                          out y: PRInt32); safecall;
    procedure SetSize(cx: PRInt32;
                      cy: PRInt32); safecall;
    procedure GetSize(out cx: PRInt32;
                      out cy: PRInt32); safecall;
    procedure SetPositionAndSize(x: PRInt32;
                                 y: PRInt32;
                                 cx: PRInt32;
                                 cy: PRInt32); safecall;
    procedure GetPositionAndSize(out x: PRInt32;
                                 out y: PRInt32;
                                 out cx: PRInt32;
                                 out cy: PRInt32); safecall;
    procedure Repaint(force: PRBool); safecall;
    function GetParentWidget: nsIWidget; safecall;
    procedure SetParentWidget(aParentWidget: nsIWidget); safecall;
    function GetNativeWindow: Pointer; safecall;
    procedure SetNativeWindow(aNativeWindow: Pointer); safecall;
    function GetVisibility: PRBool; safecall;
    procedure SetVisibility(aVisibility: PRBool); safecall;
    function GetEnabled: PRBool; safecall;
    procedure SetEnabled(aEnabled: PRBool); safecall;
    function GetBlurSupression: PRBool; safecall;
    procedure SetBlurSupression(aBlurSupression: PRBool); safecall;
    function GetMainWidget: nsIWidget; safecall;
    procedure SetFocus(); safecall;
    function GetTitle: PWideChar; safecall;
    procedure SetTitle(aTitle: PWideChar); safecall;
  end;

  nsIWebNavigation = interface(nsISupports)
  ['{F5D9E7B0-D930-11d3-B057-00A024FFC08C}']
    function GetCanGoBack: PRBool; safecall;
    function GetCanForward: PRBool; safecall;
    procedure GoBack(); safecall;
    procedure GoForward(); safecall;
    procedure GotoIndex(index: PRInt32); safecall;
    procedure LoadURI(uri: PWideChar;
                      loadFlags: PRUint32;
                      referer: nsIURI;
                      postData: nsIInputStream;
                      headers: nsIInputStream); safecall;
    procedure Reload(reloadFlags: PRUint32); safecall;
    procedure Stop(stopFlags: PRUint32); safecall;
    function GetDocument: nsIDOMDocument; safecall;
    function GetCurrentURI: nsIURI; safecall;
    function GetReferringURI: nsIURI; safecall;
    function GetSessionHistory: nsISHistory; safecall;
  end;

  nsIDOMWindowInternal = interface(nsISupports)
  ['{f914492c-0138-4123-a634-6ef8e3f126f8}']
  end;

  nsIWindowMediator = interface(nsISupports)
  ['{0659cb83-faad-11d2-8e19-b206620a657c}']
    function getMostRecentWindow(windowType: PWideChar): nsIDOMWindowInternal; stdcall;
  end;


type
  TForm1 = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FBrowser: nsIWebBrowser;
  public
    { Public declarations }
  end;


var
  Form1: TForm1;


implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
var
  navigation: nsIWebNavigation;
  basewin: nsIBaseWindow;
  wm : nsIWindowMediator;
begin
  XRE_Startup('1.9', True, '2.0', False);

  NS_CreateInstance(NS_WEBBROWSER_CONTRACTID, nsIWebBrowser, FBrowser);

  basewin := FBrowser as nsIBaseWindow;
  basewin.InitWindow({$IFDEF MSWINDOWS}Pointer(Handle),{$ENDIF}
                     {$IFDEF LCLCarbon}Pointer(TCarbonWindow(Handle).Window),{$ENDIF}
                     {$IFDEF LCLCocoa}Pointer(TCocoaForm(Handle).MainWindowView.superview),{$ENDIF}
                     {$IFDEF LCLGtk}Pointer(Handle),{$ENDIF}  //Is Handle same as GTK Window?
                     {$IFDEF LCLGtk2}Pointer(Handle),{$ENDIF}  //Is Handle same as GTK Window?
                     nil, 0, 0, ClientWidth, ClientHeight);

  basewin.Create;
  basewin.SetVisibility(True);

  navigation := FBrowser as nsIWebNavigation;
  navigation.LoadURI('http://www.lazarus.freepascal.org', 0, nil, nil, nil);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBrowser := nil;
  XRE_Shutdown;
end;


initialization
{$IFDEF LCL}
{$I Brow10.lrs}  {Include form's resource file}
{$ENDIF}

end.
