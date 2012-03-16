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
unit GeckoChromeWindow;

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
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LResources, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CallbackInterfaces, nsXPCOM, nsTypes, nsXPCOM_std19
  {$IFDEF LCLCarbon}, CarbonPrivate {$ENDIF}
  {$IFDEF LCLGtk2}, gtk2 {$ENDIF}
  {$IFDEF LCLCocoa}, CocoaAll, CocoaUtils, CocoaPrivate {$ENDIF};

type
  //In all currently supported platforms the native window handle is a pointer
  //size handle. In Linux64 THANDLE can not be used because by default it is 32
  //bits due file descriptors which are 32 bits even in 64 bit platform.
  //Win32   WindowHandle 32 bits THANDLE 32 bits
  //Win64   WindowHandle 64 bits THANDLE 64 bits
  //Linux32 WindowHandle 32 bits THANDLE 32 bits
  //Linux64 WindowHandle 64 bits THANDLE 32 bits
  nativeWindow = PtrUInt;

  TGeckoChromeForm = class(TForm,
                           IGeckoCreateWindowTarget,
                           nsIWebBrowserChrome,
                           nsIEmbeddingSiteWindow,
                           nsIWebProgressListener,
                           nsIInterfaceRequestor_std19,
                           nsIWeakReference,
                           nsISupportsWeakReference)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private 錾 }
    FWebBrowser: nsIWebBrowser;
    FChromeFlags: Longword;

    // nsIWebBrowserChrome
    procedure SetStatus(statusType: PRUint32; const status: PWideChar); safecall;
    function GetWebBrowser(): nsIWebBrowser; safecall;
    procedure SetWebBrowser(aWebBrowser: nsIWebBrowser); safecall;
    function GetChromeFlags: PRUint32; safecall;
    procedure SetChromeFlags(aChromeFlags: PRUint32); safecall;
    procedure DestroyBrowserWindow(); safecall;
    procedure SizeBrowserTo(aCX: PRInt32; aCY: PRInt32); safecall;
    procedure ShowAsModal(); safecall;
    function IsWindowModal(): PRBool; safecall;
    procedure ExitModalEventLoop(aStatus: nsresult); safecall;
    // nsIEmbeddingSiteWindow
    procedure SetDimensions(flags: PRUint32; x, y, cx, cy: PRInt32); safecall;
    procedure GetDimensions(flags: Longword; out x, y, cx, cy: PRInt32); safecall;
    procedure SetFocus; reintroduce; safecall;
    function GetVisibility(): PRBool; safecall;
    procedure SetVisibility(Value: PRBool); safecall;
    function GetTitle(): PWideChar; safecall;
    procedure SetTitle(const Value: PWideChar); safecall;
    function GetSiteWindow: Pointer; safecall;
    // nsIWebProgressListener
    procedure OnStateChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStateFlags: PRUint32; aStatus: nsresult); safecall;
    procedure OnProgressChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aCurSelfProgress: PRInt32; aMaxSelfProgress: PRInt32; aCurTotalProgress: PRInt32; aMaxTotalProgress: PRInt32); safecall;
    procedure OnLocationChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; location: nsIURI); safecall;
    procedure OnStatusChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStatus: nsresult; const aMessage: PWideChar); safecall;
    procedure OnSecurityChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; state: PRUint32); safecall;
    // nsIInterfaceRequestor
    function NS_GetInterface(constref uuid: TGUID; out Intf): nsresult; extdecl;
    function nsIInterfaceRequestor_std19.GetInterface = NS_GetInterface;
    // for nsIWeakReference
    procedure QueryReferent(constref IID: TGUID; out Obj); safecall;
    // for nsISupportsWeakReference
    function GetWeakReference(): nsIWeakReference; safecall;

    function GetNativeWindow : nativeWindow;  //FPC port: added this.
    procedure InitWebBrowser;
    procedure UpdateChrome;
    procedure ContentFinishedLoading;
  public
    { Public 錾 }
    function SafeCallException(Obj: TObject; Addr: Pointer): HResult; override;

    constructor CreateWithChromeFlags(AOwner: TComponent; aChromeFlags: Longword);

    // IGeckoCreateWindowTarget
    function DoCreateChromeWindow(chromeFlags: Longword): nsIWebBrowserChrome;
    function GetWebBrowserChrome: nsIWebBrowserChrome;
    property WebBrowser : nsIWebBrowser read FWebBrowser; //FPC port: added this.
  end;

var
  GeckoChromeForm: TGeckoChromeForm;

implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ENDIF}

uses
  nsXPCOMGlue, nsError, BrowserSupports;

{$PUSH}
{$HINTS OFF}
procedure UseParameter(var X);
begin
end;
{$POP}

constructor TGeckoChromeForm.CreateWithChromeFlags(AOwner: TComponent; AChromeFlags: Longword);
begin
  inherited Create(AOwner);
  FChromeFlags := aChromeFlags;
  UpdateChrome;
end;

procedure TGeckoChromeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

function TGeckoChromeForm.GetNativeWindow : nativeWindow;
{$IFDEF LCLCocoa}
var
  ARect : NSRect;
  AView : NSView;
{$ENDIF}
begin
   {$IFDEF MSWINDOWS}Result := Handle;{$ENDIF}

   {$IFDEF LCLCarbon}Result := THANDLE(TCarbonWindow(Handle).Window);{$ENDIF}
    //Carbon doesn't work but leave in so package compiles in Carbon IDE.

//   {$IFDEF LCLCocoa}Result := Pointer(TCocoaForm(Handle).MainWindowView.superview);{$ENDIF}
      //Old PasCocoa-based widgetset.

//NSLog(NSStringUtf8(FloatToStr(NSView(TCocoaWindow(Handle).contentView).frame.size.width)));
   {$IFDEF LCLCocoa}Result := THANDLE(TCocoaWindow(Handle).contentView);{$ENDIF}
    //New ObjC-based Cocoa widgetset.

(*
    //Does adding a view work better than using window's view (below)? No, it doesn't.
   {$IFDEF LCLCocoa}
    ARect := NSView(TCocoaWindow(Handle).contentView).visibleRect;
    ARect.size.width := ARect.size.width - 30;
    ARect.size.height := ARect.size. height - 30;
    ARect.origin.x := 15;
    ARect.origin.y := 15;
    AView := NSView.alloc.initWithFrame(ARect);
    NSView(TCocoaWindow(Handle).contentView).addSubView(AView);
    Result := THANDLE(AView);
   {$ENDIF}
*)

   {$IFDEF LCLGtk}Result := Handle;{$ENDIF}  //Is Handle same as GTK Window?

   {$IFDEF LCLGtk2}
   Result := nativeWindow(PGtkWindow(GeckoChromeForm.Handle)^.bin.child);
   {$ENDIF}  //Is Handle same as GTK Window?
end;

procedure TGeckoChromeForm.InitWebBrowser;
var
  base: nsIBaseWindow;
begin
  NS_CreateInstance(NS_WEBBROWSER_CID, nsIWebBrowser, FWebBrowser);

  FWebBrowser.ContainerWindow := Self;

  base := FWebBrowser as nsIBaseWindow;
  base.InitWindow(GetNativeWindow, nil, 0, 0, ClientWidth, ClientHeight);
  base.Create;
  FWebBrowser.AddWebBrowserListener(Self, nsIWebProgressListener);
  base.SetVisibility(True);
end;

procedure TGeckoChromeForm.UpdateChrome;
begin
  {if (FChromeFlags and CHROME_WINDOW_BORDERS)<>0 then
    if (FChromeFlags and CHROME_WINDOW_RESIZE)<>0 then
      BorderStyle := bsSizeable
    else
      if (FChromeFlags and CHROME_OPENAS_DIALOG)<>0 then
        BorderStyle := bsDialog
      else
        BorderStyle := bsSingle
  else
    BorderStyle := bsNone;}
  BorderStyle := bsSizeable;

  {
  if (FChromeFlags and CHROME_WINDOW_CLOSE)<>0 then
    BorderIcons := BorderIcons + [biClose]
  else
    BorderIcons := BorderIcons - [biClose];
  }

  if (FChromeFlags and CHROME_SCROLLBARS)<>0 then
    AutoScroll := True
  else
    AutoScroll := False;

  {
  if (FChromeFlags and CHROME_TITLEBAR)<>0 then
    BorderIcons := BorderIcons + [biSystemMenu]
  else
    BorderIcons := BorderIcons - [biSystemMenu];
  }

end;

function TGeckoChromeForm.DoCreateChromeWindow(chromeFlags: Longword): nsIWebBrowserChrome;
begin
  UseParameter(chromeFlags);
  Result := nil;
end;

function TGeckoChromeForm.GetWebBrowserChrome: nsIWebBrowserChrome;
begin
  Result := Self;
end;

procedure TGeckoChromeForm.SetStatus(statusType: Longword; const status: PWideChar);
begin
  UseParameter(statusType);
end;

function TGeckoChromeForm.GetWebBrowser: nsIWebBrowser;
begin
  Result := FWebBrowser as nsIWebBrowser;
end;

procedure TGeckoChromeForm.SetWebBrowser(aWebBrowser: nsIWebBrowser);
begin
  UseParameter(aWebBrowser);
end;

function TGeckoChromeForm.GetChromeFlags: PRUint32;
begin
  Result := FChromeFlags;
end;

procedure TGeckoChromeForm.SetChromeFlags(aChromeFlags: Longword);
begin
  FChromeFlags := aChromeFlags;
  UpdateChrome;
end;

procedure TGeckoChromeForm.DestroyBrowserWindow;
begin
  Close;
end;

procedure TGeckoChromeForm.SizeBrowserTo(aCX, aCY: Integer);
var
  dx, dy: Integer;
begin
  dx := Width - ClientWidth;
  dy := Height - ClientHeight;
  SetBounds(Left, Top, aCX+dx, aCY+dy);
end;

procedure TGeckoChromeForm.ShowAsModal;
begin
  Visible := False;
  ShowModal;
end;

function TGeckoChromeForm.IsWindowModal: PRBool;
begin
  Result := False;
end;

procedure TGeckoChromeForm.ExitModalEventLoop(aStatus: nsresult); safecall;
begin
  UseParameter(aStatus);
  ModalResult := 1;
end;

procedure TGeckoChromeForm.SetDimensions(flags: Longword; x, y, cx, cy: Longint);
const
  FLAGS_POSITION   = NS_IEMBEDDINGSITEWINDOW_DIM_FLAGS_POSITION;
  FLAGS_SIZE_INNER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_INNER;
  FLAGS_SIZE_OUTER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_OUTER;
var
  dx, dy: Integer;
begin
  dx := Width - ClientWidth;
  dy := Height - ClientHeight;

  if (flags and FLAGS_POSITION)<>0 then
  begin
    if (flags and FLAGS_SIZE_INNER)<>0 then
    begin
      Bounds(x, y, cx+dx, cy+dy);
    end else
    if (flags and FLAGS_SIZE_OUTER)<>0 then
    begin
      Bounds(x, y, cx, cy);
    end else
    begin
      Bounds(x, y, Width, Height);
    end;
  end else
  if (flags and FLAGS_SIZE_INNER)<>0 then
  begin
    Bounds(Left, Top, cx+dx, cy+dy);
  end else
  if (flags and FLAGS_SIZE_OUTER)<>0 then
  begin
    Bounds(Left, Top, cx, cy);
  end;
end;

procedure TGeckoChromeForm.GetDimensions(flags: Longword; out x, y, cx, cy: Longint);
const
  FLAGS_POSITION   = ns_IEmbeddingSiteWindow_DIM_FLAGS_POSITION;
  FLAGS_SIZE_INNER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_INNER;
  FLAGS_SIZE_OUTER = ns_IEmbeddingSiteWindow_DIM_FLAGS_SIZE_OUTER;
begin
  if (flags and FLAGS_POSITION)<>0 then
  begin
    x := Left;
    y := Top;
  end;

  if (flags and FLAGS_SIZE_INNER)<>0 then
  begin
    cx := ClientWidth;
    cy := ClientHeight;
  end else
  if (flags and FLAGS_SIZE_OUTER)<>0 then
  begin
    cx := Width;
    cy := Height;
  end;
end;

procedure TGeckoChromeForm.SetFocus();
begin
end;

function TGeckoChromeForm.GetVisibility: PRBool;
begin
  Result := True;
end;

procedure TGeckoChromeForm.SetVisibility(Value: LongBool);
begin
  UseParameter(Value);
  Visible := Value;
end;

function TGeckoChromeForm.GetTitle: PWideChar;
begin
  Result := nil;
end;

procedure TGeckoChromeForm.SetTitle(const Value: PWideChar);
begin
  Caption := WideString(Value);
end;

function TGeckoChromeForm.GetSiteWindow: Pointer;
begin
//Known "not safe" conversion.
{$PUSH}
{$HINTS OFF}
  Result := Pointer(GetNativeWindow);
{$POP}
end;

procedure TGeckoChromeForm.OnStateChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStateFlags: PRUint32; aStatus: nsresult);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aStatus);
  if ((aStateFlags and NS_IWEBPROGRESSLISTENER_STATE_STOP)<>0) and
     ((aStateFlags and NS_IWEBPROGRESSLISTENER_STATE_IS_DOCUMENT)<>0) then
  begin
    ContentFinishedLoading();
  end;
end;

procedure TGeckoChromeForm.OnProgressChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aCurSelfProgress: PRInt32; aMaxSelfProgress: PRInt32; aCurTotalProgress: PRInt32; aMaxTotalProgress: PRInt32);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aCurSelfProgress);
  UseParameter(aMaxSelfProgress);
  UseParameter(aCurTotalProgress);
  UseParameter(aMaxTotalProgress);
end;

procedure TGeckoChromeForm.OnLocationChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; location: nsIURI);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(location);
end;

procedure TGeckoChromeForm.OnStatusChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; aStatus: nsresult; const aMessage: PWideChar);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(aStatus);
end;

procedure TGeckoChromeForm.OnSecurityChange(aWebProgress: nsIWebProgress; aRequest: nsIRequest; state: PRUint32);
begin
  UseParameter(aWebProgress);
  UseParameter(aRequest);
  UseParameter(state);
end;

function TGeckoChromeForm.NS_GetInterface(constref uuid: TGUID; out Intf): nsresult;
var
  domwin: nsIDOMWindow;
begin
  if IsEqualGUID(uuid, nsIDOMWindow) then
  begin
    if Assigned(FWebBrowser) then
    begin
      domwin := FWebBrowser.ContentDOMWindow;
      Result := domwin.QueryInterface(uuid, Intf);
    end else
      Result := NS_ERROR_NOT_INITIALIZED;
  end else
  begin
// FPC port: Result is PRUInt32, but QueryInterface returns Longint,
//  so cast to nsresult to prevent range check error.
    try
    Result := nsresult(QueryInterface(uuid, Intf));
    except
      Result:=0;
      Integer(Intf):=0;
    end;
  end;
end;

procedure TGeckoChromeForm.QueryReferent(constref IID: TGUID; out Obj);
var
  rv: nsresult;
begin
  rv := QueryInterface(IID, Obj);
  if NS_FAILED(rv) then
    raise EIntfCastError.Create('QueryReferent');
end;

function TGeckoChromeForm.GetWeakReference: nsIWeakReference;
begin
  Result := Self as nsIWeakReference;
end;

procedure TGeckoChromeForm.FormCreate(Sender: TObject);
begin
  InitWebBrowser;
end;

procedure TGeckoChromeForm.FormResize(Sender: TObject);
var
  baseWin: nsIBaseWindow;
begin
  baseWin:=FWebBrowser as nsIBaseWindow;
  baseWin.SetPositionAndSize(0, 0, ClientWidth, ClientHeight, True);
  baseWin.SetVisibility(True);
end;

procedure TGeckoChromeForm.ContentFinishedLoading;
var
  contentWin: nsIDOMWindow;
  baseWin: nsIBaseWindow;
begin
  contentWin := FWebBrowser.ContentDOMWindow;
  try
    //Will try to resize the form to the size of the HTML page, but if the HTML
    //does not have a width specified (UNRESTRICTED) it will raise an exception
    //and badly resize the HTML content.
    contentWin.SizeToContent;
  except
    //Workaround
    baseWin:=FWebBrowser as nsIBaseWindow;
    //Forces reflow...
    baseWin.SetPositionAndSize(0,0,ClientWidth, ClientHeight+1, false);
    baseWin.SetPositionAndSize(0,0,ClientWidth, ClientHeight, true);
  end;
  Visible:=true;
end;

{$IFDEF LCL}
const
  E_FAIL = HRESULT($80004005);
{$ENDIF}

function TGeckoChromeForm.SafeCallException(Obj: TObject; Addr: Pointer): HResult;
begin
  UseParameter(Addr);
  if Obj is EIntfCastError then
    Result := E_NOINTERFACE
  else
    Result := E_FAIL;
end;

initialization
{$IFDEF LCL}
{$I GeckoChromeWindow.lrs}
{$ENDIF}

end.
