program ChromeWin;

uses
{$IFDEF LCL}
  Interfaces,
 {$IFDEF MSWINDOWS}
  Math,
 {$ENDIF}
{$ENDIF}
  Forms,
  nsXRE,
  BrowserSupports,
  GeckoChromeWindow in 'GeckoChromeWindow.pas' {GeckoChromeForm};

//{$IFDEF MSWINDOWS}
//{$R *.res}
//{$ENDIF}

var
  navigation: nsIWebNavigation;
begin
{$IFDEF FPC}
 {$IFDEF MSWINDOWS}
  //For now - disable all floating point exceptions or XULRUNNER will crash.
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 {$ENDIF}
{$ENDIF}

  Application.Initialize;

  XRE_Startup('1.9', True, '2.0', False);

//  GeckoChromeForm := TGeckoChromeForm.CreateWithChromeFlags(Application, 0);
  Application.CreateForm(TGeckoChromeForm, GeckoChromeForm);
  navigation := GeckoChromeForm.WebBrowser as nsIWebNavigation;
  navigation.LoadURI('http://www.lazarus.freepascal.org', 0, nil, nil, nil);
  
  Application.Run;
end.
