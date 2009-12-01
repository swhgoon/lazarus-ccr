program ChromeWin;

uses
{$IFDEF LCL}
  Interfaces,
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
  Application.Initialize;

  XRE_Startup('1.9', True, '2.0', False);

//  GeckoChromeForm := TGeckoChromeForm.CreateWithChromeFlags(Application, 0);
  Application.CreateForm(TGeckoChromeForm, GeckoChromeForm);
  navigation := GeckoChromeForm.WebBrowser as nsIWebNavigation;
  navigation.LoadURI('http://www.lazarus.freepascal.org', 0, nil, nil, nil);
  
  Application.Run;
end.
