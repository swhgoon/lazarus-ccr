program GBrowser;

uses
{$IFDEF LCL}
  Interfaces,
 {$IFDEF MSWINDOWS}
  Math,
 {$ENDIF}
{$ENDIF}
  sysutils,
  Forms,
  GeckoInit,
  gec10 in 'gec10.pas';


begin
{$IFDEF FPC}
 {$IFDEF MSWINDOWS}
  //For now - disable all floating point exceptions or XULRUNNER will crash.
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 {$ENDIF}
{$ENDIF}

  Application.Initialize;
  try
    GeckoComponentsStartup(ParamStr(1));
  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to initialize xulrunner. Try to pass the complete filename ' +
                                'of the libxpcom library (including the path) as first parameter to ' +
                                'the command line. Original error message: %s', [E.Message]);
  end;
  try
    Application.CreateForm(TForm1, Form1);
    Application.Run;
  finally
    GeckoComponentsShutdown;
  end;
end.
