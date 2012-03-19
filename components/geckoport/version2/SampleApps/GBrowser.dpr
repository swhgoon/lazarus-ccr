program GBrowser;

uses
{$IFDEF LCL}
  Interfaces,
 {$IFDEF MSWINDOWS}
  Math,
 {$ENDIF}
{$ENDIF}
  Forms,
  gec10 in 'gec10.pas';


begin
{$IFDEF FPC}
 {$IFDEF MSWINDOWS}
  //For now - disable all floating point exceptions or XULRUNNER will crash.
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 {$ENDIF}
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
