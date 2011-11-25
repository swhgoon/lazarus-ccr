library androidlcltest;

{$mode objfpc}{$H+}
{$define Android}

uses
  {$ifdef Android}
  cmem,
  android_native_app_glue,
  {$endif}
  Interfaces,
  Forms,
  mainform;

exports //android_main name 'android_main',
  ANativeActivity_onCreate name 'ANativeActivity_onCreate';

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

