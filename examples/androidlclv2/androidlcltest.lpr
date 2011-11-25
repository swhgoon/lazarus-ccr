library androidlcltest;

{$mode objfpc}{$H+}
{$define Android}

uses
  {$ifdef Android}
  cmem,
  android_native_app_glue,
  {$endif}
  Interfaces,
  mainform;

exports //android_main name 'android_main',
  ANativeActivity_onCreate name 'ANativeActivity_onCreate';

begin
end.

