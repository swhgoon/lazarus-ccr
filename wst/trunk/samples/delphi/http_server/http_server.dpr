program http_server;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ActiveX,
  indy_http_server,
  metadata_service,
  logger_extension,
  wst_delphi_rtti_utils in '..\..\..\wst_delphi_rtti_utils.pas',
  server_listener in '..\..\..\server_listener.pas';

var
  AppObject : TwstIndyHttpListener;
begin
{$IFNDEF FPC}
  CoInitialize(nil);
  try
{$ENDIF}
    AppObject := TwstIndyHttpListener.Create();
    try
      WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
      WriteLn('');
      WriteLn('http://127.0.0.1:8000/');
      WriteLn('');
      WriteLn('Press enter to quit.');
      AppObject.Start();
      ReadLn;
    finally
      FreeAndNil(AppObject);
    end;
{$IFNDEF FPC}
  finally
    CoUninitialize();
  end;
{$ENDIF}
end.

