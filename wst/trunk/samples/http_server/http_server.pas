program http_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  indy_http_server, metadata_service, logger_extension, server_listener;


var
  AppObject : TwstListener;
begin
  AppObject := TwstIndyHttpListener.Create();
  try
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    AppObject.Start();
    ReadLn();
  finally
    FreeAndNil(AppObject);
  end;
end.
