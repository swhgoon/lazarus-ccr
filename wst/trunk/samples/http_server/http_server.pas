program http_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  indy_http_server, metadata_service, logger_extension;


var
  AppObject : TwstWebApplication;
begin
  AppObject := TwstWebApplication.Create();
  try
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    ReadLn();
  finally
    FreeAndNil(AppObject);
  end;
end.
