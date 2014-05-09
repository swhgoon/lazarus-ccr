{$UNDEF LOG_TO_FILE}
program srv_calc;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils,
  server_listener, indy_http_server,
  server_service_intf, server_service_soap, metadata_service,
  calcservice, calcservice_binder, calcservice_imp
{$IFDEF LOG_TO_FILE}
  ,file_logger_extension
{$ENDIF LOG_TO_FILE}
  ;
var
  AppObject : TwstListener;
begin
  Server_service_RegisterSoapFormat();
  RegisterCalcServiceImplementationFactory();
  Server_service_RegisterCalcServiceService();
{$IFDEF LOG_TO_FILE}
  LogFileCompleteName := Format('.%slog.txt',[PathDelim]);
  GetServiceImplementationRegistry().FindFactory('ICalcService').RegisterExtension(['TFileLoggerServiceExtension']);
{$ENDIF LOG_TO_FILE}
  AppObject := TwstIndyHttpListener.Create('127.0.0.1',8000);
  try
    WriteLn('"Web Service Toolkit" HTTP Server listening at:');
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
