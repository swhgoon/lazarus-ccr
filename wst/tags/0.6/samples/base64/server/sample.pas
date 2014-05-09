// Activate this "define" to logg the messages to the screen
  { $DEFINE WST_LOGGING_CONSOLE}
// Activate this "define" to logg the messages to the file set in "LogFileCompleteName"
  { $DEFINE WST_LOGGING_FILE}
  
program sample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  server_listener, indy_http_server,
  server_service_intf, server_service_soap, metadata_service,
{$IFDEF WST_LOGGING_CONSOLE}
  logger_extension,
{$ENDIF}
{$IFDEF WST_LOGGING_FILE}
  file_logger_extension,
{$ENDIF}
  base64sample, base64sample_binder, base64sample_imp;

var
  AppObject : TwstListener;
begin
  Server_service_RegisterSoapFormat();

  RegisterSampleServiceImplementationFactory();
  Server_service_RegisterSampleServiceService();

{$IFDEF WST_LOGGING_FILE}
  LogFileCompleteName := Format('.%s%s',[PathDelim,'log.txt']);
  GetServiceImplementationRegistry().FindFactory('SampleService').RegisterExtension(['TFileLoggerServiceExtension']);
{$ENDIF}
{$IFDEF WST_LOGGING_CONSOLE}
  GetServiceImplementationRegistry().FindFactory('SampleService').RegisterExtension(['TLoggerServiceExtension']);
{$ENDIF}

  AppObject := TwstIndyHttpListener.Create('');
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
