program vtbasic;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Main,vtlogger,ipcchannel;

begin
  Logger.Channels.Add(TIPCChannel.Create);
  Logger.Clear;
  Logger.ActiveClasses:=[lcScroll,lcWarning];
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

