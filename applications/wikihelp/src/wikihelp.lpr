program wikihelp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, uMain;

begin
  Application.Initialize;
  Application.CreateForm(TfWikiHelp, fWikiHelp);
  Application.Run;
end.

