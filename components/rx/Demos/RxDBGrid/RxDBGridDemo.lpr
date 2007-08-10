program RxDBGridDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  RxDBGridMainUnit;

begin
  Application.Initialize;
  Application.CreateForm(TRxDBGridMainForm, RxDBGridMainForm);
  Application.Run;
end.

