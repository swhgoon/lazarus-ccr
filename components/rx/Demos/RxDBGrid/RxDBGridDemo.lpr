program RxDBGridDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  LResources,
  RxDBGridMainUnit;

{$IFDEF WINDOWS}{$R RxDBGridDemo.rc}{$ENDIF}

begin
  Application.Title:='RxDBGrid demo';
  {$I RxDBGridDemo.lrs}
  Application.Initialize;
  Application.CreateForm(TRxDBGridMainForm, RxDBGridMainForm);
  Application.Run;
end.

