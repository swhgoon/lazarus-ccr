program RxDBGridDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  RxDBGridMainUnit;

{$IFDEF WINDOWS}{$R RxDBGridDemo.rc}{$ENDIF}

{$R RxDBGridDemo.res}

begin
  Application.Title:='RxDBGrid demo';
  Application.Initialize;
  Application.CreateForm(TRxDBGridMainForm, RxDBGridMainForm);
  Application.Run;
end.

