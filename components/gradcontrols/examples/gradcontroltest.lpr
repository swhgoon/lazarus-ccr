program gradcontroltest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, multiloglaz
  { you can add units after this }, unit1, gradcontrols;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

