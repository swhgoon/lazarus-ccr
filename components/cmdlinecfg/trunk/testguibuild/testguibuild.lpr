program testguibuild;

{$mode objfpc}{$H+}

uses
  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, cmdlinelclctrlsbox//, patheditor
  { you can add units after this };

{$R *.res}

begin
  SetHeapTraceOutput('test.txt');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

