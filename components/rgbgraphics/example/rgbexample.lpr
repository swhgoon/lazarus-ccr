program RGBExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, RGBUnit, lazrgbgraphics;

begin
  Application.Initialize;
  Application.CreateForm(TFormExample, FormExample);
  Application.Run;
end.

