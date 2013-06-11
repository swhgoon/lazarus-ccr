program lazspreadsheet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, about, mainform, laz_fpspreadsheet_visual;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tlazfpsmainform, lazfpsmainform);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

