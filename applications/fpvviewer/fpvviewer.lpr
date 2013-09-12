program fpvviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fpvv_mainform, dxftokentotree, fpvv_drawer, fpvectorialpkg,
  printer4lazarus, CoreConRec;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFPVViewer, frmFPVViewer);
  Application.Run;
end.

