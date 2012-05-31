program TDIDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, uForm2, sysutils
  { you can add units after this };

{$R *.res}

var
   HeapTraceFile : String ;
begin

   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );

  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm) ;
  Application.CreateForm(TForm2, Form2) ;
  Application.Run;
end.

