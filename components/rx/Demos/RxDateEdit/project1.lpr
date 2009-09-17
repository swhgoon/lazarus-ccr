program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  LResources,
  sysutils
  { you can add units after this };

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  {$I project1.lrs}
  ShortDateFormat:='dd.mm.yyyy';
  DateSeparator:='.';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

