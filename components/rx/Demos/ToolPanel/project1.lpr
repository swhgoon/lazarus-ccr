program project1;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  AboutUnit, LResources;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  {$I project1.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

