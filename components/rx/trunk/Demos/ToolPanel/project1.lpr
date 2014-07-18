program project1;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  AboutUnit;

{$R *.res}

begin
  Application.Title:='Rx Toolbar test';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

