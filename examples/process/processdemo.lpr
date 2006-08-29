program ProcessDemo;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, ProcessDemoMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMultipleProcessDemoForm,MultipleProcessDemoForm);
  Application.Run;
end.

