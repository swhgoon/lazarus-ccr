{
LazEyes2

Fun application which follows the mouse wherever it goes.

The window has the form of the eyes, which are ellipses.

Version 2: Operates with a custom control
}
program lazeyes2;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  lazeyes2painter,
  lazeyes2form;

//{$IFDEF WINDOWS}{$R lazeyes2.rc}{$ENDIF}

{$R *.res}

begin
  RequireDerivedFormResource := False;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

