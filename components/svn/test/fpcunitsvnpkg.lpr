program fpcunitsvnpkg;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestSvnClasses, svnpkg;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

