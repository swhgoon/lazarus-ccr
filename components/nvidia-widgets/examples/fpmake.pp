program fpmake;

{$mode objfpc} {$H+}

uses
  fpmkunit;

var
  P: TPackage;
  i: integer;

begin
  with Installer do
  begin
    //create nvwidgets package
    P := AddPackage('nvwidget_examples');

    P.Dependencies.Add('nvwidget');
    P.UnitPath.Add('../src');
	
    //example applications
    P.Targets.AddUnit('example.pp');
    P.Targets.AddUnit('widget_test.pp');
	
    Run;
  end;
end.

