{$ifndef ALLPACKAGES}
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
{$endif ALLPACKAGES}

    //create nvwidgets package
    P := AddPackage('nvwidget_examples');

{$ifdef ALLPACKAGES}
    P.Directory := 'examples';
{$endif ALLPACKAGES}

    P.Dependencies.Add('nvwidget');
	
    //example applications
    P.Targets.AddUnit('example.pp');
    P.Targets.AddUnit('widget_test.pp');
	
{$ifndef ALLPACKAGES}
    Run;
  end;
end.
{$endif ALLPACKAGES}
