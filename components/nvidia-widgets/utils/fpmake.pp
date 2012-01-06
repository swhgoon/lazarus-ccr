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
    P := AddPackage('nvwidget_utils');
	
    //utility applications
    P.Targets.AddUnit('crop.pp');
	
    Run;
  end;
end.

