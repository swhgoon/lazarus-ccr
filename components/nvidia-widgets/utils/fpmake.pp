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
    P := AddPackage('nvwidget_utils');

{$ifdef ALLPACKAGES}
    P.Directory := 'utils';
{$endif ALLPACKAGES}

    //utility applications
    P.Targets.AddUnit('crop.pp');
	
{$ifndef ALLPACKAGES}
    Run;
  end;
end.
{$endif ALLPACKAGES}
