program fpmake;

{$mode objfpc} {$H+}

uses
  fpmkunit;

{$include config.inc}

var
  P: TPackage;
  i: integer;

begin
  with Installer do
  begin
    //create nvwidgets package
    P := AddPackage('nvwidget');

    P.Version := '1.00';
	//P.Options.Add('-MObjFPC');
	P.Options.Add('-Sc');	
	
	if NV_DEBUG then
	  for i := 0 to High(NV_DEBUG_FLAGS) do
        P.Options.Add(NV_DEBUG_FLAGS[i]);
	  
	if NV_PROFILE then
	  for i := 0 to High(NV_PROFILE_FLAGS) do
        P.Options.Add(NV_PROFILE_FLAGS[i]);
	  
    //base widget units
    P.Targets.AddUnit('./nvwidgets/nvbasefont.pas');
    P.Targets.AddUnit('./nvwidgets/nvwidgets.pas');

	write('package ', P.Name, ' configured for ');

    //context units
    case NV_ACTIVE_CONTEXT of
	  GLUT: begin
	          write('the GLUT context');
	          P.Targets.AddUnit('./nvwidgets/nvglutwidgets.pas');	
			end;
	end;
	
    //painter units
    case NV_ACTIVE_PAINTER of
	GL: begin
	      writeln(' with the OpenGL painter');
		  P.UnitPath.Add('./gl/');
		  P.UnitPath.Add('./nvglutils/.');
          P.Targets.AddUnit('./nvwidgets/nvglwidgets.pas');	
	    end;
	end;
	
    Run;
  end;
end.

