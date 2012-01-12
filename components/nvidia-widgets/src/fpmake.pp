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
    P := AddPackage('nvwidget');

{$ifdef ALLPACKAGES}
    P.Directory := 'src';
{$endif ALLPACKAGES}

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
    P.Targets.AddUnit('nvtypes.pas');
    P.Targets.AddUnit('nvbasefont.pas');
    P.Targets.AddUnit('nvcontext.pas');
    P.Targets.AddUnit('nvpainter.pas');

    write('package ', P.Name, ' configured for ');

	//select font class
    case NV_ACTIVE_FONT of
      GLFREETYPE: begin
	            write('FreeType font ');
	            P.Targets.AddUnit('./gl/glfreetype.pas');
	            P.Targets.AddUnit('./gl/glfreetypefont.pas');
                  end;
      GLUTBITMAP: begin
	            write('GLUT font ');
	            P.Targets.AddUnit('./glut/glutbitmapfont.pas');
                  end;
    end;
	
    //context units
    case NV_ACTIVE_CONTEXT of
      GLUT: begin
	      write('the GLUT context');
	      P.Targets.AddUnit('./glut/nvglutcontext.pas');
	    end;
    end;
	
    //painter units
    case NV_ACTIVE_PAINTER of
	GL: begin
	      writeln(' with the OpenGL painter');
		  P.UnitPath.Add('./gl/');
	      P.Targets.AddUnit('./gl/nvshaderutils.pas');	
	      P.Targets.AddUnit('./gl/nvglpainter.pas');	
	    end;
    end;
	
{$ifndef ALLPACKAGES}
    Run;
  end;
end.
{$endif ALLPACKAGES}
