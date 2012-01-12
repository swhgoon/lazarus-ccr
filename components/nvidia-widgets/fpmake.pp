program fpmake;

{$mode objfpc} {$H+}

uses
  fpmkunit,
  Classes,
  SysUtils;

{$define ALLPACKAGES}
{$include config.inc}

var
  P: TPackage;
  be: TBuildEngine;
  i: integer;

  procedure CreateClassChart(Sender: TObject);
  var
    i: integer;
    APackage: TPackage;
    T: TTarget;
    cmdOpts: string;
    Cmd: string;
  begin
    APackage := TPackage(Sender);

    cmdOpts := '--format=graphviz --output=nvwidgets.dot';

    for i := 0 to APackage.Targets.Count - 1 do
    begin
      T := APackage.Targets.TargetItems[i];
      if T.TargetType = ttFPDoc then
        //check if a documentation target is given
        cmdOpts := cmdOpts + ' --input=' + be.AddPathPrefix(APackage, T.Directory + T.Name + T.Extension);
    end;

    //execute fpclasschart
    Cmd := ExeSearch('fpclasschart', GetEnvironmentvariable('PATH'));
    if Cmd = '' then
      Cmd := 'fpclasschart';
    //writeln(cmdOpts);
    ExecuteProcess(Cmd, cmdOpts);

    //create the graphviz chart
    Cmd := ExeSearch('dot', GetEnvironmentvariable('PATH'));
    if Cmd = '' then
      Cmd := 'dot';
    ExecuteProcess(Cmd, '-Tpng nvwidgets.dot -o./fpdoc/images/nvwidgets.png');

    //remove the dot file
    DeleteFile('nvwidgets.dot');
  end;

  {$include fpmake_proc.inc}

begin
  {$include fpmake_add.inc}

  with Installer do
  begin
    be := BuildEngine;

    //create nvwidgets package
    P := AddPackage('nvidia-widgets');
    P.FPDocFormat := [ffHtml, ffCHM];
    P.BeforeDocProc := @CreateClassChart;

    //later add --footer-date="mmm dd yyyy"
    P.FPDocSwitches := '--auto-index --auto-toc --make-searchable --css-file=./fpdoc/fpdoc.css --image-url=fpdoc/images/';

    //base source files
    P.Targets.AddFPDoc('./src/nvtypes.pas', './fpdoc/nvtypes.xml');
    P.Targets.AddFPDoc('./src/nvpainter.pas', './fpdoc/nvpainter.xml');
    P.Targets.AddFPDoc('./src/nvcontext.pas', './fpdoc/nvcontext.xml');
    P.Targets.AddFPDoc('./src/nvbasefont.pas', './fpdoc/nvbasefont.xml');

    //contexts
    P.Targets.AddFPDoc('./src/glut/nvglutcontext.pas', './fpdoc/nvglutcontext.xml');

    //painters
    P.Targets.AddFPDoc('./src/gl/nvglpainter.pas', './fpdoc/nvglpainter.xml');
    P.Targets.AddFPDoc('./src/gl/nvshaderutils.pas', './fpdoc/nvshaderutils.xml');

    //fonts
    P.Targets.AddFPDoc('./src/gl/glfreetype.pas', './fpdoc/glfreetype.xml');
    P.Targets.AddFPDoc('./src/gl/glfreetypefont.pas', './fpdoc/glfreetypefont.xml');
    P.Targets.AddFPDoc('./src/glut/glutbitmapfont.pas', './fpdoc/glutbitmapfont.xml');

    Run;
  end;
end.

