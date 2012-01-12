program fpmake;

{$mode objfpc} {$H+}

uses
  fpmkunit,
  Classes,
  SysUtils;

var
  P: TPackage;

begin
  with Installer do
  begin
    //create nvwidgets package
    P := AddPackage('nvwidgets');

    P.FPDocFormat := [ffHtml, ffCHM];
    P.FPDocSwitches := '--auto-index --auto-toc --make-searchable --footer-date="mmm dd yyyy" --css-file=./fpdoc/fpdoc.css --image-url=images/';

    P.Targets.AddFPDoc('./src/nvtypes.pas', './fpdoc/nvtypes.xml');
    P.Targets.AddFPDoc('./src/nvpainter.pas', './fpdoc/nvpainter.xml');
    P.Targets.AddFPDoc('./src/nvcontext.pas', './fpdoc/nvcontext.xml');
    P.Targets.AddFPDoc('./src/gl/nvglpainter.pas', './fpdoc/nvglpainter.xml');
    P.Targets.AddFPDoc('./src/glut/nvglutcontext.pas', './fpdoc/nvglutcontext.xml');
    P.Targets.AddFPDoc('./src/gl/nvshaderutils.pas', './fpdoc/nvshaderutils.xml');
    P.Targets.AddFPDoc('./src/gl/glfreetype.pas', './fpdoc/glfreetype.xml');
    P.Targets.AddFPDoc('./src/nvbasefont.pas', './fpdoc/nvbasefont.xml');
    P.Targets.AddFPDoc('./src/gl/glfreetypefont.pas', './fpdoc/glfreetypefont.xml');
    P.Targets.AddFPDoc('./src/glut/glutbitmapfont.pas', './fpdoc/glutbitmapfont.xml');

    Run;
  end;
end.

