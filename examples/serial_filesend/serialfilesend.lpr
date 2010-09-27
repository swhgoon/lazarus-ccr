{
  Serial File Transfer example with Lazarus

  This project requires Synaser, which can be downloaded via svn with:

  https://synalist.svn.sourceforge.net/svnroot/synalist/trunk/
}
program serialfilesend;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, laz_synapse, mainform
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformSerial, formSerial);
  Application.Run;
end.

