program lazedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main,
  lazedit_translations,
  EditorPageControl,
  HtmlCode,
  lazedit_constants, lazedit_about;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TLazEditMainForm, LazEditMainForm);

  {$ifndef Darwin}
  // Parse the command line options

  // if there are no files to open, then
  if ParamCount = 0 then
  begin
    LazEditMainForm.NoteBook.IsCreating := True;
    LazEditMainForm.DoFileNewByType(eftNone);
    LazEditMainForm.NoteBook.IsCreating := False;
  end
  else
    LazEditMainForm.TryFileOpen(ParamStr(1), False);
  {$endif}
  Application.CreateForm(TformAbout, formAbout);
  Application.Run;
end.

