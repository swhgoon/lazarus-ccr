program lazimageeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset   Preview,
  Forms, Main, PictureManager, PictureCtrls, Test,
  NewDialog, ResizeDialog, ResizePaperDialog, PictureDialog, AboutDialog;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TNewDialogForm, NewDialogForm);
  Application.CreateForm(TResizeDialogForm, ResizeDialogForm);
  Application.CreateForm(TResizePaperDialogForm, ResizePaperDialogForm);

  // show new picture dialog
  MainForm.Show;
  //MainForm.FileNewExecute(nil);
  if ParamCount > 0 then
    MainForm.OpenImageFile(ParamStr(1))
  else
    MainForm.FileNewOnStart;

  Application.Run;
end.

