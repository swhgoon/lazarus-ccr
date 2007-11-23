program laziconeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main, Preview, PictureManager, PictureCtrls, Test, JPEGForLazarus,
  NewDialog, ResizeDialog, ResizePaperDialog, PictureDialog, AboutDialog,
  LazRGBGraphics, LazColorPalette;
  
{$IFDEF WINDOWS}
{$R laziconeditor.res}
{$ENDIF}

begin
  Application.Title:='Lazarus Icon Editor';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPreviewForm, PreviewForm);
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TNewDialogForm, NewDialogForm);
  Application.CreateForm(TResizeDialogForm, ResizeDialogForm);
  Application.CreateForm(TResizePaperDialogForm, ResizePaperDialogForm);
  
  // show new picture dialog
  MainForm.Show;
  MainForm.FileNewExecute(nil);
  Application.Run;
end.

