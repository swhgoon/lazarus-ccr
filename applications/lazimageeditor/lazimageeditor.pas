program lazimageeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset   Preview,
  Forms,
  Main,
  PictureManager,
  PictureCtrls,
  Test,
  NewDialog,
  ResizeDialog,
  ResizePaperDialog,
  PictureDialog,
  AboutDialog, DLBitmap, IconStrConsts, appsettings, lieconstants, 
iconsizeselection;

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

  // With OS X app, ParamStr not meaningful unless launched with --args switch.
  if (ParamCount > 0) {$IFDEF DARWIN} and (Copy(ParamStr(1), 1, 4) <> '-psn') {$ENDIF} then
    MainForm.OpenImageFile(ParamStr(1))
  else
    MainForm.FileNewOnStart;
  Application.CreateForm(TSelectIconSizeForm, SelectIconSizeForm);
  Application.Run;
end.

