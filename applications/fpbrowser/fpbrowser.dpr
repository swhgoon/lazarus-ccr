program fpbrowser;
{A program to demonstrate the ThtmlViewer component}

uses
{$IFDEF LCL}
  Interfaces,
{$ENDIF}
  Forms, laz_synapse,
  mainform {Form1},
  Submit in 'Submit.pas' {SubmitForm},
  Fontdlg in 'Fontdlg.pas' {FontForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
{$IFNDEF LCL}
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm},
{$ENDIF}
  ImgForm in 'ImgForm.pas', pageloader {ImageForm};

{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
