program fpbrowser;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces,
  Forms,
  printer4lazarus, turbopoweripro, laz_synapse,
  mainform {Form1},
  Submit in 'Submit.pas' {SubmitForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
(*  Fontdlg in 'Fontdlg.pas' {FontForm},
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm},
*)
  ImgForm in 'ImgForm.pas', pageloader, browsermodules {ImageForm};

begin
  Application.Initialize;
  Application.CreateForm(TformBrowser, formBrowser);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
