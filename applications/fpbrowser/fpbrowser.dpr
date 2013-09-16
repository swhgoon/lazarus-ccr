program fpbrowser;

{$define FPBROWSER_TURBOPOWERIPRO}
{.$define FPBROWSER_THTMLCOMP}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces,
  Forms,
  printer4lazarus, turbopoweripro, customdrawn, laz_synapse,
  mainform {Form1},
  Submit in 'Submit.pas' {SubmitForm},
  dlgabout in 'Htmlabt.pas' {AboutBox},
(*  Fontdlg in 'Fontdlg.pas' {FontForm},
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm},
*)
  ImgForm in 'ImgForm.pas', pageloader, browsermodules,
{$ifdef FPBROWSER_THTMLCOMP}
  viewer_thtmlcomp,
{$endif}
{$ifdef FPBROWSER_TURBOPOWERIPRO}
  viewer_ipro,
{$endif}
  browserviewer, mod_braille, browserconstants, dlgconfig, browserconfig,
  mod_testhttp;

begin
  Application.Initialize;
  Application.CreateForm(TformBrowser, formBrowser);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.CreateForm(TformConfig, formConfig);
  Application.CreateForm(TformTestHttp, formTestHttp);
  Application.Run;
end.
