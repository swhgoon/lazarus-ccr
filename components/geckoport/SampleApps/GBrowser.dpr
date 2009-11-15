program GBrowser;

uses
{$IFDEF LCL}
  Interfaces,
{$ENDIF}
  Forms,
  gec10 in 'gec10.pas' {Form1};

{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
