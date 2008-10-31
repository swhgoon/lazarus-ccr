program Exdmod;
{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  Exdmodu1 in 'EXDMODU1.PAS' {Form1},
  Exdmodu2 in 'EXDMODU2.PAS' {SNEntryDlg}, tponguard;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
