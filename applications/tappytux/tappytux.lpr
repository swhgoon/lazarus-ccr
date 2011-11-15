program tappytux;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, gameconfigform, gameplayform, tappyconfig,
  tappydrawer, tappygamedata, mod_tappywords, tappymodules, mod_tappymath;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TformConfig, formConfig);
  Application.CreateForm(TformTappyTuxGame, formTappyTuxGame);
  Application.Run;
end.

