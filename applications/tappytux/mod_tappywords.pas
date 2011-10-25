unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  ExtCtrls,
  // TappyTux
  tappymodules, tappygamedata;

type

  { TTappyWords }

  TTappyWords = class(TTappyModule)
  private
    timerWords: TTimer;
    newGame : TTappyGameData;
    procedure HandleOnTimer(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer); override;
    procedure Answered(); override;
    procedure EndGame(); override;
  end;

implementation

uses tappydrawer, gameplayform;

{ TTappyWords }

procedure TTappyWords.HandleOnTimer(Sender: TObject);
begin
  //vTappyTuxDrawer.HandleAnimationOnTimer();
  formTappyTuxGame.Question1.Top:= formTappyTuxGame.Question1.Top + 10;
  formTappyTuxGame.Question2.Top:= formTappyTuxGame.Question2.Top + 10;
  formTappyTuxGame.Question3.Top:= formTappyTuxGame.Question3.Top + 10;
  formTappyTuxGame.Question4.Top:= formTappyTuxGame.Question4.Top + 10;
  formTappyTuxGame.Question5.Top:= formTappyTuxGame.Question5.Top + 10;

  // Create falling ballons here
  //vTappyTuxDrawer.AddAnimation(TBallonAnimation.Create);
end;

constructor TTappyWords.Create;
begin
  inherited Create;

  timerWords := TTimer.Create(nil);
  timerWords.Enabled := False;
  timerWords.Interval := 1000;
  timerWords.OnTimer := @HandleOnTimer;

end;

destructor TTappyWords.Destroy;
begin
  timerWords.Free;

  inherited Destroy;
end;

procedure TTappyWords.TranslateTextsToEnglish;
begin
  ShortDescription := 'TappyWords - A game to learn typing and ortography';
end;

procedure TTappyWords.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyWords - Um jogo para aprender a digitar e ortografia';
end;

procedure TTappyWords.StartNewGame(SndFX: Integer; Music: Integer; Level: Integer);
begin

  //Causam "External: SIGSEGV"
  //gameData.SetSndFX(SndFX);
  //gameData.SetMusic(Music);
  //gameData.SetLevel(Level);

  timerWords.Enabled := True;

  //formTappyTuxGame.Question1.Top:= formTappyTuxGame.Question1.Top + timerWords.ComponentCount ;

  end;

procedure TTappyWords.Answered;
begin
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question1.Text) then formTappyTuxGame.Question1.Top := 50;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question2.Text) then formTappyTuxGame.Question2.Top := 50;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question3.Text) then formTappyTuxGame.Question3.Top := 50;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question4.Text) then formTappyTuxGame.Question4.Top := 50;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question5.Text) then formTappyTuxGame.Question5.Top := 50;

end;

procedure TTappyWords.EndGame;
begin
  timerWords.Enabled := False;
end;

initialization
  AddModule(TTappyWords.Create);
end.

