unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  ExtCtrls,
  // TappyTux
  tappymodules{, tappygamedata};

type

  { TTappyWords }

  TTappyWords = class(TTappyModule)
  private
    gameScore : Integer;
    gameLives : Integer;
    gameLevel : Integer;
    gameSLevel : Integer;
    gameSndFX : Boolean;
    gameMusic : Boolean;
    timerWords: TTimer;
    {newGame : TTappyGameData;}
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
  formTappyTuxGame.Question1.Top:= formTappyTuxGame.Question1.Top + (5*gameLevel);
  formTappyTuxGame.Question2.Top:= formTappyTuxGame.Question2.Top + (5*gameLevel);
  formTappyTuxGame.Question3.Top:= formTappyTuxGame.Question3.Top + (5*gameLevel);
  formTappyTuxGame.Question4.Top:= formTappyTuxGame.Question4.Top + (5*gameLevel);
  formTappyTuxGame.Question5.Top:= formTappyTuxGame.Question5.Top + (5*gameLevel);

  if ((formTappyTuxGame.Question1.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question1.Top:= 24;
  end;

  if ((formTappyTuxGame.Question2.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question2.Top:= 24;
  end;

  if ((formTappyTuxGame.Question3.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question3.Top:= 24;
  end;

  if ((formTappyTuxGame.Question4.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question4.Top:= 24;
  end;

  if ((formTappyTuxGame.Question5.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question5.Top:= 24;
  end;


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
  gameScore := 0;
  gameLives := 5;
  gameLevel := Level+1;
  if (Level < 0) then gameLevel := 1;
  if (SndFX <= 0) then gameSndFX := true;
  if (SndFX = 1) then gameSndFX := false;
  if (Music <= 0) then gameMusic := true;
  if (Music = 1) then gameMusic := false;
  gameSLevel := gameLevel;

  formTappyTuxGame.Answer.ReadOnly := false;
  formTappyTuxGame.GameOver.Visible := false;
  formTappyTuxGame.Yes.Visible := false;
  formTappyTuxGame.No.Visible := false;
  formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  formTappyTuxGame.Score.Text := IntToStr(gameScore);
  formTappyTuxGame.Lives.Text := IntToStr(gameLives);

end;

procedure TTappyWords.Answered;
begin
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question1.Text) then
  begin
       formTappyTuxGame.Question1.Top := 24;
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question2.Text) then
  begin
       formTappyTuxGame.Question2.Top := 24;
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question3.Text) then
  begin
       formTappyTuxGame.Question3.Top := 24;
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question4.Text) then
  begin
       formTappyTuxGame.Question4.Top := 24;
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = formTappyTuxGame.Question5.Text) then
  begin
       formTappyTuxGame.Question5.Top := 24;
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;

end;

procedure TTappyWords.EndGame;
begin
  timerWords.Enabled := False;
  formTappyTuxGame.Answer.ReadOnly := true;
  formTappyTuxGame.GameOver.Visible := true;
  formTappyTuxGame.Yes.Visible := true;
  formTappyTuxGame.No.Visible := true;
end;

initialization
  AddModule(TTappyWords.Create);
end.

