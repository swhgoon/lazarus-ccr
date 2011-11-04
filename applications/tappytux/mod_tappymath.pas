unit mod_tappymath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  ExtCtrls,
  // TappyTux
  tappymodules;

type

  { TTappyMath }

  TTappyMath = class(TTappyModule)
  private
    gameScore : Integer;
    gameLives : Integer;
    gameLevel : Integer;
    gameSLevel : Integer;
    gameSndFX : Boolean;
    gameMusic : Boolean;
    questionType : array[1..3] of Integer;
    questionAnswer : array[1..5] of Integer;
    questionNumber : Integer;
    timerMath : TTimer;
    procedure HandleOnTimer(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer); override;
    procedure Answered(); override;
    procedure EndGame(); override;
    procedure QuestionGenerator(qNumber : Integer);
  end;

implementation

uses tappydrawer, gameplayform {,tappygamedata};

{ TTappyMath }

procedure TTappyMath.HandleOnTimer(Sender: TObject);
begin

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
       QuestionGenerator(1);
  end;

  if ((formTappyTuxGame.Question2.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question2.Top:= 24;
       QuestionGenerator(2);
  end;

  if ((formTappyTuxGame.Question3.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question3.Top:= 24;
       QuestionGenerator(3);
  end;

  if ((formTappyTuxGame.Question4.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question4.Top:= 24;
       QuestionGenerator(4);
  end;

  if ((formTappyTuxGame.Question5.Top >= 370)) then
  begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       formTappyTuxGame.Question5.Top:= 24;
       QuestionGenerator(5);
  end;

end;

constructor TTappyMath.Create;
begin
  inherited Create;

  timerMath := TTimer.Create(nil);
  timerMath.Enabled := False;
  timerMath.Interval := 1000;
  timerMath.OnTimer := @HandleOnTimer;

end;

destructor TTappyMath.Destroy;
begin
  timerMath.Free;

  inherited Destroy;
end;

procedure TTappyMath.TranslateTextsToEnglish;
begin
  ShortDescription := 'TappyMath - A game to learn arithmetics';
end;

procedure TTappyMath.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyMath - Um jogo para aprender aritmética';
end;

procedure TTappyMath.StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer);
begin

  timerMath.Enabled := True;
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

  QuestionGenerator(1);
  QuestionGenerator(2);
  QuestionGenerator(3);
  QuestionGenerator(4);
  QuestionGenerator(5);

end;

procedure TTappyMath.Answered;
begin

  if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[1])) then
  begin
       formTappyTuxGame.Question1.Top := 24;
       QuestionGenerator(1);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[2])) then
  begin
       formTappyTuxGame.Question2.Top := 24;
       QuestionGenerator(2);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[3])) then
  begin
       formTappyTuxGame.Question3.Top := 24;
       QuestionGenerator(3);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[4])) then
  begin
       formTappyTuxGame.Question4.Top := 24;
       QuestionGenerator(4);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;
  if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[5])) then
  begin
       formTappyTuxGame.Question5.Top := 24;
       QuestionGenerator(5);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  end;

end;

procedure TTappyMath.EndGame;
begin
  timerMath.Enabled := False;
  formTappyTuxGame.Answer.ReadOnly := true;
  formTappyTuxGame.GameOver.Visible := true;
  formTappyTuxGame.Yes.Visible := true;
  formTappyTuxGame.No.Visible := true;
end;

procedure TTappyMath.QuestionGenerator(qNumber : Integer);
begin

  questionType[1] := random(3);

  Case questionType[1] of
  0: begin
    questionType[2] := random(21);
    questionType[3] := random(21);
    questionAnswer[qNumber] := questionType[2] + questionType[3];
    if (qNumber = 1) then formTappyTuxGame.Question1.Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    if (qNumber = 2) then formTappyTuxGame.Question2.Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    if (qNumber = 3) then formTappyTuxGame.Question3.Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    if (qNumber = 4) then formTappyTuxGame.Question4.Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    if (qNumber = 5) then formTappyTuxGame.Question5.Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    end;

  1: begin
    questionType[2] := random(21);
    questionType[3] := random(questionType[2]);
    questionAnswer[qNumber] := questionType[2] - questionType[3];
    if (qNumber = 1) then formTappyTuxGame.Question1.Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    if (qNumber = 2) then formTappyTuxGame.Question2.Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    if (qNumber = 3) then formTappyTuxGame.Question3.Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    if (qNumber = 4) then formTappyTuxGame.Question4.Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    if (qNumber = 5) then formTappyTuxGame.Question5.Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    end;

  2: begin
    questionType[2] := random(11);
    questionType[3] := random(11);
    questionAnswer[qNumber] := questionType[2] * questionType[3];
    if (qNumber = 1) then formTappyTuxGame.Question1.Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    if (qNumber = 2) then formTappyTuxGame.Question2.Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    if (qNumber = 3) then formTappyTuxGame.Question3.Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    if (qNumber = 4) then formTappyTuxGame.Question4.Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    if (qNumber = 5) then formTappyTuxGame.Question5.Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    end;

  end;

end;

initialization
  AddModule(TTappyMath.Create);
end.

