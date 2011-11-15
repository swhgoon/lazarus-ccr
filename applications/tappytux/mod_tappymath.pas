unit mod_tappymath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  ExtCtrls,
  // TappyTux
  tappyconfig, tappydrawer, tappymodules;

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
    timerMath : TTimer;
    procedure HandleOnTimer(Sender: TObject);

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer); override;
    procedure CreateQuestion(); override;
    procedure Answered(); override;
    procedure EndGame(); override;
    procedure QuestionGenerator(qNumber : Integer);
  end;

implementation

uses gameplayform;

{ TTappyMath }

procedure TTappyMath.HandleOnTimer(Sender: TObject);
var
  i: Integer;
begin
  for i:= 1 to 5 do
  begin
    Questions[i].Top := Questions[i].Top + (5*gameLevel);
    if ((Questions[i].Top >= 370)) then
    begin
       gameLives := gameLives - 1;
       formTappyTuxGame.Lives.Text := IntToStr(gameLives);
       if (gameLives = 0) then EndGame();
       Questions[i].Top:= 24;
       QuestionGenerator(i);
    end;
  end

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
  ShortDescription := 'TappyMath';
  LongDescription := 'A game to learn arithmetics';
end;

procedure TTappyMath.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyMath';
  LongDescription := 'Um jogo para aprender aritmética';
end;

procedure TTappyMath.StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer);
var
  i: Integer;
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

  for i:= 1 to 5 do
  begin
    QuestionGenerator(i);
  end

end;

procedure TTappyMath.CreateQuestion;
begin

end;

procedure TTappyMath.Answered;
var
  i: Integer;
begin
  for i:= 1 to 5 do
  begin
    if (formTappyTuxGame.Answer.Text = IntToStr(questionAnswer[i])) then
    begin
       Questions[i].Top := 24;
       QuestionGenerator(i);
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
    end;
  end

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
    Questions[qNumber].Text := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    end;

  1: begin
    questionType[2] := random(21);
    questionType[3] := random(questionType[2]);
    questionAnswer[qNumber] := questionType[2] - questionType[3];
    Questions[qNumber].Text := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    end;

  2: begin
    questionType[2] := random(11);
    questionType[3] := random(11);
    questionAnswer[qNumber] := questionType[2] * questionType[3];
    Questions[qNumber].Text := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    end;

  end;

end;

initialization
  AddModule(TTappyMath.Create);
end.

