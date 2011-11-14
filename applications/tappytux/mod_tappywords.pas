unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  // LCL
  ExtCtrls, Graphics,
  // TappyTux
  tappyconfig, tappymodules;

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
    gameQuestionList : TStringList;
    timerWords: TTimer;
    procedure HandleOnTimer(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer); override;
    procedure Answered(); override;
    procedure EndGame(); override;
  end;


implementation

uses tappydrawer, gameplayform;

{ TTappyWords }

procedure TTappyWords.HandleOnTimer(Sender: TObject);
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
       Questions[i].Text := formTappyTuxGame.Test.Lines.Strings[random(71)];
    end;
  end;

  vTappyTuxDrawer.HandleAnimationOnTimer();
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
  ShortDescription := 'TappyWords';
  LongDescription := 'A game to learn typing and ortography.'; // Hint: Try to keep looking at the screen instead of the keyboard!
end;

procedure TTappyWords.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyWords';
  LongDescription := 'Um jogo para aprender a digitar e ortografia';
end;

procedure TTappyWords.StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer);
var
  i: Integer;
  lTuxAnimation: TTappySpriteAnimation;
begin
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

  gameQuestionList := TStringList.Create;
  gameQuestionList.LoadFromFile(vTappyTuxConfig.GetResourcesDir()+ 'images'+PathDelim+'modules'+PathDelim+'tappywords'+PathDelim+'0.txt');
  //gameQuestionList.LoadFromFile('C:/0.txt');

  formTappyTuxGame.Answer.ReadOnly := false;
  formTappyTuxGame.GameOver.Visible := false;
  formTappyTuxGame.Yes.Visible := false;
  formTappyTuxGame.No.Visible := false;
  formTappyTuxGame.Level.Text := IntToStr(gameLevel);
  formTappyTuxGame.Score.Text := IntToStr(gameScore);
  formTappyTuxGame.Lives.Text := IntToStr(gameLives);

  for i:= 1 to 5 do
  begin
    //Questions[i].Text := formTappyTuxGame.Test.Lines.Strings[random(71)];
    Questions[i].Text := gameQuestionList[random(gameQuestionList.Count - 1)];
  end;

  // Adds the Tux animation
  lTuxAnimation := TTappySpriteAnimation.Create;
  lTuxAnimation.IsInfinite := True;
  lTuxAnimation.StartPoint := Point(250, 300);
  lTuxAnimation.EndPoint := lTuxAnimation.StartPoint;
  SetLength(lTuxAnimation.Bitmaps, 6);
  lTuxAnimation.Bitmaps[0] := TPortableNetworkGraphic.Create;
  lTuxAnimation.Bitmaps[0].LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_1.png');
  lTuxAnimation.Bitmaps[1] := TPortableNetworkGraphic.Create;
  lTuxAnimation.Bitmaps[1].LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_2.png');
  lTuxAnimation.Bitmaps[2] := TPortableNetworkGraphic.Create;
  lTuxAnimation.Bitmaps[2].LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_3.png');
  lTuxAnimation.Bitmaps[3] := TPortableNetworkGraphic.Create;
  lTuxAnimation.Bitmaps[3].LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_4.png');
  lTuxAnimation.Bitmaps[4] := lTuxAnimation.Bitmaps[2];
  lTuxAnimation.Bitmaps[5] := lTuxAnimation.Bitmaps[1];
  vTappyTuxDrawer.AddAnimation(lTuxAnimation);
end;

procedure TTappyWords.Answered;
var
  i: Integer;
begin
  for i:= 1 to 5 do
  begin
    if (formTappyTuxGame.Answer.Text = Questions[i].Text) then
    begin
       Questions[i].Top := 24;
       Questions[i].Text := formTappyTuxGame.Test.Lines.Strings[random(71)];
       gameScore := gameScore +1;
       gameLevel := (gameScore div 20) + gameSLevel;
       formTappyTuxGame.Score.Text := IntToStr(gameScore);
       formTappyTuxGame.Level.Text := IntToStr(gameLevel);
    end;
  end

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

