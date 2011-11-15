unit mod_tappymath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
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
    gameQuestionList : TStringList;
    questionType : array[1..3] of Integer;
    questionAnswer : array[1..5] of Integer;
    count : Integer;
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
  end;

implementation

uses gameplayform;

{ TTappyMath }

procedure TTappyMath.HandleOnTimer(Sender: TObject);
var
  i: Integer;
  j: Integer;
  frequency: Integer;
begin
  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
    begin
       if (vTappyTuxDrawer.GetAnimation(i).Position.y >= 270) then
       begin
          gameLives := gameLives - 1;
          formTappyTuxGame.Lives.Text := IntToStr(gameLives);
          vTappyTuxDrawer.RemoveAnimation(i);
          i := i - 1;
          if (gameLives = 0) then EndGame();
          CreateQuestion;
       end;
    end;
    i := i + 1;
    j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

  frequency := 30;
  count := count + 1;
  if count >= frequency then
  begin
     count := 0;
     CreateQuestion();
  end;

  vTappyTuxDrawer.HandleAnimationOnTimer();

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
  lTuxAnimation: TTappySpriteAnimation;
begin
  count := 0;
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

  // Animations Creation
  lTuxAnimation := TTappySpriteAnimation.Create;
  lTuxAnimation.IsInfinite := True;
  lTuxAnimation.StartPoint := Point(250, 328);
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

  for i:= 1 to 5 do
  begin
    CreateQuestion;
  end

end;

procedure TTappyMath.CreateQuestion;
var
  i: Integer;
  j: Integer;
  xAux: Integer;
  yAux: Integer;
  heightAux: array [0..4] of integer;
  existenceAux: array [0..4] of boolean;
  snowmanAnimation: TFallingText;

begin

  for i:= 0 to 4 do
  begin
     existenceAux[i]:= False;
     heightAux[i] := 500;
  end;

  xAux:=5;
  yAux:=5;

  snowmanAnimation := TFallingText.Create;
  snowmanAnimation.IsInfinite := False;


  for i:= 0 to vTappyTuxDrawer.GetAnimationCount - 1 do
  begin
     if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
     begin
        for j:= 0  to 4 do
        begin
           if vTappyTuxDrawer.GetAnimation(i).StartPoint.X = 5+(103*j) then
           begin
              existenceAux[j] := True;
              if vTappyTuxDrawer.GetAnimation(i).Position.Y < heightAux[j] then heightAux[j] := vTappyTuxDrawer.GetAnimation(i).Position.Y;
           end;
        end;
     end;
  end;

  for i:= 0 to 4 do
  begin
     if heightAux[i] > yAux then
     begin
        yAux := heightAux[i];
        xAux := 5 + i*103;
     end;
  end;


  if existenceAux[0] = false then xAux := 5
  else
  begin
    if existenceAux[1] = false then xAux := 108
    else
    begin
      if existenceAux[2] = false then xAux := 211
      else
      begin
        if existenceAux[3] = false then xAux := 314
        else
        begin
          if existenceAux[4] = false then xAux := 417
        end;
      end;
    end;
  end;

  snowmanAnimation.StartPoint := Point(xAux, 5);
  snowmanAnimation.EndPoint := Point(xAux, 205);
  snowmanAnimation.IsInfinite:= false;
  snowmanAnimation.Bitmap := TPortableNetworkGraphic.Create;
  //snowmanAnimation.caption:= gameQuestionList[random(gameQuestionList.Count - 1)];
  snowmanAnimation.Bitmap.LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowman.png');

  questionType[1] := random(3);

  Case questionType[1] of
  0: begin
    questionType[2] := random(21);
    questionType[3] := random(21);
    snowmanAnimation.value := IntToStr(questionType[2] + questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    end;

  1: begin
    questionType[2] := random(21);
    questionType[3] := random(questionType[2]);
    snowmanAnimation.value := IntToStr(questionType[2] - questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    end;

  2: begin
    questionType[2] := random(11);
    questionType[3] := random(11);
    snowmanAnimation.value := IntToStr(questionType[2] * questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    end;

  end;

  vTappyTuxDrawer.AddAnimation(snowmanAnimation);

end;

procedure TTappyMath.Answered;
var
  i: Integer;
  j: Integer;
begin
  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
    begin
       if (vTappyTuxDrawer.GetAnimation(i).value = formTappyTuxGame.Answer.Text) then
       begin
          gameScore := gameScore +1;
          gameLevel := (gameScore div 20) + gameSLevel;
          formTappyTuxGame.Score.Text := IntToStr(gameScore);
          formTappyTuxGame.Level.Text := IntToStr(gameLevel);
          vTappyTuxDrawer.RemoveAnimation(i);
          i := i - 1;
          CreateQuestion;
       end;
    end;
    i := i + 1;
    j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

end;

procedure TTappyMath.EndGame;
var
  i : Integer;
  j : Integer;
  gameOverScreen: TTappySpriteAnimation;
  continueBtn: TButton;
  exitBtn: TButton;
begin
  timerMath.Enabled := False;
  formTappyTuxGame.Answer.ReadOnly := true;

  //gameOverScreen := TTappySpriteAnimation.Create;
  //gameOverScreen.IsInfinite := True;
  //gameOverScreen.StartPoint := Point(90, 150);
  //gameOverScreen.EndPoint := gameOverScreen.StartPoint;
  //SetLength(gameOverScreen.Bitmaps, 1);
  //gameOverScreen.Bitmaps[0] := TPortableNetworkGraphic.Create;
  //gameOverScreen.Bitmaps[0].LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'gameover.png');
  //vTappyTuxDrawer.AddAnimation(gameOverScreen);

  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
     vTappyTuxDrawer.RemoveAnimation(i);
     j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

  formTappyTuxGame.GameOver.Visible := true;
  formTappyTuxGame.Yes.Visible := true;
  formTappyTuxGame.No.Visible := true;
end;

initialization
  AddModule(TTappyMath.Create);
end.

