unit mod_tappymath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // LCL
  ExtCtrls, LCLIntf, LCLType,
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
    function GetFallingDurationFromLevel: Integer;
    procedure Answered(AText: string); override;
    procedure EndGame(); override;
    procedure GameWon(); override;
    procedure GameLost(); override;
    procedure ProcessFallingTextEnd(); override;
    procedure ProcessSpriteEnd(AUserData: TObject; APosition: TPoint); override;
  end;

implementation

{ TTappyMath }

procedure TTappyMath.HandleOnTimer(Sender: TObject);
var
  i: Integer;
  j: Integer;
  frequency: Integer;
  snowmanWrong: TFallingText;
  lAnimation: TTappyTuxAnimation;
begin
  frequency := 60;
  count := count + 1;
  if count >= frequency then
  begin
     count := 0;
     CreateQuestion();
  end;

  vTappyTuxDrawer.HandleAnimationOnTimer(timerMath.Interval);
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
  timerMath.Interval:= 100;
  gameScore := 0;
  gameLives := 5;
  gameLevel := Level+1;
  if (Level < 0) then gameLevel := 1;
  if (SndFX <= 0) then gameSndFX := true;
  if (SndFX = 1) then gameSndFX := false;
  if (Music <= 0) then gameMusic := true;
  if (Music = 1) then gameMusic := false;
  gameSLevel := gameLevel;

  UpdateLevel(gameLevel);
  UpdateScore(gameScore);
  UpdateLives(gameLives);

  // Animations Creation
  lTuxAnimation := TTappySpriteAnimation.Create;
  lTuxAnimation.IsInfinite := True;
  lTuxAnimation.StartPoint := Point(250, 328);
  lTuxAnimation.EndPoint := lTuxAnimation.StartPoint;
  SetLength(lTuxAnimation.Images, 6);
  lTuxAnimation.LoadImageFromPng(0, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_1.png');
  lTuxAnimation.LoadImageFromPng(1, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_2.png');
  lTuxAnimation.LoadImageFromPng(2, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_3.png');
  lTuxAnimation.LoadImageFromPng(3, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_4.png');
  lTuxAnimation.LoadImageFromPng(4, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_3.png');
  lTuxAnimation.LoadImageFromPng(5, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_2.png');
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
  snowmanAnimation.EndPoint := Point(xAux, 270);
  snowmanAnimation.StepCount := GetFallingDurationFromLevel();
  snowmanAnimation.IsInfinite:= false;
  snowmanAnimation.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowman.png');
  //snowmanAnimation.caption:= gameQuestionList[random(gameQuestionList.Count - 1)];

  questionType[1] := random(3);

  Case questionType[1] of
  0: begin
    questionType[2] := random(21);
    questionType[3] := random(21);
    snowmanAnimation.value := (questionType[2] + questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' + ' +IntToStr(questionType[3]);
    end;

  1: begin
    questionType[2] := random(21);
    questionType[3] := random(questionType[2]);
    snowmanAnimation.value := (questionType[2] - questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' - ' +IntToStr(questionType[3]);
    end;

  2: begin
    questionType[2] := random(11);
    questionType[3] := random(11);
    snowmanAnimation.value := (questionType[2] * questionType[3]);
    snowmanAnimation.caption := IntToStr(questionType[2])+' x ' +IntToStr(questionType[3]);
    end;

  end;

  vTappyTuxDrawer.AddAnimation(snowmanAnimation);

end;

function TTappyMath.GetFallingDurationFromLevel: Integer;
begin
  case gameLevel of
  1: Result := 25000;
  2: Result := 20000;
  3: Result := 15000;
  4: Result := 10000;
  else
    Result := 7000;
  end;
end;

procedure TTappyMath.Answered(AText: string);
var
  i: Integer;
  j: Integer;
  lAnimation: TTappyTuxAnimation;
  hammer: TTappySpriteAnimation;
begin
  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    lAnimation := vTappyTuxDrawer.GetAnimation(i);
    if lAnimation is TFallingText then
    begin
       if TFallingText(lAnimation).value = StrToInt(AText) then
       begin
          gameScore := gameScore +1;
          gameLevel := (gameScore div 20) + gameSLevel;
          UpdateScore(gameScore);
          UpdateLevel(gameLevel);

          lAnimation.Stopped := True;

          hammer := TTappySpriteAnimation.Create;
          hammer.IsInfinite := False;
          hammer.StartPoint := Point(250, 328);
          hammer.EndPoint := lAnimation.Position;
          hammer.StepCount := 1000;
          hammer.SpriteChangeInterval := 200;
          SetLength(hammer.Images, 4);
          hammer.LoadImageFromPng(0, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'hammer_1.png');
          hammer.LoadImageFromPng(1, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'hammer_2.png');
          hammer.LoadImageFromPng(2, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'hammer_3.png');
          hammer.LoadImageFromPng(3, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'hammer_4.png');
          hammer.UserData := lAnimation;
          hammer.UserPosition := lAnimation.Position;
          vTappyTuxDrawer.AddAnimation(hammer);
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

  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
     vTappyTuxDrawer.RemoveAnimation(i);
     j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

  GoToConfigForm();
end;

procedure TTappyMath.GameWon;
var
  lRes: Integer;
begin
  timerMath.Enabled := False;

  // Now check what the user desires to do
  lRes := Application.MessageBox(
    'Congratulations, you have won the game =D Would you like to play again?', '', MB_YESNO);
  if lRes = ID_YES then RestartGame()
  else EndGame();
end;

procedure TTappyMath.GameLost;
var
  lRes: Integer;
begin
  timerMath.Enabled := False;

  // Now check what the user desires to do
  lRes := Application.MessageBox(
    'Unfortunately you have lost =P Would you like to play again?', '', MB_YESNO);
  if lRes = ID_YES then RestartGame()
  else EndGame();
end;

procedure TTappyMath.ProcessFallingTextEnd;
begin
  gameLives := gameLives - 1;
  UpdateLives(gameLives);
  if gameLives <= 0 then GameLost();
end;

procedure TTappyMath.ProcessSpriteEnd(AUserData: TObject; APosition: TPoint);
var
  snowmanRight: TFallingText;
  lIndex: Integer;
begin
  snowmanRight := TFallingText.Create;
  snowmanRight.IsInfinite := False;
  snowmanRight.StartPoint := APosition;
  snowmanRight.EndPoint := APosition;
  snowmanRight.Position := APosition;
  snowmanRight.StepCount := 2000;
  snowmanRight.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowmanright.png');
  snowmanRight.caption:= 'OK!';
  snowmanRight.ProcessOnEnd := False;
  vTappyTuxDrawer.AddAnimation(snowmanRight);
  lIndex := vTappyTuxDrawer.GetAnimationIndex(TTappyTuxAnimation(AUserData));
  if lIndex >= 0 then vTappyTuxDrawer.RemoveAnimation(lIndex);
end;

initialization
  AddModule(TTappyMath.Create);
end.

