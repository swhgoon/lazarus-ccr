unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, fpSound,
  // LCL
  ExtCtrls, IntfGraphics,
  // TappyTux
  tappyconfig, tappydrawer, tappymodules;

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
    count : Integer;
    timerWords: TTimer;
    procedure HandleOnTimer(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer); override;
    procedure createQuestion(); override;
    procedure Answered(); override;
    procedure EndGame(); override;
  end;

var
   backgroundMusic: TSoundDocument;
   startupSound: TSoundDocument;


implementation

uses gameplayform;

{ TTappyWords }

procedure TTappyWords.HandleOnTimer(Sender: TObject);
var
  i: Integer;
  j: Integer;
  frequency: Integer;
  snowmanWrong: TFallingText;
begin
  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
    begin
       if (vTappyTuxDrawer.GetAnimation(i).Caption = 'OK!') then
       begin
          if (vTappyTuxDrawer.GetAnimation(i).Value = '1') then
          begin
             vTappyTuxDrawer.RemoveAnimation(i);
             i := i - 1;
          end;
          if (vTappyTuxDrawer.GetAnimation(i).Value = '0') then vTappyTuxDrawer.GetAnimation(i).Value := '1';
       end;
       if (vTappyTuxDrawer.GetAnimation(i).Caption = 'Oh-oh!') then
       begin
          vTappyTuxDrawer.RemoveAnimation(i);
          gameLives := gameLives - 1;
          formTappyTuxGame.Lives.Text := IntToStr(gameLives);
          CreateQuestion();
          if gameLives <= 0 then EndGame();
          i := i - 1;
       end;
    end;
    i := i + 1;
    j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
    begin
       if ((vTappyTuxDrawer.GetAnimation(i).Position.y >= 270) AND (vTappyTuxDrawer.GetAnimation(i).Caption <> 'Oh-oh!')) then
       begin
          snowmanWrong := TFallingText.Create;
          snowmanWrong.IsInfinite := False;
          snowmanWrong.StartPoint := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanWrong.EndPoint := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanWrong.Position := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanWrong.caption:= 'Oh-oh!';
          snowmanWrong.value:= '0';
          snowmanWrong.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowmanwrong.png');
          vTappyTuxDrawer.AddAnimation(snowmanWrong);
          vTappyTuxDrawer.RemoveAnimation(i);
          i := i -1;
       end;
    end;
    i := i + 1;
    j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;

  frequency := 60;
  count := count + 1;
  if count >= frequency then
  begin
     count := 0;
     CreateQuestion();
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
  count := 5;
  timerWords.Enabled := True;
  timerWords.Interval:= 500;
  gameScore := 0;
  gameLives := 5;
  gameLevel := Level+1;
  if (Level < 0) then gameLevel := 1;
  if (SndFX <= 0) then gameSndFX := true;
  if (SndFX = 1) then gameSndFX := false;
  if (Music <= 0) then gameMusic := true;
  if (Music = 1) then gameMusic := false;
  gameSLevel := gameLevel;

  if QuestionList < 0 then QuestionList := 0;
  gameQuestionList := TStringList.Create;
  gameQuestionList.LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images'+PathDelim+'modules'+PathDelim+'tappywords'+PathDelim+'0.txt');
  //gameQuestionList.LoadFromFile('C:/'+IntToStr(QuestionList)+'.txt');

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
  SetLength(lTuxAnimation.Images, 6);
  lTuxAnimation.LoadImageFromPng(0, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_1.png');
  lTuxAnimation.LoadImageFromPng(1, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_2.png');
  lTuxAnimation.LoadImageFromPng(2, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_3.png');
  lTuxAnimation.LoadImageFromPng(3, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_4.png');
  lTuxAnimation.LoadImageFromPng(4, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_3.png');
  lTuxAnimation.LoadImageFromPng(5, vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'tux_2.png');
  vTappyTuxDrawer.AddAnimation(lTuxAnimation);

  //Sound Creation
  //backgroundMusic := TSoundDocument.Create;
  //backgroundMusic.LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'music' + PathDelim + 'tux_1.png');
  //startupSound := TSoundDocument.Create;
  //startupSound.LoadFromFile(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sounds' + PathDelim + 'startup.wav');

  for i:= 1 to 5 do
  begin
       CreateQuestion;
  end;

  startupSound.Play;

end;

procedure TTappyWords.CreateQuestion();
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
  snowmanAnimation.EndPoint := Point(xAux, 100);
  snowmanAnimation.IsInfinite:= false;
  snowmanAnimation.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowman.png');
  snowmanAnimation.caption:= gameQuestionList[random(gameQuestionList.Count - 1)];
  vTappyTuxDrawer.AddAnimation(snowmanAnimation);

end;

procedure TTappyWords.Answered;
var
  i: Integer;
  j: Integer;
  snowmanRight: TFallingText;
begin
  i:= 0;
  j:= vTappyTuxDrawer.GetAnimationCount - 1;
  while (i<= j) do
  begin
    if vTappyTuxDrawer.GetAnimation(i).InheritsFrom(TFallingText) then
    begin
       if (vTappyTuxDrawer.GetAnimation(i).caption = formTappyTuxGame.Answer.Text) then
       begin
          gameScore := gameScore +1;
          gameLevel := (gameScore div 20) + gameSLevel;
          formTappyTuxGame.Score.Text := IntToStr(gameScore);
          formTappyTuxGame.Level.Text := IntToStr(gameLevel);
          snowmanRight := TFallingText.Create;
          snowmanRight.IsInfinite := False;
          snowmanRight.StartPoint := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanRight.EndPoint := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanRight.Position := vTappyTuxDrawer.GetAnimation(i).Position;
          snowmanRight.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowmanright.png');
          snowmanRight.caption:= 'OK!';
          snowmanRight.value:= '0';
          vTappyTuxDrawer.AddAnimation(snowmanRight);
          vTappyTuxDrawer.RemoveAnimation(i);
          i := i - 1;
       end;
    end;
    i := i + 1;
    j := vTappyTuxDrawer.GetAnimationCount - 1;
  end;
  CreateQuestion;

end;

procedure TTappyWords.EndGame;
var
  i : Integer;
  j : Integer;
  gameOverScreen: TTappySpriteAnimation;
  continueBtn: TButton;
  exitBtn: TButton;
begin
  timerWords.Enabled := False;
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
  AddModule(TTappyWords.Create);
end.

