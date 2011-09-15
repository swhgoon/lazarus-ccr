unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  ExtCtrls,
  // TappyTux
  tappymodules, tappydrawer;

type

  { TTappyWords }

  TTappyWords = class(TTappyModule)
  private
    timerWords: TTimer;
    procedure HandleOnTimer(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
    procedure StartNewGame(); override;
    procedure EndGame(); override;
  end;

implementation

{ TTappyWords }

procedure TTappyWords.HandleOnTimer(Sender: TObject);
begin
  vTappyTuxDrawer.HandleAnimationOnTimer();

  // Create falling ballons here
  vTappyTuxDrawer.AddAnimation(TBallonAnimation.Create);
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

procedure TTappyWords.StartNewGame;
begin
  timerWords.Enabled := True;
end;

procedure TTappyWords.EndGame;
begin
  timerWords.Enabled := False;
end;

initialization
  AddModule(TTappyWords.Create);
end.

