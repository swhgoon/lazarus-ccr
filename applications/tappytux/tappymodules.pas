unit tappymodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  // TappyTux
  tappyconfig;

type

  { TTappyModule }

  TTappyModule = class
  public
    imgLevel2, imgLevel3: TJPEGImage;
    imgPenguim : TBitmap;
    ShortDescription, LongDescription: string;
    ConfigCaption, ConfigItems: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadImages; virtual;
    function GetBackgroundImage(ALevel: Integer): TJPEGImage;
    procedure GoToConfigForm;
    procedure RestartGame;
    procedure UpdateLevel(ALevel: Integer);
    procedure UpdateScore(AScore: Integer);
    procedure UpdateLives(ALives: Integer);
    procedure TranslateTexts(ALanguage: Integer);
    procedure TranslateTextsToEnglish; virtual;
    procedure TranslateTextsToPortuguese; virtual;
    procedure InitModule(); virtual;
    procedure StartNewGame(SndFX: Integer; Music: Integer; Level: Integer; QuestionList: Integer); virtual; abstract;
    procedure CreateQuestion(); virtual; abstract;
    procedure Answered(AText: string); virtual; abstract;
    procedure EndGame(); virtual; abstract;
    procedure GameWon(); virtual; abstract;
    procedure GameLost(); virtual; abstract;
    procedure ProcessFallingTextEnd(); virtual; abstract;
    procedure ProcessSpriteEnd(AUserData: TObject; APosition: TPoint); virtual; abstract;
  end;

procedure AddModule(AModule: TTappyModule);
function GetCurrentModule: TTappyModule;
function GetModule(AIndex: Integer): TTappyModule;
function GetModuleCount: Integer;
procedure SetCurrentModule(AIndex: Integer);

implementation

uses
  gameplayform, gameconfigform;

var
  gTappyModules: TFPList;
  gCurrentTappyModule: Integer = 0;  //=-1

procedure AddModule(AModule: TTappyModule);
begin
  if gTappyModules = nil then gTappyModules := TFPList.Create;
  gTappyModules.Add(Pointer(AModule));
end;

function GetCurrentModule: TTappyModule;
begin
  Result := GetModule(gCurrentTappyModule);
end;

function GetModule(AIndex: Integer): TTappyModule;
begin
  Result := TTappyModule(gTappyModules.Items[AIndex]);
end;

function GetModuleCount: Integer;
begin
  Result := gTappyModules.Count;
end;

procedure SetCurrentModule(AIndex: Integer);
begin
  gCurrentTappyModule := AIndex;
end;

{ TTappyModule }

constructor TTappyModule.Create;
begin
  inherited Create;

  imgLevel2 := TJPEGImage.Create;
  imgLevel3 := TJPEGImage.Create;

  TranslateTexts(ID_ENGLISH);
end;

destructor TTappyModule.Destroy;
begin
  imgLevel2.Free;
  imgLevel3.Free;

  inherited Destroy;
end;

procedure TTappyModule.LoadImages;
var
  lDir: string;
begin
  lDir := vTappyTuxConfig.GetResourcesDir();

  imgLevel2.LoadFromFile(lDir + 'images'+PathDelim+'levels'+PathDelim+'levelp.jpg');
  imgLevel3.LoadFromFile(lDir + 'images'+PathDelim+'levels'+PathDelim+'levelp.jpg');
end;

function TTappyModule.GetBackgroundImage(ALevel: Integer): TJPEGImage;
begin
  Result := imgLevel3;
end;

procedure TTappyModule.GoToConfigForm;
begin
  formConfig.Show;
  formTappyTuxGame.Hide;
end;

procedure TTappyModule.RestartGame;
begin
  StartNewGame(formConfig.comboSound.ItemIndex,
               formConfig.comboMusic.ItemIndex,
               formConfig.comboLevel.ItemIndex,
               formConfig.listWordlist.ItemIndex);
end;

procedure TTappyModule.UpdateLevel(ALevel: Integer);
begin
  formTappyTuxGame.Level.Text := IntToStr(ALevel);
end;

procedure TTappyModule.UpdateScore(AScore: Integer);
begin
  formTappyTuxGame.Score.Text := IntToStr(AScore);
end;

procedure TTappyModule.UpdateLives(ALives: Integer);
begin
  formTappyTuxGame.Lives.Text := IntToStr(ALives);
end;

procedure TTappyModule.TranslateTexts(ALanguage: Integer);
begin
  case ALanguage of
  ID_ENGLISH: TranslateTextsToEnglish();
  ID_PORTUGUESE: TranslateTextsToEnglish();
  end;
end;

procedure TTappyModule.TranslateTextsToEnglish;
begin

end;

procedure TTappyModule.TranslateTextsToPortuguese;
begin

end;

procedure TTappyModule.InitModule;
begin
  LoadImages();
end;

initialization
  if gTappyModules = nil then
    gTappyModules := TFPList.Create;
finalization
  gTappyModules.Free;
end.

