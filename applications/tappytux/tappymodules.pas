unit tappymodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  tappyconfig;

type

  { TTappyModule }

  TTappyModule = class
  public
    imgLevel2, imgLevel3: TJPEGImage;
    ShortDescription, LongDescription: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadImages; virtual;
    function GetBackgroundImage(ALevel: Integer): TJPEGImage;
    procedure TranslateTexts(ALanguage: Integer);
    procedure TranslateTextsToEnglish; virtual;
    procedure TranslateTextsToPortuguese; virtual;
    procedure InitModule(); virtual;
    procedure StartNewGame(); virtual; abstract;
    procedure EndGame(); virtual; abstract;
  end;

procedure AddModule(AModule: TTappyModule);
function GetCurrentModule: TTappyModule;
function GetModule(AIndex: Integer): TTappyModule;
function GetModuleCount: Integer;
procedure SetCurrentModule(AIndex: Integer);

implementation

var
  gTappyModules: TFPList;
  gCurrentTappyModule: Integer = -1;

procedure AddModule(AModule: TTappyModule);
begin
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

  imgLevel2.LoadFromFile(lDir + 'images'+PathDelim+'levels'+PathDelim+'level2.jpg');
  imgLevel3.LoadFromFile(lDir + 'images'+PathDelim+'levels'+PathDelim+'level3.jpg');
end;

function TTappyModule.GetBackgroundImage(ALevel: Integer): TJPEGImage;
begin
  Result := imgLevel3;
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
  gTappyModules := TFPList.Create;
finalization
  gTappyModules.Free;
end.

