unit mod_tappywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tappymodules;

type

  { TTappyWords }

  TTappyWords = class(TTappyModule)
  public
    constructor Create; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
  end;

implementation

{ TTappyWords }

constructor TTappyWords.Create;
begin
  inherited Create;
end;

procedure TTappyWords.TranslateTextsToEnglish;
begin
  ShortDescription := 'TappyWords - A game to learn typing and ortography';
end;

procedure TTappyWords.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyWords - Um jogo para aprender a digitar e ortografia';
end;

initialization
  AddModule(TTappyWords.Create);
end.

