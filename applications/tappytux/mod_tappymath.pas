unit mod_tappymath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tappymodules;

type

  { TTappyMath }

  TTappyMath = class(TTappyModule)
  public
    constructor Create; override;
    procedure TranslateTextsToEnglish; override;
    procedure TranslateTextsToPortuguese; override;
  end;

implementation

{ TTappyWords }

constructor TTappyMath.Create;
begin
  inherited Create;
end;

procedure TTappyMath.TranslateTextsToEnglish;
begin
  ShortDescription := 'TappyMath - A game to learn arithmetics';
end;

procedure TTappyMath.TranslateTextsToPortuguese;
begin
  ShortDescription := 'TappyMath - Um jogo para aprender aritmética';
end;

initialization
  AddModule(TTappyMath.Create);
end.

