unit nvBaseFont;

{$mode objfpc}{$H+}

interface

uses
  fpimage;

type

  { TNVBaseFont }

  TNVBaseFont = class
  private
    FAngle: double;
    FFlags: word;
    FName: string;

    function GetFlags(AIndex: integer): boolean;
  protected
    FSize: integer;

    procedure SetAngle(AValue: double); virtual;
    procedure SetFlags(AIndex: integer; AValue: boolean); virtual;
  public
    constructor Create(AName: string; ASize: integer); virtual;

    //add stylized fonts
    procedure Add(AName: string; ABold, AItalic, AStrikeTrough, AUnderline: boolean); virtual; abstract;

    //font characteristics
    property Name: string read FName;
    property Size: integer read FSize;

    //text metrics
    function TextHeight(Text: string): integer; virtual; abstract;
    function TextWidth(Text: string): integer; virtual; abstract;
    procedure TextSize(Text: string; var w, h: integer); virtual;

    //font flags <<possibly not implemented for all font classes>>
    property Angle: double read FAngle write SetAngle;
    property Bold: boolean index 1 read GetFlags write SetFlags;
    property Italic: boolean index 2 read GetFlags write SetFlags;
    property StrikeTrough: boolean index 3 read GetFlags write SetFlags;
    property Underline: boolean index 4 read GetFlags write SetFlags;

    //printing function
    procedure TextOut(x, y: double; Text: string); virtual; abstract;
  end;


implementation

{ TNVBaseFont }

function TNVBaseFont.GetFlags(AIndex: integer): boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

procedure TNVBaseFont.SetFlags(AIndex: integer; AValue: boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

procedure TNVBaseFont.SetAngle(AValue: double);
begin
  if FAngle <> AValue then
    FAngle := AValue;
end;

constructor TNVBaseFont.Create(AName: string; ASize: integer);
begin
  FAngle := 0;
  FFlags:= 0;
  FName := AName;
  FSize := ASize;
end;

procedure TNVBaseFont.TextSize(Text: string; var w, h: integer);
begin
  w := TextWidth(Text);
  h := TextHeight(Text);
end;

end.

