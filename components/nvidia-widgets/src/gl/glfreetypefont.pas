unit GLFreeTypeFont;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, GLFreeType,
  nvBaseFont;

type
  TFontStyles = record
    Name: string;
    Bold: boolean;
    Italic: boolean;
    StrikeTrough: boolean;
    Underline: boolean;
    Font: TGLFreeType;
  end;

  { TGLFreeTypeFont }

  TGLFreeTypeFont = class(TNVBaseFont)
  private
    FFontList: array of TFontStyles;
    FCount: integer;
    FActiveFont: integer;
    procedure FindStylizedFont;
  protected
    procedure SetFlags(AIndex: integer; AValue: boolean); override;
  public
    constructor Create(AName: string; ASize: integer); override;
    destructor Destroy; override;

    //add stylized fonts
    procedure Add(AName: string; ABold, AItalic, AStrikeTrough, AUnderline: boolean); override;

    //text metrics
    function TextHeight(Text: string): integer; override;
    function TextWidth(Text: string): integer; override;

    //printing function
    procedure TextOut(x, y: double; Text: string); override;
  end;

implementation

{ TGLFreeTypeFont }

procedure TGLFreeTypeFont.FindStylizedFont;
var
  item: TFontStyles;
  i: integer;
begin
  //if more fonts defined then find stylized font
  if FCount > 1 then
    for i := 0 to FCount - 1 do
    begin
      item := FFontList[i];

      if (item.Bold = Bold) and
        (item.Italic = Italic) and
        (item.StrikeTrough = StrikeTrough) and
        (item.Underline = Underline) then
      begin
        FActiveFont := i;
        exit;
      end;
    end;

  //no font found, select default one
  FActiveFont := 0;
end;

procedure TGLFreeTypeFont.SetFlags(AIndex: integer; AValue: boolean);
begin
  inherited SetFlags(AIndex, AValue);

  FindStylizedFont;
end;

constructor TGLFreeTypeFont.Create(AName: string; ASize: integer);
begin
  inherited Create(AName, ASize);

  FCount := 0;
  FSize := ASize;
  Add(AName, False, False, False, False);
end;

destructor TGLFreeTypeFont.Destroy;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    FFontList[i].Font.Clean;

  inherited Destroy;
end;

procedure TGLFreeTypeFont.Add(AName: string; ABold, AItalic, AStrikeTrough, AUnderline: boolean);
begin
  Inc(FCount);
  SetLength(FFontList, FCount);

  with FFontList[FCount - 1] do
  begin
    Font.Init(AName, Size);
    Name := AName;
    Bold := ABold;
    Italic := AItalic;
    StrikeTrough := AStrikeTrough;
    Underline := AUnderline;
  end;

  FindStylizedFont;
end;

function TGLFreeTypeFont.TextHeight(Text: string): integer;
begin
  Result := Size;
end;

function TGLFreeTypeFont.TextWidth(Text: string): integer;
begin
  //only one font available or style not found then show default
  Result := FFontList[FActiveFont].Font.TextWidth(Text);
end;

procedure TGLFreeTypeFont.TextOut(x, y: double; Text: string);
begin
  FFontList[FActiveFont].Font.Print(x, y, Text);
end;

end.

