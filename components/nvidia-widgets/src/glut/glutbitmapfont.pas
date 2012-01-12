unit GLUTBitmapFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GL, GLut, nvBaseFont;

type
  TFontStyles = record
    Name: string;
    Bold: boolean;
    Italic: boolean;
    StrikeTrough: boolean;
    Underline: boolean;
    Font: pointer;
    TextListBase: integer;
  end;

  { TGLUTBitmapFont }

  TGLUTBitmapFont = class(TNVBaseFont)
  private
    FFontList: array of TFontStyles;
    FCount: integer;
    FActiveFont: integer;
    procedure FindStylizedFont;
    function InitializeFont(AFont: pointer): integer;
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

{ TGLUTBitmapFont }

procedure TGLUTBitmapFont.FindStylizedFont;
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

function TGLUTBitmapFont.InitializeFont(AFont: pointer): integer;
var
  TextListBase: integer;
  i: integer;
begin
  //just doing 7-bit ascii
  TextListBase := glGenLists(128);

  for i := 0 to 127 do
  begin
    glNewList(TextListBase + i, GL_COMPILE);
    glutBitmapCharacter(AFont, i);
    glEndList;
  end;

  Result := TextListBase;
end;

procedure TGLUTBitmapFont.SetFlags(AIndex: integer; AValue: boolean);
begin
  inherited SetFlags(AIndex, AValue);

  FindStylizedFont;
end;

constructor TGLUTBitmapFont.Create(AName: string; ASize: integer);
begin
  inherited Create(AName, ASize);

  FSize := ASize;
  Add(AName, False, False, False, False);
  FActiveFont := 0;
end;

destructor TGLUTBitmapFont.Destroy;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    glDeleteLists(FFontList[i].TextListBase, 128);

  inherited Destroy;
end;

procedure TGLUTBitmapFont.Add(AName: string; ABold, AItalic, AStrikeTrough, AUnderline: boolean);
begin
  Inc(FCount);
  SetLength(FFontList, FCount);

  with FFontList[FCount - 1] do
  begin
    case LowerCase(AName) of
      'helvetica': case Size of
                     10: Font := GLUT_BITMAP_HELVETICA_10;
                     12: Font := GLUT_BITMAP_HELVETICA_12;
                     18: Font := GLUT_BITMAP_HELVETICA_18;
                   else
                     raise Exception.CreateFmt('GLUT font size %d does not exist for font %s', [Size, AName]);
                   end;
      'times roman': case Size of
                       10: Font := GLUT_BITMAP_TIMES_ROMAN_10;
                       24: Font := GLUT_BITMAP_TIMES_ROMAN_24;
                     else
                       raise Exception.CreateFmt('GLUT font size %d does not exist for font %s', [Size, AName]);
                     end;
      'fixed': case Size of
                 13: Font := GLUT_BITMAP_8_BY_13;
                 15: Font := GLUT_BITMAP_9_BY_15;
               else
                 raise Exception.CreateFmt('GLUT font size %d does not exist for font %s', [Size, AName]);
               end;
    else
      raise Exception.CreateFmt('GLUT font name not supported %s', [AName]);
    end;

    Name := AName;
    Bold := ABold;
    Italic := AItalic;
    StrikeTrough := AStrikeTrough;
    Underline := AUnderline;
    TextListBase := InitializeFont(Font);
  end;

  FindStylizedFont;
end;

function TGLUTBitmapFont.TextHeight(Text: string): integer;
begin
  Result := Size;
end;

function TGLUTBitmapFont.TextWidth(Text: string): integer;
begin
  Result := glutBitmapLength(FFontList[FActiveFont].Font, PChar(Text));
end;

procedure TGLUTBitmapFont.TextOut(x, y: double; Text: string);
begin
  glListBase(FFontList[FActiveFont].TextListBase);
  glRasterPos2f(x, y);
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, @Text[1]);
end;

end.

