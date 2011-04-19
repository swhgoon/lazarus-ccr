{
 /***************************************************************************
                              ColorPalette.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    Color palette grid with custom palette support.
    The OnColorPick event is fired when user picks a color.
    The LoadPalette procedure loads custom palette.
    Custom palette example:
    
    $COLS 8
    # sets count of palette grid columns

    0,0,0
    # inserts color r,g,b
    255,255,255

    $NONE
    # inserts empty palette grid cell

    $BLENDWB 128,128,128 3
    # creates color gradient white -> color -> black with specified steps

}
unit ColorPalette;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Controls, Forms, Graphics, Math,
  LCLType;
  
type

  TColorMouseEvent = procedure (Sender: TObject; AColor: TColor; Shift: TShiftState) of Object;

  { TCustomColorPalette }

  TCustomColorPalette = class(TGraphicControl)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FCols: Integer;
    FOnColorMouseMove: TColorMouseEvent;
    FOnColorPick: TColorMouseEvent;
    FRows: Integer;
    FColors: TList;
    MX, MY: integer;
    function GetColors(Index: Integer): TColor;
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetColors(Index: Integer; const AValue: TColor);
    procedure UpdateSize;
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseMove(Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure ColorPick(AColor: TColor; Shift: TShiftState); dynamic;
    procedure ColorMouseMove(AColor: TColor; Shift: TShiftState); dynamic;
  public
    PickedColor: TColor;
    PickShift: TShiftState;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  public
    procedure LoadPalette(const FileName: String);
  
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property Colors[Index: Integer]: TColor read GetColors write SetColors;
    
    property OnColorPick: TColorMouseEvent read FOnColorPick write FOnColorPick;
    property OnColorMouseMove: TColorMouseEvent read FOnColorMouseMove write FOnColorMouseMove;
    
    property Height stored False;
    property Width stored False;
  end;
  
  TColorPalette = class(TCustomColorPalette)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    
    property OnChangeBounds;
    property OnClick;
    property OnColorMouseMove;
    property OnColorPick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;
  
  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TColorPalette]);
end;

{ TCustomColorPalette }

procedure TCustomColorPalette.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  FButtonHeight := AValue;
  if FButtonHeight < 1 then FButtonHeight := 1;
  UpdateSize;
end;

function TCustomColorPalette.GetColors(Index: Integer): TColor;
begin
  Result := TColor(FColors.Items[Index]);
end;

procedure TCustomColorPalette.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  FButtonWidth := AValue;
  if FButtonWidth < 1 then FButtonWidth := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetColors(Index: Integer; const AValue: TColor);
begin
  FColors.Items[Index] := Pointer(AValue);
end;

procedure TCustomColorPalette.UpdateSize;
begin
  if (FCols = 0) or (FColors.Count = 0) then FRows := 0
  else
    FRows := Ceil(FColors.Count / FCols);

  SetBounds(Left, Top, FCols * FButtonWidth + 1, FRows * FButtonHeight + 1)
end;

procedure TCustomColorPalette.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  MX := X;
  MY := Y;

  X := X div FButtonWidth;
  Y := Y div FButtonHeight;

  if X + Y * FCols < 0 then
    Exit;

  if X + Y * FCols < FColors.Count then
  begin
    PickedColor := TColor(FColors.Items[X + Y * FCols]);
    PickShift := Shift;
  end;
end;

procedure TCustomColorPalette.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (PickedColor <> clNone) and (MX = X) and (MY = Y) then
    ColorPick(PickedColor, PickShift);
  inherited;
end;

procedure TCustomColorPalette.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TColor;
begin
  inherited;
  
  X := X div FButtonWidth;
  Y := Y div FButtonHeight;

  if X + Y * FCols < 0 then
    Exit;
  if X + Y * FCols < FColors.Count then
  begin
    C := TColor(FColors.Items[X + Y * FCols]);
    if C <> clNone then ColorMouseMove(C, Shift);
  end;
end;

procedure TCustomColorPalette.ColorPick(AColor: TColor; Shift: TShiftState);
begin
  if Assigned(FOnColorPick) then FOnColorPick(Self, AColor, Shift);
end;

procedure TCustomColorPalette.ColorMouseMove(AColor: TColor; Shift: TShiftState);
begin
  if Assigned(FOnColorMouseMove) then FOnColorMouseMove(Self, AColor, Shift);
end;

constructor TCustomColorPalette.Create(TheOwner: TComponent);
begin
  inherited;
  
  FColors := TList.Create;
  FButtonWidth := 12;
  FButtonHeight := 12;
  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];
  
  FCols := 8;
  
  FColors.Add(Pointer(clBlack));
  FColors.Add(Pointer(clGray));
  FColors.Add(Pointer(clMaroon));
  FColors.Add(Pointer(clOlive));
  FColors.Add(Pointer(clGreen));
  FColors.Add(Pointer(clTeal));
  FColors.Add(Pointer(clNavy));
  FColors.Add(Pointer(clPurple));
  
  FColors.Add(Pointer(clWhite));
  FColors.Add(Pointer(clSilver));
  FColors.Add(Pointer(clRed));
  FColors.Add(Pointer(clYellow));
  FColors.Add(Pointer(clLime));
  FColors.Add(Pointer(clAqua));
  FColors.Add(Pointer(clBlue));
  FColors.Add(Pointer(clFuchsia));
  
  UpdateSize;
end;

destructor TCustomColorPalette.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TCustomColorPalette.Paint;
var
  I, X, Y: Integer;
begin
  Canvas.Pen.Color := clBlack;
  for I := 0 to Pred(FColors.Count) do
  begin
    Y := I div FCols;
    X := I mod FCols;
    if TColor(FColors.Items[I]) <> clNone then
    begin
      Canvas.Brush.Color := TColor(FColors.Items[I]);
      Canvas.Rectangle(Bounds(X * FButtonWidth, Y * FButtonHeight, FButtonWidth,
        FButtonHeight));
    end;
  end;
end;

procedure TCustomColorPalette.LoadPalette(const FileName: String);
var
  F: TextFile;
  Line: String;
  C: TColor;
  
  function ParseColor(var S: String): TColor;
  var
    R, G, B: Integer;
    I: Integer;
  begin
    R := StrToIntDef(Copy(S, 1, Pos(',', S) - 1), 0);
    Delete(S, 1, Pos(',', S));
    G := StrToIntDef(Copy(S, 1, Pos(',', S) - 1), 0);
    Delete(S, 1, Pos(',', S));
    
    S := TrimLeft(S);
    I := 1;
    while (I <= Length(S)) and (S[I] in ['0'..'9']) do Inc(I);
    B := StrToIntDef(Copy(S, 1, Pred(I)), 0);
    Delete(S, 1, Pred(I));

    Result := RGBToColor(Max(0, Min(R, 255)), Max(0, Min(G, 255)), Max(0, Min(B, 255)));
  end;
  
  procedure BlendWBColor(Color: TColor; Steps: Integer);
  var
    I: Integer;
    R, G, B, NR, NG, NB: Byte;
  begin
    RedGreenBlue(Color, R, G, B);
    
    for I := 1 to Steps do
    begin
      NR := Round((R * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      NG := Round((G * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      NB := Round((B * I + 255 * (Steps + 1 - I)) / (Steps + 1));
      FColors.Add(Pointer(RGBToColor(NR, NG, NB)));
    end;
    
    FColors.Add(Pointer(Color));
    
    for I := Steps downto 1 do
    begin
      NR := Round(R * I / (Steps + 1));
      NG := Round(G * I / (Steps + 1));
      NB := Round(B * I / (Steps + 1));
      FColors.Add(Pointer(RGBToColor(NR, NG, NB)));
    end;
  end;
  
begin
  AssignFile(F, FileName);
  try
    Reset(F);

    FColors.Clear;
    FCols := 1;
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if Length(Line) < 2 then Continue;
      if Line[1] = '#' then Continue;
      if Line[1] = '$' then
      begin
        if Copy(Line, 2, 4) = 'NONE' then FColors.Add(Pointer(clNone));
        if Copy(Line, 2, 4) = 'COLS' then FCols := StrToIntDef(Copy(Line, 6, MaxInt), 8);
        if Copy(Line, 2, 7) = 'BLENDWB' then
        begin
          Delete(Line, 1, 8);
          C := ParseColor(Line);
          BlendWBColor(C, StrToInt(Line));
        end;
      end
      else
        if Pos(',', Line) > 0 then FColors.Add(Pointer(ParseColor(Line)));
    end;
  finally
    Close(F);
  end;
  
  UpdateSize;
end;

initialization
{$I colorpalette.lrs}

end.

