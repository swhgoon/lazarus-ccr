{
@name GradButton
@author Eugen Bolz
@lastchange 23.01.2011
@version 1.5
@thx to http://www.delphipraxis.net/topic67805_farbverlauf+berechnen.html
@license http://creativecommons.org/licenses/LGPL/2.1/
@wiki http://wiki.lazarus.freepascal.org/TGradButton
}

unit ugradbtn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics, LCLType,LResources,
  LCLIntf ,Buttons, urotatebitmap, types, Menus, gradcustomcontrol;

type
  TGradButton = class;
  
  TDropDownMarkDirection = (mdUp, mdLeft, mdDown, mdRight);
  TDropDownMarkPosition = (mpLeft, mpRight);
  TTextAlignment = (taLeftJustify, taRightJustify, taCenter);
  TBorderSide = (bsTopLine, bsBottomLine, bsLeftLine, bsRightLine);
  TBorderSides = set of TBorderSide;
  TGradientType = (gtHorizontal,gtVertical);

  TGBBackgroundPaintEvent = procedure(Sender: TGradButton;
    TargetCanvas: TCanvas; R: TRect; BState : TButtonState) of object;

  { TStateBitmap }

  TStateBitmap = class
  private
    FBitmaps : Array of TBitmap;
    function StateToInt(AState: TButtonState): integer;
    function GetBitmap(State: TButtonState): TBitmap;                
  public
    constructor Create;
    destructor Destroy; override;
    
  public
    property Bitmap[State: TButtonState] : TBitmap read GetBitmap; default;
  end;
  
  { TDropDownSettings }

  TDropDownSettings = class(TPersistent)
  private
    FColor: TColor;
    FMarkDirection: TDropDownMarkDirection;
    FMarkPosition: TDropDownMarkPosition;
    FOnlyOnMark: boolean;
    FPopupMenu: TPopupMenu;
    FPressedColor: TColor;
    FShow: Boolean;
    FSize: integer;
    FNotify: TNotifyEvent; 
    FSplitButton: Boolean;
    procedure SetColor(const AValue: TColor);
    procedure SetMarkDirection(const AValue: TDropDownMarkDirection);
    procedure SetMarkPosition(const AValue: TDropDownMarkPosition);
    procedure SetOnlyOnMark(const AValue: boolean);
    procedure SetPopupMenu(const AValue: TPopupMenu); 
    procedure SetPressedColor(const AValue: TColor);
    procedure SetShow(const AValue: Boolean);
    procedure SetSize(const AValue: integer); 
    
    procedure Notify;
    procedure SetSplitButton(const AValue: Boolean); 
  public
    constructor Create(ANotify: TNotifyEvent);
    procedure AssignTo(Dest: TPersistent); override;
    function IsPopupStored: boolean; 
  published
    property Color : TColor read FColor write SetColor default clSilver;
    property MarkDirection : TDropDownMarkDirection read FMarkDirection 
      write SetMarkDirection default mdDown;
    property MarkPosition : TDropDownMarkPosition read FMarkPosition 
      write SetMarkPosition default mpRight;
    property OnlyOnMark: boolean read FOnlyOnMark write SetOnlyOnMark;
    property PopupMenu : TPopupMenu read FPopupMenu write SetPopupMenu;
    property PressedColor: TColor read FPressedColor write SetPressedColor default clBlack;
    property Show : Boolean read FShow write SetShow;
    property Size: integer read FSize write SetSize default 8;  
    property SplitButton: Boolean read FSplitButton write SetSplitButton;
  end;
  
  { TGradButton }

  TGradButton = class(TGradCustomControl)
  private
    FDropDownSettings: TDropDownSettings;
    FAutoHeight: Boolean;
    FAutoHeightBorderSpacing: Integer;
    FAutoWidthBorderSpacing: Integer;
    FOnBorderBackgroundPaint: TGBBackgroundPaintEvent;
    FRotateDirection : TRotateDirection;
    FTextAlignment : TTextAlignment;
    FButtonLayout: TButtonLayout;
    FDropdownMarkRect, FDropdownMarkAreaRect: TRect;
    FTextPoint, FGlyphPoint: TPoint;
    FTextSize, FGlyphSize, FAutoSize : TSize;
    FBackground, bm : TBitmap;
    FBackgroundCaches, FBackgroundSplitCaches : TStateBitmap;
    FRotatedGlyph : TRotatedGlyph;
    FTextGlyphSpacing: Integer;
    FGradientType : TGradientType;
    FShowFocusBorder, FOnlyBackground,
    FAutoWidth, FShowGlyph, FEnabled, FFocused : Boolean;
    FBorderSides: TBorderSides;
    FOnNormalBackgroundPaint, FOnHotBackgroundPaint,
    FOnDownBackgroundPaint, FOnDisabledBackgroundPaint : TGBBackgroundPaintEvent;
    procedure DrawDropDownArrow;
    procedure PaintGradient(TrgCanvas: TCanvas; pr : TRect; AState: TButtonState);
    procedure SetDropDownSettings(const AValue: TDropDownSettings);
    procedure UpdateBackground;
    procedure PaintBackground(AState: TButtonState; Normal: Boolean = true);
    procedure ShowDropdownPopupMenu;
    procedure DropDownSettingsChanged(Sender: TObject);
  protected
    FState, FOldState, FDropDownState: TButtonState;
    FNormalBlend,FOverBlend : Extended;
    FBaseColor, FNormalBlendColor, FOverBlendColor, FDisabledColor,
    FBackgroundColor, FGlyphBackgroundColor, FClickColor: TColor;
    FOwnerBackgroundDraw : Boolean;
    function DropDownEnabled : Boolean;
    procedure SetAutoHeight(const AValue: Boolean); virtual;
    procedure SetAutoHeightBorderSpacing(const AValue: Integer); virtual;
    procedure SetAutoWidthBorderSpacing(const AValue: Integer); virtual;
    procedure InvPaint(StateCheck:Boolean=false);
    procedure FontChanged(Sender: TObject); override;
    procedure GlyphChanged(Sender: TObject); virtual;
    function GetDropDownAreaSize : Integer;
    procedure GetFocusContentRect(var TheRect: TRect; OnlyForFocus : Boolean);
    procedure GetContentRect(var TheRect: TRect); virtual;
    procedure GetContentRect(var TheRect: TRect; Sides: TBorderSides); overload; virtual;
    procedure GetContentRect(var TheRect: TRect;  Sides: TBorderSides; 
      AWidth: Integer; AHeight: Integer); overload; virtual;
    function GetGlyph : TBitmap;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetAutoWidth(const Value : Boolean); virtual;
    procedure SetNormalBlend(const Value: Extended); virtual;
    procedure SetOverBlend(const Value: Extended); virtual;
    procedure SetBaseColor(const Value: TColor);  virtual;
    procedure SetNormalBlendColor(const Value: TColor); virtual;
    procedure SetOverBlendColor(const Value: TColor); virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetBorderSides(const Value: TBorderSides); virtual;
    procedure SetOwnerBackgroundDraw(const Value: Boolean); virtual;
    procedure SetGradientType(const Value: TGradientType); virtual;
    procedure SetRotateDirection(const Value: TRotateDirection); virtual;
    procedure SetShowGlyph(const Value: Boolean); virtual;
    procedure SetGlyphBackgroundColor(const Value: TColor); virtual;
    procedure SetTextAlignment(const Value: TTextAlignment); virtual;
    procedure SetTextGlyphSpacing(const Value: Integer); virtual;
    procedure SetButtonLayout(const Value: TButtonLayout); virtual;
    procedure SetClickColor(const Value: TColor); virtual;
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetShowFocusBorder(const Value: Boolean); virtual;
    procedure SetGlyph(const Value: TBitmap); virtual;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; 
       KeepBase: boolean); override;
    
    procedure Paint; override;
    procedure _Paint(ACanvas: TCanvas); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton;
           Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
           Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
           X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function  GetBackground : TCanvas;
    procedure Click; override;
    function Focused: Boolean; override;
    procedure UpdateButton;
    procedure UpdatePositions;
    function GetAutoWidth : Integer;
    function GetAutoHeight : Integer;
    class function GetControlClassDefaultSize: TSize; override;
  published
    property Action;
    property Anchors;
    property Align;
    property BorderSpacing;
    property Caption;
    property Enabled;
    property DropDownSettings: TDropDownSettings read FDropDownSettings 
      write SetDropDownSettings;
    property PopupMenu;
    property Font;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
    property DragMode;
    property DragKind;
    property DragCursor;
    property TabOrder;
    property TabStop;
    property NormalBlend : Extended read FNormalBlend write SetNormalBlend;
    property OverBlend : Extended read FOverBlend write SetOverBlend;
    property BaseColor: TColor read FBaseColor write SetBaseColor;
    property Color: TColor read FBaseColor write SetBaseColor;
    property NormalBlendColor: TColor read FNormalBlendColor write SetNormalBlendColor;
    property OverBlendColor: TColor read FOverBlendColor write SetOverBlendColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property AutoWidth : Boolean read FAutoWidth write SetAutoWidth default false;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default false;
    property BorderSides : TBorderSides read FBorderSides write SetBorderSides default [bsTopLine,bsBottomLine,bsLeftLine,bsRightLine];
    property GradientType : TGradientType read FGradientType write SetGradientType default gtHorizontal;
    property ShowFocusBorder : Boolean read FShowFocusBorder write SetShowFocusBorder;
    property RotateDirection : TRotateDirection read FRotateDirection write SetRotateDirection default rdNormal;
    property ButtonLayout : TButtonLayout read FButtonLayout write SetButtonLayout default blGlyphLeft;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property ShowGlyph : Boolean read FShowGlyph write SetShowGlyph;
    property GlyphBackgroundColor : TColor read FGlyphBackgroundColor write SetGlyphBackgroundColor;
    property TextAlignment : TTextAlignment read FTextAlignment write SetTextAlignment default taCenter;
    property TextGlyphSpacing : Integer read FTextGlyphSpacing write SetTextGlyphSpacing default 2;
    property ClickColor : TColor read FClickColor write SetClickColor;
    property DisabledColor : TColor read FDisabledColor write SetDisabledColor default clGray;
    property OwnerBackgroundDraw : Boolean read FOwnerBackgroundDraw write SetOwnerBackgroundDraw;
    property AutoWidthBorderSpacing : Integer read FAutoWidthBorderSpacing write SetAutoWidthBorderSpacing;
    property AutoHeightBorderSpacing : Integer read FAutoHeightBorderSpacing write SetAutoHeightBorderSpacing;

    property OnNormalBackgroundPaint : TGBBackgroundPaintEvent read FOnNormalBackgroundPaint write FOnNormalBackgroundPaint;
    property OnHotBackgroundPaint : TGBBackgroundPaintEvent read FOnHotBackgroundPaint write FOnHotBackgroundPaint;
    property OnDownBackgroundPaint : TGBBackgroundPaintEvent read FOnDownBackgroundPaint write FOnDownBackgroundPaint;
    property OnDisabledBackgroundPaint : TGBBackgroundPaintEvent read FOnDisabledBackgroundPaint write FOnDisabledBackgroundPaint;
    property OnBorderBackgroundPaint : TGBBackgroundPaintEvent read FOnBorderBackgroundPaint write FOnBorderBackgroundPaint;
  end;

  function ColorBetween(C1, C2 : TColor; blend:Extended):TColor;
  function ColorsBetween(colors:array of TColor; blend:Extended):TColor;
  function AlignItem(ItemLength, AreaLength,Spacing: Integer; ATextAlignment: TTextAlignment):Integer;
  function IfThen(ATest, ValA: Boolean; ValB: Boolean = false): Boolean; overload;
  function IfThen(ATest: Boolean; ValA, ValB: TRect): TRect; overload;
  
  procedure Register;

implementation

uses
    LCLProc, math;

procedure PaintArrow(ACanvas: TCanvas; ARect: TRect; ADirection: TDropDownMarkDirection; AColor: TColor);
var
  Points : Array of TPoint;
  ASize: TSize; 
  i: Integer; 
begin
  SetLength(Points, 3);
  
  ASize := Size(ARect);
  ASize.cx:= ASize.cx - 1;
  ASize.cy:= ASize.cy - 1;
  
  case ADirection of
    mdUp:
    begin
      Points[0] := Point(0, ASize.cy);
      Points[1] := Point(ASize.cx, ASize.cy);
      Points[2] := Point(ASize.cx div 2, 0);
    end;
    mdDown:
    begin
      Points[0] := Point(0, 0);
      Points[1] := Point(ASize.cx, 0);
      Points[2] := Point(ASize.cx div 2, ASize.cy); 
    end;
    mdLeft:
    begin
      Points[0] := Point(ASize.cx, 0);
      Points[1] := Point(ASize.cx, ASize.cy);
      Points[2] := Point(0, ASize.cy div 2); 
    end;
    mdRight:
    begin
      Points[0] := Point(0, 0);
      Points[1] := Point(0, ASize.cy);
      Points[2] := Point(ASize.cx, ASize.cy div 2); 
    end;
  end;
  
  for i := 0 to 2 do
  with Points[i] do
  begin
    Inc(X, ARect.Left);
    Inc(Y, ARect.Top);
  end;
  
  ACanvas.Brush.Style:=bsSolid;
  ACanvas.Brush.Color:=AColor;
  ACanvas.Pen.Color:=AColor;
  
  ACanvas.Polygon(Points);

  SetLength(Points, 0);
end;

// Debug PaintRect
procedure PaintRect(ACanvas: TCanvas; ARect: TRect);
begin
  with ACanvas do
  begin
    with ARect do
    begin
      Rectangle(Left, Top, Right, Bottom);

      MoveTo(Left,Top);
      LineTo(Right, Bottom);
  
      MoveTo(Right, Top);
      LineTo(Left, Bottom);    
    end; 
  end;
end;      

function AlignItem(ItemLength, AreaLength,Spacing: Integer; ATextAlignment: TTextAlignment):Integer;
begin
  case ATextAlignment of
    taLeftJustify : Result := Spacing;
    taRightJustify: begin
      Result := AreaLength-ItemLength-Spacing;
    end;
    taCenter      : begin
      Result := (AreaLength div 2)-(ItemLength div 2);
    end;
  end;
end;

procedure TGradButton.SetShowFocusBorder(const Value: Boolean);
begin
  FShowFocusBorder:=Value;
  
  InvPaint;
end;


procedure TGradButton.SetGlyph(const Value: TBitmap);
begin
  FRotatedGlyph.Bitmap := Value;
end;

procedure TGradButton.TextChanged;
begin
  inherited TextChanged;

  if FAutoWidth then
    UpdateButton
  else
    UpdatePositions;

  InvPaint;
end;
    
procedure TGradButton.SetName(const Value: TComponentName);
begin
  if (Caption='') OR (Caption=Name) then
  begin
    Caption:=Value;
  end;
    
  inherited;
end;

function TGradButton.Focused: Boolean;
begin
  FFocused:=FFocused OR (Inherited Focused);
  Result := FFocused;
end;

procedure TGradButton.SetAutoWidth(const Value : Boolean);
begin
  if FAutoWidth = Value then
    Exit;
  FAutoWidth := Value;
  
  UpdateButton;
end;

procedure TGradButton.UpdatePositions;
var
  tempTS,tempGS,Area : TSize;
  Offset1, Offset2 :Integer;
  tempBL : TButtonLayout;
  DropDownAreaSize: TSize;
  TheBackgroundRect: TRect;
begin
  GetFocusContentRect(TheBackgroundRect, false);

  Area := Size(TheBackgroundRect);
                              
  tempGS.cx:=0;
  tempGS.cy:=0;

  if FShowGlyph and not FRotatedGlyph.Empty then
  begin
    tempGS.cx:=FRotatedGlyph.Width;
    tempGS.cy:=FRotatedGlyph.Height;
  end;

  tempTS := FBuffer.Canvas.TextExtent(Caption);
  if FRotateDirection <> rdNormal then
  begin
    FTextSize.cx := tempTS.cy;
    FTextSize.cy := tempTS.cx;
    tempTS := FTextSize;
  end
  else
    FTextSize := tempTS;

  tempBL := FButtonLayout;

  if FShowGlyph and not FRotatedGlyph.Empty then begin
    case tempBL of
      blGlyphLeft:  begin
        FGlyphPoint.x := AlignItem(tempGS.cx+FTextGlyphSpacing+tempTS.cx,Area.cx,4,FTextAlignment);
        FGlyphPoint.y := AlignItem(tempGS.cy,Area.cy,0, taCenter);

        FTextPoint.x := FGlyphPoint.x+tempGS.cx+FTextGlyphSpacing;
        FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);
      end;
      blGlyphRight: begin
        //Glyph Right, Text Left
        FTextPoint.x := AlignItem(tempTS.cx+FTextGlyphSpacing+tempGS.cx,Area.cx,4, FTextAlignment);
        FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);

        FGlyphPoint.x := FTextPoint.x+tempTS.cx+FTextGlyphSpacing;
        FGlyphPoint.y := AlignItem(tempGS.cy,Area.cy,0, taCenter);
      end;
      blGlyphTop:  begin
        //Glyph Top, Text Bottom
        FGlyphPoint.x := AlignItem(tempGS.cx, Area.cx, 0, FTextAlignment);
        FTextPoint.x  := AlignItem(tempTS.cx, Area.cx, 0, FTextAlignment);

        FGlyphPoint.y := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, Area.cy, 4, taCenter);
        FTextPoint.y  := FGlyphPoint.y+tempGS.cy+FTextGlyphSpacing;
      end;
      blGlyphBottom: begin
        //Glyph Bottom, Text Top
        FGlyphPoint.x := AlignItem(tempGS.cx, Area.cx, 0, FTextAlignment);
        FTextPoint.x  := AlignItem(tempTS.cx, Area.cx, 0, FTextAlignment);

        FTextPoint.y  := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, Area.cy, 4, taCenter);
        FGlyphPoint.y := FTextPoint.y+tempTS.cy+FTextGlyphSpacing;
      end;
    end;
  end else begin
    FGlyphPoint.x := 0;
    FGlyphPoint.y := 0;

    FTextPoint.x := AlignItem(tempTS.cx,Area.cx,4, FTextAlignment);
    FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);
  end;
  
  FAutoSize.cx := Max(FGlyphPoint.x + FGlyphSize.cx, FTextPoint.x + FTextSize.cx);
  FAutoSize.cy := Max(FGlyphPoint.y + FGlyphSize.cy, FTextPoint.y + FTextSize.cy);
  
  FTextPoint.x  := TheBackgroundRect.Left + FTextPoint.x;
  FGlyphPoint.x := TheBackgroundRect.Left + FGlyphPoint.x;
    
  if DropDownEnabled then
  begin
    FDropdownMarkRect.Top    := AlignItem(FDropDownSettings.Size, Area.cy, 0, taCenter); 
        
    if not FDropDownSettings.SplitButton then
    begin                   
      Offset1 := IfThen(FDropDownSettings.MarkPosition<>mpLeft, 
        FTextSize.cx, -FDropDownSettings.Size - 2);
      Offset2 := IfThen(FDropDownSettings.MarkPosition<>mpLeft, 
        FGlyphSize.cx, -FDropDownSettings.Size - 2);
      
      FDropdownMarkRect.Left := IfThen(FDropDownSettings.MarkPosition=mpRight,
        Max(FTextPoint.X+Offset1, FGlyphPoint.X+Offset2),
        Min(FTextPoint.X+Offset1, FGlyphPoint.X+Offset2));   
    end
    else
    begin
      Offset1 := GetDropDownAreaSize;
      
      FDropdownMarkAreaRect.Top    := 0;
      FDropdownMarkAreaRect.Bottom := Height;
      FDropdownMarkAreaRect.Left   := Ifthen(FDropDownSettings.MarkPosition=mpRight, Width - Offset1);
      FDropdownMarkAreaRect.Right  := FDropdownMarkAreaRect.Left + Offset1;

      DropDownAreaSize := Size(FDropdownMarkAreaRect);
      
      FDropdownMarkRect.Left:=FDropdownMarkAreaRect.Left+
        AlignItem(FDropDownSettings.Size, DropDownAreaSize.cx, 0, taCenter);
    end;
    
    FAutoSize.cy := Max(FAutoSize.cy, FDropdownMarkRect.Bottom);
    FAutoSize.cx := Max(FAutoSize.cx, FDropdownMarkRect.Right);
    
    if FDropDownSettings.SplitButton then
    begin
      DropDownAreaSize := Size(FDropdownMarkAreaRect);
      FAutoSize.cx := Max(Area.cx, FAutoSize.cx)+DropDownAreaSize.cx;  
    end;
    
    FDropdownMarkRect.Right  := FDropdownMarkRect.Left + FDropDownSettings.Size;
    FDropdownMarkRect.Bottom := FDropdownMarkRect.Top + FDropDownSettings.Size;               
  end;
  FGlyphSize:=tempGS;
end;

function TGradButton.GetAutoWidth: Integer;
begin
  Result := FAutoSize.cx + FAutoWidthBorderSpacing;
end;

function TGradButton.GetAutoHeight: Integer;
begin
  Result := FAutoSize.cy + FAutoHeightBorderSpacing;
end;

class function TGradButton.GetControlClassDefaultSize: TSize;  
begin            
  Result.CX := 80;
  Result.CY := 25;   
end;

procedure TGradButton.PaintBackground(AState: TButtonState; Normal: Boolean);
var
  FOnTemp : TGBBackgroundPaintEvent;
  TrgBitmap: TBitmap;
  TempSides: TBorderSides;
  TempRect: TRect; 
begin                     
  TempSides:= BorderSides;

  if not FEnabled then AState:=bsDisabled;
  
  if Normal then
    TrgBitmap := FBackgroundCaches[AState]
  else
    TrgBitmap := FBackgroundSplitCaches[AState];                              
  
  with TrgBitmap do
  begin
    Canvas.Font.Color := Self.Font.Color;
    Canvas.Font := Self.Font;
    
    if Normal then
    begin
      Width := Self.Width;
      Height:= Self.Height;
    end
    else
    begin
      if not FDropDownSettings.SplitButton then Exit;
      
      Width := GetDropDownAreaSize;
      Height:= Self.Height;
      
      if AState = bsUp then
        if FDropDownSettings.MarkPosition = mpLeft then
        begin
          TempSides := TempSides - [bsRightLine]
        end
        else
        begin
          TempSides := TempSides - [bsLeftLine];
        end;
    end; 
    
    GetContentRect(TempRect, TempSides, Width, Height); 
    
    TempRect.Left := TempRect.Left 
      - IfThen(not Normal and (FDropDownSettings.MarkPosition = mpRight), 1);
                                                                                                      

    if not Normal then
      WriteLn('Paint: ', DbgS(TempRect));
    
    if Self.Parent is TGradButton then
    begin
      Canvas.CopyRect(Rect(0, 0, Width, Height), (Self.Parent as TGradButton).GetBackground,
        Rect(Left,Top,Left+Width,Top+Height));
    end else begin
      Canvas.Brush.Color:=FBackgroundColor;

      Canvas.FillRect(0 , 0, Width, Height);
    end;

    if FOwnerBackgroundDraw AND (FOnBorderBackgroundPaint<>nil) then
    begin
      //FOnBorderBackgroundPaint(Self, Canvas, FBackgroundRect, AState);
    end else begin
      //Top
      if (bsTopLine in TempSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(TempRect.Left,0,TempRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},0);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(TempRect.Left,1,TempRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},1);
      end;

      //Left
      if (bsLeftLine in TempSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(0,TempRect.Top,0,TempRect.Bottom);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(1,TempRect.Top,1,TempRect.Bottom);
      end;

      //Right
      if (bsRightLine in TempSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(Width-1,TempRect.Top,Width-1,TempRect.Bottom);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(Width-2,TempRect.Top,Width-2,TempRect.Bottom);
      end;

      //Bottom
      if (bsBottomLine in TempSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(TempRect.Left,Height-1,TempRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},Height-1);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(TempRect.Left,Height-2,TempRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},Height-2);
      end;

      //TopLeft
      if (bsTopLine in TempSides) AND (bsLeftLine in TempSides) then
        Canvas.Pixels[1,1]:=clBlack;

      //TopRight
      if (bsTopLine in TempSides) AND (bsRightLine in TempSides) then
        Canvas.Pixels[Width-2,1] := clBlack;

      //BottomLeft
      if (bsBottomLine in TempSides) AND (bsLeftLine in TempSides) then
        Canvas.Pixels[1, Height-2]:=clBlack;

      //BottomRight
      if (bsBottomLine in TempSides) AND (bsRightLine in TempSides) then
        Canvas.Pixels[Width-2,Height-2]:=clBlack;
    end;

    {if FOwnerBackgroundDraw then
    begin
      if not FEnabled then
        FState := bsDisabled;

      case FState of
        bsUp: FOnTemp := FOnNormalBackgroundPaint;
        bsHot:FOnTemp := FOnHotBackgroundPaint;
        bsDown: FOnTemp := FOnDownBackgroundPaint;
        bsDisabled: FOnTemp := FOnDisabledBackgroundPaint;
      end;

      if FOnTemp <> nil then
      begin
        FOnTemp(Self, Canvas, FBackgroundRect, FState);
      end;
    end else begin}
    
    
       //PaintRect(Canvas, TempRect);
      PaintGradient(Canvas, TempRect, AState);
    //end;
  end;
end;

procedure TGradButton.ShowDropdownPopupMenu; 
var
  lowerLeft: TPoint;
begin
  if not DropDownEnabled then Exit;
  lowerLeft := Point(0, Height);
  lowerLeft := ControlToScreen(lowerLeft);
  FDropDownSettings.PopupMenu.Popup(lowerLeft.X, lowerLeft.Y);
end;

procedure TGradButton.DropDownSettingsChanged(Sender: TObject); 
begin
  UpdateButton;
  
  InvPaint();
end;

function TGradButton.DropDownEnabled: Boolean; 
begin
  Result := FDropDownSettings.Show and FDropDownSettings.IsPopupStored;
end;

procedure TGradButton.UpdateBackground;
var
  FTempState : TButtonState;
begin
  FTempState:= FState;

  FEnabled:=true;
  PaintBackground(bsUp);
  PaintBackground(bsUp, false);
  PaintBackground(bsHot);
  PaintBackground(bsHot, false);
  PaintBackground(bsDown);
  PaintBackground(bsDown, false);
  FEnabled:=false;      
  PaintBackground(bsUp);
  PaintBackground(bsUp, false);
  FEnabled:=Enabled;

  FState:=FTempState;

  InvPaint;
end;

function TGradButton.GetGlyph : TBitmap;
begin
    Result := FRotatedGlyph.Bitmap;
end;

procedure TGradButton.SetDisabledColor(const Value: TColor);
begin
   FDisabledColor:=Value;
   
   UpdateBackground;
   
   InvPaint;
end;

procedure TGradButton.SetClickColor(const Value: TColor);
begin
   FClickColor:=Value;
   
   UpdateBackground;
   
   InvPaint;
end;

procedure TGradButton.SetButtonLayout(const Value: TButtonLayout);
begin
    FButtonLayout:=Value;

    UpdatePositions;
    
    InvPaint;
end;

procedure TGradButton.SetGlyphBackgroundColor(const Value: TColor);
begin
   FGlyphBackgroundColor:=Value;
   //todo: see the desired behavior of GlyphBackgroundColor
   //FRotatedGlyph.TransparentColor:=Value;
   
   InvPaint;
end;

procedure TGradButton.SetTextAlignment(const Value: TTextAlignment);
begin
   FTextAlignment:=Value;
   
   UpdatePositions;
   
   InvPaint;
end;

procedure TGradButton.SetTextGlyphSpacing(const Value: Integer);
begin
   FTextGlyphSpacing:=Value;
   
   UpdatePositions;
   
   InvPaint;
end;
    
procedure TGradButton.SetEnabled(Value: Boolean);
begin
    Inherited;

    FEnabled:=Value;
    
    InvPaint;
end;

procedure TGradButton.UpdateButton;
begin
   UpdateBackground;
   UpdatePositions;
end;
    
procedure TGradButton.SetShowGlyph(const Value: Boolean);
begin
    if (FShowGlyph <> Value) AND FRotatedGlyph.IsBitmapStored then
    begin
      FShowGlyph:=Value;
    
      UpdatePositions;
    
      InvPaint;
    end;
end;
    
procedure TGradButton.SetRotateDirection(const Value: TRotateDirection);
begin
    if FRotateDirection = Value then
      Exit;
    FRotateDirection:=Value;

    //Rotate and Cache
    FRotatedGlyph.Direction:=FRotateDirection;
    
    UpdatePositions;

    UpdateButton;

    InvPaint;
end;

procedure TGradButton.SetBackgroundColor(const Value: TColor);
begin
   FBackgroundColor:=Value;
   
   UpdateBackground;
   
   InvPaint;
end;

procedure TGradButton.SetGradientType(const Value: TGradientType);
begin
   if FGradientType = Value then
     Exit;
   FGradientType:=Value;
   
   UpdateBackground;
   
   InvPaint;
end;

function TGradButton.GetBackground : TCanvas;
begin
    FOnlyBackground:=true;
    Paint;
    FOnlyBackground:=false;
    Result := FBackground.Canvas;
end;            

procedure TGradButton.DrawDropDownArrow;
var 
  ArrowColor: Integer; 
begin                       
  ArrowColor := FDropDownSettings.Color;
  
  if FDropDownState = bsDown then
    ArrowColor:=FDropDownSettings.PressedColor;
  
  PaintArrow(FBuffer.Canvas, FDropdownMarkRect, FDropDownSettings.FMarkDirection, 
    ArrowColor);
end; 

procedure TGradButton.PaintGradient(TrgCanvas: TCanvas; pr: TRect; 
  AState: TButtonState);
var
  r : Integer;
  t1,t2,t3 : TColor;
begin
  case AState of
    bsHot,bsDown : begin
      t3 := FOverBlendColor;
    end;
    else begin
      t3 := FNormalBlendColor;
    end;
   end;

  if AState = bsDown then begin
    t1 := FClickColor;
  end else if FEnabled then begin
    t1 := FBaseColor;
  end else begin
    t1 := FDisabledColor;
    t3 := FNormalBlendColor;
  end;

  t2 := ColorBetween(t1, t3, FNormalBlend);

  if GradientType = gtHorizontal then
  begin
    if AState = bsDown then
    begin
      for r := (pr.Bottom)-1 downto pr.Top do
      begin
        if (r > (pr.Bottom/2)) then
        begin
          TrgCanvas.Pen.Color:= ColorBetween(t1,t2,FOverBlend);
        end else begin
          TrgCanvas.Pen.Color := ColorsBetween([t1,t2], 1.0-(r / pr.Bottom));
        end;
        TrgCanvas.Line(pr.Left,r,pr.Right{$IFDEF DARWIN}+1{$ENDIF},r);
      end;
     end else
       for r := (pr.Bottom)-1 downto pr.Top do
       begin
         if (r <= (pr.Bottom/2)) then
         begin
           //WriteLn('R: ', r, ' M: ', Trunc(pr.Bottom/2));
           TrgCanvas.Pen.Color:= ColorBetween(t1,t2,FOverBlend);
         end else begin
           TrgCanvas.Pen.Color := ColorsBetween([t1,t2], r / pr.Bottom);
         end;
         TrgCanvas.Line(pr.Left,r,pr.Right{$IFDEF DARWIN}+1{$ENDIF},r);
       end;
   end else begin
   if AState = bsDown then
   begin
       for r := (pr.Right)-{$IFDEF DARWIN}0{$ELSE}1{$ENDIF} downto pr.Left do
       begin
          if (r >= (pr.Right/2)) then
          begin
             TrgCanvas.Pen.Color:= ColorBetween(t1,t2,FOverBlend);
          end else begin
             TrgCanvas.Pen.Color := ColorsBetween([t1,t2], 1.0-(r / pr.Right));
          end;
          TrgCanvas.Line(r,pr.Top,r,pr.Bottom);
       end;
   end else
       for r := (pr.Right)-{$IFDEF DARWIN}0{$ELSE}1{$ENDIF} downto pr.Left do
       begin
          if (r <= (pr.Right/2)) then
          begin
             TrgCanvas.Pen.Color:= ColorBetween(t1,t2,FOverBlend);
          end else begin
             TrgCanvas.Pen.Color := ColorsBetween([t1,t2], r / pr.Right);
          end;
          TrgCanvas.Line(r,pr.Top,r,pr.Bottom);
       end;
   end;
end;

procedure TGradButton.SetDropDownSettings(const AValue: TDropDownSettings); 
begin
  if FDropDownSettings=AValue then exit; 
  FDropDownSettings.Assign(AValue);
  
  FDropDownSettings.Notify;
end;
                          

procedure TGradButton.SetAutoHeight(const AValue: Boolean);
begin
  if FAutoHeight=AValue then exit;
  FAutoHeight:=AValue;

  UpdateButton;
end;

procedure TGradButton.SetAutoHeightBorderSpacing(const AValue: Integer);
begin
  if FAutoHeightBorderSpacing=AValue then exit;
  FAutoHeightBorderSpacing:=AValue;

  UpdateButton;
end;

procedure TGradButton.SetAutoWidthBorderSpacing(const AValue: Integer);
begin
  if FAutoWidthBorderSpacing=AValue then exit;
  FAutoWidthBorderSpacing:=AValue;

  UpdateButton;
end;

constructor TGradButton.Create(AOwner: TComponent);
begin
  inherited;
                            
  FDropDownSettings := TDropDownSettings.Create(@DropDownSettingsChanged);
  
  FBackgroundCaches := TStateBitmap.Create;
  FBackgroundSplitCaches := TStateBitmap.Create;
  
  FAutoWidthBorderSpacing:=15;
  FAutoHeightBorderSpacing:=15;
  FNormalBlend:=0.5;
  FOverBlend:=0.653;
  FBaseColor:=clBlue;
  FNormalBlendColor:=clWhite;
  FOverBlendColor:=clSilver;
  FBackgroundColor:=clBtnFace;
  FClickColor:=clBlue;
  FDisabledColor:=clGray;
  FEnabled:=true;
  FAutoWidth:=false;
  FAutoHeight:=false;
  FOwnerBackgroundDraw := false;
    
  FOnlyBackground:=false;
  FShowFocusBorder:=true;
  FRotateDirection:=rdNormal;
  FTextAlignment:=taCenter;
  FTextGlyphSpacing := 2;
  TabStop:=true;
    
  ControlStyle := ControlStyle + [csAcceptsControls];
    
  DoubleBuffered:=true;

  FBackground := TBitmap.Create;
  with FBackground do
  begin
    Width:=Self.Width;
    Height:=Self.Height;
  end;


  FRotatedGlyph := TRotatedGlyph.Create;
  FRotatedGlyph.OnChange := @GlyphChanged;
  FButtonLayout:=blGlyphLeft;
  FGlyphBackgroundColor:=clWhite;
                         
  FBorderSides:=[bsTopLine,bsBottomLine,bsLeftLine,bsRightLine];

  bm := TBitmap.Create;
  bm.Canvas.Brush.Style := bsClear;

  UpdateBackground;
    
  Font.Color:=clWhite;
end;

destructor TGradButton.Destroy;
begin
  FDropDownSettings.Free; 
  
  FBackgroundCaches.Free;
  FBackgroundSplitCaches.Free;
  
  //DebugLn('bm.Free');
  bm.Free;
  //DebugLn('FRotatedGlyph.Free');
  FRotatedGlyph.Free;
  //DebugLn('FBackground.Free');
  FBackground.Free;
  
  inherited;
end;

procedure TGradButton.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;  
  KeepBase: boolean);  
begin
  if FAutoWidth then
    AWidth := GetAutoWidth;
  
  if FAutoHeight then
    AHeight := GetAutoHeight;
                            
  if (HasParent) then
  begin
    if FAutoWidth or FAutoHeight then
      UpdateButton
    else begin
      UpdatePositions;
      UpdateBackground;
    end;
  end;         
  
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);  
end;

procedure TGradButton.Paint;  
begin
  _Paint(FBuffer.Canvas);
  
  Canvas.Draw(0,0, FBuffer);
end;

procedure TGradButton._Paint(ACanvas: TCanvas);  
var
  TextOffset : Integer;
  tempState: TButtonState;
  FocusRect: TRect; 
begin
  GetFocusContentRect(FocusRect, true);  
  
  FBackground.Width:=Width;
  FBackground.Height:=Height;

  ACanvas.Brush.Color:=clBlack;
  ACanvas.FillRect(0,0,Width, Height);

  if not FEnabled then
    tempState := bsDisabled
  else
    tempState := FState;
  
  ACanvas.Draw(0,0, FBackgroundCaches[tempState]);
                                                 
  if DropDownEnabled and FDropDownSettings.SplitButton then
  begin                                                              
    ACanvas.Brush.Style:=bsSolid;
    ACanvas.Brush.Color:=FBackgroundColor;

    ACanvas.FillRect(FDropdownMarkAreaRect); 
    ACanvas.Draw(FDropdownMarkAreaRect.Left, FDropdownMarkAreaRect.Top, FBackgroundSplitCaches[FDropDownState]);
  end else
    ACanvas.Brush.Style := bsClear;

  TextOffset := IfThen(tempState = bsDown, 1);
  
  DrawRotatedText(ACanvas, FTextPoint.x + TextOffset, FTextPoint.y + TextOffset,
    FTextSize.cx, FTextSize.cy, Caption, FRotateDirection);
  
  if FShowGlyph and FRotatedGlyph.IsBitmapStored then
  begin
    FRotatedGlyph.State := FState;
    FRotatedGlyph.Draw(ACanvas, FGlyphPoint.x+TextOffset, FGlyphPoint.y+TextOffset);
  end;
  
  if not (csDesigning in ComponentState) then
    if FFocused and FShowFocusBorder then
      ACanvas.DrawFocusRect(RECT(FocusRect.Left+2, FocusRect.Top+2,
        FocusRect.Right-2, FocusRect.Bottom-2));
  
  if DropDownEnabled then
    DrawDropDownArrow;
end;

procedure TGradButton.SetBorderSides(const Value: TBorderSides);
begin
    FBorderSides:=Value;

    UpdateBackground;
    UpdatePositions;
    
    InvPaint;
end;

procedure TGradButton.SetOwnerBackgroundDraw(const Value: Boolean);
begin
  FOwnerBackgroundDraw:=Value;

  if Value then
  begin
    UpdateBackground;
    InvPaint;
  end;
end;

procedure TGradButton.SetNormalBlend(const Value: Extended);
begin
  FNormalBlend:=Value;
    
  UpdateBackground;

  InvPaint;
end;

procedure TGradButton.SetOverBlend(const Value: Extended);
begin
  FOverBlend:=Value;

  UpdateBackground;

  InvPaint;
end;

procedure TGradButton.SetBaseColor(const Value: TColor);
begin
    if FBaseColor = FClickColor then FClickColor := Value;

    FBaseColor:=Value;

    UpdateBackground;
    
    InvPaint;
end;

procedure TGradButton.SetNormalBlendColor(const Value: TColor);
begin
    FNormalBlendColor:=Value;

    UpdateBackground;
    
    InvPaint;
end;

procedure TGradButton.SetOverBlendColor(const Value: TColor);
begin
    FOverBlendColor:=Value;

    UpdateBackground;
    
    InvPaint;
end;

procedure TGradButton.InvPaint(StateCheck:Boolean);
var
   doIt : Boolean;
begin
  doIt := true;
  
  if StateCheck then
  begin
    doIt := (FOldState<>FState);
  end;

  if doIt then
  begin
    FOldState:=FState;
    Invalidate;
  end;
end;

procedure TGradButton.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  FBuffer.Canvas.Font := Font;
  UpdatePositions;
end;

procedure TGradButton.GlyphChanged(Sender: TObject);
begin
  UpdatePositions;
  Invalidate;
end;

function TGradButton.GetDropDownAreaSize: Integer; 
begin
  Result := FAutoWidthBorderSpacing + FDropDownSettings.Size;
end;

procedure TGradButton.GetFocusContentRect(var TheRect: TRect; 
  OnlyForFocus: Boolean); 
var
  Offset1: LongInt;
  TempSides: TBorderSides; 
  Split: Boolean; 
begin
  GetContentRect(TheRect);
  Split := FDropDownSettings.SplitButton;
  
  if DropDownEnabled then begin
    Offset1 := IfThen(Split, GetDropDownAreaSize,
      IfThen(not OnlyForFocus, FDropDownSettings.Size + 2));
    
    if FDropDownSettings.MarkPosition = mpLeft then
    begin
      if Split then
      begin
        TempSides := BorderSides - [bsLeftLine];
        GetContentRect(TheRect, TempSides);
      end;
      
      TheRect.Left := TheRect.Left + Offset1
    end
    else begin
      if Split then
      begin
        TempSides := BorderSides - [bsRightLine];
        GetContentRect(TheRect, TempSides); 
      end;
      TheRect.Right := TheRect.Right - Offset1;
    end;
  end;
end;

procedure TGradButton.GetContentRect(var TheRect: TRect); 
begin
  GetContentRect(TheRect, BorderSides, Width, Height);
end;

procedure TGradButton.GetContentRect(var TheRect: TRect; Sides: TBorderSides); 
begin
  GetContentRect(TheRect, Sides, Width, Height);
end;

procedure TGradButton.GetContentRect(var TheRect: TRect; Sides: TBorderSides; 
  AWidth: Integer; AHeight: Integer);
begin
  TheRect := Rect(0, 0,AWidth, AHeight);

  //Top
  TheRect.Top := IfThen(bsTopLine in Sides, 2);

  //Left
  TheRect.Left := IfThen(bsLeftLine in Sides, 2);

  //Right
  if (bsRightLine in Sides) then
  begin
    TheRect.Right := TheRect.Right-{$IFDEF windows}2{$ELSE}3{$ENDIF};
  end;

  //Bottom
  if (bsBottomLine in Sides) then
  begin
    TheRect.Bottom := TheRect.Bottom - 2;
  end;
end;


procedure TGradButton.DoEnter;
begin
  //FState:=bsHot;
  FFocused:=true;
  InvPaint;
  
  inherited;
end;

procedure TGradButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_SPACE] then
     inherited Click;
  
  inherited;
end;

procedure TGradButton.DoExit;
begin
  FState:=bsUp;
  FDropDownState:=bsUp;
  FFocused:=false;
  
  InvPaint;
  
  inherited;
end;

procedure TGradButton.MouseEnter;
begin
  inherited;
  
  {if FState<>bsDown then
  begin
    if not FDropDownSettings.SplitButton then
      FState:=bsHot;
     
    InvPaint(true);
  end; }
end;

procedure TGradButton.MouseMove(Shift: TShiftState;
                 X, Y: Integer);
var
  TempPoint: TPoint;

  function ShiftToState: TButtonState;
  begin
    if ssLeft in Shift then
      Result := bsDown
    else
      Result := bsHot;
  end;

begin
  TempPoint := Point(X, Y);

  if FDropDownSettings.SplitButton then
  begin
    FDropDownState:= bsUp;
    FState:=bsUp;
   
    if PtInRect(FDropdownMarkAreaRect, TempPoint) then
      FDropDownState:= ShiftToState
    else
      if PtInRect(Rect(0,0,Width, Height), TempPoint) then
        FState:= ShiftToState;
  end
  else
  begin
    FState:=ShiftToState;                            
  end;

  InvPaint;

  //inherited MouseMove calls OnMouseMove
  inherited MouseMove(Shift, X, Y);
end;

procedure TGradButton.MouseLeave;
begin
    inherited;
    //WriteLn('MouseLeave');
    FDropDownState:= bsUp;
    FState:=bsUp;
    //FFocused:=false;
    InvPaint;
end;

procedure TGradButton.Click;
begin
    //inherited;
end;

procedure TGradButton.MouseDown(Button: TMouseButton;
                 Shift: TShiftState; X, Y: Integer);
var
  TempPoint : TPoint;
  TempRect: TRect; 
begin
  TempPoint:= Point(X,Y); 
  
  TempRect := IfThen(FDropDownSettings.SplitButton, FDropdownMarkAreaRect, FDropdownMarkRect);
  
  if PtInRect(TempRect, TempPoint) then
  begin
      FState := bsUp;
      FDropDownState := bsDown;
    
      InvPaint;
  end
  else
  begin
    if PtInRect(Rect(0,0,Width,Height),TempPoint) then
    begin
       FState:=bsDown;

       InvPaint;
    end else begin
       FState:=bsUp;
       FFocused:=false;

       if Assigned(PopupMenu) then
          PopupMenu.Close;
       
       InvPaint;
    end;
    
    FDropDownState:=bsUp;
  end;
  
    inherited;
  
end;

procedure TGradButton.MouseUp(Button: TMouseButton;
                 Shift: TShiftState; X, Y: Integer);
var
  TempPoint : TPoint;
  TempRect: TRect; 
begin
  TempPoint:= Point(X,Y);
  
  TempRect := IfThen(FDropDownSettings.SplitButton, FDropdownMarkAreaRect, FDropdownMarkRect);
  
  if PtInRect(TempRect, TempPoint) then
  begin
    if not FDropDownSettings.SplitButton then
      FState := bsHot;
    FDropDownState := bsHot;
    InvPaint;
    
    if Button = mbLeft then
      ShowDropdownPopupMenu;
  end
  else
  begin
    if PtInRect(Rect(0,0,Width,Height),TempPoint) then
    begin
      FState:=bsHot;
      InvPaint;
  
      if Button = mbLeft then
        inherited Click; //Faster, than the Overrided Click procedure
    end 
    else 
    begin
      FState := bsUp;
      FFocused:=false;
      InvPaint; 
    end;
  
    FDropDownState:=bsUp;
    InvPaint; 
    inherited;
  end;
end;

{ TDropDownSettings }

procedure TDropDownSettings.SetMarkPosition(const AValue: TDropDownMarkPosition); 
begin
  if FMarkPosition=AValue then exit; 
  FMarkPosition:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetMarkDirection(
  const AValue: TDropDownMarkDirection); 
begin
  if FMarkDirection=AValue then exit; 
  FMarkDirection:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetColor(const AValue: TColor); 
begin
  if FColor=AValue then exit; 
  FColor:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetOnlyOnMark(const AValue: boolean); 
begin
  if FOnlyOnMark=AValue then exit; 
  FOnlyOnMark:=AValue;
  
  Notify;
end;

procedure TDropDownSettings.SetPopupMenu(const AValue: TPopupMenu); 
begin
  if FPopupMenu=AValue then exit; 
  FPopupMenu:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetPressedColor(const AValue: TColor); 
begin
  if FPressedColor=AValue then exit; 
  FPressedColor:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetShow(const AValue: Boolean); 
begin
  if FShow=AValue then exit; 
  FShow:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.SetSize(const AValue: integer); 
begin
  if FSize=AValue then exit; 
  FSize:=AValue; 
  
  Notify;
end;

procedure TDropDownSettings.Notify; 
begin
  if FNotify <> nil then
    FNotify(Self);
end;

procedure TDropDownSettings.SetSplitButton(const AValue: Boolean); 
begin
  if FSplitButton=AValue then exit; 
  FSplitButton:=AValue; 
  
  Notify;
end;

constructor TDropDownSettings.Create(ANotify: TNotifyEvent); 
begin
  FNotify := ANotify;

  FColor:= clSilver;
  FPressedColor:= clBlack;
  FMarkDirection:= mdDown;
  FMarkPosition:= mpRight;
  FOnlyOnMark:= false;
  FShow:= false;
  FSize:= 8;
end;

procedure TDropDownSettings.AssignTo(Dest: TPersistent);  
begin
  if Dest is TDropDownSettings then
  begin
    with TDropDownSettings(Dest) do
    begin
      FNotify := Self.FNotify;
      FColor:= Self.FColor;
      FPressedColor:=Self.FPressedColor;
      FMarkDirection:=Self.FMarkDirection;
      FMarkPosition:=Self.FMarkPosition;
      FOnlyOnMark:=Self.FOnlyOnMark;
      FShow:=Self.FShow;
      FSize:=Self.FSize;
    end;
  end
  else
    inherited;  
end;
                     
function TDropDownSettings.IsPopupStored: boolean;
begin
  Result := FPopupMenu <> nil;  
end;

{ TStateBitmap }

function TStateBitmap.StateToInt(AState: TButtonState): integer; 
begin
  case AState of
     bsUp: Result := 0;
     bsDown: Result := 1;
     bsHot: Result := 2;
     else Result := 3;
  end;
end;

function TStateBitmap.GetBitmap(State: TButtonState): TBitmap;
begin
  Result := FBitmaps[StateToInt(State)];
end;        

constructor TStateBitmap.Create; 
var
  i: Integer;
begin
  SetLength(FBitmaps, 4);
  
  for i := 0 to 3 do
    FBitmaps[i] := TBitmap.Create;
end;

destructor TStateBitmap.Destroy;  
var
  i: Integer;
begin
  
  for i := 0 to 3 do
    FBitmaps[i].Free;  
  
  SetLength(FBitmaps, 0);
  
  inherited Destroy;  
end;

//Thx to: http://www.delphipraxis.net/topic67805_farbverlauf+berechnen.html
function ColorBetween(C1, C2 : TColor; blend:Extended):TColor;
var
   r, g, b : Byte;

   y1, y2 : Byte;
begin
   C1 := ColorToRGB(C1);
   C2 := ColorToRGB(C2);

   y1 := Red(C1);
   y2 := Red(C2);

   r := Round(y1 + (y2-y1)*blend);

   y1 := Green(C1);
   y2 := Green(C2);

   g := Round(y1 + (y2-y1)*blend);

   y1 := Blue(C1);
   y2 := Blue(C2);

   b := Round(y1 + (y2-y1)*blend);
   Result := RGBToColor(r, g, b);
end;

// Farbe zwischen beliebig vielen vorgegebenen Farbwerten berechnen
function ColorsBetween(colors:array of TColor; blend:Extended):TColor;
var
   a : Integer;
   faktor : Extended;
begin
   if Length(colors) < 2 then
      raise Exception.Create('ColorsBetween() at least 2 Colors required');

   if blend <= 0.0 then
      Result := colors[0]
   else if blend >= 1.0 then
      Result := colors[High(colors)]
   else
   begin
      a := Trunc(High(colors) * blend);
      faktor := 1.0 / High(colors);

      Result := ColorBetween(colors[a], colors[a+1], (blend-(a * faktor)) / faktor);
   end;
end;

function IfThen(ATest, ValA: Boolean; ValB: Boolean): Boolean;
begin
  if ATest then
    Result := ValA
  else
    Result := ValB;
end;

function IfThen(ATest: Boolean; ValA, ValB: TRect): TRect;
begin
  if ATest then
    Result := ValA
  else
    Result := ValB;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TGradButton]);
end;

initialization
  {$I tgradbutton.lrs}

end.

