{
@name GradButton
@author Eugen Bolz
@lastchange 21.07.2008
@version 1.4
@thx to http://www.delphipraxis.net/topic67805_farbverlauf+berechnen.html
@license http://creativecommons.org/licenses/LGPL/2.1/
@wiki http://wiki.lazarus.freepascal.org/TGradButton
}

unit ugradbtn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics, LCLType,LResources,
  LCLIntf ,Buttons, urotatebitmap, types, Menus;

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
    procedure SetColor(const AValue: TColor);
    procedure SetMarkDirection(const AValue: TDropDownMarkDirection);
    procedure SetMarkPosition(const AValue: TDropDownMarkPosition);
    procedure SetOnlyOnMark(const AValue: boolean);
    procedure SetPopupMenu(const AValue: TPopupMenu); 
    procedure SetPressedColor(const AValue: TColor);
    procedure SetShow(const AValue: Boolean);
    procedure SetSize(const AValue: integer); 
    
    procedure Notify;
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
  end;
  
  { TGradButton }

  TGradButton = class(TCustomControl)
  private
    FDropDownSettings: TDropDownSettings;
    FPaintToActive: Boolean;
    FAutoHeight: Boolean;
    FAutoHeightBorderSpacing: Integer;
    FAutoWidthBorderSpacing: Integer;
    FOnBorderBackgroundPaint: TGBBackgroundPaintEvent;
    FRotateDirection : TRotateDirection;
    FTextAlignment : TTextAlignment;
    FButtonLayout: TButtonLayout;
    FDropdownMarkRect: TRect;
    FTextPoint, FGlyphPoint: TPoint;
    FTextSize, FGlyphSize, FDropdownSize, FAutoSize : TSize;
    FBackground, bm,
    FNormalBackgroundCache, FHotBackgroundCache,
    FDownBackgroundCache, FDisabledBackgroundCache : TBitmap;
    FRotatedGlyph : TRotatedGlyph;
    FTextGlyphSpacing: Integer;
    FGradientType : TGradientType;
    FShowFocusBorder, FOnlyBackground,
    FAutoWidth, FShowGlyph, FEnabled, FFocused : Boolean;
    FBackgroundRect: TRect;
    FBorderSides: TBorderSides;
    FOnNormalBackgroundPaint, FOnHotBackgroundPaint,
    FOnDownBackgroundPaint, FOnDisabledBackgroundPaint : TGBBackgroundPaintEvent;
    procedure DrawDropDownArrow;
    procedure PaintGradient(TrgCanvas: TCanvas; pr : TRect);
    procedure SetDropDownSettings(const AValue: TDropDownSettings);
    procedure UpdateBackground;
    procedure PaintBackground(AState: TButtonState; TrgBitmap: TBitmap);
    procedure ShowDropdownPopupMenu;
    procedure DropDownSettingsChanged(Sender: TObject);
  protected
    FState, FOldState, FDropDownState: TButtonState;
    FNormalBlend,FOverBlend : Extended;
    FBaseColor, FNormalBlendColor, FOverBlendColor, FDisabledColor,
    FBackgroundColor, FGlyphBackgroundColor, FClickColor: TColor;
    FOwnerBackgroundDraw : Boolean;
    procedure SetAutoHeight(const AValue: Boolean); virtual;
    procedure SetAutoHeightBorderSpacing(const AValue: Integer); virtual;
    procedure SetAutoWidthBorderSpacing(const AValue: Integer); virtual;
    procedure InvPaint(StateCheck:Boolean=false);
    procedure FontChanged(Sender: TObject); override;
    procedure GlyphChanged(Sender: TObject); virtual;
    procedure GetContentRect(var TheRect: TRect); virtual;
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
    procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); overload; 
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
  p,t,midx, midy, textmidx, textmidy,
  groupwidth, groupheight, Offset1, Offset2 :Integer;
  tempBL : TButtonLayout;
begin
  GetContentRect(FBackgroundRect);

  Area := Size(FBackgroundRect);
                              
  tempGS.cx:=0;
  tempGS.cy:=0;

  if FShowGlyph and not FRotatedGlyph.Empty then
  begin
    tempGS.cx:=FRotatedGlyph.Width;
    tempGS.cy:=FRotatedGlyph.Height;
  end;

  tempTS := bm.Canvas.TextExtent(Caption);
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

        FTextPoint.x := FGlyphPoint.x+tempGS.cx+FTextGlyphSpacing+FDropDownSettings.Size;
        FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);
      end;
      blGlyphRight: begin
        //Glyph Right, Text Left
        FTextPoint.x := AlignItem(tempTS.cx+FTextGlyphSpacing+tempGS.cx,Area.cx,4, FTextAlignment);
        FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);

        FGlyphPoint.x := FTextPoint.x+tempTS.cx+FTextGlyphSpacing+FDropDownSettings.Size;
        FGlyphPoint.y := AlignItem(tempGS.cy,Area.cy,0, taCenter);
      end;
      blGlyphTop:  begin
        //Glyph Top, Text Bottom
        FGlyphPoint.x := AlignItem(tempGS.cx + FDropDownSettings.Size, Area.cx, 0, FTextAlignment);
        FTextPoint.x  := AlignItem(tempTS.cx + FDropDownSettings.Size, Area.cx, 0, FTextAlignment);

        FGlyphPoint.y := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, Area.cy, 4, taCenter);
        FTextPoint.y  := FGlyphPoint.y+tempGS.cy+FTextGlyphSpacing;
      end;
      blGlyphBottom: begin
        //Glyph Bottom, Text Top
        FGlyphPoint.x := AlignItem(tempGS.cx+FDropDownSettings.Size, Area.cx, 0, FTextAlignment);
        FTextPoint.x  := AlignItem(tempTS.cx+FDropDownSettings.Size, Area.cx, 0, FTextAlignment);

        FTextPoint.y  := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, Area.cy, 4, taCenter);
        FGlyphPoint.y := FTextPoint.y+tempTS.cy+FTextGlyphSpacing;
      end;
    end;
  end else begin
    FGlyphPoint.x := 0;
    FGlyphPoint.y := 0;

    FTextPoint.x := AlignItem(tempTS.cx+FDropDownSettings.Size,Area.cx,4, FTextAlignment);
    FTextPoint.y := AlignItem(tempTS.cy,Area.cy,0, taCenter);
  end;
  
  Offset1 := IfThen(FDropDownSettings.MarkPosition=mpLeft, FDropDownSettings.Size);

  FTextPoint.x := Offset1 + FTextPoint.x+FBackgroundRect.Left;
  FTextPoint.y := FTextPoint.y+FBackgroundRect.Top;

  FGlyphPoint.x := Offset1 + FGlyphPoint.x+FBackgroundRect.Left;
  FGlyphPoint.y := FGlyphPoint.y+FBackgroundRect.Top;
                                                                 
  Offset1 := IfThen(FDropDownSettings.MarkPosition<>mpLeft, FTextSize.cx, -FDropDownSettings.Size - 2);
  Offset2 := IfThen(FDropDownSettings.MarkPosition<>mpLeft, FGlyphSize.cx, -FDropDownSettings.Size - 2);
  
  FDropdownMarkRect.Left   := Max(FTextPoint.X+Offset1, FGlyphPoint.X+Offset2);
  FDropdownMarkRect.Top    := AlignItem(FDropDownSettings.Size, Area.cy, 0, taCenter) + FBackgroundRect.Top;
  FDropdownMarkRect.Right  := FDropdownMarkRect.Left + FDropDownSettings.Size;
  FDropdownMarkRect.Bottom := FDropdownMarkRect.Top + FDropDownSettings.Size;               
  
  FAutoSize.cx := Max(FGlyphPoint.x + FGlyphSize.cx, FTextPoint.x + FTextSize.cx);
  FAutoSize.cy := Max(FGlyphPoint.y + FGlyphSize.cy, FTextPoint.x + FTextSize.cx);
  
  if FDropDownSettings.Show and FDropDownSettings.IsPopupStored then
  begin
    FAutoSize.cx := Max(FAutoSize.cx, FDropdownMarkRect.Right);
    FAutoSize.cy := Max(FAutoSize.cy, FDropdownMarkRect.Bottom);
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

procedure TGradButton.PaintBackground(AState: TButtonState; TrgBitmap: TBitmap);
var
  FTempState : TButtonState;
  FOnTemp : TGBBackgroundPaintEvent;
begin
  FTempState:=FState;

  GetContentRect(FBackgroundRect);

  with TrgBitmap do
  begin
    Canvas.Font.Color := Self.Font.Color;
    Canvas.Font := Self.Font;
    Width := Self.Width;
    Height := Self.Height;

    Canvas.Brush.Color:=clWhite;
    Canvas.FillRect(0, 0, Width, Height);

    if Self.Parent is TGradButton then
    begin
      bm.Canvas.CopyRect(Rect(0, 0, Width, Height), (Self.Parent as TGradButton).GetBackground,
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
      if (bsTopLine in BorderSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(FBackgroundRect.Left,0,FBackgroundRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},0);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(FBackgroundRect.Left,1,FBackgroundRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},1);
      end;

      //Left
      if (bsLeftLine in BorderSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(0,FBackgroundRect.Top,0,FBackgroundRect.Bottom);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(1,FBackgroundRect.Top,1,FBackgroundRect.Bottom);
      end;

      //Right
      if (bsRightLine in BorderSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(Width-1,FBackgroundRect.Top,Width-1,FBackgroundRect.Bottom);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(Width-2,FBackgroundRect.Top,Width-2,FBackgroundRect.Bottom);
      end;

      //Bottom
      if (bsBottomLine in BorderSides) then
      begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(FBackgroundRect.Left,Height-1,FBackgroundRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},Height-1);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(FBackgroundRect.Left,Height-2,FBackgroundRect.Right+{$IFDEF DARWIN}1{$ELSE}0{$ENDIF},Height-2);
      end;

      //TopLeft
      if (bsTopLine in BorderSides) AND (bsLeftLine in BorderSides) then
        Canvas.Pixels[1,1]:=clBlack;

      //TopRight
      if (bsTopLine in BorderSides) AND (bsRightLine in BorderSides) then
        Canvas.Pixels[Width-2,1] := clBlack;

      //BottomLeft
      if (bsBottomLine in BorderSides) AND (bsLeftLine in BorderSides) then
        Canvas.Pixels[1, Height-2]:=clBlack;

      //BottomRight
      if (bsBottomLine in BorderSides) AND (bsRightLine in BorderSides) then
        Canvas.Pixels[Width-2,Height-2]:=clBlack;
    end;

    FState:=AState;

    if FOwnerBackgroundDraw then
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
    end else begin
      PaintGradient(Canvas, FBackgroundRect);
    end;
  end;

  FState:=FTempState;
end;

procedure TGradButton.ShowDropdownPopupMenu; 
var
  lowerLeft: TPoint;
begin
  if FDropDownSettings.Show and FDropDownSettings.IsPopupStored then Exit;
  lowerLeft := Point(0, Height);
  lowerLeft := ControlToScreen(lowerLeft);
  FDropDownSettings.PopupMenu.Popup(lowerLeft.X, lowerLeft.Y);
end;

procedure TGradButton.DropDownSettingsChanged(Sender: TObject); 
begin
  UpdateButton;
  
  InvPaint();
end;

procedure TGradButton.UpdateBackground;
var
  FTempState : TButtonState;
begin
  FTempState:= FState;

  FEnabled:=true;
  PaintBackground(bsUp,FNormalBackgroundCache);
  PaintBackground(bsHot, FHotBackgroundCache);
  PaintBackground(bsDown, FDownBackgroundCache);
  FEnabled:=false;
  PaintBackground(bsUp, FDisabledBackgroundCache);
  FEnabled:=Enabled;

  FState:=FTempState;

  InvPaint;
end;

procedure TGradButton.GetContentRect(var TheRect: TRect);
begin
   TheRect := Rect(0,0,Width,Height);

   //Top
   if (bsTopLine in BorderSides) then
   begin
        TheRect.Top := 2;
   end else
        TheRect.Top := 0;

   //Left
   if (bsLeftLine in BorderSides) then
   begin
      TheRect.Left := 2;
   end else
      TheRect.Left := 0;

   //Right
   if (bsRightLine in BorderSides) then
   begin
      TheRect.Right := TheRect.Right-{$IFDEF windows}2{$ELSE}3{$ENDIF};
   end;

   //Bottom
   if (bsBottomLine in BorderSides) then
   begin
       TheRect.Bottom := TheRect.Bottom - 2;
   end;
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
  Points : Array of TPoint; 
begin          
  SetLength(Points, 3);
                              
  // ArrowState
  {if FDropDownState = bsUp then ArrowState:=ttbSplitButtonDropDownNormal;
  if FDropDownState = bsDown then ArrowState:=ttbSplitButtonDropDownPressed;
  if FDropDownState = bsHot then ArrowState:=ttbSplitButtonDropDownHot;
  if FDropDownState = bsDisabled then ArrowState:=ttbSplitButtonDropDownDisabled;
  
  if (FDropDownState = bsDown) and Enabled then
    ArrowState := ttbSplitButtonDropDownPressed;
   }      
  PaintArrow(bm.Canvas, FDropdownMarkRect, FDropDownSettings.FMarkDirection, clGray);
  
  SetLength(Points, 0);
end; 

procedure TGradButton.PaintGradient(TrgCanvas: TCanvas; pr : TRect);
var
  r : Integer;
  t1,t2,t3 : TColor;
begin
  case FState of
    bsHot,bsDown : begin
      t3 := FOverBlendColor;
    end;
    else begin
      t3 := FNormalBlendColor;
    end;
   end;

  if FState = bsDown then begin
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
    if FState = bsDown then
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
   if FState = bsDown then
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
    
  FNormalBackgroundCache := TBitmap.Create;
  FHotBackgroundCache := TBitmap.Create;
  FDownBackgroundCache := TBitmap.Create;
  FDisabledBackgroundCache := TBitmap.Create;

  FBorderSides:=[bsTopLine,bsBottomLine,bsLeftLine,bsRightLine];

  bm := TBitmap.Create;
  bm.Canvas.Brush.Style := bsClear;

  UpdateBackground;
    
  Font.Color:=clWhite;
end;

destructor TGradButton.Destroy;
begin
   //DebugLn('bm.Free');
   bm.Free;
   //DebugLn('FRotatedGlyph.Free');
   FRotatedGlyph.Free;
   //DebugLn('FBackground.Free');
   FBackground.Free;
   //DebugLn('FNormalBackgroundCache.Free');
   FNormalBackgroundCache.Free;
   //DebugLn('FHotBackgroundCache.Free');
   FHotBackgroundCache.Free;
   //DebugLn('FDownBackgroundCache.Free');
   FDownBackgroundCache.Free;
   //DebugLn('FDisabledBackgroundCache.Free');
   FDisabledBackgroundCache.Free;

   
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

  bm.Canvas.Font := Font;
  UpdatePositions;
end;

procedure TGradButton.GlyphChanged(Sender: TObject);
begin
  UpdatePositions;
  Invalidate;
end;

procedure TGradButton.DoEnter;
begin
  FState:=bsHot;
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
  FFocused:=false;
  
  InvPaint;
  
  inherited;
end;

procedure TGradButton.Paint;
var
  TextOffset : Integer;
  tempState: TButtonState;
begin
   if not HasParent then
      Exit;

   with bm do
   begin
     Width := Self.Width;
     Height := Self.Height;

     FBackground.Width:=Width;
     FBackground.Height:=Height;

     Canvas.Brush.Color:=clBlack;
     Canvas.FillRect(0,0,Width, Height);

     if not FEnabled then
       tempState := bsDisabled
     else
       tempState := FState;
     case tempState of
       bsUp  : Canvas.Draw(0,0,FNormalBackgroundCache);
       bsDown: Canvas.Draw(0,0,FDownBackgroundCache);
       bsHot : Canvas.Draw(0,0,FHotBackgroundCache);
       else Canvas.Draw(0,0,FDisabledBackgroundCache);
     end;

     TextOffset := IfThen(tempState = bsDown, 1);

     DrawRotatedText(Canvas, FTextPoint.x + TextOffset, FTextPoint.y + TextOffset,
       FTextSize.cx, FTextSize.cy, Caption, FRotateDirection);

     if FShowGlyph and FRotatedGlyph.IsBitmapStored then
     begin
        FRotatedGlyph.State := FState;
        FRotatedGlyph.Draw(bm.Canvas, FGlyphPoint.x+TextOffset, FGlyphPoint.y+TextOffset);
     end;

     if not (csDesigning in ComponentState) then
       if FFocused and FShowFocusBorder then
          Canvas.DrawFocusRect(RECT(FBackgroundRect.Left+2, FBackgroundRect.Top+2,
            FBackgroundRect.Right-2, FBackgroundRect.Bottom-2));
   end;
   
   if FDropDownSettings.Show and FDropDownSettings.IsPopupStored then
    DrawDropDownArrow;
   
   if not FPaintToActive then
   begin
    Canvas.Draw(0,0,bm); 
   
    inherited Paint;
   end;
end;

procedure TGradButton.PaintTo(ACanvas: TCanvas; X, Y: Integer); 
begin
  FPaintToActive := true;
  Paint;
  ACanvas.CopyRect(Rect(X,Y, X+Width, Y+Height),
    bm.Canvas, ClientRect);
  FPaintToActive:= false;
end;

procedure TGradButton.MouseEnter;
begin
    inherited;
    
    if FState<>bsDown then
    begin
       FState:=bsHot;
       InvPaint(true);
    end;
end;

procedure TGradButton.MouseMove(Shift: TShiftState;
                 X, Y: Integer);
var
  TempPoint: TPoint;
begin
  TempPoint := Point(X, Y);
  
  if ssLeft in Shift then
    FState := bsDown
  else                   
    FState := bsHot;     
   
  if PtInRect(FDropdownMarkRect, TempPoint) and (ssLeft in Shift) then
    FDropDownState:= bsDown
  else
    FDropDownState:= bsHot;

  InvPaint(true);

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
    InvPaint(true);
end;

procedure TGradButton.Click;
begin
    //inherited;
end;

procedure TGradButton.MouseDown(Button: TMouseButton;
                 Shift: TShiftState; X, Y: Integer);
var
  TempPoint : TPoint;
begin
  TempPoint:= Point(X,Y); 
  
  if PtInRect(FDropdownMarkRect, TempPoint) then
  begin
    FState := bsDown;
    FDropDownState := bsDown;
    InvPaint(true);
    
  end
  else
    if PtInRect(Rect(0,0,Width,Height),Point(X,Y)) then
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

    inherited;
end;

procedure TGradButton.MouseUp(Button: TMouseButton;
                 Shift: TShiftState; X, Y: Integer);
var
  TempPoint : TPoint;
begin
  TempPoint:= Point(X,Y);
  
  if PtInRect(FDropdownMarkRect, TempPoint) then
  begin
    FState := bsHot;
    FDropDownState := bsHot;
    InvPaint(true);
    
    if Button = mbLeft then
      ShowDropdownPopupMenu;
  end
  else
  begin
    if PtInRect(Rect(0,0,Width,Height),TempPoint) then
    begin
      FState:=bsHot;
      InvPaint(true);
  
      if Button = mbLeft then
        inherited Click; //Faster, than the Overrided Click procedure
    end 
    else 
    begin
      FState := bsUp;
      FFocused:=false;
      InvPaint(true);
    end;
  
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

procedure Register;
begin
  RegisterComponents('Misc',[TGradButton]);
end;

initialization
  {$I tgradbutton.lrs}

end.

