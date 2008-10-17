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
  LCLIntf ,Buttons, urotatebitmap, types;

type
    TGradButton = class;

    TTextAlignment = (taLeftJustify, taRightJustify, taCenter);
    TBorderSide = (bsTopLine, bsBottomLine, bsLeftLine, bsRightLine);
    TBorderSides = set of TBorderSide;
    TGradientType = (gtHorizontal,gtVertical);

    TGBBackgroundPaintEvent = procedure(Sender: TGradButton;
       TargetCanvas: TCanvas; R: TRect; BState : TButtonState) of object;

    { TGradButton }

    TGradButton = class(TCustomControl)
    private
      FAutoHeight: Boolean;
      FAutoHeightBorderSpacing: Integer;
       FAutoWidthBorderSpacing: Integer;
       FRotateDirection : TRotateDirection;
       FTextAlignment : TTextAlignment;
       FButtonLayout: TButtonLayout;
       FTextPoint, FGlyphPoint : TPoint;
       FTextSize, FGlyphSize : TSize;
       FBackground, bm,
       FNormalBackgroundCache, FHotBackgroundCache,
       FDownBackgroundCache, FDisabledBackgroundCache : TBitmap;
       FRotatedGlyph : TRotatedGlyph;
       FRotatedText : TRotatedText;
       FTextGlyphSpacing: Integer;
       FGradientType : TGradientType;
       FShowFocusBorder, FOnlyBackground, FOwnerBackgroundDraw,
       FAutoWidth, FShowGlyph, FEnabled, FFocused : Boolean;
       FBackgroundRect: TRect;
       FBorderSides: TBorderSides;
       FOnNormalBackgroundPaint, FOnHotBackgroundPaint,
       FOnDownBackgroundPaint, FOnDisabledBackgroundPaint : TGBBackgroundPaintEvent;
       procedure PaintGradient(TrgCanvas: TCanvas; pr : TRect);
       procedure UpdateText;
       procedure UpdateBackground;
       procedure PaintBackground(AState: TButtonState; TrgBitmap: TBitmap);
    protected
       FState, FOldState: TButtonState;
       FNormalBlend,FOverBlend : Extended;
       FBaseColor, FNormalBlendColor, FOverBlendColor, FDisabledColor,
         FBackgroundColor, FGlyphBackgroundColor, FClickColor: TColor;
       procedure SetAutoHeight(const AValue: Boolean); virtual;
       procedure SetAutoHeightBorderSpacing(const AValue: Integer); virtual;
       procedure SetAutoWidthBorderSpacing(const AValue: Integer); virtual;
       procedure InvPaint(StateCheck:Boolean=false);
       procedure FontChanged(Sender: TObject); override;
       procedure GlyphChanged(Sender: TObject); virtual;
       procedure GetBackgroundRect(var TheRect : TRect); virtual;
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
       //procedure CreateParams(var Params: TCreateParams); override;
       procedure Paint; override;
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
       procedure Resize; override;
       function Focused: Boolean; override;
       procedure UpdateButton;
       procedure UpdatePositions;
       function GetAutoWidth : Integer;
       function GetAutoHeight : Integer;
    published
       property Action;
       property Anchors;
       property Align;
       property Caption;
       property Enabled;
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

       //property OnGetBackgroundRect { TODO }

       //BackgroundPaintEvents
       property OnNormalBackgroundPaint : TGBBackgroundPaintEvent read FOnNormalBackgroundPaint write FOnNormalBackgroundPaint;
       property OnHotBackgroundPaint : TGBBackgroundPaintEvent read FOnHotBackgroundPaint write FOnHotBackgroundPaint;
       property OnDownBackgroundPaint : TGBBackgroundPaintEvent read FOnDownBackgroundPaint write FOnDownBackgroundPaint;
       property OnDisabledBackgroundPaint : TGBBackgroundPaintEvent read FOnDisabledBackgroundPaint write FOnDisabledBackgroundPaint;
    end;

    function ColorBetween(C1, C2 : TColor; blend:Extended):TColor;
    function ColorsBetween(colors:array of TColor; blend:Extended):TColor;
    function AlignItem(ItemLength, AreaLength,Spacing: Integer; ATextAlignment: TTextAlignment):Integer;
    procedure DbgsGradButton(AButton : TGradButton);

    procedure Register;

implementation

uses
    LCLProc, math;

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

procedure DbgsGradButton(AButton: TGradButton);
begin
  DebugLn('######GradButton#####');

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
  FRotatedText.Text := Caption;

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

procedure TGradButton.UpdateText;
begin
    //UpdatePositions;

    //FRotatedText.Canvas.Font.Color := Canvas.Font.Color;
    //FRotatedText.Canvas.Font := Canvas.Font;
    //FRotatedText.Update;
end;

procedure TGradButton.SetAutoWidth(const Value : Boolean);
begin
    if FAutoWidth = Value then
      Exit;
    FAutoWidth := Value;
    
    UpdateButton;
end;

procedure TGradButton.Resize;
begin
    if (HasParent) then
    begin
      if FAutoWidth then
         UpdateButton
      else begin
         UpdatePositions;
         UpdateBackground;
      end;
    end;
    
    inherited;
end;

procedure TGradButton.UpdatePositions;
var
    tempTS,tempGS : TSize;
    p,t,midx, midy, textmidx, textmidy,
    groupwidth, groupheight, AreaWidth, AreaHeight :Integer;
    tempBL : TButtonLayout;
begin

    GetBackgroundRect(FBackgroundRect);

    AreaWidth := FBackgroundRect.Right-FBackgroundRect.Left;
    AreaHeight := FBackgroundRect.Bottom-FBackgroundRect.Top;

    tempGS.cx:=0;
    tempGS.cy:=0;

    if FShowGlyph and not FRotatedGlyph.Empty then
    begin
       tempGS.cx:=FRotatedGlyph.Width;
       tempGS.cy:=FRotatedGlyph.Height;
    end;

       //tempTS := Canvas.TextExtent(Caption);
       tempTS.cx:= FRotatedText.Width;
       tempTS.cy:= FRotatedText.Height;
       
       tempBL := FButtonLayout;

       {if FRotateDirection=rdRight then
       begin
           case FButtonLayout of
              blGlyphTop : tempBL := blGlyphBottom;
              blGlyphBottom: tempBL := blGlyphTop;
           end;
       end;}

{if FRotateDirection in [rdRight,rdLeft] then
       begin
           p := tempTS.cx;
           tempTS.cx := tempTS.cy;
           tempTS.cy := p;
           p := tempGS.cx;
           tempGS.cx:=tempGS.cy;
           tempGS.cy := p;
       end;  }

       if FShowGlyph and not FRotatedGlyph.Empty then begin
           case tempBL of
             blGlyphLeft:  begin
               FGlyphPoint.x := AlignItem(tempGS.cx+FTextGlyphSpacing+tempTS.cx,AreaWidth,4,FTextAlignment);
               FGlyphPoint.y := AlignItem(tempGS.cy,AreaHeight,0, taCenter);

               FTextPoint.x := FGlyphPoint.x+tempGS.cx+FTextGlyphSpacing;
               FTextPoint.y := AlignItem(tempTS.cy,AreaHeight,0, taCenter);
             end;
             blGlyphRight: begin
               //Glyph Right, Text Left
               FTextPoint.x := AlignItem(tempTS.cx+FTextGlyphSpacing+tempGS.cx,AreaWidth,4, FTextAlignment);
               FTextPoint.y := AlignItem(tempTS.cy,AreaHeight,0, taCenter);

               FGlyphPoint.x := FTextPoint.x+tempTS.cx+FTextGlyphSpacing;
               FGlyphPoint.y := AlignItem(tempGS.cy,AreaHeight,0, taCenter);
             end;
             blGlyphTop:  begin
               //Glyph Top, Text Bottom
               FGlyphPoint.x := AlignItem(tempGS.cx,AreaWidth, 0, FTextAlignment);
               FTextPoint.x := AlignItem(tempTS.cx, AreaWidth, 0, FTextAlignment);

               FGlyphPoint.y := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, AreaHeight, 4, taCenter);
               FTextPoint.y := FGlyphPoint.y+tempGS.cy+FTextGlyphSpacing;
             end;
             blGlyphBottom: begin
               //Glyph Bottom, Text Top
               FGlyphPoint.x := AlignItem(tempGS.cx,AreaWidth, 0, FTextAlignment);
               FTextPoint.x := AlignItem(tempTS.cx, AreaWidth, 0, FTextAlignment);

               FTextPoint.y := AlignItem(tempGS.cy+FTextGlyphSpacing+tempTS.cy, AreaHeight, 4, taCenter);
               FGlyphPoint.y := FTextPoint.y+tempTS.cy+FTextGlyphSpacing;
             end;
           end;
       end else begin
           FGlyphPoint.x := 0;
           FGlyphPoint.y := 0;

           FTextPoint.x := AlignItem(tempTS.cx,AreaWidth,4, FTextAlignment);
           FTextPoint.y := AlignItem(tempTS.cy,AreaHeight,0, taCenter);
       end;
       //WritePoints([TP^, GP^]);

       {TP^.x := TP^.x + p;
       TP^.y := TP^.y + p;

       GP^.x := GP^.x + p;
       GP^.y := GP^.y + p; }

       FTextPoint.x := FTextPoint.x+FBackgroundRect.Left;
       FTextPoint.y := FTextPoint.y+FBackgroundRect.Top;

       FGlyphPoint.x := FGlyphPoint.x+FBackgroundRect.Left;
       FGlyphPoint.y := FGlyphPoint.y+FBackgroundRect.Top;


       {$IFDEF DEBUGGRADBUTTON}
       WriteLn('Text');
       WritePoint(FTextPoint);
       WriteLn('Glyph');
       WritePoint(FGlyphPoint);
       {$ENDIF}

       //tempTS := Canvas.TextExtent(Caption);

       FTextSize:=tempTS;
       FGlyphSize:=tempGS;

end;

function TGradButton.GetAutoWidth: Integer;
begin
  if FShowGlyph then begin
    if FButtonLayout in [blGlyphLeft,blGlyphRight] then
       Result := FRotatedText.Width+ FRotatedGlyph.Width+FTextGlyphSpacing+FAutoWidthBorderSpacing
    else
       Result := Max(FRotatedText.Width,FRotatedGlyph.Width)+FAutoWidthBorderSpacing;
  end else begin
    Result := FRotatedText.Width+FAutoWidthBorderSpacing;
  end;
end;

function TGradButton.GetAutoHeight: Integer;
begin
  if FShowGlyph then begin
    if FButtonLayout in [blGlyphTop,blGlyphBottom] then
       Result := FRotatedText.Height+ FRotatedGlyph.Height+FTextGlyphSpacing+FAutoHeightBorderSpacing
    else
       Result := Max(FRotatedText.Height,FRotatedGlyph.Height)+FAutoHeightBorderSpacing;
  end else begin
    Result := FRotatedText.Height+FAutoHeightBorderSpacing;
  end;
end;

procedure TGradButton.PaintBackground(AState: TButtonState; TrgBitmap: TBitmap);
var
   FTempState : TButtonState;
begin
   FTempState:=FState;

   GetBackgroundRect(FBackgroundRect);

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

   //Top
   if (bsTopLine in BorderSides) then
   begin
        Canvas.Pen.Color:=clBlack;
        Canvas.Line(FBackgroundRect.Left,0,FBackgroundRect.Right,0);
        Canvas.Pen.Color:=clWhite;
        Canvas.Line(FBackgroundRect.Left,1,FBackgroundRect.Right,1);
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
      Canvas.Line(FBackgroundRect.Left,Height-1,FBackgroundRect.Right,Height-1);
      Canvas.Pen.Color:=clWhite;
      Canvas.Line(FBackgroundRect.Left,Height-2,FBackgroundRect.Right,Height-2);
   end;

   //Todo
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

   FState:=AState;

   PaintGradient(Canvas, FBackgroundRect);

   end;

   FState:=FTempState;
end;

procedure TGradButton.UpdateBackground;
var
   FTempState : TButtonState;
   R : TRect;
begin
   FTempState:= FState;

   GetBackgroundRect(R);

   if FOwnerBackgroundDraw then
   begin
      if FOnNormalBackgroundPaint<>nil then
         FOnNormalBackgroundPaint(Self, FNormalBackgroundCache.Canvas, R, bsUp);

      if FOnHotBackgroundPaint<>nil then
         FOnHotBackgroundPaint(Self, FHotBackgroundCache.Canvas, R, bsHot);

      if FOnDownBackgroundPaint<>nil then
         FOnDownBackgroundPaint(Self, FDownBackgroundCache.Canvas, R, bsDown);

      if FOnDisabledBackgroundPaint<>nil then
         FOnDisabledBackgroundPaint(Self, FDisabledBackgroundCache.Canvas, R, bsDisabled);
   end else begin

   FEnabled:=true;
   PaintBackground(bsUp,FNormalBackgroundCache);
   PaintBackground(bsHot, FHotBackgroundCache);
   PaintBackground(bsDown, FDownBackgroundCache);
   FEnabled:=false;
   PaintBackground(bsUp, FDisabledBackgroundCache);
   FEnabled:=Enabled;
   
   end;

   FState:=FTempState;
end;

procedure TGradButton.GetBackgroundRect(var TheRect: TRect);
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
   if FAutoWidth then Width := GetAutoWidth;
   if FAutoHeight then Height := GetAutoHeight;

   UpdateBackground;
   UpdateText;
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
    FRotatedText.Direction:=FRotateDirection;
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

procedure TGradButton.PaintGradient(TrgCanvas: TCanvas; pr : TRect);
var
   r : Integer;
   t1,t2,t3 : TColor;
begin

   case FState of
       bsHot,bsDown : begin
               t3 := FOverBlendColor;
               end;
       else    begin
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
          TrgCanvas.Line(pr.Left,r,pr.Right,r);
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
          TrgCanvas.Line(pr.Left,r,pr.Right,r);
       end;
   end else begin
   if FState = bsDown then
   begin
       for r := (pr.Right)-1 downto pr.Left do
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
       for r := (pr.Right)-1 downto pr.Left do
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
    
    Width:=80;
    Height:=25;
    
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
    FRotatedText := TRotatedText.Create;
    FButtonLayout:=blGlyphLeft;
    FGlyphBackgroundColor:=clWhite;
    
    FNormalBackgroundCache := TBitmap.Create;
    FHotBackgroundCache := TBitmap.Create;
    FDownBackgroundCache := TBitmap.Create;
    FDisabledBackgroundCache := TBitmap.Create;
    
    FBorderSides:=[bsTopLine,bsBottomLine,bsLeftLine,bsRightLine];

    bm := TBitmap.Create;

    UpdateBackground;
    
    Font.Color:=clWhite;
end;

destructor TGradButton.Destroy;
begin
   //DebugLn('bm.Free');
   bm.Free;
   //DebugLn('FRotatedGlyph.Free');
   FRotatedGlyph.Free;
   //DebugLn('FRotatedText.Free');
   FRotatedText.Free;
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

  FRotatedText.Font := Font;
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
   radius, r,t,p,x,y : Integer;
   rt,pr : TRect;
   t1,t2,t3 : TColor;
   s : String;
   r1,r2 : Extended;
   tempState : TButtonState;
   temp : TBitmap;
begin
   if not HasParent then
      Exit;

   tempState:=FState;
      
   with bm do
   begin

   Width := Self.Width;
   Height := Self.Height;

   FBackground.Width:=Width;
   FBackground.Height:=Height;

   p := 0;

   if tempState = bsDown then
      p := 1;
   
   if not FEnabled then tempState := bsDisabled;
   
   case tempState of
     bsUp  : Canvas.Draw(0,0,FNormalBackgroundCache);
     bsDown: Canvas.Draw(0,0,FDownBackgroundCache);
     bsHot : Canvas.Draw(0,0,FHotBackgroundCache);
     else Canvas.Draw(0,0,FDisabledBackgroundCache);
   end;
   
   if Caption <> '' then
      FRotatedText.Draw(bm.Canvas, FTextPoint.x+p, FTextPoint.y+p);

   if FShowGlyph AND FRotatedGlyph.IsBitmapStored then
   begin
      if not FEnabled then
         tempState := bsDisabled
      else
         tempState := FState;
         
      FRotatedGlyph.State:=tempState;
      FRotatedGlyph.Draw(bm.Canvas, FGlyphPoint.x+p, FGlyphPoint.y+p);
   end;
   
   if not (csDesigning in ComponentState) then
     if FFocused AND FShowFocusBorder then
        Canvas.DrawFocusRect(RECT(FBackgroundRect.Left+2, FBackgroundRect.Top+2,
          FBackgroundRect.Right-2, FBackgroundRect.Bottom-2));
   end;
   
   Canvas.Draw(0,0,bm);
   
   inherited Paint;
end;

procedure TGradButton.MouseEnter;
begin
    //WriteLn('MouseEnter');
    inherited;
    
    if FState<>bsDown then
    begin
       FState:=bsHot;
       InvPaint(true);
    end;
end;

procedure TGradButton.MouseMove(Shift: TShiftState;
                 X, Y: Integer);
begin
   if ssLeft in Shift then
      FState := bsDown
   else
      FState := bsHot;
         
   InvPaint(true);

   //inherited MouseMove calls OnMouseMove
   inherited MouseMove(Shift, X, Y);
end;

procedure TGradButton.MouseLeave;
begin
    inherited;
    //WriteLn('MouseLeave');
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
begin
    inherited;

    if PtInRect(Rect(0,0,Width,Height),Point(X,Y)) then
    begin


       //WriteLn('X: ',X,' Y: ',Y);
       FState:=bsDown;
       //FFocused:=true;
       //LCLIntf.SetFocus(Handle);
       InvPaint;
    end else begin
       FState:=bsUp;
       FFocused:=false;

       if Assigned(PopupMenu) then
          PopupMenu.Close;
       
       InvPaint;
    end;
end;

procedure TGradButton.MouseUp(Button: TMouseButton;
                 Shift: TShiftState; X, Y: Integer);
begin
    if PtInRect(Rect(0,0,Width,Height),Point(X,Y)) then
    begin

    //WriteLn('MouseUp');

    FState:=bsHot;
    //FFocused:=true;
    InvPaint(true);

    if Button = mbLeft then
       inherited Click; //Faster, than the Overrided Click procedure

   { if (Button = mbRight) AND Assigned(PopupMenu) then
       PopupMenu.PopUp(X,Y);
   }
    end else begin
        FState := bsUp;
        FFocused:=false;
        InvPaint(true);
    end;

    inherited;
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

procedure Register;
begin
  RegisterComponents('Misc',[TGradButton]);
end;

initialization
  {$I tgradbutton.lrs}

end.

