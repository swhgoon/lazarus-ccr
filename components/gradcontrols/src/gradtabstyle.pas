unit gradtabstyle;

{$mode objfpc}{$H+}

{.$DEFINE DEBUGTAB}

{-------------------------------------
Style-Class for TGradTabControl
--------------------------------------}

interface

uses
  Classes, SysUtils, Controls, Graphics, Buttons, ComCtrls, ExtCtrls, ugradbtn
  {$IFDEF DEBUGTAB}
  , sharedloggerlcl
  {$ELSE}
  , DummyLogger
  {$ENDIF};

type
  TStylePaintEvent = procedure(Sender: TCustomControl; AIndex: Integer;
    Button: TGradButton; TargetCanvas: TCanvas; R: TRect;
    BState : TButtonState) of object;

  { TGradTabStyleBase }

  TStyleBaseOptions = set of (sbTabButton, sbLeftRightButton, sbBorderButton, sbCloseButton);

  TGradTabStyleBase = class
  private
    FRoundedCorners: Boolean;
    function GetHasCloseButtonPaint: Boolean;
    function GetHasTabButtonPaint: Boolean;
    function GetHasLeftRightButtonPaint: Boolean;
    function GetHasBorderButtonPaint: Boolean;
  protected
    FTheTabControl: TCustomControl;
    FOptions : TStyleBaseOptions;
  public
    constructor Create; virtual;
    procedure TabControl(Sender: TCustomControl;
      TargetCanvas: TCanvas); virtual; abstract;
    procedure TabButton(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); virtual; abstract;
    procedure TabCloseButton(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); virtual; abstract;
    procedure TabButtonBorder(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); virtual; abstract;
    procedure TabLeftRightButton(Sender: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); virtual; abstract;
    procedure TabLeftRightBorderButton(Sender: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); virtual; abstract;
    procedure PrepareButton(Button: TGradButton); virtual;
    property TheTabControl : TCustomControl read FTheTabControl write FTheTabControl;
    property HasTabButtonPaint : Boolean read GetHasTabButtonPaint;
    property HasLeftRightButtonPaint : Boolean read GetHasLeftRightButtonPaint;
    property HasBorderButtonPaint : Boolean read GetHasBorderButtonPaint;
    property HasCloseButtonPaint : Boolean read GetHasCloseButtonPaint;
    property RoundedCorners : Boolean read FRoundedCorners write FRoundedCorners;
  end;

  { TGradTabStandardStyle }

  TGradTabStandardStyle = class(TGradTabStyleBase)
  public
    procedure TabControl(Sender: TCustomControl; TargetCanvas: TCanvas);
      override;
  end;

  { TGradTabVistaStyle }

  TButtonVistaStyle = record
    BorderColor : TColor;
    InnerBorderColor : TColor;
    TopStartColor : TColor;
    TopStopColor : TColor;
    BottomStartColor : TColor;
    BottomStopColor : TColor;
  end;

  TGradTabVistaStyle = class(TGradTabStyleBase)
  protected
    Normal : TButtonVistaStyle;
    Hover : TButtonVistaStyle;
    ActiveButton : TButtonVistaStyle;
  public
    constructor Create; override;
    procedure TabControl(Sender: TCustomControl; TargetCanvas: TCanvas);
      override;
    procedure TabButton(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); override;
    procedure TabCloseButton(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState: TButtonState); override;
    procedure TabButtonBorder(Sender: TCustomControl; AIndex: Integer;
      Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState); override;
    procedure TabLeftRightButton(Sender: TGradButton; TargetCanvas: TCanvas;
      R: TRect; BState: TButtonState); override;
    procedure TabLeftRightBorderButton(Sender: TGradButton; TargetCanvas: TCanvas;
      R: TRect; BState: TButtonState); override;
    procedure PrepareButton(Button: TGradButton); override;
  end;

  { TGradTabVistaBlueStyle }

  TGradTabVistaBlueStyle = class(TGradTabVistaStyle)
  public
    constructor Create; override;
  end;

  procedure LoggerButtonVistaStyle(Self: TButtonVistaStyle; Name: String);

implementation

uses
  ugradtabcontrol, LCLProc;

procedure LoggerButtonVistaStyle(Self: TButtonVistaStyle; Name: String);
  procedure C(aName: String; aColor: TColor);
  begin
    Logger.Send(aName, ColorToString(aColor));
  end;

begin
  Logger.EnterMethod('Style: '+ Name);

  C('BorderColor', Self.BorderColor);
  C('InnerBorderColor', Self.InnerBorderColor);
  C('TopStartColor', Self.TopStartColor);
  C('TopStopColor', Self.TopStopColor);
  C('BottomStartColor', Self.BottomStartColor);
  C('BottomStopColor', Self.BottomStopColor);

  Logger.ExitMethod('Style');
end;

{ TGradTabStyleBase }

function TGradTabStyleBase.GetHasCloseButtonPaint: Boolean;
begin
  Result := sbCloseButton in FOptions;
end;

function TGradTabStyleBase.GetHasTabButtonPaint: Boolean;
begin
  Result := sbTabButton in FOptions;
end;

function TGradTabStyleBase.GetHasLeftRightButtonPaint: Boolean;
begin
  Result := sbLeftRightButton in FOptions;
end;

function TGradTabStyleBase.GetHasBorderButtonPaint: Boolean;
begin
  Result := sbBorderButton in FOptions;
end;

constructor TGradTabStyleBase.Create;
begin
  FOptions:= [];

  FRoundedCorners := true;
end;

procedure TGradTabStyleBase.PrepareButton(Button: TGradButton);
begin
  // nothing
end;

{ TGradTabStandardStyle }

procedure TGradTabStandardStyle.TabControl(Sender: TCustomControl;
  TargetCanvas: TCanvas);
var
  AClientRect : TRect;
begin
  TargetCanvas.Brush.Color:=Sender.Color;
  TargetCanvas.FillRect(0,0,Sender.Width, Sender.Height);

  TargetCanvas.Pen.Color:=clBlack;

  AClientRect := (Sender as TGradTabControl).GetClientRect;

  TargetCanvas.Rectangle(AClientRect.Left-2, AClientRect.Top-2,
                   AClientRect.Right+2, AClientRect.Bottom+2);
end;

{ TGradTabVistaStyle }

constructor TGradTabVistaStyle.Create;
begin
  inherited Create;

  FOptions:=[sbTabButton, sbBorderButton, sbCloseButton, sbLeftRightButton];

  with Normal do
  begin
    BorderColor:=TColor($8b8c90);
    InnerBorderColor:= TColor($fbfcff);
    TopStartColor := TColor($f2f2f2);
    TopStopColor := TColor($ebebeb);
    BottomStartColor := TColor($dddddb);
    BottomStopColor := TColor($cfcfcf);
  end;

  with Hover do
  begin
    BorderColor:=RGBToColor(85, 121, 145);
    InnerBorderColor:= RGBToColor(224, 255, 255);
    TopStartColor := RGBToColor(233, 246, 254);
    TopStopColor := RGBToColor(219, 238, 252);
    BottomStartColor := RGBToColor(190, 231, 253);
    BottomStopColor := RGBToColor(167, 217, 244);
  end;

  with ActiveButton do
  begin
    BorderColor:=TColor($8b8c91);
    InnerBorderColor:= TColor($FFFFFF);
    TopStartColor := TColor($FFFFFF);
    TopStopColor := TColor($FFFFFF);
    BottomStartColor := TColor($FFFFFF);
    BottomStopColor := TColor($FFFFFF);
  end;
end;

procedure TGradTabVistaStyle.TabControl(Sender: TCustomControl;
  TargetCanvas: TCanvas);
var
  AClientRect : TRect;
begin
  TargetCanvas.Brush.Color:=clWhite;
  TargetCanvas.FillRect(0,0,Sender.Width, Sender.Height);

  TargetCanvas.Pen.Color:=Normal.BorderColor;

  AClientRect := (Sender as TGradTabControl).GetClientRect;

  TargetCanvas.Rectangle(AClientRect.Left-2, AClientRect.Top-2,
                   AClientRect.Right+2, AClientRect.Bottom+2);
end;

procedure TGradTabVistaStyle.TabButton(Sender: TCustomControl; AIndex: Integer;
    Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState);
var
  ColorSet: TButtonVistaStyle;
  UpperRect, LowerRect : TRect;
  FGradTabControl : TGradTabControl;
  FGradientDirection : TGradientDirection;
begin
  FGradTabControl := nil;

  if Sender <> nil then
    FGradTabControl := Sender as TGradTabControl;

  if (FGradTabControl <> nil)
  and (AIndex = FGradTabControl.PageIndex) then
    BState := bsDown;

  case BState of
    bsDown: ColorSet := ActiveButton;
    bsHot: ColorSet := Hover;
    else ColorSet := Normal;
  end;

  UpperRect := R;
  UpperRect.Bottom:= UpperRect.Bottom div 2;

  LowerRect := R;
  LowerRect.Top := UpperRect.Bottom;

  case FGradTabControl.TabPosition of
    tpTop, tpBottom: FGradientDirection := gdVertical;
    tpLeft, tpRight:
    if FGradTabControl.LongTabs then
      FGradientDirection := gdVertical
    else
      FGradientDirection := gdHorizontal;
  end;

  with TargetCanvas do
  begin
    GradientFill(UpperRect, ColorSet.TopStartColor,
      ColorSet.TopStopColor, FGradientDirection);
    GradientFill(LowerRect, ColorSet.BottomStartColor,
      ColorSet.BottomStopColor, FGradientDirection);
  end;
end;

procedure TGradTabVistaStyle.TabCloseButton(Sender: TCustomControl;
  AIndex: Integer; Button: TGradButton; TargetCanvas: TCanvas; R: TRect;
  BState: TButtonState);
var
  FGradTabControl : TGradTabControl;
begin
  FGradTabControl := nil;

  if Sender <> nil then
    FGradTabControl := Sender as TGradTabControl;

  if (FGradTabControl <> nil)
  and (AIndex <> FGradTabControl.PageIndex) then
    BState := bsUp;

  TabButton(Sender, AIndex, Button, TargetCanvas, R, BState);
end;

procedure TGradTabVistaStyle.TabButtonBorder(Sender: TCustomControl; AIndex: Integer;
    Button: TGradButton; TargetCanvas: TCanvas; R: TRect; BState : TButtonState);
var
  ColorSet: TButtonVistaStyle;
  Temp : Integer;
begin
  //DebugLn('Border R: ',DbgS(R));

  case BState of
    bsDown: ColorSet := ActiveButton;
    bsHot: ColorSet := Hover;
    else ColorSet := Normal;
  end;

  Temp := 0;

  if not RoundedCorners then
  begin
    if R.Left > 0 then
      R.Left := R.Left -1;

    if R.Right < Button.Width then
      R.Right := R.Right + 1;

    if R.Top > 0 then
      R.Top := R.Top - 1;

    if R.Bottom < Button.Height then
      R.Bottom := R.Bottom + 1;

    Temp := 1;
  end;

  with Button do
  begin
    //Top
    if (bsTopLine in BorderSides) then
    begin
      TargetCanvas.Pen.Color:=ColorSet.BorderColor;
      TargetCanvas.Line(R.Left,0,R.Right,0);
      TargetCanvas.Pen.Color:=ColorSet.InnerBorderColor;
      TargetCanvas.Line(R.Left,1,R.Right,1);
    end;

    //Left
    if (bsLeftLine in BorderSides) then
    begin
      TargetCanvas.Pen.Color:=ColorSet.BorderColor;
      TargetCanvas.Line(0,R.Top,0,R.Bottom);
      TargetCanvas.Pen.Color:=ColorSet.InnerBorderColor;
      TargetCanvas.Line(1,R.Top,1,R.Bottom);
    end;

    //Right
    if (bsRightLine in BorderSides) then
    begin
      TargetCanvas.Pen.Color:=ColorSet.BorderColor;
      TargetCanvas.Line(Width-1,R.Top,Width-1,R.Bottom);
      TargetCanvas.Pen.Color:=ColorSet.InnerBorderColor;
      TargetCanvas.Line(Width-2,R.Top,Width-2,R.Bottom);
    end;

    //Bottom
    if (bsBottomLine in BorderSides) then
    begin
      TargetCanvas.Pen.Color:=ColorSet.BorderColor;
      TargetCanvas.Line(R.Left,Height-1,R.Right,Height-1);
      TargetCanvas.Pen.Color:=ColorSet.InnerBorderColor;
      TargetCanvas.Line(R.Left,Height-2,R.Right,Height-2);
    end;

    //TopLeft
    if (bsTopLine in BorderSides) AND (bsLeftLine in BorderSides) then
      TargetCanvas.Pixels[1-Temp,1-Temp]:=ColorSet.BorderColor;

    //TopRight
    if (bsTopLine in BorderSides) AND (bsRightLine in BorderSides) then
      TargetCanvas.Pixels[Width-2+Temp,1-Temp] := ColorSet.BorderColor;

    //BottomLeft
    if (bsBottomLine in BorderSides) AND (bsLeftLine in BorderSides) then
      TargetCanvas.Pixels[1-Temp, Height-2+Temp]:=ColorSet.BorderColor;

    //BottomRight
    if (bsBottomLine in BorderSides) AND (bsRightLine in BorderSides) then
      TargetCanvas.Pixels[Width-2+Temp,Height-2+Temp]:=ColorSet.BorderColor;
  end;
end;

procedure TGradTabVistaStyle.TabLeftRightButton(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  if BState = bsDown then
    BState := bsHot;

  if Sender.Font.Color = clWhite then
    Sender.Font.Color := clBlack;

  TabButton(Sender.Owner as TCustomControl, 0, Sender, TargetCanvas, R, BState);
end;

procedure TGradTabVistaStyle.TabLeftRightBorderButton(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  if BState = bsDown then
     BState := bsHot;

  TabButtonBorder(Sender.Owner as TCustomControl, 0, Sender, TargetCanvas, R, BState);
end;

procedure TGradTabVistaStyle.PrepareButton(Button: TGradButton);
begin
  Button.Font.Color:=clBlack;
end;

{ TGradTabVistaBlueStyle }

constructor TGradTabVistaBlueStyle.Create;
begin
  inherited Create;

  with Normal do
  begin
    BorderColor:=RGBToColor(145, 150, 162);       //#9196A2
    InnerBorderColor:= RGBToColor(224, 255, 255);
    TopStartColor := RGBToColor(252, 253, 254);
    TopStopColor := RGBToColor(231, 235, 255);
    BottomStartColor := RGBToColor(207, 215, 235);
    BottomStopColor := RGBToColor(221, 227, 243);
  end;

  with Hover do
  begin
    BorderColor:=RGBToColor(145, 150, 162);       //#9196A2
    InnerBorderColor:= RGBToColor(224, 255, 255);
    TopStartColor := RGBToColor(232, 240, 251);
    TopStopColor := RGBToColor(184, 212, 244);
    BottomStartColor := RGBToColor(115, 176, 232);
    BottomStopColor := RGBToColor(173, 208, 241);
  end;

  with ActiveButton do
  begin
    BorderColor:=RGBToColor(145, 150, 162);       //#9196A2
    InnerBorderColor:= TColor($FFFFFF);
    TopStartColor := RGBToColor(255, 245, 224);   //#FFF5E0
    TopStopColor := RGBToColor(255, 222, 147);    //#FFDE93
    BottomStartColor := RGBToColor(255, 184, 17); //#FFB811
    BottomStopColor := RGBToColor(255, 213, 114);
  end;

  LoggerButtonVistaStyle(ActiveButton, 'ActiveButton');
end;

end.

