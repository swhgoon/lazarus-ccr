{ ----------------------------------------------------------------------------}
{ A Gradient Fill component for Delphi.                                       }
{ TGradientFill, Copyright 1995, Curtis White.  All Rights Reserved.          }
{ TNetGradient, Copyright 1997, Heiko Webers.  All Rights Reserved.           }
{ This component can be freely used and distributed in commercial and private }
{ environments, provided this notice is not modified in any way.              }
{ ----------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions  }
{ at cwhite@teleport.com 						      }
{ Or me at heikowebers@usa.net                                                }
{ ----------------------------------------------------------------------------}
{ Date last modified:  18/10/2009                                             }
{ ----------------------------------------------------------------------------}
{ ----------------------------------------------------------------------------}
{ TNetGradient v2.1                                                           }
{ ----------------------------------------------------------------------------}
{ Description:                                                                }
{   A gradient fill like in the new Netscape Communicator Options Box.        }
{ Features:                                                                   }
{   The begin and end colors can be any colors.                               }
{   The fill direction can be set to Right-To-Left or Left-To-Right.          }
{   The number of colors, between 1 and 255 can be set for the fill.          }
{   The Caption can be anything and anywhere on TNetGradient.		      }
{ ----------------------------------------------------------------------------}
{ ----------------------------------------------------------------------------}
{ Revision History:                                                           }
{ 1.00:  Initial release                                                      }
{ 1.00:  Changed to TNetGradient                                              }
{ 1.01:  Border Update by Enzo Scozzaro www.scozzaro.it  www.thefox.it        }
{ 2.00:  +Caption Alignment, +Layout, +DataSource                             }
{ 2.01:  +SubCaption, +Font, +MarginLeft, +MarginTop, +SubCapField            }
{ 2.03:  -Bug TextLetf                                                        }
{ 2.04:  FillDirection: +ftTopToBottom, +ftBottomToTop                        }
{ 2.05:  +Begin/EndUpdate, Fix crash in frames, Fix memory leaks, Cleanup     }
{ 2.1:   +FlatBorder, +TDBNetGradient, allow children controls, Cleanup       }
{ ----------------------------------------------------------------------------}
 
unit SMNetGradient;

{$MODE Delphi}
 
interface
 
uses
   LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, GraphType, Db, DBCtrls, LMessages;

type
  { Direction of fill }
  TFillDirection = (fdLeftToRight, fdRightToLeft, ftTopToBottom, ftBottomToTop);
  { Range of valid colors }
  TNumberOfColors = 1..255;
  TLabelBevel       = TBevelCut;

  TMargin           = 0..MaxInt;

  TPointArray = array of TPoint;

  TCustomNetGradient = class;

  { TSubCaption }

  TSubCaption = class(TPersistent)
  private
    { Private-Deklarationen }
    Parent:           TCustomNetGradient;
    FCaption:         TCaption;
    FFont:            TFont;
    FHotTrack:        Boolean;
    FMarginLeft:      Integer;
    FMarginTop:       Integer;
    FVisible:         Boolean;
    procedure         OnFontChanged(Sender: TObject);
  protected
    { Protected declarations }
    procedure         SetCaption(const Value: TCaption);
    procedure         SetFont(Value: TFont);
    procedure         SetMarginLeft(Value: Integer);
    procedure         SetMarginTop(Value: Integer);
    procedure         SetVisible(Value: Boolean);
  public
    { Public declarations }
    constructor       Create(AOwner: TCustomNetGradient); overload;
    destructor        Destroy; override;
    procedure         Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property          Caption: TCaption read FCaption write SetCaption;
    property          HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property          MarginLeft: Integer read FMarginLeft write SetMarginLeft default 5;
    property          MarginTop: Integer read FMarginTop write SetMarginTop default 0;
    property          Font: TFont read FFont write SetFont;
    property          Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TCustomNetGradient }

  TCustomNetGradient = class(TCustomControl)
  private
    //*** Enzo *** Bordi
    FBevelInner:      TLabelBevel;
    FBevelOuter:      TLabelBevel;

    //*** Emzp *** Allineamento Caption
    FAlignment  : TAlignment;
    FBorderColor: TColor;
    FFlatBorder: Boolean;
    FLayout     : TTextLayout;

    //FMargin:          TMargin;
    { Variables for properties }
    FDirection: TFillDirection;
    FBeginColor: TColor;
    FEndColor: TColor;
    // FCenter: Boolean;
    FNumberOfColors: TNumberOfColors;
    FTextTop : Integer;
    FTextLeft: Integer;
    FSubCaption: TSubCaption;
    FUpdateCount: Integer;
    procedure OnFontChanged(Sender: TObject);
    procedure Changed;
    function GetBorderPoints(const R: TRect): TPointArray;
    procedure SetBorderColor(const Value: TColor);
    { Procedures for setting property values }
    procedure SetFillDirection(Value: TFillDirection);
    procedure SetBeginColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetFlatBorder(const Value: Boolean);
    procedure SetNumberOfColors(Value: TNumberOfColors);
    procedure SetSubCaption(const Value: TSubCaption);
    procedure SetTextTop(Value: Integer);
    procedure SetTextLeft(Value: Integer);
    { Fill procedure }
    procedure GradientFill;
  protected
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure Paint; override;
    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TTextLayout);
    procedure SetBevelInner(Value: TLabelBevel);
    procedure SetBevelOuter(Value: TLabelBevel);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    property CaptionAlignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property CaptionLayout: TTextLayout read FLayout write SetLayout default tlCenter;
    property BevelInner: TLabelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TLabelBevel read FBevelOuter write SetBevelOuter default bvRaised;
    { Starting color of fill }
    property BeginColor: TColor read FBeginColor write SetBeginColor default clBlue;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWhite;
    { Ending color of fill }
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder default False;
    { Direction of fill }
    property FillDirection: TFillDirection read FDirection write SetFillDirection default fdLeftToRight;
    { Number of colors to use in the fill (1 - 256) - default is 255.  If 1 }
    { then it uses the Begin Color.                                         }
    property NumberOfColors: TNumberOfColors read FNumberOfColors write SetNumberOfColors default 255;
    property TextTop: Integer read FTextTop write SetTextTop default 0;
    property TextLeft: Integer read FTextLeft write SetTextLeft default 0;
    property SubCaption: TSubCaption read FSubCaption write SetSubCaption;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  { TNetGradient }

  TNetGradient = class (TCustomNetGradient)
  published
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BeginColor;
    property BorderColor;
    property CaptionAlignment;
    property CaptionLayout;
    property EndColor;
    property FlatBorder;
    property FillDirection;
    property NumberOfColors;
    property Font;
    property Caption;
    property TextTop;
    property TextLeft;
    property SubCaption;
    //default properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TDBNetGradient }

  TDBNetGradient = class (TCustomNetGradient)
  private
    FDataLink : TFieldDataLink;
    FSubCapField: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: String;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: String);
    procedure SetDataSource(Value: TDataSource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property SubCapField: Boolean read FSubCapField write FSubCapField default false;

    property CaptionAlignment;
    property CaptionLayout;
    property BevelInner;
    property BevelOuter;
    property BeginColor;
    property BorderColor;
    property EndColor;
    property FlatBorder;
    property FillDirection;
    property NumberOfColors;
    property Font;
    property Caption;
    property TextTop;
    property TextLeft;
    property SubCaption;
    //default properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;
 
implementation
 
{ TCustomNetGradient }

{ Override the constructor to initialize variables }
constructor TCustomNetGradient.Create(AOwner: TComponent);
begin
  { Inherit original constructor }
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csNoFocus];
  { Add new initializations }
  FAlignment     := taLeftJustify;
  FLayout        := tlCenter;
  FBevelInner    := bvNone;
  FBevelOuter    := bvRaised;

  Height := 25;
  Width := 400;
  FBeginColor := clSilver;
  FEndColor := $00A56D39;
  FBorderColor := clWhite;
  FDirection := fdLeftToRight;
  FNumberOfColors:= 255;
  //FTextLeft := 0;
  //FTextTop := 0;
  FSubCaption  := TSubCaption.Create(Self);
end;

destructor TCustomNetGradient.Destroy;
begin
  FSubCaption.Destroy;
  inherited Destroy;
end;

{ Set begin color when property is changed }
procedure TCustomNetGradient.SetBeginColor(Value: TColor);
begin
  if Value <> FBeginColor then
  begin
    FBeginColor := Value;
    Changed;
  end;
end;
 
{ Set end color when property is changed }
procedure TCustomNetGradient.SetEndColor(Value: TColor);
begin
  if Value <> FEndColor then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.SetFlatBorder(const Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    Changed;
  end;
end;

{ Set the number of colors to be used in the fill }
procedure TCustomNetGradient.SetNumberOfColors(Value: TNumberOfColors);
begin
  if Value <> FNumberOfColors then
  begin
    FNumberOfColors := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.SetSubCaption(const Value: TSubCaption);
begin
  FSubCaption.Assign(Value);
end;
 
// Set the Position of the Caption  (Top)
procedure TCustomNetGradient.SetTextTop(Value: Integer);
begin
  if Value <> FTextTop then
  begin
    FTextTop := Value;
    Changed;
  end;
end;
 
// Set the Position of the Caption (Left)
procedure TCustomNetGradient.SetTextLeft(Value: Integer);
begin
  if Value <> FTextLeft then
  begin
   FTextLeft := Value;
   Changed;
  end;
end;
 
{ Perform the fill when paint is called }
procedure TCustomNetGradient.Paint;
begin
  GradientFill;
end;
 
{ Gradient fill procedure - the actual routine }
procedure TCustomNetGradient.GradientFill;
var
  { Set up working variables }
  BeginRGBValue  : array[0..2] of Byte;    { Begin RGB values }
  RGBDifference  : array[0..2] of integer; { Difference between begin and end }
                                           { RGB values                       }
  ColorBand , rp: TRect;    { Color band rectangular coordinates }
  I         : Integer;  { Color band index }
  R         : Byte;     { Color band Red value }
  G         : Byte;     { Color band Green value }
  B         : Byte;     { Color band Blue value }
  WorkBmp   : TBitmap;  { Off screen working bitmap }
  TS         : TTextStyle;
  BorderOffset: Integer;

begin
  { Create the working bitmap and set its width and height }
  WorkBmp := TBitmap.Create;
  WorkBmp.Width := Width;
  WorkBmp.Height := Height;

  { Use working bitmap to draw the gradient }
  with WorkBmp do
  begin
    Rp := GetClientRect;
    { Extract the begin RGB values }
    case FDirection of
       fdLeftToRight, ftTopToBottom:   begin
          { Set the Red, Green and Blue colors }
          BeginRGBValue[0] := GetRValue (ColorToRGB (FBeginColor));
          BeginRGBValue[1] := GetGValue (ColorToRGB (FBeginColor));
          BeginRGBValue[2] := GetBValue (ColorToRGB (FBeginColor));
          { Calculate the difference between begin and end RGB values }
          RGBDifference[0] := GetRValue (ColorToRGB (FEndColor)) -
                              BeginRGBValue[0];
          RGBDifference[1] := GetGValue (ColorToRGB (FEndColor)) -
                              BeginRGBValue[1];
          RGBDifference[2] := GetBValue (ColorToRGB (FEndColor)) -
                              BeginRGBValue[2];
        end;

        fdRightToLeft,ftBottomToTop:    begin
          { Set the Red, Green and Blue colors }
          BeginRGBValue[0] := GetRValue (ColorToRGB (FEndColor));
          BeginRGBValue[1] := GetGValue (ColorToRGB (FEndColor));
          BeginRGBValue[2] := GetBValue (ColorToRGB (FEndColor));
          { Calculate the difference between begin and end RGB values }
          RGBDifference[0] := GetRValue (ColorToRGB (FBeginColor)) -
                              BeginRGBValue[0];
          RGBDifference[1] := GetGValue (ColorToRGB (FBeginColor)) -
                              BeginRGBValue[1];
          RGBDifference[2] := GetBValue (ColorToRGB (FBeginColor)) -
                              BeginRGBValue[2];
        end;
    end;

    { Set the pen style and mode }
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmCopy;

    case FDirection of

     { Calculate the color band's left and right coordinates }
     { for LeftToRight and RightToLeft fills }
      fdLeftToRight, fdRightToLeft:
        begin
          ColorBand.Top := 0;
          ColorBand.Bottom := Height;
        end;
      ftTopToBottom, ftBottomToTop:
        begin
          ColorBand.Left := 0;
          ColorBand.Right := Width;
        end;
    end;

    { Perform the fill }
    if FNumberOfColors = 1 then
    begin
      Canvas.Brush.Color := FBeginColor;
      Canvas.FillRect(rp);
    end
    else
    begin
      for I := 0 to FNumberOfColors do
      begin
        case FDirection of
          { Calculate the color band's left and right coordinates }
          fdLeftToRight, fdRightToLeft:
            begin
              ColorBand.Left := MulDiv (I, Self.Width, FNumberOfColors);
              ColorBand.Right := MulDiv (I + 1, Self.Width, FNumberOfColors);
            end;
          ftTopToBottom, ftBottomToTop:
            begin
              ColorBand.Top := MulDiv (I, Self.Height, FNumberOfColors);
              ColorBand.Bottom := MulDiv (I + 1, Self.Height, FNumberOfColors);
            end;
        end;
        { Calculate the color band's color }
        R := BeginRGBValue[0] + MulDiv (I, RGBDifference[0], FNumberOfColors - 1);
        G := BeginRGBValue[1] + MulDiv (I, RGBDifference[1], FNumberOfColors - 1);
        B := BeginRGBValue[2] + MulDiv (I, RGBDifference[2], FNumberOfColors - 1);
        { Select the brush and paint the color band }
        Canvas.Brush.Color := RGB (R, G, B);
        Canvas.FillRect (ColorBand);
       end;
    end;
  end;
 
  { Copy the working bitmap to the main canvas }
  Canvas.Draw(0, 0, WorkBmp);

  if FFlatBorder then
  begin
    BorderOffset := BorderWidth;
    if BorderWidth > 0 then
    begin
      Canvas.Pen.Width := BorderWidth;
      Canvas.Pen.EndCap := pecSquare;
      Canvas.Pen.Color := FBorderColor;
      //see if there's a better way of drawing a rectangle since
      // in BorderWidth >= 3 glitches occurs
      Canvas.Polyline(GetBorderPoints(rp));
    end;
  end
  else
  begin
    BorderOffset := 0;
    if FBevelOuter <> bvNone then
    begin
      Canvas.Frame3D(rp, 1, FBevelOuter);
      Inc(BorderOffset);
    end;
    if FBevelInner <> bvNone then
    begin
      Canvas.Frame3D(rp, 1, FBevelInner);
      Inc(BorderOffset);
    end;
  end;

  Canvas.Font.Assign(Font);
  TS := Canvas.TextStyle;
  TS.Alignment := FAlignment;
  TS.Layout := FLayout;
  TS.Opaque := False;
  TS.SystemFont := Canvas.Font.IsDefault;

  if Caption <> '' then
  begin
    Rp := GetClientRect;
    InflateRect(Rp, -BorderOffset, -BorderOffset);

    case FAlignment of
      taLeftJustify:
        Inc(Rp.Left, FTextLeft);
      taRightJustify:
        Dec(Rp.Right, FTextLeft);
      taCenter:
        Inc(Rp.Left, FTextLeft * 2);
    end;

    case FLayout of
      tlTop:
        Inc(rp.Top, FTextTop);
      tlBottom:
        Dec(rp.Bottom, FTextTop);
      tlCenter:
        Inc(rp.Top, FTextTop * 2);
    end;

    Canvas.TextRect(rp, rp.Left, rp.Top, Caption, TS);
  end;

  if FSubCaption.Caption <> '' then
  begin
    Canvas.Font.Assign(FSubCaption.Font);
    Rp := GetClientRect;
    InflateRect(Rp, -BorderOffset, -BorderOffset);
    TS.Alignment := taRightJustify;
    Dec(Rp.Right, FSubCaption.MarginLeft);
    case FLayout of
      tlTop:
        Inc(rp.Top, FSubCaption.MarginTop);
      tlBottom:
        Dec(rp.Bottom, FSubCaption.MarginTop);
      tlCenter:
        Inc(Rp.Top, FSubCaption.MarginTop * 2);
    end;
    Canvas.TextRect(rp, rp.Left, rp.Top, FSubCaption.Caption, TS);
  end;

  { Release the working bitmap resources }
  WorkBmp.Free;
end;

procedure TCustomNetGradient.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  Changed;
end;
 
{ Set the fill direction }
procedure TCustomNetGradient.SetFillDirection(Value: TFillDirection);
begin
  if Value <> FDirection then
  begin
    FDirection := Value;
    Changed;
  end;
end;

//*** Enzo ***

procedure TCustomNetGradient.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.SetLayout(Value: TTextLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.SetBevelInner(Value: TLabelBevel);
begin
  if Value <> FBevelInner then  begin
    FBevelInner := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.SetBevelOuter(Value: TLabelBevel);
begin
  if Value <> FBevelOuter then
  begin
    FBevelOuter := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Changed;
end;

procedure TCustomNetGradient.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // Do nothing. Just to avoid flicker.
end;

procedure TCustomNetGradient.OnFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomNetGradient.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomNetGradient.Changed;
begin
  if (FUpdateCount = 0) and not (csLoading in ComponentState) then
  begin
    Invalidate;
  end;
end;

function TCustomNetGradient.GetBorderPoints(const R: TRect): TPointArray;
var
  Offset, Fix: Integer;
begin
  Offset := BorderWidth div 2;
  Fix := BorderWidth mod 2;
  SetLength(Result, 5);
  Result[0].x := R.Left + BorderWidth - Offset - Fix;
  Result[0].y := R.Top + BorderWidth - Offset - Fix;

  Result[1].x := R.Right - BorderWidth + Offset;
  Result[1].y := R.Top + BorderWidth - Offset - Fix;

  Result[2].x := R.Right - BorderWidth + Offset;
  Result[2].y := R.Bottom - BorderWidth + Offset;

  Result[3].x := R.Left + BorderWidth - Offset - Fix;
  Result[3].y := R.Bottom - BorderWidth + Offset;

  Result[4].x := R.Left + BorderWidth - Offset - Fix;
  Result[4].y := R.Top + BorderWidth  - Offset - Fix;
end;

procedure TCustomNetGradient.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TCustomNetGradient.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  Changed;
end;

//**********

constructor TSubCaption.Create(AOwner: TCustomNetGradient);
begin
  inherited Create;
  Parent         := AOwner;
  //FCaption       := '';
  FFont          := TFont.Create;
  FFont.OnChange := OnFontChanged;
  //FHotTrack      := False;
  FMarginLeft        := 5;
  //FMarginTop       := 0;
  FVisible       := True;
end;

destructor TSubCaption.Destroy;
begin
  FFont.Destroy;
  inherited Destroy;
end;    // Destroy

procedure TSubCaption.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Parent.Changed;
  end
end;

procedure TSubCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSubCaption.OnFontChanged(Sender: TObject);
begin
  Parent.Changed;
end;

procedure TSubCaption.Assign(Source: TPersistent);
begin
  if Source is TSubCaption then
  begin
    FCaption := TSubCaption(Source).Caption;
    FHotTrack := TSubCaption(Source).HotTrack;
    FMarginLeft := TSubCaption(Source).MarginLeft;
    FMarginTop := TSubCaption(Source).MarginTop;
    FVisible := TSubCaption(Source).Visible;
    Font := TSubCaption(Source).Font;
  end
  else
    inherited Assign(Source);
end;

procedure TSubCaption.SetMarginLeft(Value: Integer);
begin
  if Value <> FMarginLeft then
  begin
    FMarginLeft := Value;
    Parent.Changed;
  end;
end;

procedure TSubCaption.SetMarginTop(Value: Integer);
begin
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    Parent.Changed;
  end;
end;

procedure TSubCaption.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Parent.Changed;
  end;
end;

procedure TDBNetGradient.DataChange(Sender: TObject);
begin
  if FDataLink.DataSet.Active then
  begin
    if not FSubCapField then
    begin
      Caption := FDataLink.Field.DisplayText;
    end else
    begin
     fSubCaption.SetCaption( FDataLink.Field.DisplayText);
     //fSubCaption.SetCaption( FDataLink.Field.AsString);
    end;
  end;
end;

function TDBNetGradient.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBNetGradient.SetDataField (const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDBNetGradient.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBNetGradient.SetDataSource (Value: TDataSource);

  procedure ChangeDataSource(AControl: TControl; Link: TDataLink;
    NewDataSource: TDataSource);
  begin
    if Link.DataSource=NewDataSource then exit;
    if Link.DataSource<>nil then
      Link.DataSource.RemoveFreeNotification(AControl);
    Link.DataSource:=NewDataSource;
    if Link.DataSource<>nil then
      Link.DataSource.FreeNotification(AControl);
  end;

begin
//*     enzo
  //FDataLink.DataSource := Value;
  ChangeDataSource(Self, FDataLink, Value);
  // useless                           e
  {if Value <> nil then
    Value.FreeNotification (Value);}
end;

constructor TDBNetGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //*** Enzo ***
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  (*
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
  *)
end;

destructor TDBNetGradient.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;


end.
