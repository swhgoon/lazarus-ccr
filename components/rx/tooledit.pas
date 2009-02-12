unit tooledit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LCLType, LMessages, Graphics, MaskEdit, Controls, EditBtn,
  pickdate, dateutil;

type
  { TCustomDateEdit }

  TYearDigits = (dyDefault, dyFour, dyTwo);
  TPopupAlign = (epaRight, epaLeft);
  TCalendarStyle = (csPopup, csDialog);

const
{$IFDEF DEFAULT_POPUP_CALENDAR}
  dcsDefault = csPopup;
{$ELSE}
  dcsDefault = csDialog;
{$ENDIF DEFAULT_POPUP_CALENDAR}

type

  { TCustomRxDateEdit }

  TCustomRxDateEdit = class(TCustomEditButton)
  private
    FCalendarHints: TStrings;
    FBlanksChar: Char;
    FCancelCaption: TCaption;
    FDefaultToday: Boolean;
    FDialogTitle: TCaption;
    FPopupColor: TColor;
    FNotInThisMonthColor:TColor;
    FOKCaption: TCaption;
    FOnAcceptDAte: TAcceptDateEvent;
    FStartOfWeek: TDayOfWeekName;
    FWeekendColor: TColor;
    FWeekends: TDaysOfWeek;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FFormatting: Boolean;
    FPopupVisible: Boolean;
    FPopupAlign: TPopupAlign;
    function GetCalendarStyle: TCalendarStyle;
    function GetDate: TDateTime;
    function GetPopupColor: TColor;
    function GetPopupVisible: Boolean;
    function GetValidDate: boolean;
    function IsStoreTitle: boolean;
    procedure SetBlanksChar(const AValue: Char);
    procedure SetCalendarStyle(const AValue: TCalendarStyle);
    procedure SetDate(const AValue: TDateTime);
    procedure SetPopupColor(const AValue: TColor);
    procedure SetStartOfWeek(const AValue: TDayOfWeekName);
    procedure SetWeekendColor(const AValue: TColor);
    procedure SetWeekends(const AValue: TDaysOfWeek);
    procedure SetYearDigits(const AValue: TYearDigits);
    procedure CalendarHintsChanged(Sender: TObject);

//    function AcceptPopup(var Value: Variant): Boolean;
    procedure AcceptValue(const AValue: TDateTime);
//    procedure SetPopupValue(const Value: Variant);
  protected
    FPopup: TPopupCalendar;
    procedure UpdateFormat;
    procedure UpdatePopup;
    function TextStored: Boolean;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean);
    procedure HidePopup; virtual;
    procedure ShowPopup(AOrigin: TPoint); virtual;
    procedure ApplyDate(Value: TDateTime); virtual;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoButtonClick (Sender: TObject); override;
    function GetDefaultGlyphName: String; override;

    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property DialogTitle:TCaption Read FDialogTitle Write FDialogTitle Stored IsStoreTitle;
    Property OnAcceptDate : TAcceptDateEvent Read FOnAcceptDAte Write FOnAcceptDate;
    property OKCaption:TCaption Read FOKCaption Write FOKCaption;
    property CancelCaption:TCaption Read FCancelCaption Write FCancelCaption;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;
    property PopupColor: TColor read GetPopupColor write SetPopupColor
      default clBtnFace;
    property CalendarStyle: TCalendarStyle read GetCalendarStyle
      write SetCalendarStyle default dcsDefault;
    property PopupVisible: Boolean read GetPopupVisible;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaLeft;
    property NotInThisMonthColor:TColor read FNotInThisMonthColor write FNotInThisMonthColor default clSilver;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckValidDate;
    function GetDateMask: string;
    procedure UpdateMask; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property Formatting: Boolean read FFormatting;
    property ValidDate:boolean read GetValidDate;
  end;
  
type
  TRxDateEdit = class(TCustomRxDateEdit)
  public
    property PopupVisible;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BlanksChar;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property CalendarStyle;
    property CancelCaption;
    property CharCase;
    property Color;
    property Constraints;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property Glyph;
    property MaxLength;
    property NotInThisMonthColor;
    property NumGlyphs;
    property OKCaption;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property StartOfWeek;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WeekendColor;
    property Weekends;
    property YearDigits;

    property OnAcceptDate;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;
  

function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TLMPaint): Boolean;
function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;

implementation
uses lclintf, LCLStrConsts, rxconst, rxstrutils, LResources, Forms;

type
  TPopupCalendarAccess = class(TPopupCalendar)
  end;

function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with Editor do
  begin
    if NewStyleControls then
    begin
      if BorderStyle = bsNone then
        I := 0
      else
{      if Ctl3D then
        I := 1
      else}
        I := 2;
      Result.X := {SendMessage(Handle, LM_GETMARGINS, 0, 0) and $0000FFFF} + I;
      Result.Y := I;
    end
    else
    begin
      if BorderStyle = bsNone then
        I := 0
      else
      begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then
          I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I;
    end;
  end;
end;

function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TLMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
{$IFDEF USED_BiDi}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
{$ENDIF}
begin
  Result := True;
  with Editor do
  begin
{$IFDEF USED_BiDi}
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$ENDIF}
    if StandardPaint and not(csPaintCopy in ControlState) then
    begin
{$IFDEF USED_BiDi}
      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollbar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end;
{$ENDIF USED_BiDi}
      Result := False;
      { return false if we need to use standard paint handler }
      Exit;
    end;
    { Since edit controls do not handle justification unless multi-line (and
      then only poorly) we will draw right and center justify manually unless
      the edit has the focus. }
    if ACanvas = nil then
    begin
      ACanvas := TControlCanvas.Create;
      ACanvas.Control := Editor;
    end;

    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;

    try
      ACanvas.Font := Font;
      if not Enabled and NewStyleControls and not
        (csDesigning in ComponentState) and
        (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
        ACanvas.Font.Color := clGrayText;
      with ACanvas do
      begin
        R := ClientRect;
{
        if not (NewStyleControls ) and (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
}
        Brush.Color := Color;
        S := AText;
        AWidth := TextWidth(S);
        Margins := EditorTextMargins(Editor);
{        if PopupVisible then
          ALeft := Margins.X
        else
        begin}
//          if ButtonWidth > 0 then Inc(AWidth);
          case AAlignment of
            taLeftJustify:
              ALeft := Margins.X;
            taRightJustify:
              ALeft := ClientWidth {- ButtonWidth} - AWidth - Margins.X - 2;
            else
              ALeft := (ClientWidth {- ButtonWidth} - AWidth) div 2;
          end;
{        end;}
{$IFDEF USED_BiDi}
        if SysLocale.MiddleEast then UpdateTextFlags;
{$ENDIF}
        Brush.Style := bsClear;
        TextRect(R, ALeft, Margins.Y, S);
      end;
    finally
      ACanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

{ TCustomRxDateEdit }

function TCustomRxDateEdit.IsStoreTitle: boolean;
begin
  Result:=DialogTitle<>rsPickDate;
end;

procedure TCustomRxDateEdit.SetBlanksChar(const AValue: Char);
begin
  if FBlanksChar=AValue then exit;
  if (AValue < ' ') then
    FBlanksChar:=' '
  else
    FBlanksChar:=AValue;
  UpdateMask;
end;

function TCustomRxDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then
    Result := csPopup
  else
    Result := csDialog;
end;

function TCustomRxDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  if Text<>'' then
    Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

function TCustomRxDateEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then Result := TPopupCalendar(FPopup).Color
  else Result := FPopupColor;
end;

function TCustomRxDateEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

function TCustomRxDateEdit.GetValidDate: boolean;
begin
  try
    StrToDateFmt(FDateFormat, Text);
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure TCustomRxDateEdit.SetCalendarStyle(const AValue: TCalendarStyle);
begin
  if AValue <> CalendarStyle then
  begin
    case AValue of
      csPopup:
        begin
          if FPopup = nil then
            FPopup := CreatePopupCalendar(Self{$IFDEF USED_BiDi}, BiDiMode {$ENDIF});
          FPopup.OnCloseUp := @PopupCloseUp;
          FPopup.Color := FPopupColor;
          TRxCalendarGrid(FPopup.Calendar).NotInThisMonthColor:=FNotInThisMonthColor;
//          UpdatePopup;
        end;
      csDialog:
        begin
          FPopup.Free;
          FPopup := nil;
        end;
    end;
  end;
end;

procedure TCustomRxDateEdit.SetDate(const AValue: TDateTime);
var
  D: TDateTime;
begin
{  if not ValidDate(AValue) or (AValue = NullDate) then
  begin
    if DefaultToday then AValue := SysUtils.Date
    else Value := NullDate;
  end;}
  D := Date;
  if AValue = NullDate then
    Text := ''
  else
    Text := FormatDateTime(FDateFormat, AValue);
  Modified := D <> Date;
end;

procedure TCustomRxDateEdit.SetPopupColor(const AValue: TColor);
begin
  if AValue <> FPopupColor then
  begin
    if FPopup <> nil then FPopup.Color := AValue;
    FPopupColor := AValue;
  end;
end;

procedure TCustomRxDateEdit.SetStartOfWeek(const AValue: TDayOfWeekName);
begin
  if FStartOfWeek=AValue then exit;
  FStartOfWeek:=AValue;
end;

procedure TCustomRxDateEdit.SetWeekendColor(const AValue: TColor);
begin
  if FWeekendColor=AValue then exit;
  FWeekendColor:=AValue;
end;

procedure TCustomRxDateEdit.SetWeekends(const AValue: TDaysOfWeek);
begin
  if FWeekends=AValue then exit;
  FWeekends:=AValue;
end;

procedure TCustomRxDateEdit.SetYearDigits(const AValue: TYearDigits);
begin
  if FYearDigits=AValue then exit;
  FYearDigits:=AValue;
end;

procedure TCustomRxDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while (FCalendarHints.Count > 4) do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := @CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then UpdatePopup;
end;

{function TCustomRxDateEdit.AcceptPopup(var Value: Variant): Boolean;
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then begin
    if VarIsNull(Value) or VarIsEmpty(Value) then D := NullDate
    else
      try
        D := VarToDateTime(Value);
      except
        if DefaultToday then D := SysUtils.Date else D := NullDate;
      end;
    FOnAcceptDate(Self, D, Result);
    if Result then Value := VarFromDateTime(D);
  end;
end;}

procedure TCustomRxDateEdit.AcceptValue(const AValue: TDateTime);
begin
  SetDate(AValue);
//  UpdatePopupVisible;
  if Modified then inherited Change;
end;

{procedure TCustomRxDateEdit.SetPopupValue(const Value: Variant);
begin

end;}

procedure TCustomRxDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;

procedure TCustomRxDateEdit.UpdatePopup;
begin
  if FPopup <> nil then SetupPopupCalendar(FPopup, FStartOfWeek,
    FWeekends, FWeekendColor, FCalendarHints, FourDigitYear);
end;

function TCustomRxDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', DateSeparator, FBlanksChar]);
end;

procedure TCustomRxDateEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;

procedure DoTrySetDate;
var
  D:TDateTime;
begin
  if Text<>'' then
  begin
    try
      D:=StrToDate(Text);
      FPopup.Date:=D;
    except
      if FDefaultToday then
        FPopup.Date:=sysutils.Date;
    end;
  end
  else
  if FDefaultToday then
    FPopup.Date:=sysutils.Date;
end;

begin
  if (FPopup <> nil) and not (ReadOnly {or FPopupVisible}) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then P.X := 0
    else if P.X + FPopup.Width > Screen.Width then
      P.X := Screen.Width - FPopup.Width;

    DoTrySetDate;

    ShowPopup(Point(P.X, Y));
//    FPopupVisible := True;
{    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;}
  end;
end;

procedure TCustomRxDateEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  AValue: Variant;
begin
(*
  if (FPopup <> nil) and FPopupVisible then
  begin
{    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);}
//    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
        begin
          SetFocus;
//          if GetFocus = Handle then SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
//      DirectInput:=DirectInput;
      Invalidate;
{      if Accept and AcceptPopup(AValue) and EditCanModify then
      begin
        AcceptValue(AValue);
        if FFocused then inherited SelectAll;
      end;}
    finally
      FPopupVisible := False;
    end;
  end;
*)
end;

procedure TCustomRxDateEdit.HidePopup;
begin
  FPopup.Hide;
end;

procedure TCustomRxDateEdit.ShowPopup(AOrigin: TPoint);
var
  FAccept:boolean;
begin
  FPopup.Left:=AOrigin.X;
  FPopup.Top:=AOrigin.Y;
  FPopup.AutoSizeForm;
  TRxCalendarGrid(FPopup.Calendar).NotInThisMonthColor := FNotInThisMonthColor;
  FAccept:=FPopup.ShowModal = mrOk;
  if CanFocus then SetFocus;

  if FAccept {and AcceptPopup(AValue) and EditCanModify }then
  begin
    AcceptValue(FPopup.Date);
    if Focused then inherited SelectAll;
  end;

{  FPopup.Show(AOrigin);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;}
end;

procedure TCustomRxDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

procedure TCustomRxDateEdit.Change;
begin
  if not FFormatting then inherited Change;
end;

procedure TCustomRxDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT]) and
    PopupVisible then
  begin
    TPopupCalendarAccess(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else
  if (Shift = []) and DirectInput then
  begin
    case Key of
      VK_ADD:
        begin
          ApplyDate(NvlDate(Date, Now) + 1);
          Key := 0;
        end;
      VK_SUBTRACT:
        begin
          ApplyDate(NvlDate(Date, Now) - 1);
          Key := 0;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomRxDateEdit.KeyPress(var Key: Char);
begin
  if (Key in ['T', 't', '+', '-']) and PopupVisible then
  begin
//    FPopup.KeyPress(Key);
    Key := #0;
  end
  else
  if DirectInput then
  begin
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        begin
          Key := #0;
        end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TCustomRxDateEdit.DoButtonClick(Sender: TObject);
var
  D: TDateTime;
  A: Boolean;
begin
  inherited DoButtonClick(Sender);
  if FPopup <> nil then
  begin
{    if FPopupVisible then
      PopupCloseUp(FPopup, True)
    else}
      PopupDropDown(True);
  end
  else
  if CalendarStyle = csDialog then
  begin
    D := Self.Date;
    A := SelectDate(D, DialogTitle, FStartOfWeek, FWeekends,
      FWeekendColor, FCalendarHints);
    if CanFocus then SetFocus;
    if A then
    begin
      if Assigned(FOnAcceptDate) then FOnAcceptDate(Self, D, A);
      if A then
      begin
        Self.Date := D;
//        if FFocused then
          inherited SelectAll;
      end;
    end;
  end;
end;

function TCustomRxDateEdit.GetDefaultGlyphName: String;
begin
  Result:='picDateEdit';
end;

constructor TCustomRxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlanksChar := ' ';
  FDialogTitle := sDateDlgTitle;
  FPopupColor := clWindow;
  FNotInThisMonthColor := clSilver;
  FPopupAlign := epaLeft;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FYearDigits := dyDefault;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := @CalendarHintsChanged;
  ControlState := ControlState + [csCreating];
  try
    UpdateFormat;
{$IFDEF DEFAULT_POPUP_CALENDAR}
    FPopup := CreatePopupCalendar(Self {$IFDEF USED_BiDi}, BiDiMode {$ENDIF});
    FPopup.OnCloseUp := @PopupCloseUp;
    FPopup.Color := FPopupColor;
{$ELSE}
    FPopup:=nil;
{$ENDIF DEFAULT_POPUP_CALENDAR}
//    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
//  Glyph:=LoadBitmapFromLazarusResource('picDateEdit');
  NumGlyphs := 2;
end;

destructor TCustomRxDateEdit.Destroy;
begin
  if Assigned(FPopup) then
  begin
    FPopup.OnCloseUp := nil;
    FreeAndNil(FPopup);
  end;
  TStringList(FCalendarHints).OnChange := nil;
  FreeAndNil(FCalendarHints);
  inherited Destroy;
end;

procedure TCustomRxDateEdit.CheckValidDate;
begin
  if TextStored then
    try
      FFormatting := True;
      try
        SetDate(StrToDateFmt(FDateFormat, Text));
      finally
        FFormatting := False;
      end;
    except
      if CanFocus then SetFocus;
      raise;
    end;
end;

function TCustomRxDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

procedure TCustomRxDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
{  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then
  begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;}
  UpdatePopup;
  SetDate(DateValue);
end;

initialization
  {$I tooledit.lrs}
end.

