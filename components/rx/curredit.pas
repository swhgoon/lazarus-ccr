unit curredit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages, MaskEdit;

type

  { TCustomNumEdit }

  TCustomNumEdit = class(TCustomMaskEdit)
  private
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FBeepOnError: Boolean;
    FCheckOnExit: Boolean;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: string;
    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    FMaxValue: Extended;
    FMinValue: Extended;
    FValue: Extended;
    FFocused: Boolean;
    FZeroEmpty: Boolean;
    function GetAsInteger: Longint;
    function GetText: string;
    function GetValue: Extended;
    function IsFormatStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetAsInteger(const AValue: Longint);
    procedure SetBeepOnError(const AValue: Boolean);
    procedure SetDecimalPlaces(const AValue: Cardinal);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetFormatOnEditing(const AValue: Boolean);
    procedure SetMaxValue(const AValue: Extended);
    procedure SetMinValue(const AValue: Extended);
    procedure SetText(const AValue: string);
    procedure SetValue(const AValue: Extended);
    procedure SetZeroEmpty(const AValue: Boolean);
    function TextToValText(const AValue: string): string;
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    procedure SetFocused(Value: Boolean);
  protected
    //messages
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMPaste(var Message: TLMessage); message LM_PASTE;
    procedure GetSel(var ASelStart: Integer; var SelStop: Integer);
    procedure DoEnter; override;
    procedure DoExit; override;
//    procedure AcceptValue(const Value: Variant); override;
    procedure Change; override;
    procedure ReformatEditText; dynamic;
    procedure DataChanged; virtual;
    function DefaultDisplayFormat: string; virtual;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    function FormatDisplayText(Value: Extended): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Formatting: Boolean read FFormatting;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError
      default True;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces
      default 2;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat stored IsFormatStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property FormatOnEditing: Boolean read FFormatOnEditing
      write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property Value: Extended read GetValue write SetValue;
  published
    { Published declarations }
  end;
  
  { TCurrencyEdit }

  TCurrencyEdit = class(TCustomNumEdit)
  protected
    function DefaultDisplayFormat: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property BorderSpacing;
    property CheckOnExit;
    property Color;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
//    property HideSelection;
    property Anchors;
//    property BiDiMode;
    property Constraints;
    property DragKind;
//    property ParentBiDiMode;
{$IFDEF WIN32}
  {$IFNDEF VER90}
//    property ImeMode;
//    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

implementation
uses rxstrutils, Math, tooledit;

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue, fvExtended);
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in ['-', '+']);
  if IsSign then MinSym := 2
  else MinSym := 1;
  I := Pos(DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then begin
      Group := 0;
      Result := ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

{ TCustomNumEdit }

function TCustomNumEdit.GetAsInteger: Longint;
begin
  Result := Trunc(Value);
end;

function TCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(FValue);
end;

procedure TCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

constructor TCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  MaxLength := 0;
  FBeepOnError := True;
  FAlignment := taRightJustify;
  FDisplayFormat := DefaultDisplayFormat;
  FDecimalPlaces := 2;
  FZeroEmpty := True;
//  inherited Text := '';
//  inherited Alignment := taLeftJustify;
//  FDefNumGlyphs := 2;
  { forces update }
  DataChanged;
  ControlState := ControlState + [csCreating];
end;

destructor TCustomNumEdit.Destroy;
begin
  FCanvas.Free;
{  if FPopup <> nil then begin
    TPopupWindow(FPopup).OnCloseUp := nil;
    FPopup.Free;
    FPopup := nil;
  end;}
  inherited Destroy;
end;

function TCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TCustomNumEdit.GetValue: Extended;
begin
  if not (csDesigning in ComponentState) then
    try
      UpdateData;
    except
      FValue := FMinValue;
    end;
  Result := FValue;
end;

function TCustomNumEdit.IsFormatStored: boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

procedure TCustomNumEdit.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  Invalidate;
end;

procedure TCustomNumEdit.SetAsInteger(const AValue: Longint);
begin
  SetValue(AValue);
end;

procedure TCustomNumEdit.SetBeepOnError(const AValue: Boolean);
begin
  if FBeepOnError=AValue then exit;
  FBeepOnError:=AValue;
end;

procedure TCustomNumEdit.SetDecimalPlaces(const AValue: Cardinal);
begin
  if FDecimalPlaces=AValue then exit;
  FDecimalPlaces:=AValue;
  DataChanged;
  Invalidate;
end;

procedure TCustomNumEdit.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat=AValue then exit;
  FDisplayFormat:=AValue;
  Invalidate;
  DataChanged;
end;

procedure TCustomNumEdit.SetFormatOnEditing(const AValue: Boolean);
begin
  if FFormatOnEditing=AValue then exit;
  FFormatOnEditing:=AValue;
  if FFormatOnEditing then
//    FAlignment := AValue
  else
    FAlignment := taLeftJustify;
  if FFormatOnEditing and FFocused then
    ReformatEditText
  else
  if FFocused then
  begin
    UpdateData;
    DataChanged;
  end;
end;

procedure TCustomNumEdit.SetMaxValue(const AValue: Extended);
begin
  if FMaxValue=AValue then exit;
  FMaxValue:=AValue;
  if Value > AValue then
    Value:=AValue;
end;

procedure TCustomNumEdit.SetMinValue(const AValue: Extended);
begin
  if FMinValue=AValue then exit;
  FMinValue:=AValue;
  if Value < AValue then
    Value:=AValue;
end;

procedure TCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then
  begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TCustomNumEdit.SetValue(const AValue: Extended);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

procedure TCustomNumEdit.SetZeroEmpty(const AValue: Boolean);
begin
  if FZeroEmpty=AValue then exit;
  FZeroEmpty:=AValue;
  DataChanged;
end;

function TCustomNumEdit.TextToValText(const AValue: string): string;
begin
  Result := DelRSpace(AValue);
  if DecimalSeparator <> ThousandSeparator then begin
    Result := DelChars(Result, ThousandSeparator);
  end;
  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', DecimalSeparator);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', DecimalSeparator);
  if Result = '' then Result := '0'
  else if Result = '-' then Result := '-0';
end;

function TCustomNumEdit.CheckValue(NewValue: Extended; RaiseOnError: Boolean
  ): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if (FMaxValue > FMinValue) then begin
      if NewValue < FMinValue then Result := FMinValue
      else if NewValue > FMaxValue then Result := FMaxValue;
    end
    else begin
      if FMaxValue = 0 then begin
        if NewValue < FMinValue then Result := FMinValue;
      end
      else if FMinValue = 0 then begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateFmt(ReplaceStr('SOutOfRange %d %d %d %d', '%d', '%.*f'),
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;

procedure TCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;

procedure TCustomNumEdit.CMEnabledChanged(var Message: TLMessage);
begin
  inherited;
  if NewStyleControls and not FFocused then Invalidate;
end;

procedure TCustomNumEdit.CMEnter(var Message: TLMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited;
end;

procedure TCustomNumEdit.WMExit(var Message: TLMExit);
begin
  inherited;
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  Cursor:=0;
  DoExit;
end;

procedure TCustomNumEdit.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomNumEdit.WMPaint(var Message: TLMPaint);
var
  S: string;
begin
  S := GetDisplayText;
//  if not FFocused then
//  else
  if not PaintComboEdit(Self, S, FAlignment, FFocused {and not PopupVisible}, FCanvas, Message) then
      inherited WMPaint(Message);
end;

procedure TCustomNumEdit.WMPaste(var Message: TLMessage);
var
  S: string;
begin
  S := EditText;
  try
    inherited;
    UpdateData;
  except
    EditText := S;
    SelectAll;
    if CanFocus then SetFocus;
//    if BeepOnError then MessageBeep(0);
  end;
end;

procedure TCustomNumEdit.GetSel(var ASelStart: Integer; var SelStop: Integer);
begin
  ASelStart:=SelStart;
  SelStop:=SelStart + SelLength;
end;

procedure TCustomNumEdit.DoEnter;
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited DoEnter;
end;

procedure TCustomNumEdit.DoExit;
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  Cursor:=0;

  inherited DoExit;
  Invalidate;
end;

{procedure TCustomNumEdit.AcceptValue(const Value: Variant);
begin
  inherited AcceptValue(Value);
end;}

procedure TCustomNumEdit.Change;
begin
  if not FFormatting then
  begin
    if FFormatOnEditing and FFocused then ReformatEditText;
    inherited Change;
  end;
end;

procedure TCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: Boolean;
  OldLen, ASelStart, SelStop: Integer;
begin
  FFormatting := True;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if HandleAllocated then GetSel(ASelStart, SelStop);
    if not IsEmpty then S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
{    if HandleAllocated and (GetFocus = Handle) and not
      (csDesigning in ComponentState) then
    begin
      Inc(ASelStart, Length(S) - OldLen);
      SetCursor(ASelStart);
    end;}
  finally
    FFormatting := False;
  end;
end;

procedure TCustomNumEdit.DataChanged;
var
  EditFormat: string;
begin
  EditFormat := '0';
  if FDecimalPlaces > 0 then
    EditFormat := EditFormat + '.' + MakeStr('#', FDecimalPlaces);
  if (FValue = 0.0) and FZeroEmpty then
    EditText := ''
  else
    EditText := FormatFloat(EditFormat, FValue);
end;

function TCustomNumEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

procedure TCustomNumEdit.KeyPress(var Key: Char);
begin
{  if PopupVisible and (UpCase(Key) in ['0'..'9', DecimalSeparator, '.', ',',
    '+', '-', '*', '/', '_', '=', 'C', 'R', 'Q', '%', #8, #13] -
    [ThousandSeparator]) then
  begin
    THack(FPopup).KeyPress(Key);
    Key := #0;
  end;}
  if Key in ['.', ','] - [ThousandSeparator] then
    Key := DecimalSeparator;
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and not IsValidChar(Key) then begin
//    if BeepOnError then MessageBeep(0);
    Key := #0;
  end
  else if Key = #27 then begin
    Reset;
    Key := #0;
  end;
end;

function TCustomNumEdit.IsValidChar(Key: Char): Boolean;
var
  S: string;
  ASelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  S := EditText;
  GetSel(ASelStart, SelStop);
  System.Delete(S, ASelStart + 1, SelStop - ASelStart);
  System.Insert(Key, S, ASelStart + 1);
  S := TextToValText(S);
  DecPos := Pos(DecimalSeparator, S);
  if (DecPos > 0) then
  begin
    ASelStart := Pos('E', UpperCase(S));
    if (ASelStart > DecPos) then
      DecPos := ASelStart - DecPos
    else
      DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then
      Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

function TCustomNumEdit.FormatDisplayText(Value: Extended): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else
    Result := FloatToStr(Value);
end;

procedure TCustomNumEdit.Clear;
begin

end;

{ TCurrencyEdit }

function TCurrencyEdit.DefaultDisplayFormat: string;
var
  CurrStr: string;
  I: Integer;
  C: Char;
begin
  Result := ',0.' + MakeStr('0', CurrencyDecimals);
  CurrStr := '';
  for I := 1 to Length(CurrencyString) do
  begin
    C := CurrencyString[I];
    if C in [',', '.'] then
    begin
      CurrStr := CurrStr + '''' + C + ''''
    end
    else CurrStr := CurrStr + C;
  end;
  if Length(CurrStr) > 0 then
    case CurrencyFormat of
      0: Result := CurrStr + Result; { '$1' }
      1: Result := Result + CurrStr; { '1$' }
      2: Result := CurrStr + ' ' + Result; { '$ 1' }
      3: Result := Result + ' ' + CurrStr; { '1 $' }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

constructor TCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
//    ButtonWidth := 0;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

end.
