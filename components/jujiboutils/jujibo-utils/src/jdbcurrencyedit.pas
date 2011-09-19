unit jdbcurrencyedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, StdCtrls, DB, DBCtrls, LMessages, LCLType, Dialogs,
  SysUtils;

type

  { TJDBCurrencyEdit }

  TJDBCurrencyEdit = class(TEdit)
  private
    fFormat: string;
    FDataLink: TFieldDataLink;
    fDecimales: integer;

    procedure DataChange(Sender: TObject);
    function getDecimals: integer;
    procedure setDecimals(AValue: integer);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function IsReadOnly: boolean;

    function getFormat: string;
    procedure setFormat(const AValue: string);
    procedure formatInput;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;

    function IsValidCurrency(const Value: string): boolean;
    function ScaleTo(const AValue: currency; const NDecimals: integer): currency;

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    function GetReadOnly: boolean; override;
    procedure SetReadOnly(Value: boolean); override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property Field: TField read GetField;

  published
    property DisplayFormat: string read getFormat write setFormat;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Decimals: integer read getDecimals write setDecimals;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;

    // From TEdit
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  {$I currencydbicon.lrs}
  RegisterComponents('Data Controls', [TJDBCurrencyEdit]);
end;

{ TJDBCurrencyEdit }

procedure TJDBCurrencyEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if not Focused then
      formatInput
    else
      Caption := FDataLink.Field.AsString;
  end
  else
    Text := '';
end;

function TJDBCurrencyEdit.getDecimals: integer;
begin
  Result := fDecimales;
end;

procedure TJDBCurrencyEdit.setDecimals(AValue: integer);
begin
  if AValue >= 0 then
    fDecimales := AValue;
end;


procedure TJDBCurrencyEdit.UpdateData(Sender: TObject);
var
  theValue: currency;
begin
  if FDataLink.Field <> nil then
  begin
    if IsValidCurrency(Text) then
    begin
      theValue := StrToCurr(Text);
      theValue := ScaleTo(theValue, fDecimales);
      Text := CurrToStr(theValue);
      FDataLink.Field.Text := Text;
    end
    else
    begin
      if FDataLink.Field <> nil then
      begin
        ShowMessage(Caption + ' no es un valor válido');
        Caption := FDataLink.Field.AsString;
        SelectAll;
        SetFocus;
      end;
    end;
  end
  else
    Text := '';
end;

procedure TJDBCurrencyEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBCurrencyEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBCurrencyEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBCurrencyEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBCurrencyEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBCurrencyEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBCurrencyEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBCurrencyEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    //FDataLink.Field.DisplayText -> formatted  (tdbgridcolumns/persistent field DisplayFormat
    if fFormat <> '' then
      Caption := FormatFloat(fFormat, FDataLink.Field.AsCurrency)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBCurrencyEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBCurrencyEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBCurrencyEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBCurrencyEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBCurrencyEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

function TJDBCurrencyEdit.IsValidCurrency(const Value: string): boolean;
begin
  if StrToCurrDef(Value, MaxCurrency) = MaxCurrency then
    Result := False
  else
    Result := True;
end;

function TJDBCurrencyEdit.ScaleTo(const AValue: currency;
  const NDecimals: integer): currency;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

procedure TJDBCurrencyEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBCurrencyEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBCurrencyEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBCurrencyEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_DELETE, VK_BACK] then
  begin
    if not IsReadOnly then
      FDatalink.Edit
    else
      Key := VK_UNKNOWN;
  end;
end;

procedure TJDBCurrencyEdit.KeyPress(var Key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimales = 0) then
    Key := #0;

  if (Key <> #0) and (not IsReadOnly) then
    FDatalink.Edit;
  inherited KeyPress(Key);
end;

procedure TJDBCurrencyEdit.DoEnter;
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBCurrencyEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  // Set default values
  //fDecimales := 2;
  //fFormat := '0.00';
end;

destructor TJDBCurrencyEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBCurrencyEdit.EditingDone;
begin
  inherited EditingDone;
  UpdateData(self);
end;

end.

