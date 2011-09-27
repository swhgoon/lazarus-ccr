unit JDBFloatEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, StdCtrls, DB, DBCtrls, LMessages, LCLType, Dialogs,
  SysUtils;

type

  { TJDBFloatEdit }

  TJDBFloatEdit = class(TCustomEdit)
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

    function IsValidFloat(const Value: string): boolean;
    function ScaleTo(const AValue: double; const NDecimals: integer): double;

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
  {$I jdbfloatedit_icon.lrs}
  RegisterComponents('Data Controls', [TJDBFloatEdit]);
end;


procedure TJDBFloatEdit.DataChange(Sender: TObject);
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

function TJDBFloatEdit.getDecimals: integer;
begin
  Result := fDecimales;
end;

procedure TJDBFloatEdit.setDecimals(AValue: integer);
begin
  if AValue >= 0 then
    fDecimales := AValue;
end;


procedure TJDBFloatEdit.UpdateData(Sender: TObject);
var
  theValue: double;
begin
  if FDataLink.Field <> nil then
  begin
    if IsValidFloat(Text) then
    begin
      theValue := StrToFloat(Text);
      theValue := ScaleTo(theValue, fDecimales);
      Text :=  FloatToStr(theValue);
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

procedure TJDBFloatEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBFloatEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBFloatEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBFloatEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBFloatEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBFloatEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBFloatEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBFloatEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    //FDataLink.Field.DisplayText -> formatted  (tdbgridcolumns/persistent field DisplayFormat
    if fFormat <> '' then
      Caption := FormatFloat(fFormat, FDataLink.Field.AsFloat)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBFloatEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBFloatEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBFloatEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBFloatEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBFloatEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

function TJDBFloatEdit.IsValidFloat(const Value: string): boolean;
begin
  if StrToFloatDef(Value, MaxDouble) = MaxDouble then
    Result := False
  else
    Result := True;
end;

function TJDBFloatEdit.ScaleTo(const AValue: double;
  const NDecimals: integer): double;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

procedure TJDBFloatEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBFloatEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBFloatEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBFloatEdit.KeyDown(var Key: word; Shift: TShiftState);
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

procedure TJDBFloatEdit.KeyPress(var Key: char);
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

procedure TJDBFloatEdit.DoEnter;
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBFloatEdit.Create(TheOwner: TComponent);
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

destructor TJDBFloatEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBFloatEdit.EditingDone;
begin
  inherited EditingDone;
  if DataSource.State in [dsEdit, dsInsert] then
    UpdateData(self);
end;


end.

