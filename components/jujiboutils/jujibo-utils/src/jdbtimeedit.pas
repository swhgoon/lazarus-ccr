unit JDBTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, StdCtrls, DB, DBCtrls, LMessages, LCLType, Dialogs,
  SysUtils, jcontrolutils;

type
  TJDBTimeEdit = class(TCustomEdit)
  private
    { Private declarations }
    fFormat: string;
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
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

procedure Register;
begin
  {$I jdbtimeedit_icon.lrs}
  RegisterComponents('Data Controls', [TJDBTimeEdit]);
end;

{ TJDBDateEdit }

procedure TJDBTimeEdit.DataChange(Sender: TObject);
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

procedure TJDBTimeEdit.UpdateData(Sender: TObject);
var
  theValue: string;
begin
  if FDataLink.Field <> nil then
  begin
    theValue := NormalizeTime(Text, FDataLink.Field.AsDateTime);
    if Text = '' then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
    end
    else
    if IsValidTimeString(theValue) then
    begin
      FDataLink.Field.Text := theValue;
    end
    else
    begin
      ShowMessage(Caption + ' no es una hora válida');
      Caption := FDataLink.Field.AsString;
      SelectAll;
      SetFocus;
    end;
  end
  else
    Text := '';
end;

procedure TJDBTimeEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBTimeEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBTimeEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    if (fFormat <> '') and (not FDataLink.Field.IsNull) then
      Caption := FormatDateTime(fFormat, FDataLink.Field.AsDateTime)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBTimeEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBTimeEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBTimeEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBTimeEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBTimeEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

procedure TJDBTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBTimeEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBTimeEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBTimeEdit.KeyDown(var Key: word; Shift: TShiftState);
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

procedure TJDBTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0
  else
  if not IsReadOnly then
    FDatalink.Edit;
  inherited KeyPress(Key);
end;

procedure TJDBTimeEdit.DoEnter;
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  // Set default values
  //fFormat := ShortDateFormat;
end;

destructor TJDBTimeEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBTimeEdit.EditingDone;
begin
  inherited EditingDone;
  UpdateData(self);
end;


end.

