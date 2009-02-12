unit RxDBTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  RxTimeEdit, DB, DbCtrls, LMessages, LCLType;

type

  { TCustomRxDBTimeEdit }

  TCustomRxDBTimeEdit = class(TCustomRxTimeEdit)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);

    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    function IsReadOnly: boolean;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBTimeEdit = class(TCustomRxDBTimeEdit)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonHint;
    property CharCase;
    property Color;
//    property DirectInput;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
//    property Flat;
    property Font;
//    property Glyph;
    property MaxLength;
//    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


implementation
uses dbutils;

type
  TFieldDataLinkHack = class(TFieldDataLink)
  end;

{ TCustomRxDBTimeEdit }

procedure TCustomRxDBTimeEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in TimeDataTypes) then
    Self.Time:=FDatalink.Field.AsDateTime
  else
    Text := '';
end;

function TCustomRxDBTimeEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBTimeEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBTimeEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomRxDBTimeEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomRxDBTimeEdit.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBTimeEdit.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBTimeEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

procedure TCustomRxDBTimeEdit.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in TimeDataTypes) then
  begin
    FDataLink.Field.AsDateTime := Self.Time;
  end;
end;

procedure TCustomRxDBTimeEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

procedure TCustomRxDBTimeEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBTimeEdit.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBTimeEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

function TCustomRxDBTimeEdit.IsReadOnly: boolean;
begin
  result := true;
  if FDatalink.Active and not Self.ReadOnly then
    result := (Field=nil) or Field.ReadOnly;
end;

procedure TCustomRxDBTimeEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TCustomRxDBTimeEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    TFieldDataLinkHack(FDatalink).UpdateData;
end;

procedure TCustomRxDBTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key=VK_ESCAPE then
  begin
    //cancel out of editing by reset on esc
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key=VK_DELETE then
  begin
    if not IsReadOnly then
      FDatalink.Edit;
  end
  else
  if Key=VK_TAB then
  begin
    if FDataLink.CanModify and FDatalink.Editing then
      FDataLink.UpdateRecord;
  end;
end;

procedure TCustomRxDBTimeEdit.Change;
begin
  FDatalink.Edit;
  FDataLink.Modified;
  inherited Change;
end;

procedure TCustomRxDBTimeEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomRxDBTimeEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

constructor TCustomRxDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
end;

destructor TCustomRxDBTimeEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
