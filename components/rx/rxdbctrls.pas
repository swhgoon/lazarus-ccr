unit RxDBCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, DB, DbCtrls, LMessages, LCLType;

type

  { TCustomRxDBProgressBar }

  TCustomRxDBProgressBar = class(TCustomProgressBar)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBProgressBar = class(TCustomRxDBProgressBar)
  published
    property DataField;
    property DataSource;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Smooth;
    property Step;
    property TabOrder;
    property TabStop;
    property Visible;
    property BarShowText;
  end;

type

  { TCustomRxDBTrackBar }

  TCustomRxDBTrackBar = class(TCustomTrackBar)
  private
    FDataLink: TFieldDataLink;
    FInScrollEvent:boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure LayoutChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);
    function IsReadOnly: boolean;
    procedure UpdateData(Sender: TObject);
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    procedure DoChange(var msg); message LM_CHANGED;
//    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  end;

  TRxDBTrackBar = class(TCustomRxDBTrackBar)
  published
    property DataField;
    property DataSource;
    property ReadOnly;

    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Orientation;
    property PageSize;
    property ParentShowHint;
    property PopupMenu;
    property ScalePos;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property Visible;
  end;

implementation
uses dbutils;

type
  TFieldDataLinkHack = class(TFieldDataLink)
  end;

{ TCustomRxDBProgressBar }

function TCustomRxDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomRxDBProgressBar.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Text := '';
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBProgressBar.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBProgressBar.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomRxDBProgressBar.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBProgressBar.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBProgressBar.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    Self.Position:=FDatalink.Field.AsInteger
  else
    Position:=Min
end;

constructor TCustomRxDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
end;

destructor TCustomRxDBProgressBar.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

{ TCustomRxDBTrackBar }

function TCustomRxDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomRxDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRxDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomRxDBTrackBar.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TCustomRxDBTrackBar.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TCustomRxDBTrackBar.DataChange(Sender: TObject);
begin
  FInScrollEvent:=true;
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    Self.Position:=FDatalink.Field.AsInteger
  else
    Self.Position:=0;
  FInScrollEvent:=false;
end;

procedure TCustomRxDBTrackBar.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    DataChange(Sender)
  else
  begin
    Position:=0;
    FDataLink.Reset;
  end;
end;

procedure TCustomRxDBTrackBar.LayoutChange(Sender: TObject);
begin
  DataChange(Sender);
end;

procedure TCustomRxDBTrackBar.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomRxDBTrackBar.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset;
end;

procedure TCustomRxDBTrackBar.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  if not FDatalink.Editing then
    FDatalink.Reset
  else
    TFieldDataLinkHack(FDatalink).UpdateData;
end;

function TCustomRxDBTrackBar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomRxDBTrackBar.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly := AValue;
end;

function TCustomRxDBTrackBar.IsReadOnly: boolean;
begin
  result := true;
  if FDatalink.Active and not Self.ReadOnly then
    result := (Field=nil) or Field.ReadOnly;
end;

procedure TCustomRxDBTrackBar.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and (FDataLink.Field.DataType in IntegerDataTypes) then
    FDataLink.Field.AsInteger := Self.Position;
end;

procedure TCustomRxDBTrackBar.DoChange(var msg);
begin
  inherited DoChange(Msg);
  if not FInScrollEvent then
  begin
    FDatalink.Edit;
    FDataLink.Modified;
  end;
end;

{
procedure TCustomRxDBTrackBar.Change;
begin
  FDatalink.Edit;
  FDataLink.Modified;
  inherited Change;
end;
}
procedure TCustomRxDBTrackBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TCustomRxDBTrackBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

constructor TCustomRxDBTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInScrollEvent:=false;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnLayoutChange := @LayoutChange;
end;

destructor TCustomRxDBTrackBar.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.

