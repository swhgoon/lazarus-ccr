unit dbdateedit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, LMessages, LCLType, Controls, Graphics,
  DB, DbCtrls, EditBtn, tooledit;

type

  { TDBDateEdit }
{$IFDEF DBDateEdit_OLD}
  TDBDateEdit = class(TDateEdit)
{$ELSE}
  TDBDateEdit = class(TRxDateEdit)
{$ENDIF}
  private
    FDataLink:TFieldDataLink;
    FDefaultToday: Boolean;
    procedure DoCheckEnable;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
  protected
    procedure ActiveChange(Sender:TObject);
    procedure DataChange(Sender:TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender:TObject);
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure LMCut(var Message: TLMessage); message LM_CUT;
    procedure LMPaste(var Message: TLMessage); message LM_PASTE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditingDone; override;
    Procedure RunDialog; virtual;
    procedure DoButtonClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
  end;


  { TRxDBCalcEdit }

  TRxDBCalcEdit = class(TCalcEdit)
  private
    FDataLink: TFieldDataLink;
    procedure DoCheckEnable;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
  protected
    procedure ActiveChange(Sender:TObject);
    procedure DataChange(Sender:TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender:TObject);
    procedure CMExit(var Message:TLMessage); message CM_EXIT;
    procedure LMCut(var Message: TLMessage); message LM_CUT;
    procedure LMPaste(var Message: TLMessage); message LM_PASTE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditingDone; override;
    Procedure RunDialog; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;
  
implementation
uses DateUtil;

{ TDBDateEdit }

procedure TDBDateEdit.DoCheckEnable;
begin
  Enabled:=FDataLink.Active and (FDataLink.Field<>nil) and (not FDataLink.Field.ReadOnly);
end;

function TDBDateEdit.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBDateEdit.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBDateEdit.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBDateEdit.SetDataField(const AValue: string);
begin
  try
    FDataLink.FieldName:=AValue;
  finally
    DoCheckEnable;
  end;
end;

procedure TDBDateEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource:=AValue;
  DoCheckEnable;
end;

procedure TDBDateEdit.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBDateEdit.CMExit(var Message: TLMessage);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    SelectAll;
    raise;
  end;
  inherited;
end;

procedure TDBDateEdit.LMCut(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBDateEdit.LMPaste(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
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
  if (Key<>VK_UNKNOWN) then
  begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't realy in edit
    FDataLink.Edit;
  end;
end;

procedure TDBDateEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TDBDateEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  // if the datasource is being removed then we need to make sure
  // we are updated or we can get AV/Seg's *cough* as I foolishly
  // discovered firsthand....
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBDateEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TDBDateEdit.RunDialog;
begin
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TDBDateEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

procedure TDBDateEdit.ActiveChange(Sender: TObject);
begin
  DoCheckEnable;
end;

procedure TDBDateEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
    (FDataLink.Field is TDateTimeField) then
  begin
    if FDataLink.Field.IsNull then
      Text:=''
    else
      Date:=FDataLink.Field.AsDateTime
  end
  else Text:='';
end;

procedure TDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;
end;

procedure TDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  if Assigned(FDataLink.Field) then
  begin
    D := Self.Date;
    if (D <> NullDate) then
      FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime)
    else
      FDataLink.Field.Clear;
  end;
end;

constructor TDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnActiveChange:=@ActiveChange;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
  Text:='';
end;

destructor TDBDateEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;


{ TRxDBCalcEdit }

procedure TRxDBCalcEdit.DoCheckEnable;
begin
  Enabled:=FDataLink.Active and (FDataLink.Field<>nil) and (not FDataLink.Field.ReadOnly);
end;

function TRxDBCalcEdit.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TRxDBCalcEdit.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TRxDBCalcEdit.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TRxDBCalcEdit.SetDataField(const AValue: string);
begin
  try
    FDataLink.FieldName:=AValue;
  finally
    DoCheckEnable;
  end;
end;

procedure TRxDBCalcEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource:=AValue;
  DoCheckEnable;
end;

procedure TRxDBCalcEdit.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TRxDBCalcEdit.ActiveChange(Sender: TObject);
begin
  DoCheckEnable;
end;

procedure TRxDBCalcEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
    (FDataLink.Field is TNumericField) then
  begin
    if FDataLink.Field.IsNull then
      Text:=''
    else
      Self.AsFloat:=FDataLink.Field.AsFloat;
  end
  else Text:='';
end;

procedure TRxDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
{  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;}
end;

procedure TRxDBCalcEdit.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if Self.Text<>'' then
      FDataLink.Field.AsFloat := Self.AsFloat
    else
      FDataLink.Field.Clear;
  end;
end;

procedure TRxDBCalcEdit.CMExit(var Message: TLMessage);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    SelectAll;
    raise;
  end;
  inherited;
end;

procedure TRxDBCalcEdit.LMCut(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBCalcEdit.LMPaste(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
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
  if (Key<>VK_UNKNOWN) then
  begin
    //make sure we call edit to ensure the datset is in edit,
    //this is for where the datasource is in autoedit, so we aren't
    //read only even though the dataset isn't realy in edit
    FDataLink.Edit;
  end;
end;

procedure TRxDBCalcEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TRxDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // if the datasource is being removed then we need to make sure
  // we are updated or we can get AV/Seg's *cough* as I foolishly
  // discovered firsthand....
  if (Operation=opRemove) then
  begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TRxDBCalcEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

procedure TRxDBCalcEdit.RunDialog;
begin
  inherited RunDialog;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

constructor TRxDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnActiveChange:=@ActiveChange;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

destructor TRxDBCalcEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

end.
