unit rxDateRangeEditUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Controls, Buttons, StdCtrls, Spin;

type

  { TRxCustomDateRangeEdit }

  TRxCustomDateRangeEdit = class(TCustomControl)
  private
    FOnEditChange: TNotifyEvent;
    FOnEditClick: TNotifyEvent;
    FOnEditEnter: TNotifyEvent;
    FOnEditExit: TNotifyEvent;
    FsbDecYear: TSpeedButton;
    FsbDecMonth: TSpeedButton;
    FsbIncYear: TSpeedButton;
    FsbIncMonth: TSpeedButton;
    FEditYear: TSpinEdit;
    FEditMonth: TComboBox;
    procedure DoIncMonth(Sender: TObject);
    procedure DoIncYear(Sender: TObject);
    procedure DoDecMonth(Sender: TObject);
    procedure DoDecYear(Sender: TObject);
    function GetMonth: word;
    function GetPeriod: TDateTime;
    function GetYear: word;
    procedure SetMonth(AValue: word);
    procedure SetPeriod(AValue: TDateTime);
    procedure SetYear(AValue: word);
    procedure InternalOnEditChange(Sender: TObject);
    procedure InternalOnEditClick(Sender: TObject);
    procedure InternalOnEditEnter(Sender: TObject);
    procedure InternalOnEditExit(Sender: TObject);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure FillMonthNames;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure EditChange; virtual;
    procedure EditClick; virtual;
    procedure EditEnter; virtual;
    procedure EditExit; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Year:word read GetYear write SetYear;
    property Month:word read GetMonth write SetMonth;
    property Period:TDateTime read GetPeriod write SetPeriod;
    property OnChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnEnter: TNotifyEvent read FOnEditEnter write FOnEditEnter;
    property OnExit: TNotifyEvent read FOnEditExit write FOnEditExit;
  end;

type
  TRxDateRangeEdit = class(TRxCustomDateRangeEdit)
  published
    property Autosize default True;
    property Year;
    property Month;
    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
  end;

implementation

{ TRxCustomDateRangeEdit }

procedure TRxCustomDateRangeEdit.DoIncMonth(Sender: TObject);
begin
  if FEditMonth.ItemIndex < 11 then
    FEditMonth.ItemIndex := FEditMonth.ItemIndex + 1
  else
  begin
    FEditMonth.ItemIndex := 0;
    FEditYear.Value:=FEditYear.Value + 1;
  end;
end;

procedure TRxCustomDateRangeEdit.DoIncYear(Sender: TObject);
begin
  FEditYear.Value:=FEditYear.Value + 1;
end;

procedure TRxCustomDateRangeEdit.DoDecMonth(Sender: TObject);
begin
  if FEditMonth.ItemIndex > 0 then
    FEditMonth.ItemIndex := FEditMonth.ItemIndex - 1
  else
  begin
    FEditMonth.ItemIndex := 11;
    FEditYear.Value:=FEditYear.Value - 1;
  end;
end;

procedure TRxCustomDateRangeEdit.DoDecYear(Sender: TObject);
begin
  FEditYear.Value:=FEditYear.Value - 1;
end;

function TRxCustomDateRangeEdit.GetMonth: word;
begin
  Result:=FEditMonth.ItemIndex+1;
end;

function TRxCustomDateRangeEdit.GetPeriod: TDateTime;
begin
  Result:=EncodeDate(Year, Month, 1);
end;

function TRxCustomDateRangeEdit.GetYear: word;
begin
  Result:=FEditYear.Value;
end;

procedure TRxCustomDateRangeEdit.SetMonth(AValue: word);
begin
  if (AValue>0) and (AValue < 13) then
    FEditMonth.ItemIndex:=AValue-1;
end;

procedure TRxCustomDateRangeEdit.SetPeriod(AValue: TDateTime);
var
  Y, M, D: word;
begin
  DecodeDate(AValue, Y, M, D);
  FEditMonth.ItemIndex:=M-1;
  FEditYear.Value:=Y;
end;

procedure TRxCustomDateRangeEdit.SetYear(AValue: word);
begin
  FEditYear.Value:=AValue;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditChange(Sender: TObject);
begin
  EditChange;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditClick(Sender: TObject);
begin
  EditClick;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditEnter(Sender: TObject);
begin
  EditEnter;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditExit(Sender: TObject);
begin
  EditExit;
end;

class function TRxCustomDateRangeEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 80 + 70 + 23 * 4;
  Result.CY := 23;
end;

procedure TRxCustomDateRangeEdit.FillMonthNames;
var
  i: Integer;
begin
  for i:=1 to 12 do
    FEditMonth.Items.Add(DefaultFormatSettings.LongMonthNames[i]);
end;

procedure TRxCustomDateRangeEdit.SetAutoSize(AValue: Boolean);
begin
  if AutoSize = AValue then
    Exit;
  inherited SetAutosize(AValue);
  FEditMonth.AutoSize := AValue;
  FEditYear.AutoSize := AValue;
end;

procedure TRxCustomDateRangeEdit.EditChange;
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TRxCustomDateRangeEdit.EditClick;
begin
  if Assigned(FOnEditClick) then FOnEditClick(Self);
end;

procedure TRxCustomDateRangeEdit.EditEnter;
begin
  if Assigned(FOnEditEnter) then FOnEditEnter(Self);
end;

procedure TRxCustomDateRangeEdit.EditExit;
begin
  if Assigned(FOnEditExit) then FOnEditExit(Self);
end;

constructor TRxCustomDateRangeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditYear:=TSpinEdit.Create(Self);
  FEditMonth:=TComboBox.Create(Self);
  FEditMonth.Style:=csDropDownList;
  FEditYear.Width:=70;
  FEditMonth.Width:=80;

  FsbDecYear:=TSpeedButton.Create(Self);
  FsbDecMonth:=TSpeedButton.Create(Self);
  FsbIncYear:=TSpeedButton.Create(Self);
  FsbIncMonth:=TSpeedButton.Create(Self);

  FsbDecYear.OnClick:=@DoDecYear;
  FsbDecMonth.OnClick:=@DoDecMonth;
  FsbIncYear.OnClick:=@DoIncYear;
  FsbIncMonth.OnClick:=@DoIncMonth;


  FEditYear.Parent:=Self;
  FsbDecYear.Parent:=Self;
  FsbDecMonth.Parent:=Self;
  FsbIncYear.Parent:=Self;
  FsbIncMonth.Parent:=Self;
  FEditMonth.Parent:=Self;

  FsbDecYear.Caption:='<<';
  FsbDecMonth.Caption:='<';
  FsbIncYear.Caption:='>>';
  FsbIncMonth.Caption:='>';

  FsbDecYear.Left:=0;
  FsbDecMonth.Left:=23;
  FEditMonth.Left:=46;
  FEditYear.Left:=126;
  FsbIncMonth.Left:=206;
  FsbIncYear.Left:=229;


  ControlStyle := ControlStyle + [csNoFocus];


  FsbDecYear.Align:=alLeft;
  FsbDecMonth.Align:=alLeft;
  FsbIncYear.Align:=alRight;
  FsbIncMonth.Align:=alRight;

  FEditYear.Align:=alRight;
  FEditMonth.Align:=alClient;

  FEditYear.MaxValue:=9999;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FillMonthNames;

  SetPeriod(Now);
  AutoSize := True;

  FEditMonth.OnChange:=@InternalOnEditChange;
  FEditYear.OnChange:=@InternalOnEditChange;

  FEditMonth.OnClick:=@InternalOnEditClick;
  FEditYear.OnClick:=@InternalOnEditClick;

  FEditMonth.OnEnter:=@InternalOnEditEnter;
  FEditYear.OnEnter:=@InternalOnEditEnter;

  FEditMonth.OnExit:=@InternalOnEditExit;
  FEditYear.OnExit:=@InternalOnEditExit;
end;

destructor TRxCustomDateRangeEdit.Destroy;
begin
  inherited Destroy;
end;

end.

