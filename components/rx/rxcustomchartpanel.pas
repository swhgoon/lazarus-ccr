unit rxcustomchartpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TRxCustomChart = class;
  TRxChartSeriesItem = class;
  
  { TRxChartValuesItem }

  TRxChartValuesItem = class(TCollectionItem)
  private
    FCaption: string;
    FColor: TColor;
    FVisible: boolean;
    FLabel:double;
    procedure SetCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
    procedure SetVisible(const AValue: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption:string read FCaption write SetCaption;
    property Color:TColor read FColor write SetColor;
    property Visible:boolean read FVisible write SetVisible;
    property LabelAsFloat:double read FLabel write FLabel;
    property LabelAsDateTime:TDateTime read FLabel write FLabel;
  end;
  
  { TRxChartValues }

  TRxChartValues = class(TCollection)
  private
    FRxChartSeriesItem:TRxChartSeriesItem;
  public
    constructor Create(ARxChartSeriesItem:TRxChartSeriesItem);
  end;
  
  { TRxChartSeriesItem }

  TRxChartSeriesItem = class(TCollectionItem)
  private
    FCaption: string;
    FColor: TColor;
    FVisible: boolean;
    FChartValues:TRxChartValues;
    function GetItems: TRxChartValues;
    procedure SetCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
    procedure SetItems(const AValue: TRxChartValues);
    procedure SetVisible(const AValue: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Values:TRxChartValues read GetItems write SetItems;
    property Caption:string read FCaption write SetCaption;
    property Color:TColor read FColor write SetColor;
    property Visible:boolean read FVisible write SetVisible;
  end;

  { TRxChartSeries }

  TRxChartSeries = class(TCollection)
  private
    FChart:TRxCustomChart;
    function GetToolbarItem(Index: Integer): TRxChartSeriesItem;
    procedure SetToolbarItem(Index: Integer; const AValue: TRxChartSeriesItem);
  public
    constructor Create(AChart:TRxCustomChart);
    property Items[Index: Integer]: TRxChartSeriesItem read GetToolbarItem write SetToolbarItem; default;
  published
  end;

  { TRxCustomChart }

  TRxCustomChart = class(TCustomPanel)
  private
    FChartSeries:TRxChartSeries;
    function GetItems: TRxChartSeries;
    procedure SetItems(const AValue: TRxChartSeries);
    { Private declarations }
  protected
    property ChartSeries:TRxChartSeries read GetItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TRxChart = class(TRxCustomChart)
  published
    property ChartSeries;
  
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
  
implementation



{ TRxChartSeriesItem }

procedure TRxChartSeriesItem.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

function TRxChartSeriesItem.GetItems: TRxChartValues;
begin
  Result:=FChartValues;
end;

procedure TRxChartSeriesItem.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;

procedure TRxChartSeriesItem.SetItems(const AValue: TRxChartValues);
begin
  FChartValues.Assign(AValue);
end;

procedure TRxChartSeriesItem.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

function TRxChartSeriesItem.GetDisplayName: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else
    Result:=inherited GetDisplayName;
end;

constructor TRxChartSeriesItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FChartValues:=TRxChartValues.Create(Self);
end;

destructor TRxChartSeriesItem.Destroy;
begin
  FreeAndNil(FChartValues);
  inherited Destroy;
end;

{ TRxChartSeries }

function TRxChartSeries.GetToolbarItem(Index: Integer): TRxChartSeriesItem;
begin
  result := TRxChartSeriesItem( inherited Items[Index] );
end;

procedure TRxChartSeries.SetToolbarItem(Index: Integer;
  const AValue: TRxChartSeriesItem);
begin
  Items[Index].Assign( AValue );
end;

constructor TRxChartSeries.Create(AChart: TRxCustomChart);
begin
  inherited Create(TRxChartSeriesItem);
  FChart:=AChart;
end;

{ TRxCustomChart }

function TRxCustomChart.GetItems: TRxChartSeries;
begin
  Result:=FChartSeries;
end;

procedure TRxCustomChart.SetItems(const AValue: TRxChartSeries);
begin
  FChartSeries.Assign(AValue);
end;

constructor TRxCustomChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChartSeries:=TRxChartSeries.Create(Self);
end;

destructor TRxCustomChart.Destroy;
begin
  FreeAndNil(FChartSeries);
  inherited Destroy;
end;

{ TRxChartValues }

constructor TRxChartValues.Create(ARxChartSeriesItem: TRxChartSeriesItem);
begin
  inherited Create(TRxChartValuesItem);
  FRxChartSeriesItem := ARxChartSeriesItem;
end;

{ TRxChartValuesItem }

procedure TRxChartValuesItem.SetCaption(const AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TRxChartValuesItem.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;

procedure TRxChartValuesItem.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

function TRxChartValuesItem.GetDisplayName: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else
    Result:=inherited GetDisplayName;
end;

constructor TRxChartValuesItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TRxChartValuesItem.Destroy;
begin
  inherited Destroy;
end;

end.
