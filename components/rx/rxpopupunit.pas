unit rxpopupunit;

{$I rx.inc}

interface

uses
  Classes, SysUtils, DB, Forms, DBGrids, rxdbgrid, LCLType, Controls, ComCtrls,
  Buttons, Grids, Graphics, vclutils;

type
  TPopUpCloseEvent = procedure(AResult:boolean) of object;
  TPopUpFormOptions = class;
  
  { TPopUpGrid }

  TPopUpGrid = class(TRxDBGrid)
  private
    FFindLine:string;
    FLookupDisplayIndex: integer;
    FLookupDisplayField:string;
    procedure ClearFind;
    procedure FindNextChar(AChar:Char);
    procedure FindPriorChar;
    procedure SetLookupDisplayIndex(const AValue: integer);
  protected
    procedure KeyPress(var Key: char); dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property LookupDisplayIndex:integer read FLookupDisplayIndex write SetLookupDisplayIndex;
  end;
  
  TPopUpGridOption = (pfgIndicator, pfgColLines, pfgRowLines, pfgColumnResize,
    pfgColumnMove);

  TPopUpGridOptions = set of TPopUpGridOption;

  { TPopUpColumnTitle }

  TPopUpColumnTitle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FCaption: string;
    FColor: TColor;
    FLayout: TTextLayout;
    FOrientation: TTextOrientation;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetOrientation(const AValue: TTextOrientation);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Orientation:TTextOrientation read FOrientation write SetOrientation;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Layout: TTextLayout read FLayout write SetLayout;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
  end;

  TPopUpColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FDisplayFormat: string;
    FFieldName: string;
    FFont: TFont;
    FImageList: TImageList;
    FTitle: TPopUpColumnTitle;
    FValueChecked: string;
    FValueUnchecked: string;
    FWidth: Integer;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetColor(const AValue: TColor);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetFieldName(const AValue: string);
    procedure SetFont(const AValue: TFont);
    procedure SetImageList(const AValue: TImageList);
    procedure SetTitle(const AValue: TPopUpColumnTitle);
    procedure SetValueChecked(const AValue: string);
    procedure SetValueUnchecked(const AValue: string);
    procedure SetWidth(const AValue: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Color: TColor read FColor write SetColor;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property Font: TFont read FFont write SetFont;
    property FieldName:string read FFieldName write SetFieldName;
    property ImageList:TImageList read FImageList write SetImageList;
    property ValueChecked: string read FValueChecked write SetValueChecked;
    property ValueUnchecked: string read FValueUnchecked write SetValueUnchecked;
    property Title:TPopUpColumnTitle read FTitle write SetTitle;
    property Width: Integer read FWidth write SetWidth;
  end;
  
  { TPopUpFormColumns }

  TPopUpFormColumns = class(TCollection)
  private
    FPopUpFormOptions: TPopUpFormOptions;
    function GetPopUpColumn(Index: Integer): TPopUpColumn;
    procedure SetPopUpColumn(Index: Integer; const AValue: TPopUpColumn);
  public
    property PopUpFormOptions:TPopUpFormOptions read FPopUpFormOptions write FPopUpFormOptions;
    property Items[Index: Integer]: TPopUpColumn read GetPopUpColumn write SetPopUpColumn; default;
  end;
  
  { TPopUpFormOptions }

  TPopUpFormOptions = class(TPersistent)
  private
    FAutoFillColumns: boolean;
    FAutoSort: boolean;
    FBorderStyle: TBorderStyle;
    FColumns: TPopUpFormColumns;
    FDataSource: TDataSource;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FOnGetCellProps: TGetCellPropsEvent;
    FOptions: TPopUpGridOptions;
    FShowTitles: boolean;
    FTitleButtons: boolean;
    FTitleStyle: TTitleStyle;
    function GetColumns: TPopUpFormColumns;
    procedure SetAutoFillColumns(const AValue: boolean);
    procedure SetAutoSort(const AValue: boolean);
    procedure SetColumns(const AValue: TPopUpFormColumns);
    procedure SetDropDownCount(const AValue: integer);
    procedure SetDropDownWidth(const AValue: integer);
    procedure SetOptions(const AValue: TPopUpGridOptions);
    procedure SetShowTitles(const AValue: boolean);
    procedure SetTitleButtons(const AValue: boolean);
    procedure SetTitleStyle(const AValue: TTitleStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataSource:TDataSource read FDataSource write FDataSource;
  published
    property AutoFillColumns:boolean read FAutoFillColumns write SetAutoFillColumns default false;
    property AutoSort:boolean read FAutoSort write SetAutoSort default false;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle default bsNone;
    property Columns:TPopUpFormColumns read GetColumns write SetColumns;
    property DropDownCount:integer read FDropDownCount write SetDropDownCount default 8;
    property DropDownWidth:integer read FDropDownWidth write SetDropDownWidth default 0;
    property Options:TPopUpGridOptions read FOptions write SetOptions default [pfgColLines, pfgRowLines];
    property ShowTitles:boolean read FShowTitles write SetShowTitles default false;
    property TitleButtons:boolean read FTitleButtons write SetTitleButtons default false;
    property TitleStyle:TTitleStyle read FTitleStyle write SetTitleStyle default tsLazarus;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps
      write FOnGetCellProps;
  end;

  { TPopUpForm }
  TPopUpForm = class(TForm)
  private
    CloseBtn: TBitBtn;
    FFindResult:boolean;
    FGrid:TPopUpGrid;
    FDataSource:TDataSource;
    FOnPopUpCloseEvent:TPopUpCloseEvent;
    FPopUpFormOptions:TPopUpFormOptions;
    FRowCount:word;
    function GetDataSet: TDataSet;
    function GetLookupDisplayIndex: integer;
    procedure SetDataSet(const AValue: TDataSet);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetLookupDisplayIndex(const AValue: integer);
  protected
    FFieldList:string;
    procedure Deactivate; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure GridDblClick(Sender: TObject);
    procedure GridClickEvent(Column: TColumn);
    procedure CloseOk;
    procedure Paint;override;
    procedure CreateWnd;override;
    //
    procedure DoSetFieldsFromString(FL:string);
    procedure DoSetFieldsFromColList;
  public
    procedure KeyPress(var Key: char); override;
    constructor CreatePopUp(AOwner: TComponent;
      APopUpFormOptions:TPopUpFormOptions; AFieldList:string; BtnWidtn:integer);
    property DataSet:TDataSet read GetDataSet write SetDataSet;
    property LookupDisplayIndex:integer read GetLookupDisplayIndex write SetLookupDisplayIndex;
  end;

function ShowRxDBPopUpForm(AControl:TWinControl; ADataSet:TDataSet;
  AOnPopUpCloseEvent:TPopUpCloseEvent; APopUpFormOptions:TPopUpFormOptions;
  AFieldList:string; ALookupDisplayIndex, BtnWidtn: integer; const Font:TFont):TPopUpForm;
  
procedure FillPopupWidth(APopUpFormOptions:TPopUpFormOptions; ARxPopUpForm:TPopUpForm);

implementation
uses dbutils, math;

{.$DEFINE LINUX}
function ShowRxDBPopUpForm(AControl:TWinControl; ADataSet:TDataSet;
  AOnPopUpCloseEvent:TPopUpCloseEvent; APopUpFormOptions:TPopUpFormOptions;
  AFieldList:string; ALookupDisplayIndex, BtnWidtn: integer; const Font:TFont):TPopUpForm;
begin
  Result:=TPopUpForm.CreatePopUp(AControl, APopUpFormOptions, AFieldList, BtnWidtn);
  Result.FOnPopUpCloseEvent:=AOnPopUpCloseEvent;
  Result.DataSet:=ADataSet;
  Result.LookupDisplayIndex:=ALookupDisplayIndex;

  if Assigned(Font) then
  begin
    Result.FGrid.Font.Assign(Font);
//    Result.Font.Assign(Font);
  end;

{$IFDEF LINUX}
  if Result.ShowModal = mrOk then
    if Assigned(AOnPopUpCloseEvent) then
      AOnPopUpCloseEvent(true);
  Result.Free;
  Result:=nil;
{$ELSE  LINUX}
  Result.Show;
  Result.FGrid.UpdateActive;
{$ENDIF LINUX}
end;

procedure FillPopupWidth(APopUpFormOptions: TPopUpFormOptions;
  ARxPopUpForm: TPopUpForm);
var
  i, w:integer;
begin
  w:=Min(APopUpFormOptions.Columns.Count, ARxPopUpForm.FGrid.Columns.Count);
  for i:=0 to w-1 do
  begin
    APopUpFormOptions.Columns[i].Width:=ARxPopUpForm.FGrid.Columns[i].Width;
  end;
end;

{ TPopUpForm }

procedure TPopUpForm.SetDataSet(const AValue: TDataSet);
begin
  if FDataSource.DataSet=AValue then exit;
  FDataSource.DataSet:=AValue;
  if FPopUpFormOptions.Columns.Count>0 then
    DoSetFieldsFromColList
  else
    DoSetFieldsFromString(FFieldList);
end;

procedure TPopUpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  if Assigned(FOnPopUpCloseEvent) then
    FOnPopUpCloseEvent(FFindResult);
end;

procedure TPopUpForm.SetLookupDisplayIndex(const AValue: integer);
begin
  FGrid.LookupDisplayIndex:=AValue;
end;

function TPopUpForm.GetDataSet: TDataSet;
begin
  Result:=FDataSource.DataSet;
end;

function TPopUpForm.GetLookupDisplayIndex: integer;
begin
  Result:=FGrid.FLookupDisplayIndex;
end;

procedure TPopUpForm.Deactivate;
begin
  inherited Deactivate;
  if Assigned(FOnPopUpCloseEvent) then
    FOnPopUpCloseEvent(FFindResult);
  Close;
end;

procedure TPopUpForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:Deactivate;
    VK_RETURN:CloseOk;
  else
    inherited KeyDown(Key, Shift);
  end;
  FGrid.KeyDown(Key, Shift);
  Key:=0;
  Invalidate;
end;

procedure TPopUpForm.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  FGrid.KeyPress(Key);
end;

procedure TPopUpForm.GridDblClick(Sender: TObject);
begin
  CloseOk;
end;

procedure TPopUpForm.GridClickEvent(Column: TColumn);
begin
  CloseOk;
end;

procedure TPopUpForm.CloseOk;
begin
  FFindResult:=true;
{$IFDEF LINUX}
  ModalResult:=mrOk;
{$ELSE LINUX}
  Deactivate;
{$ENDIF LINUX}
end;

procedure TPopUpForm.Paint;
var
  CR:TRect;
begin
  inherited Paint;
  if FPopUpFormOptions.BorderStyle<>bsNone then
  begin
    CR:=ClientRect;
    RxFrame3D(Canvas, CR, clBtnHighlight, clWindowFrame, 1);
    RxFrame3D(Canvas, CR, clBtnFace, clBtnShadow, 1);
  end
  else
  begin
    Canvas.Pen.Color:=clWindowText;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(0, 0, Width-1, Height-1)
  end;
end;

procedure TPopUpForm.CreateWnd;
begin
  inherited CreateWnd;
  Height:=FGrid.DefaultRowHeight * FRowCount;
end;

procedure TPopUpForm.DoSetFieldsFromString(FL: string);
var
  FieldName:string;
  GK:TRxColumn;
  K:integer;
begin
  while (FL<>'') do
  begin
    K:=Pos(';', FL);
    if K<>0 then
    begin
      FieldName:=Copy(FL, 1, K-1);
      Delete(FL, 1, K);
    end
    else
    begin
      FieldName:=FL;
      FL:='';
    end;
    GK:=FGrid.Columns.Add as TRxColumn;
    GK.Field:=FGrid.DataSource.DataSet.FieldByName(FieldName);
  end;
end;

procedure TPopUpForm.DoSetFieldsFromColList;
var
  GK:TRxColumn;
  i:integer;
  Column:TPopUpColumn;
begin
  for i:=0 to FPopUpFormOptions.Columns.Count - 1 do
  begin
    GK:=FGrid.Columns.Add as TRxColumn;
    Column:=FPopUpFormOptions.Columns[i];
    GK.Field:=FGrid.DataSource.DataSet.FieldByName(Column.FieldName);
    GK.Alignment:=Column.Alignment;
    GK.Color:=Column.Color;
    GK.DisplayFormat:=Column.DisplayFormat;
//    GK.Font:=Column.Font;
    GK.ImageList:=Column.ImageList;
    GK.ValueChecked:=Column.ValueChecked;
    GK.ValueUnchecked:=Column.ValueUnchecked;

    if Column.Width<>0 then
      GK.Width:=Column.Width;

    GK.Title.Color:=Column.Title.Color;
    (GK.Title as TRxColumnTitle).Orientation:=Column.Title.Orientation;
    GK.Title.Alignment:=Column.Title.Alignment;
    GK.Title.Layout:=Column.Title.Layout;
    GK.Title.Caption:=Column.Title.Caption;
  end;
end;

constructor TPopUpForm.CreatePopUp(AOwner: TComponent;
  APopUpFormOptions:TPopUpFormOptions; AFieldList:string; BtnWidtn:integer);
var
  PopupOrigin:TPoint;
begin
  inherited Create(nil);
//  inherited Create(AOwner);
  BorderStyle := bsNone;
  Caption:='RxPopUp';
  KeyPreview:=true;
  Visible := false;
  FDataSource:=TDataSource.Create(Self);
  FPopUpFormOptions:=APopUpFormOptions;
  FFieldList:=AFieldList;

{$IFDEF LINUX}
  PopupOrigin:=TCustomControl(AOwner).Parent.ControlToScreen(Point(TCustomControl(AOwner).Left, TCustomControl(AOwner).Height + TCustomControl(AOwner).Top));
{$ELSE}
  PopupOrigin:=TCustomControl(AOwner).ControlToScreen(Point(0, TCustomControl(AOwner).Height));
{$ENDIF}
  Top:=PopupOrigin.y;
  Left:=PopupOrigin.x;
  
  if FPopUpFormOptions.DropDownWidth = 0 then
    Width:=TCustomControl(AOwner).Width + BtnWidtn
  else
    Width:=FPopUpFormOptions.DropDownWidth;

{$IFDEF LINUX}
  CloseBtn:=TBitBtn.Create(Self);
  CloseBtn.Parent:=Self;
  CloseBtn.Align:=alBottom;
  CloseBtn.Kind:=bkCancel;
{$ENDIF}
  FGrid:=TPopUpGrid.Create(Self);
  FGrid.Parent:=Self;
  FGrid.ReadOnly:=true;
  FGrid.Options:=FGrid.Options - [dgEditing];
  FGrid.DataSource:=FDataSource;
  FGrid.OnDblClick:=@GridDblClick;
  FGrid.OnCellClick:=@GridClickEvent;
  if FPopUpFormOptions.BorderStyle = bsSingle then
  begin
    FGrid.Top:=2;
    FGrid.Left:=2;
    FGrid.Width:=Width - 4;
{$IFDEF LINUX}
    FGrid.Height:=Height - CloseBtn.Height - 2;
{$ELSE}
    FGrid.Height:=Height - 4;
{$ENDIF}
    FGrid.Anchors:=[akLeft, akRight, akTop, akBottom];
  end
  else
  begin
    FGrid.Top:=1;
    FGrid.Left:=1;
    FGrid.Width:=Width - 3;
{$IFDEF LINUX}
    FGrid.Height:=Height - CloseBtn.Height - 2;
{$ELSE}
    FGrid.Height:=Height - 3;
{$ENDIF}
    FGrid.Anchors:=[akLeft, akRight, akTop, akBottom];
  end;
  
  //Set options
  if not (pfgIndicator in FPopUpFormOptions.FOptions) then
    FGrid.Options:=FGrid.Options - [dgIndicator];

  if not (pfgColLines in FPopUpFormOptions.FOptions) then
    FGrid.Options:=FGrid.Options - [dgColLines];

  if not (pfgRowLines in FPopUpFormOptions.FOptions) then
    FGrid.Options:=FGrid.Options - [dgRowLines];

  if not (pfgColumnResize in FPopUpFormOptions.FOptions) then
    FGrid.Options:=FGrid.Options - [dgColumnResize];

  if not (pfgColumnMove in FPopUpFormOptions.FOptions) then
    FGrid.Options:=FGrid.Options - [dgColumnMove];

  if FPopUpFormOptions.ShowTitles then
    FGrid.Options:=FGrid.Options + [dgTitles]
  else
    FGrid.Options:=FGrid.Options - [dgTitles];

  FGrid.AutoSort:=FPopUpFormOptions.AutoSort;
  FGrid.TitleButtons:=FPopUpFormOptions.TitleButtons;
  FGrid.TitleStyle:=FPopUpFormOptions.TitleStyle;
  FGrid.BorderStyle:=FPopUpFormOptions.BorderStyle;
  FGrid.OnGetCellProps:=FPopUpFormOptions.OnGetCellProps;
  FGrid.AutoFillColumns:=FPopUpFormOptions.AutoFillColumns;
  if FPopUpFormOptions.DropDownCount < 1 then
    FRowCount:=10 + ord(dgTitles in FGrid.Options)
  else
    FRowCount:=FPopUpFormOptions.DropDownCount + 2 + ord(dgTitles in FGrid.Options);
end;

{ TPopUpFormOptions }

procedure TPopUpFormOptions.SetAutoSort(const AValue: boolean);
begin
  if FAutoSort=AValue then exit;
  FAutoSort:=AValue;
end;

function TPopUpFormOptions.GetColumns: TPopUpFormColumns;
begin
  Result:=FColumns;
end;

procedure TPopUpFormOptions.SetAutoFillColumns(const AValue: boolean);
begin
  if FAutoFillColumns=AValue then exit;
  FAutoFillColumns:=AValue;
end;

procedure TPopUpFormOptions.SetColumns(const AValue: TPopUpFormColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TPopUpFormOptions.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount=AValue then exit;
  FDropDownCount:=AValue;
end;

procedure TPopUpFormOptions.SetDropDownWidth(const AValue: integer);
begin
  if FDropDownWidth=AValue then exit;
  FDropDownWidth:=AValue;
end;

procedure TPopUpFormOptions.SetOptions(const AValue: TPopUpGridOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TPopUpFormOptions.SetShowTitles(const AValue: boolean);
begin
  if FShowTitles=AValue then exit;
  FShowTitles:=AValue;
end;

procedure TPopUpFormOptions.SetTitleButtons(const AValue: boolean);
begin
  if FTitleButtons=AValue then exit;
  FTitleButtons:=AValue;
end;

procedure TPopUpFormOptions.SetTitleStyle(const AValue: TTitleStyle);
begin
  if FTitleStyle=AValue then exit;
  FTitleStyle:=AValue;
end;

constructor TPopUpFormOptions.Create;
begin
  inherited Create;
  FAutoSort:=false;
  FDropDownCount:=8;
  FDropDownWidth:=0;
  FOptions:=[pfgColLines, pfgRowLines];
  FShowTitles:=false;
  FTitleButtons:=false;
  FTitleStyle:=tsLazarus;
  FBorderStyle:=bsNone;
  FColumns:=TPopUpFormColumns.Create(TPopUpColumn);
  FColumns.FPopUpFormOptions:=Self;
end;

destructor TPopUpFormOptions.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TPopUpFormOptions.Assign(Source: TPersistent);
begin
  if Source is TPopUpFormOptions then
  begin
    FAutoSort:=TPopUpFormOptions(Source).FAutoSort;
    FDropDownCount:=TPopUpFormOptions(Source).FDropDownCount;
    FDropDownWidth:=TPopUpFormOptions(Source).FDropDownWidth;
    FOptions:=TPopUpFormOptions(Source).FOptions;
    FShowTitles:=TPopUpFormOptions(Source).FShowTitles;
    FTitleButtons:=TPopUpFormOptions(Source).FTitleButtons;
    FTitleStyle:=TPopUpFormOptions(Source).FTitleStyle;
    FBorderStyle:=TPopUpFormOptions(Source).FBorderStyle;
  end
  else
    inherited Assign(Source);
end;

{ TPopUpColumnTitle }


procedure TPopUpColumnTitle.SetAlignment(const AValue: TAlignment);
begin
  FAlignment:=AValue;
end;

procedure TPopUpColumnTitle.SetCaption(const AValue: string);
begin
  FCaption:=AValue;
end;

procedure TPopUpColumnTitle.SetColor(const AValue: TColor);
begin
  FColor:=AValue;
end;

procedure TPopUpColumnTitle.SetLayout(const AValue: TTextLayout);
begin
  FLayout:=AValue;
end;

procedure TPopUpColumnTitle.SetOrientation(const AValue: TTextOrientation);
begin
  if FOrientation=AValue then exit;
  FOrientation:=AValue;
end;

constructor TPopUpColumnTitle.Create;
begin
  inherited Create;
  FColor:=clBtnFace;
{$IFDEF NEW_STYLE_TITLE_ALIGNMENT_RXDBGRID}
  Alignment:=taCenter;
{$ENDIF}
end;

procedure TPopUpColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TPopUpColumnTitle then
  begin
    FAlignment:=TPopUpColumnTitle(Source).FAlignment;
    FCaption:=TPopUpColumnTitle(Source).FCaption;
    FColor:=TPopUpColumnTitle(Source).FColor;
    FLayout:=TPopUpColumnTitle(Source).FLayout;
    FOrientation:=TPopUpColumnTitle(Source).FOrientation;
  end
  else
    inherited Assign(Source);
end;

{ TPopUpColumn }

procedure TPopUpColumn.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
end;

procedure TPopUpColumn.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;

procedure TPopUpColumn.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat=AValue then exit;
  FDisplayFormat:=AValue;
end;

procedure TPopUpColumn.SetFieldName(const AValue: string);
begin
  if FFieldName=AValue then exit;
  if (FTitle.Caption = '') or (FTitle.Caption = FFieldName) then
    FTitle.Caption:=AValue;
  FFieldName:=AValue;
end;

procedure TPopUpColumn.SetFont(const AValue: TFont);
begin
  if FFont=AValue then exit;
  FFont:=AValue;
end;

procedure TPopUpColumn.SetImageList(const AValue: TImageList);
begin
  if FImageList=AValue then exit;
  FImageList:=AValue;
end;

procedure TPopUpColumn.SetTitle(const AValue: TPopUpColumnTitle);
begin
  FTitle.Assign(AValue);
end;

procedure TPopUpColumn.SetValueChecked(const AValue: string);
begin
  if FValueChecked=AValue then exit;
  FValueChecked:=AValue;
end;

procedure TPopUpColumn.SetValueUnchecked(const AValue: string);
begin
  if FValueUnchecked=AValue then exit;
  FValueUnchecked:=AValue;
end;

procedure TPopUpColumn.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

function TPopUpColumn.GetDisplayName: string;
begin
  if FFieldName<>'' then
  begin
    Result:=FFieldName;
    if FTitle.Caption<>'' then
      Result:=FTitle.Caption+' -> '+FFieldName;
  end
  else
    Result:=inherited GetDisplayName;
end;

constructor TPopUpColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTitle:=TPopUpColumnTitle.Create;
  FColor:=clWindow;
  FWidth:=65;
end;

destructor TPopUpColumn.Destroy;
begin
  FreeAndNil(FTitle);
  inherited Destroy;
end;

{ TPopUpFormColumns }

function TPopUpFormColumns.GetPopUpColumn(Index: Integer): TPopUpColumn;
begin
  Result := TPopUpColumn( inherited Items[Index] );
end;

procedure TPopUpFormColumns.SetPopUpColumn(Index: Integer;
  const AValue: TPopUpColumn);
begin
  Items[Index].Assign( AValue );
end;

{ TPopUpGrid }

procedure TPopUpGrid.ClearFind;
begin
  FFindLine:='';
  if DatalinkActive then
    DataSource.DataSet.First;
end;

procedure TPopUpGrid.FindNextChar(AChar: Char);
begin
  FFindLine:=FFindLine + AChar;
  if DatalinkActive then
    DataSetLocateThrough(DataSource.DataSet, FLookupDisplayField, FFindLine, [loCaseInsensitive, loPartialKey]);
end;

procedure TPopUpGrid.FindPriorChar;
begin
  if FFindLine = '' then exit;
  Delete(FFindLine, Length(FFindLine), 1);
  if DatalinkActive then
    if (FFindLine<>'') then
      DataSetLocateThrough(DataSource.DataSet, FLookupDisplayField, FFindLine, [loCaseInsensitive, loPartialKey])
    else
      DataSource.DataSet.First;
end;

procedure TPopUpGrid.SetLookupDisplayIndex(const AValue: integer);
begin
  FLookupDisplayIndex:=AValue;
  FLookupDisplayField:=Columns[FLookupDisplayIndex].FieldName;
end;

procedure TPopUpGrid.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Key>#32 then
    FindNextChar(Key)
  else
  if Key = #8 then
    ClearFind;
end;

procedure TPopUpGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key>=Ord('0')) and (Key<=Ord('9')) then
    FindNextChar(Char(Key))
  else
  if (Key>=VK_NUMPAD0) and (Key<=VK_NUMPAD9) then
    FindNextChar(Char(Key - VK_NUMPAD0 + Ord('0')))
  else
  if Key = VK_DELETE then
    ClearFind
  else
  if Key = VK_BACK then
    FindPriorChar
  else
  begin
    inherited KeyDown(Key, Shift);
    exit;
  end;
  Key:=0;
end;

end.

