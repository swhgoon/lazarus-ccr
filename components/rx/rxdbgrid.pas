unit rxdbgrid;

{$I rx.inc}

interface

uses
  Classes, SysUtils, Types, LResources, LCLType, LCLIntf, Forms, Controls,
  Graphics, Dialogs, Grids, DBGrids, DB, PropertyStorage, vclutils;

type
  TSortMarker = (smNone, smDown, smUp);
  TGetBtnParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
    IsDown: Boolean) of object;
  TGetCellPropsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object;
  TRxDBGridAllowedOperation = (aoInsert, aoUpdate, aoDelete, aoAppend);
  TRxDBGridAllowedOperations = set of TRxDBGridAllowedOperation;

  TDataSetClass = class of TDataSet;

  TExDBGridSortEngine = class
  private
    FDataSetClass:TDataSetClass;
  public
    procedure Sort(Field:TField; ADataSet:TDataSet; Asc:boolean);virtual;abstract;
  end;
  TExDBGridSortEngineClass = class of TExDBGridSortEngine;

  { TRxColumnTitle }
  TRxColumnTitle = class(TGridColumnTitle)
  private
    FOrientation: TTextOrientation;
    procedure SetOrientation(const AValue: TTextOrientation);
  published
    property Orientation:TTextOrientation read FOrientation write SetOrientation;
  end;
  { TRxColumn }

  TRxColumn = class(TColumn)
  private
    FImageList: TImageList;
    FKeyList:TStrings;
    FNotInKeyListIndex: Integer;
    function GetKeyList: TStrings;
    procedure SetImageList(const AValue: TImageList);
    procedure SetKeyList(const AValue: TStrings);
    procedure SetNotInKeyListIndex(const AValue: Integer);
  protected
    function  CreateTitle: TGridColumnTitle; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
  published
    property KeyList: TStrings read GetKeyList write SetKeyList;
    property ImageList:TImageList read FImageList write SetImageList;
    property NotInKeyListIndex: Integer read FNotInKeyListIndex write SetNotInKeyListIndex default -1;
  end;
  
  { TRxDbGridColumns }
  TRxDbGridColumns = class(TDbGridColumns)
  protected
    function  Add: TRxColumn;
  end;
  { TRxDBGrid }

  TRxDBGrid = class(TCustomDbGrid)
  private
    FAllowedOperations: TRxDBGridAllowedOperations;
    FOnGetCellProps: TGetCellPropsEvent;
    FTitleLines: Integer;
    FAutoSort: boolean;
    FMarkerUp, FMarkerDown: TBitmap;
    FOnGetBtnParams: TGetBtnParamsEvent;
    FTitleButtons: boolean;
    //auto sort support
    FSortField:TField;
    FSortOrder:TSortMarker;
    FSortEngine:TExDBGridSortEngine;
    FPressedCol: TColumn;
    FPressed: Boolean;
    FSwapButtons: Boolean;
    FTracking: Boolean;
    //storage
    FVersion: Integer;
    FPropertyStorageLink:TPropertyStorageLink;
    
    function GetColumns: TRxDbGridColumns;
    function GetPropertyStorage: TCustomPropertyStorage;
    function IsColumnsStored: boolean;
    procedure SetAutoSort(const AValue: boolean);
    procedure SetColumns(const AValue: TRxDbGridColumns);
    procedure SetPropertyStorage(const AValue: TCustomPropertyStorage);
    procedure SetTitleButtons(const AValue: boolean);
    function GetMasterColumn(ACol, ARow: Integer): TColumn;
    function DatalinkActive:boolean;
    procedure TrackButton(X, Y: Integer);
    procedure StopTracking;
    procedure CalcTitle;
    //storage
    procedure OnIniSave(Sender: TObject);
    procedure OnIniLoad(Sender: TObject);
  protected
  {$IFDEF Win32}
    procedure CreateWnd; override;
  {$ENDIF}
    procedure DefaultDrawCellA(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DefaultDrawCellData(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure LinkActive(Value: Boolean); override;
    procedure DoTitleClick(ACol: Longint; AField: TField); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    function  CreateColumns: TGridColumns; override;
    procedure DrawCellBitmap(RxColumn:TRxColumn; aRect: TRect; aState: TGridDrawState; AImageIndex:integer); virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LayoutChanged; override;
  published
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property TitleButtons: boolean read FTitleButtons write SetTitleButtons;
    property AutoSort:boolean read FAutoSort write SetAutoSort;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps
      write FOnGetCellProps;
    property Columns: TRxDbGridColumns read GetColumns write SetColumns stored IsColumnsStored;

    //storage
    property PropertyStorage:TCustomPropertyStorage read GetPropertyStorage write SetPropertyStorage;
    property Version: Integer read FVersion write FVersion default 0;
    property AllowedOperations:TRxDBGridAllowedOperations read FAllowedOperations
      write FAllowedOperations default [aoInsert, aoUpdate, aoDelete, aoAppend];
    //from DBGrid
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance default aaRightDown;
    property AutoFillColumns;
    //property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight default 18;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property FixedColor;
    property Flat;
    property Font;
    //property ImeMode;
    //property ImeName;
    property Options;
    property OptionsExtra;
    //property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Scrollbars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleStyle;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawColumnCell;
    property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    property OnEditButtonClick;
    //property OnEndDock;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldEditMask;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPrepareCanvas;
    //property OnStartDock;
    //property OnStartDrag;
    property OnTitleClick;
    property OnUserCheckboxBitmap;
  end;

type
  PCharArray1   = Array[0..12] of PChar;

const
  IMGMarkerUp : PCharArray1 =
  (
   '10 9 3 1',
   '. c None',
   '# c #808080',
   'a c #ffffff',

   '..........',
   '....#a....',
   '...#..a...',
   '...#..a...',
   '..#....a..',
   '..#....a..',
   '.#......a.',
   '.aaaaaaaa.',
   '..........'
  );

  IMGMarkerDown : PCharArray1 =
  (
   '10 9 3 1',
   '. c None',
   '# c #808080',
   'a c #ffffff',
   '..........',
   '.#######a.',
   '.#......a.',
   '..#....a..',
   '..#....a..',
   '...#..a...',
   '...#..a...',
   '....#a....',
   '..........')
  ;


procedure RegisterExDBGridSortEngine(ExDBGridSortEngineClass:TExDBGridSortEngineClass; DataSetClass:TDataSetClass);

implementation
uses Math, rxdconst, rxstrutils;

var
  ExDBGridSortEngineList:TStringList;

procedure RegisterExDBGridSortEngine(ExDBGridSortEngineClass:TExDBGridSortEngineClass; DataSetClass:TDataSetClass);
var
  Pos:integer;
  ExDBGridSortEngine:TExDBGridSortEngine;
begin
  if not ExDBGridSortEngineList.Find(DataSetClass.ClassName, Pos) then
  begin
    ExDBGridSortEngine:=ExDBGridSortEngineClass.Create;
    ExDBGridSortEngine.FDataSetClass:=DataSetClass;
    ExDBGridSortEngineList.AddObject(DataSetClass.ClassName, ExDBGridSortEngine);
  end
end;

procedure GridInvalidateRow(Grid: TRxDBGrid; Row: Longint);
var
  I: Longint;
begin
  for I := 0 to Grid.ColCount - 1 do Grid.InvalidateCell(I, Row);
end;

{ TRxDBGrid }
const
  ALIGN_FLAGS: array[TAlignment] of Integer =
  (DT_LEFT or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX,
    DT_RIGHT or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX,
    DT_CENTER or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX);
const
  ALIGN_FLAGS_HEADER: array[TAlignment] of Integer =
  (DT_LEFT  or {DT_EXPANDTABS or} DT_NOPREFIX,
    DT_RIGHT or {DT_EXPANDTABS or }DT_NOPREFIX,
    DT_CENTER or {DT_EXPANDTABS or }DT_NOPREFIX);

  TITLE_SUBHEADER = 2;
  TITLE_DEFAULT = 1;

const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);

procedure WriteTextHeader(ACanvas: TCanvas; ARect: TRect; const Text: string;
  Alignment: TAlignment);
var
  DrawRect: TRect;
  W, CnvW:integer;
begin
  DrawRect := Rect(ARect.Left + 1, ARect.Top + 1, ARect.Right, ARect.Bottom);

  CnvW:=DrawRect.Right - DrawRect.Left;
  W:=(ACanvas.TextWidth(Text) div CnvW) + 1;

  DrawRect.Top:=((ARect.Top + ARect.Bottom) div 2) - W * ACanvas.TextHeight('W') div 2;
  if DrawRect.Top < ARect.Top + 1 then
    DrawRect.Top := ARect.Top + 1;

  DrawText(ACanvas.Handle, PChar(Text), Length(Text), DrawRect,
//    DT_VCENTER or  DT_WORDBREAK or DT_CENTER
    ALIGN_FLAGS_HEADER[Alignment] {or DT_VCENTER or  DT_END_ELLIPSIS }or DT_WORDBREAK
    );
end;


procedure TRxDBGrid.SetTitleButtons(const AValue: boolean);
begin
  if FTitleButtons=AValue then exit;
  FTitleButtons:=AValue;
end;

procedure TRxDBGrid.SetAutoSort(const AValue: boolean);
var
  S:string;
  Pos:integer;
begin
  if FAutoSort=AValue then exit;
  FAutoSort:=AValue;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
  begin
    S:=DataSource.DataSet.ClassName;
    if ExDBGridSortEngineList.Find(S, Pos) then
      FSortEngine:=ExDBGridSortEngineList.Objects[Pos] as TExDBGridSortEngine
    else
      FSortEngine:=nil;
    FSortField:=nil;
    FSortOrder:=smNone;
  end
end;

function TRxDBGrid.GetColumns: TRxDbGridColumns;
begin
  result := TRxDbGridColumns(TCustomGrid(Self).Columns);
end;

function TRxDBGrid.GetPropertyStorage: TCustomPropertyStorage;
begin
  Result:=FPropertyStorageLink.Storage;
end;

function TRxDBGrid.IsColumnsStored: boolean;
begin
  result := TRxDbGridColumns(TCustomGrid(Self).Columns).Enabled;
end;

procedure TRxDBGrid.SetColumns(const AValue: TRxDbGridColumns);
begin
  TRxDbGridColumns(TCustomGrid(Self).Columns).Assign(Avalue);
end;

procedure TRxDBGrid.SetPropertyStorage(const AValue: TCustomPropertyStorage);
begin
  FPropertyStorageLink.Storage:=AValue;
end;

function TRxDBGrid.GetMasterColumn(ACol, ARow: Integer): TColumn;
begin
  if (dgIndicator in Options) then Dec(ACol, 1);//IndicatorOffset);
  if DatalinkActive and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    Result := Columns[ACol] as TColumn;
//    Result := ColumnAtDepth(Result, ARow);
  end
  else Result := nil;
end;

function TRxDBGrid.DatalinkActive: boolean;
begin
  Result:=Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

procedure TRxDBGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
  I, Offset: Integer;
begin
  Cell := MouseCoord(X, Y);
  Offset := RowCount;//[0];
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
    (FPressedCol = GetMasterColumn(Cell.X, Cell.Y) { Cell.X }) and (Cell.Y < Offset);
  if FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    for I := 0 to Offset - 1 do
      GridInvalidateRow(Self, I);
  end;
end;

procedure TRxDBGrid.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRxDBGrid.CalcTitle;
var
  i:integer;
  H:integer;
begin
  H:=0;
  for i:=0 to Columns.Count-1 do
  begin
    if TRxColumnTitle(Columns[i].Title).Orientation in [toVertical270, toVertical90] then
      H:=Max((Canvas.TextWidth(Columns[i].Title.Caption)+ Canvas.TextWidth('W')) div 19, H)
    else
      H:=Max((Canvas.TextWidth(Columns[i].Title.Caption)) div Max(Columns[i].Width-2, 1), H);
  end;
  RowHeights[0] := 19 * (FTitleLines+H);
end;

procedure TRxDBGrid.OnIniSave(Sender: TObject);
var
  i:integer;
  S, S1:string;
  C:TRxColumn;
begin
  S:=Owner.Name+'.'+Name;
  FPropertyStorageLink.Storage.WriteInteger(S+sVersion, FVersion);
  FPropertyStorageLink.Storage.WriteInteger(S+sCount, Columns.Count);
  S:=S+sItem;
  for i:=0 to Columns.Count-1 do
  begin
    S1:=S+IntToStr(i);
    C:=TRxColumn(Columns[i]);
    FPropertyStorageLink.Storage.WriteString(S1+sCaption, StrToHexText(C.Title.Caption));
    FPropertyStorageLink.Storage.WriteInteger(S1+sWidth, C.Width);
  end;
end;

procedure TRxDBGrid.OnIniLoad(Sender: TObject);

function GetColByCaption(Cap:string):TRxColumn;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to Columns.Count - 1 do
    if Cap = Columns[i].Title.Caption then
    begin
      Result:=TRxColumn(Columns[i]);
      exit;
    end;
end;

var
  i, ACount:integer;
  S, S1, ColumName, S2:string;
  C:TRxColumn;

begin
  S:=Owner.Name+'.'+Name;
  ACount:=FPropertyStorageLink.Storage.ReadInteger(S+sVersion, FVersion); //Check cfg version
  if ACount = FVersion then
  begin
    ACount:=FPropertyStorageLink.Storage.ReadInteger(S+sCount, 0);
    S:=S+sItem;
    for i:=0 to ACount-1 do
    begin
      S1:=S+IntToStr(i);
      ColumName:=HexTextToStr(FPropertyStorageLink.Storage.ReadString(S1+sCaption, ''));
      if ColumName<>'' then
      begin
        C:=GetColByCaption(ColumName);
        if Assigned(C) then
          C.Width:=FPropertyStorageLink.Storage.ReadInteger(S1+sWidth, C.Width);
      end;
    end;
  end;
end;

{$IFDEF Win32}
procedure TRxDBGrid.CreateWnd;
begin
  BeginUpdate;
  try
    inherited CreateWnd;
    CalcTitle;
  finally
    EndUpdate;
  end;
end;
{$ENDIF}

procedure TRxDBGrid.DefaultDrawCellA(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

  procedure FixRectangle;
  begin
    case Canvas.TextStyle.Alignment of
      Classes.taLeftJustify: Inc(aRect.Left, 3);
      Classes.taRightJustify: Dec(aRect.Right, 3);
    end;
    case Canvas.TextStyle.Layout of
      tlTop: Inc(aRect.Top, 3);
      tlBottom: Dec(aRect.Bottom, 3);
    end;
  end;

var
  ASortMarker: TSortMarker;
  Background: TColor;
  X1,Y1, dW:integer;
  Down:boolean;
  aRect1: TRect;
  FTit:TRxColumnTitle;
  FCap:string;
begin
  Down := FPressed and FTitleButtons and (FPressedCol = GetMasterColumn( aCol , 0));
  PrepareCanvas(aCol, aRow, aState);

  ASortMarker := smNone;
  if (FSortField = GetFieldFromGridColumn(aCol)) then ASortMarker := FSortOrder;
  if Assigned(FOnGetBtnParams) and Assigned(GetFieldFromGridColumn(aCol)) then
  begin
    Background:=Canvas.Brush.Color;
    FOnGetBtnParams(Self, GetFieldFromGridColumn(aCol), Canvas.Font,
                    Background, ASortMarker, Down);
    Canvas.Brush.Color:=Background;
  end;


  Canvas.FillRect(aRect);
  DrawCellGrid(aCol,aRow,aRect,aState);

  if (gdFixed in aState) and (aRow=0)and(ACol>=FixedCols) then
  begin
    aRect1:=aRect;
    FixRectangle;
    FTit:=TRxColumnTitle(ColumnFromGridColumn(aCol).Title);
    if not Assigned(FTit) then
      FCap:=GetDefaultColumnTitle(aCol)
    else
      FCap:=FTit.Caption;

    if FTit.Orientation = toHorizontal then
      WriteTextHeader(Canvas, ARect, FCap, GetColumnAlignment(aCol, true))
    else
    begin
      if FTit.Orientation in [toVertical90, toVertical270] then
        dW:=((aRect.Bottom - aRect.Top) - Canvas.TextWidth(FCap)) div 2
      else
        dw:=0;
      OutTextXY90(Canvas, ARect.Left, ARect.Top+dw, FCap, FTit.Orientation);
    end;

    if FTitleButtons then
    begin
      if ASortMarker = smDown then
      begin
        X1:=aRect.Right - FMarkerDown.Width - 6;
        Y1:=Trunc((aRect.Top+aRect.Bottom-FMarkerDown.Height)/2);
        Canvas.Draw(X1, Y1, FMarkerDown);
      end
      else
      if ASortMarker = smUp then
      begin
        X1:=aRect.Right - FMarkerUp.Width - 6;
        Y1:=Trunc((aRect.Top+aRect.Bottom-FMarkerUp.Height)/2);
        Canvas.Draw(X1, Y1, FMarkerUp);
      end;
    end;

    if (FTitleButtons or ([dgRowLines, dgColLines] * Options =
          [dgRowLines, dgColLines])) and Down then
    begin
      DrawEdge(Canvas.Handle, ARect1, EdgeFlag[Down], BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, ARect1, EdgeFlag[Down], BF_TOPLEFT);
    end
  end
  else
    DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TRxDBGrid.DefaultDrawCellData(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  S: string;
  F: TField;
  C:TRxColumn;
  j:integer;
begin
  F := GetFieldFromGridColumn(aCol);
  C := ColumnFromGridColumn(aCol) as TRxColumn;
  case ColumnEditorStyle(aCol, F) of
    cbsCheckBoxColumn : DrawCheckBoxBitmaps(aCol, aRect, F);
  else
    if F<>nil then
    begin
      if F.dataType <> ftBlob then
      begin
        S := F.DisplayText;
        if Assigned(C) and (C.KeyList.Count > 0) and (C.PickList.Count>0) then
        begin
          J:=C.KeyList.IndexOf(S);
          if (J>=0) and (J<C.PickList.Count) then
            S:=C.PickList[j];
        end;
      end
      else
        S := '(blob)';
    end
    else
      S := '';
    DrawCellText(aCol,aRow,aRect,aState,S);
  end;
end;

procedure TRxDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  RxColumn:TRxColumn;
  AImageIndex:integer;
  FBackground: TColor;
begin
  if {FTitleButtons and }(gdFixed in aState) and (aRow=0)and(ACol>=FixedCols) then
    DefaultDrawCellA(aCol, aRow, aRect, aState)
  else
  if  not ((gdFixed in aState) or (aCol=0) or (aRow=0)) then
  begin

    PrepareCanvas(aCol, aRow, aState);

    if Assigned(FOnGetCellProps) then
    begin
      FBackground:=Canvas.Brush.Color;
      FOnGetCellProps(Self, GetFieldFromGridColumn(aCol), Canvas.Font, FBackground);
      Canvas.Brush.Color:=FBackground;
    end;
    
    Canvas.FillRect(aRect);
    DrawCellGrid(aCol,aRow, aRect, aState);

    RxColumn:=TRxColumn(ColumnFromGridColumn(aCol));
    if Assigned(RxColumn) and Assigned(RxColumn.Field) and Assigned(RxColumn.ImageList) then
    begin
      AImageIndex:=StrToIntDef(RxColumn.KeyList.Values[RxColumn.Field.AsString], RxColumn.FNotInKeyListIndex);
      if (AImageIndex > -1) and (AImageIndex < RxColumn.ImageList.Count) then
        DrawCellBitmap(RxColumn, aRect, aState, AImageIndex);
    end
    else
    DefaultDrawCellData(aCol, aRow, aRect, aState);
//      inherited DrawCell(aCol, aRow, aRect, aState);
  end
  else
    inherited DrawCell(aCol, aRow, aRect, aState);
end;

procedure TRxDBGrid.LinkActive(Value: Boolean);
var
  S:string;
  Pos:integer;
begin
  inherited LinkActive(Value);
  if Value then
  begin
    S:=DataSource.DataSet.ClassName;
    if ExDBGridSortEngineList.Find(S, Pos) then
      FSortEngine:=ExDBGridSortEngineList.Objects[Pos] as TExDBGridSortEngine
    else
      FSortEngine:=nil;
  end
  else
    FSortEngine:=nil;
  FSortField:=nil;
  FSortOrder:=smNone;
end;

procedure TRxDBGrid.DoTitleClick(ACol: Longint; AField: TField);
begin
  if FAutoSort and (FSortEngine<>nil) then
  begin
    if AField=FSortField then
    begin
      if FSortOrder=smUp then
        FSortOrder:=smDown
      else
        FSortOrder:=smUp;
    end
    else
    begin
      FSortField:=AField;
      FSortOrder:=smUp;
    end;
    FSortEngine.Sort(FSortField, DataSource.DataSet, FSortOrder=smUp);
  end;
//  if Assigned(FOnTitleBtnClick) then FOnTitleBtnClick(Self, ACol, AField);
end;

procedure TRxDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
  if (MouseToGridZone(X,Y) = gzFixedCols) then
    CalcTitle;
end;

procedure TRxDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Cell: TGridCoord;
begin
  if FAutoSort then
  begin
    Cell := MouseCoord(X, Y);
    if (Cell.Y=0) and (Cell.X >= ord(dgIndicator in Options))  then
    begin
      if (dgColumnResize in Options) and (Button = mbRight) then
      begin
        Button := mbLeft;
        FSwapButtons := True;
        MouseCapture := True;
        Shift:=Shift + [ssLeft];
        inherited MouseDown(Button, Shift, X, Y);
      end
      else
      if Button = mbLeft then
      begin
        if (MouseToGridZone(X,Y) = gzFixedCols) and (dgColumnResize in Options) and (Cursor=crHSplit) then
        begin
          inherited MouseDown(Button, Shift, X, Y);
        end
        else
        begin
          MouseCapture := True;
          FTracking := True;
          FPressedCol := GetMasterColumn(Cell.X, Cell.Y);
          TrackButton(X, Y);
        end
      end
    end
    else
      inherited MouseDown(Button, Shift, X, Y);
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Cell: TGridCoord;
  ACol: Longint;
  DoClick: Boolean;
begin
  if FTitleButtons and FTracking and (FPressedCol <> nil) then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y < RowHeights[0]) and
      (FPressedCol = GetMasterColumn(Cell.X, Cell.Y));
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      if (dgIndicator in Options) then Dec(ACol);
      if DataLinkActive and (ACol >= 0) and (ACol <  Columns.Count ) then
      begin
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
      end;
    end;
  end
  else
  if FSwapButtons then begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then Button := mbLeft;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TRxDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  FTmpReadOnly:boolean;
begin
  case Key of
    VK_DELETE:if not (aoDelete in FAllowedOperations) then exit;
    VK_INSERT:if not (aoInsert in FAllowedOperations) then exit;
    VK_DOWN:if not (aoAppend in FAllowedOperations) then
      begin
        FTmpReadOnly:=ReadOnly;
        ReadOnly:=true;
        inherited KeyDown(Key, Shift);
        ReadOnly:=FTmpReadOnly;
        exit;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

function TRxDBGrid.CreateColumns: TGridColumns;
begin
  Result := TRxDbGridColumns.Create(Self, TRxColumn);
end;

procedure TRxDBGrid.DrawCellBitmap(RxColumn: TRxColumn; aRect: TRect;
  aState: TGridDrawState; AImageIndex: integer);
var
  ClientSize: TSize;
  IBitmap, Stub:TBitmap;
  ImageRect: TRect;
begin
  InflateRect(aRect, -1, -1);

  RxColumn.ImageList.GetInternalImage(AImageIndex, IBitmap, Stub, ImageRect);

  ClientSize.cx:= Min(aRect.Right - aRect.Left, IBitmap.Width);
  ClientSize.cy:= Min(aRect.Bottom - aRect.Top, IBitmap.Height);

  if ClientSize.cx = IBitmap.Width then
  begin
    aRect.Left:= (aRect.Left + aRect.Right - IBitmap.Width) div 2;
    aRect.Right:=aRect.Left + IBitmap.Width;
  end;

  if ClientSize.cy = IBitmap.Height then
  begin
    aRect.Top:= (aRect.Top + aRect.Bottom - IBitmap.Height) div 2;
    aRect.Bottom:=aRect.Top + IBitmap.Height;
  end;

  Canvas.StretchDraw(aRect, IBitmap);
end;

procedure TRxDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  C:TRxColumn;
  j:integer;
  S:string;
begin
  C := ColumnFromGridColumn(aCol) as TRxColumn;
  S:=Value;
  if Assigned(C) and (C.KeyList.Count>0) and (C.PickList.Count>0) then
  begin
    J:=C.PickList.IndexOf(S);
    if (J>=0) and (J<C.KeyList.Count) then
      S:=C.KeyList[j];
  end;
  inherited SetEditText(ACol, ARow, S);
end;

constructor TRxDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FMarkerUp := TBitmap.Create;
  FMarkerUp.Handle := CreatePixmapIndirect(@IMGMarkerUp[0],
    GetSysColor(COLOR_BTNFACE));
  FMarkerDown := TBitmap.Create;
  FMarkerDown.Handle := CreatePixmapIndirect(@IMGMarkerDown[0],
    GetSysColor(COLOR_BTNFACE));

  FPropertyStorageLink:=TPropertyStorageLink.Create;
  FPropertyStorageLink.OnSave:=@OnIniSave;
  FPropertyStorageLink.OnLoad:=@OnIniLoad;

  FTitleLines := TITLE_DEFAULT;
  FAllowedOperations:=[aoInsert, aoUpdate, aoDelete, aoAppend];
end;

destructor TRxDBGrid.Destroy;
begin
  FreeAndNil(FMarkerDown);
  FreeAndNil(FMarkerUp);
  FreeAndNil(FPropertyStorageLink);
  inherited Destroy;
end;

procedure TRxDBGrid.LayoutChanged;
begin
  inherited LayoutChanged;
//  CalcTitle;
end;

{ TRxDbGridColumns }
function TRxDbGridColumns.Add: TRxColumn;
begin
  result := TRxColumn( inherited Add);
end;

{ TRxColumn }

function TRxColumn.GetKeyList: TStrings;
begin
  if FKeyList=nil then
    FKeyList := TStringList.Create;
  Result := FKeyList;
end;

procedure TRxColumn.SetImageList(const AValue: TImageList);
begin
  if FImageList=AValue then exit;
  FImageList:=AValue;
  if Grid <> nil then
    Grid.Invalidate;
end;

procedure TRxColumn.SetKeyList(const AValue: TStrings);
begin
  if AValue=nil then
  begin
    if FKeyList<>nil then
      FKeyList.Clear
  end
  else
    KeyList.Assign(AValue);
end;

procedure TRxColumn.SetNotInKeyListIndex(const AValue: Integer);
begin
  if FNotInKeyListIndex=AValue then exit;
  FNotInKeyListIndex:=AValue;
  if Grid <> nil then
    Grid.Invalidate;
end;

function TRxColumn.CreateTitle: TGridColumnTitle;
begin
  Result:=TRxColumnTitle.Create(Self);
end;

constructor TRxColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FNotInKeyListIndex:=-1;
end;

destructor TRxColumn.destroy;
begin
  if FKeyList<>nil then
  begin
    FKeyList.Free;
    FKeyList:=nil;
  end;
  inherited destroy;
end;

{ TRxColumnTitle }
procedure TRxColumnTitle.SetOrientation(const AValue: TTextOrientation);
begin
  if FOrientation=AValue then exit;
  FOrientation:=AValue;
  TRxDBGrid(TRxColumn(Column).Grid).CalcTitle;
  TRxColumn(Column).ColumnChanged;
end;

initialization
  ExDBGridSortEngineList:=TStringList.Create;
  ExDBGridSortEngineList.Sorted:=true;
finalization
  ExDBGridSortEngineList.Clear;
  ExDBGridSortEngineList.Free;
end.

