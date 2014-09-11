{ rxdbgrid unit

  Copyright (C) 2005-2014 Lagunov Aleksey alexs@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I rx.inc}

unit rxdbgrid;

interface

uses
  Classes, SysUtils, LResources, LCLType, LCLIntf, Forms, Controls, Buttons,
  Graphics, Dialogs, Grids, dbutils, DBGrids, DB, PropertyStorage, vclutils,
  LMessages, types, StdCtrls, Menus, rxspin;

const
  CBadQuickSearchSymbols = [VK_UNKNOWN..VK_HELP] + [VK_LWIN..VK_SLEEP] +
    [VK_NUMLOCK..VK_SCROLL] + [VK_LSHIFT..VK_OEM_102] + [VK_PROCESSKEY] +
    [VK_ATTN..VK_UNDEFINED];
  CCancelQuickSearchKeys = [VK_ESCAPE, VK_CANCEL, VK_DELETE, VK_INSERT,
    VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_TAB, VK_RETURN, VK_HOME,
    VK_END, VK_SPACE, VK_MULTIPLY];

type
  //forward declarations
  TRxDBGrid = class;
  TRxColumn = class;
  TRxDBGridAbstractTools = class;


  TRxQuickSearchNotifyEvent = procedure(Sender: TObject; Field: TField;
    var AValue: string) of object;

  TSortMarker = (smNone, smDown, smUp);

  TGetBtnParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
    IsDown: boolean) of object;

  TGetCellPropsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object;

//Freeman35 added
  TOnRxCalcFooterValues = procedure(Sender: TObject; Column: TRxColumn; var AValue : Variant) of object;

{$IFDEF RxDBGridDepricatedProps}
  //TRxDBGridAllowedOperation = (aoInsert, aoUpdate, aoDelete, aoAppend);
  //TRxDBGridAllowedOperations = set of TRxDBGridAllowedOperation;
{$ENDIF}

  TRxColumnOption = (coCustomizeVisible, coCustomizeWidth);
  TRxColumnOptions = set of TRxColumnOption;

  TRxColumnEditButtonStyle = (ebsDropDownRx, ebsEllipsisRx, ebsGlyphRx, ebsUpDownRx,
    ebsPlusRx, ebsMinusRx);

  TFooterValueType = (fvtNon, fvtSum, fvtAvg, fvtCount, fvtFieldValue,
    fvtStaticText, fvtMax, fvtMin, fvtRecNo);

  TOptionRx = (rdgAllowColumnsForm,
    rdgAllowDialogFind,
    rdgHighlightFocusCol,          //TODO:
    rdgHighlightFocusRow,          //TODO:
    rdgDblClickOptimizeColWidth,
    rdgFooterRows,
    rdgXORColSizing,
    rdgFilter,
    rdgMultiTitleLines,
    rdgMrOkOnDblClik,
    rdgAllowQuickSearch,
    rdgAllowQuickFilter,
    rdgAllowFilterForm,
    rdgAllowSortForm,
    rdgAllowToolMenu,
    rdgCaseInsensitiveSort,
    rdgWordWrap
    );

  TOptionsRx = set of TOptionRx;

  TCreateLookup = TNotifyEvent;
  TDisplayLookup = TNotifyEvent;
  //  TDataSetClass = class of TDataSet;

  TRxDBGridCommand = (rxgcNone, rxgcShowFindDlg, rxgcShowColumnsDlg,
    rxgcShowFilterDlg, rxgcShowSortDlg, rxgcShowQuickFilter,
    rxgcHideQuickFilter, rxgcSelectAll, rxgcDeSelectAll, rxgcInvertSelection,
    rxgcOptimizeColumnsWidth, rxgcCopyCellValue

    );

  TRxDSState = (rxdsInactive, rxdsActive);

  TRxFilterOpCode = (fopEQ, fopNotEQ, fopStartFrom, fopEndTo, fopLike, fopNotLike);
  { TRxDBGridKeyStroke }

  TRxDBGridKeyStroke = class(TCollectionItem)
  private
    FCommand: TRxDBGridCommand;
    FEnabled: boolean;
    FShortCut: TShortCut;
    FKey: word;          // Virtual keycode, i.e. VK_xxx
    FShift: TShiftState;
    procedure SetCommand(const AValue: TRxDBGridCommand);
    procedure SetShortCut(const AValue: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Command: TRxDBGridCommand read FCommand write SetCommand;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  { TRxDBGridKeyStrokes }

  TRxDBGridKeyStrokes = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TRxDBGridKeyStroke;
    procedure SetItem(Index: integer; const AValue: TRxDBGridKeyStroke);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
    function Add: TRxDBGridKeyStroke;
    function AddE(ACommand: TRxDBGridCommand; AShortCut: TShortCut): TRxDBGridKeyStroke;
    procedure ResetDefaults;
    function FindRxCommand(AKey: word; AShift: TShiftState): TRxDBGridCommand;
    function FindRxKeyStrokes(ACommand: TRxDBGridCommand): TRxDBGridKeyStroke;
  public
    property Items[Index: integer]: TRxDBGridKeyStroke read GetItem write SetItem; default;
  end;

  { TRxDBGridCollumnConstraint }

  TRxDBGridCollumnConstraints = class(TPersistent)
  private
    FMaxWidth: integer;
    FMinWidth: integer;
    FOwner: TPersistent;
    procedure SetMaxWidth(AValue: integer);
    procedure SetMinWidth(AValue: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent);
  published
    property MinWidth:integer read FMinWidth write SetMinWidth default 0;
    property MaxWidth:integer read FMaxWidth write SetMaxWidth default 0;
  end;

  { TRxDBGridFooterOptions }

  TRxDBGridFooterOptions = class(TPersistent)
  private
    FActive: boolean;
    FColor: TColor;
    FOwner: TRxDBGrid;
    FRowCount: integer;
    FStyle: TTitleStyle;
    procedure SetActive(AValue: boolean);
    procedure SetColor(AValue: TColor);
    procedure SetRowCount(AValue: integer);
    procedure SetStyle(AValue: TTitleStyle);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TRxDBGrid);
    destructor Destroy; override;
  published
    property Active: boolean read FActive write SetActive default false;
    property Color: TColor read FColor write SetColor default clWindow;
    property RowCount: integer read FRowCount write SetRowCount default 0;
    property Style: TTitleStyle read FStyle write SetStyle default tsLazarus;
  end;


  { TRxDBGridSortEngine }
  TRxSortEngineOption = (seoCaseInsensitiveSort);
  TRxSortEngineOptions = set of TRxSortEngineOption;

  TRxDBGridSortEngine = class
  protected
    FGrid:TRxDBGrid;
    procedure UpdateFooterRows(ADataSet:TDataSet; AGrid:TRxDBGrid);virtual; abstract;
    function EnabledFooterRowsCalc:boolean;virtual;
  public
    procedure Sort(FieldName: string; ADataSet: TDataSet; Asc: boolean; SortOptions: TRxSortEngineOptions); virtual; abstract;
    procedure SortList(ListField: string; ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions); virtual;
  end;

  TRxDBGridSortEngineClass = class of TRxDBGridSortEngine;

  TMLCaptionItem = class
    Caption: string;
    Width: integer;
    Hegth: integer;
    Next: TMLCaptionItem;
    Prior: TMLCaptionItem;
    FInvalidDraw:integer;
    Col: TGridColumn;
  end;

  { TRxColumnTitle }
  TRxColumnTitle = class(TColumnTitle)
  private
    FHint: string;
    FOrientation: TTextOrientation;
    FShowHint: boolean;
    FCaptionLines: TFPList;
    function GetCaptionLinesCount: integer;
    procedure SetOrientation(const AValue: TTextOrientation);
    procedure ClearCaptionML;
  protected
    procedure SetCaption(const AValue: TCaption); override;
  public
    constructor Create(TheColumn: TGridColumn); override;
    destructor Destroy; override;
    property CaptionLinesCount: integer read GetCaptionLinesCount;
    function CaptionLine(ALine: integer): TMLCaptionItem;
  published
    property Orientation: TTextOrientation read FOrientation write SetOrientation;
    property Hint: string read FHint write FHint;
    property ShowHint: boolean read FShowHint write FShowHint default False;
  end;

  { TRxColumnFooter }

  TRxColumnFooter = class(TPersistent)
  private
    FLayout: TTextLayout;
    FOwner: TRxColumn;
    FAlignment: TAlignment;
    FDisplayFormat: string;
    FFieldName: string;
    FField:TField;
    FValue: string;
    FValueType: TFooterValueType;
    FTestValue: double;
    FCountRec:integer;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetFieldName(const AValue: string);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetValue(const AValue: string);
    procedure SetValueType(const AValue: TFooterValueType);
    function GetFieldValue: string;
    function GetRecordsCount: string;
    function GetRecNo: string;
    function GetStatTotal: string;
    procedure ResetTestValue;
    procedure UpdateTestValue;

    function DeleteTestValue: boolean;
    function PostTestValue: boolean;
    function ErrorTestValue: boolean;
  protected
    procedure UpdateTestValueFromVar(AValue:Variant);
  public
    constructor Create(Owner: TRxColumn);
    property Owner: TRxColumn read FOwner;
    property NumericValue: double read FTestValue;
    function DisplayText: string;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taLeftJustify;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property FieldName: string read FFieldName write SetFieldName;
    property Value: string read FValue write SetValue;
    property ValueType: TFooterValueType
      read FValueType write SetValueType default fvtNon;
  end;


  { TRxFilterItem }

  TRxFilterItem = class
    FVAlue:string;
    FCol:TRxColumn;
    OpCode:TRxFilterOpCode;
    function TestValue:Boolean;
  end;

  { TRxFilterItems }

  TRxFilterItems = class(TFPList)
    function AcceptRecord:boolean;
  end;

  { TRxColumnFilter }

  TRxColumnFilter = class(TPersistent)
  private
    FEnabled: boolean;
    FOwner: TRxColumn;
    FValue: string;
    FValueList: TStringList;
    FEmptyValue: string;
    FEmptyFont: TFont;
    FFont: TFont;
    FAlignment: TAlignment;
    FDropDownRows: integer;
    FColor: TColor;
    function GetItemIndex: integer;
    procedure SetColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetItemIndex(const AValue: integer);
  public
    constructor Create(Owner: TRxColumn); virtual;
    destructor Destroy; override;
  published
    property Value: string read FValue write FValue;
    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write FAlignment default
      taLeftJustify;
    property DropDownRows: integer read FDropDownRows write FDropDownRows;
    property Color: TColor read FColor write SetColor default clWhite;
    property ValueList: TStringList read FValueList write FValueList;
    property EmptyValue: string read FEmptyValue write FEmptyValue;
    property EmptyFont: TFont read FEmptyFont write FEmptyFont;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Enabled:boolean read FEnabled write FEnabled default true;
  end;

  { TRxColumnEditButton }

  TRxColumnEditButton = class(TCollectionItem)
  private
    FShortCut: TShortCut;
    FStyle: TRxColumnEditButtonStyle;
    FButton:TSpeedButton;
    FVisible: Boolean;
    //
    FSpinBtn:TRxSpinButton;
    function GetGlyph: TBitmap;
    function GetHint: String;
    function GetNumGlyphs: Integer;
    function GetOnButtonClick: TNotifyEvent;
    function GetWidth: Integer;
    procedure SetGlyph(AValue: TBitmap);
    procedure SetHint(AValue: String);
    procedure SetNumGlyphs(AValue: Integer);
    procedure SetOnButtonClick(AValue: TNotifyEvent);
    procedure SetStyle(AValue: TRxColumnEditButtonStyle);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);

    procedure DoBottomClick(Sender: TObject);
    procedure DoTopClick(Sender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    //property DropdownMenu: TPopupMenu read FDropdownMenu write FDropdownMenu; :TODO:
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Hint: String read GetHint write SetHint;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property ShortCut: TShortCut read FShortCut write FShortCut default scNone;
    property Style: TRxColumnEditButtonStyle read FStyle write SetStyle default ebsDropDownRx;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: Integer read GetWidth write SetWidth default 15;
    property OnClick: TNotifyEvent read GetOnButtonClick write SetOnButtonClick;
    //property OnDown: TNotifyEvent read FOnButtonDown write FOnButtonDown;
  end;

  TRxColumnEditButtons = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: integer): TRxColumnEditButton;
    procedure SetItem(Index: integer; AValue: TRxColumnEditButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRxColumnEditButton;
  public
    property Items[Index: integer]: TRxColumnEditButton read GetItem write SetItem; default;
  end;

  { TRxColumn }

  TRxColumn = class(TColumn)
  private
    FDirectInput: boolean;
    FEditButtons: TRxColumnEditButtons;
    FFooter: TRxColumnFooter;
    FConstraints:TRxDBGridCollumnConstraints;
    FFilter: TRxColumnFilter;
    FImageList: TImageList;
    FKeyList: TStrings;
    FNotInKeyListIndex: integer;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOptions: TRxColumnOptions;
    FSortFields: string;
    FSortOrder: TSortMarker;
    FSortPosition: integer;
    FWordWrap: boolean;
    function GetConstraints: TRxDBGridCollumnConstraints;
    function GetFooter: TRxColumnFooter;
    function GetKeyList: TStrings;
    function GetSortFields:string;
    procedure SetConstraints(AValue: TRxDBGridCollumnConstraints);
    procedure SetEditButtons(AValue: TRxColumnEditButtons);
    procedure SetFilter(const AValue: TRxColumnFilter);
    procedure SetFooter(const AValue: TRxColumnFooter);
    procedure SetImageList(const AValue: TImageList);
    procedure SetKeyList(const AValue: TStrings);
    procedure SetNotInKeyListIndex(const AValue: integer);
    procedure SetWordWrap(AValue: boolean);
  protected
    function CreateTitle: TGridColumnTitle; override;
    procedure ColumnChanged; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure OptimizeWidth;
    property SortOrder: TSortMarker read FSortOrder write FSortOrder;
    property SortPosition: integer read FSortPosition;
  published
    property Constraints:TRxDBGridCollumnConstraints read GetConstraints write SetConstraints;
    property DirectInput : boolean read FDirectInput write FDirectInput default true;
    property EditButtons:TRxColumnEditButtons read FEditButtons write SetEditButtons;
    property Filter: TRxColumnFilter read FFilter write SetFilter;
    property Footer: TRxColumnFooter read GetFooter write SetFooter;
    property ImageList: TImageList read FImageList write SetImageList;
    property KeyList: TStrings read GetKeyList write SetKeyList;
    property NotInKeyListIndex: integer read FNotInKeyListIndex write SetNotInKeyListIndex default -1;
    property Options:TRxColumnOptions read FOptions write FOptions default [coCustomizeVisible, coCustomizeWidth];
    property SortFields: string read FSortFields write FSortFields;
    property WordWrap:boolean read FWordWrap write SetWordWrap default false;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
  end;

  { TRxDbGridColumns }
  TRxDbGridColumns = class(TDbGridColumns)
  protected
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
  public
    function Add: TRxColumn;
  end;

  { TRxDbGridColumnsSortList }

  TRxDbGridColumnsSortList = class(TFPList)
  private
    function GetCollumn(Index: Integer): TRxColumn;
  public
    property Collumn[Index: Integer]: TRxColumn read GetCollumn; default;
  end;

  { TFilterListCellEditor }

  TFilterListCellEditor = class(TComboBox)
  private
    FGrid: TCustomGrid;
    FCol: integer;
    FMouseFlag: boolean;
  protected
    procedure WndProc(var TheMessage: TLMessage); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    procedure Show(Grid: TCustomGrid; Col: integer);
    property Grid: TCustomGrid read FGrid;
    property Col: integer read FCol;
    property MouseFlag: boolean read FMouseFlag write FMouseFlag;
  end;



  { TRxDBGrid }
  TRxDBGrid = class(TCustomDBGrid)
  private
    FrxDSState:TRxDSState;
    FFooterOptions: TRxDBGridFooterOptions;
    FSortColumns: TRxDbGridColumnsSortList;
    FSortingNow:Boolean;
    FInProcessCalc: integer;
//    FAllowedOperations: TRxDBGridAllowedOperations;
    //
    FKeyStrokes: TRxDBGridKeyStrokes;
    FOnGetCellProps: TGetCellPropsEvent;
    FOptionsRx: TOptionsRx;
    //    FTitleLines: Integer;

    FOnGetBtnParams: TGetBtnParamsEvent;
    FOnFiltred: TNotifyEvent;
    FOnRxCalcFooterValues :TOnRxCalcFooterValues;
    //auto sort support

    FMarkerUp   : TBitmap;
    FMarkerDown : TBitmap;
    FAutoSort   : boolean;
    FSortEngine : TRxDBGridSortEngine;
    FPressedCol : TRxColumn;
    //
    FPressed: boolean;
    FSwapButtons: boolean;
    FTracking: boolean;
    FDrawFullLine: boolean;

    F_Clicked: boolean;
    F_PopupMenu: TPopupMenu;
    F_MenuBMP: TBitmap;

    F_EventOnFilterRec: TFilterRecordEvent;
    F_EventOnBeforeDelete: TDataSetNotifyEvent;
    F_EventOnBeforePost: TDataSetNotifyEvent;
    F_EventOnDeleteError: TDataSetErrorEvent;
    F_EventOnPostError: TDataSetErrorEvent;
    F_LastFilter: TStringList;
    //F_SortListField: TStringList;
    F_CreateLookup: TCreateLookup;
    F_DisplayLookup: TDisplayLookup;

    //storage
    //Column resize
    FColumnResizing: boolean;

    FFilterListEditor: TFilterListCellEditor;

    FOldPosition: Integer;
    FVersion: integer;
    FPropertyStorageLink: TPropertyStorageLink;
    FRxDbGridLookupComboEditor: TCustomControl;
    FRxDbGridDateEditor: TWinControl;

    FAfterQuickSearch: TRxQuickSearchNotifyEvent;
    FBeforeQuickSearch: TRxQuickSearchNotifyEvent;
    FQuickUTF8Search: string;
    FOldDataSetState:TDataSetState;

    FOnSortChanged: TNotifyEvent;

    procedure DoCreateJMenu;
    //function GetAllowedOperations: TRxDBGridAllowedOperations;
    function GetColumns: TRxDbGridColumns;
    function GetFooterColor: TColor;
    function GetFooterRowCount: integer;
    function GetMarkerDown: TBitmap;
    function GetMarkerUp: TBitmap;
    function GetPropertyStorage: TCustomPropertyStorage;
    function GetSortField: string;
    function GetSortOrder: TSortMarker;
    function GetTitleButtons: boolean;
    function IsColumnsStored: boolean;

{$IFDEF RxDBGridDepricatedProps}
    procedure SetAllowedOperations(AValue: TRxDBGridAllowedOperations);
{$ENDIF}
    procedure SetAutoSort(const AValue: boolean);
    procedure SetColumns(const AValue: TRxDbGridColumns);
    procedure SetFooterColor(const AValue: TColor);
    procedure SetFooterOptions(AValue: TRxDBGridFooterOptions);
    procedure SetFooterRowCount(const AValue: integer);
    procedure SetKeyStrokes(const AValue: TRxDBGridKeyStrokes);
    procedure SetMarkerDown(AValue: TBitmap);
    procedure SetMarkerUp(AValue: TBitmap);
    procedure SetOptionsRx(const AValue: TOptionsRx);
    procedure SetPropertyStorage(const AValue: TCustomPropertyStorage);
    procedure SetTitleButtons(const AValue: boolean);
    procedure TrackButton(X, Y: integer);
    function GetDrawFullLine: boolean;
    procedure SetDrawFullLine(Value: boolean);
    procedure StopTracking;
    procedure CalcTitle;
    procedure ClearMLCaptionPointers;
    function getFilterRect(bRect: TRect): TRect;
    function getTitleRect(bRect: TRect): TRect;
    procedure OutCaptionCellText(aCol, aRow: integer; const aRect: TRect;
      aState: TGridDrawState; const ACaption: string);
    procedure OutCaptionCellText90(aCol, aRow: integer; const aRect: TRect;
      aState: TGridDrawState; const ACaption: string;
      const TextOrient: TTextOrientation);
    procedure OutCaptionSortMarker(const aRect: TRect; ASortMarker: TSortMarker; ASortPosition:integer);
    procedure OutCaptionMLCellText(aCol, aRow: integer; aRect: TRect;
      aState: TGridDrawState; MLI: TMLCaptionItem);
    procedure UpdateJMenuStates;
    procedure UpdateJMenuKeys;
    function SortEngineOptions: TRxSortEngineOptions;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure GetScrollbarParams(out aRange, aPage, aPos: Integer);
    procedure RestoreEditor;
    //storage
    procedure OnIniSave(Sender: TObject);
    procedure OnIniLoad(Sender: TObject);

    procedure CleanDSEvent;


    function UpdateRowsHeight:integer;
    procedure ResetRowHeght;

    procedure DoClearInvalidTitle;
    procedure DoDrawInvalidTitle;
    procedure DoSetColEdtBtn;
    procedure AddTools(ATools:TRxDBGridAbstractTools);
    procedure RemoveTools(ATools:TRxDBGridAbstractTools);
  protected
    procedure CollumnSortListUpdate;
    procedure CollumnSortListClear;
    procedure CollumnSortListApply;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function DatalinkActive: boolean;
    procedure AdjustEditorBounds(NewCol,NewRow:Integer); override;
    procedure LinkActive(Value: Boolean); override;

    procedure DefaultDrawCellA(aCol, aRow: integer; aRect: TRect;
      aState: TGridDrawState);
    procedure DefaultDrawTitle(aCol, aRow: integer; aRect: TRect;
      aState: TGridDrawState);
    procedure DefaultDrawFilter(aCol, aRow: integer; aRect: TRect;
      aState: TGridDrawState);
    procedure DefaultDrawCellData(aCol, aRow: integer; aRect: TRect;
      aState: TGridDrawState);
    procedure DrawCell(aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
      override;
    procedure SetDBHandlers(Value: boolean);virtual;

    procedure DrawFooterRows; virtual;

    procedure DoTitleClick(ACol: longint; ACollumn: TRxColumn; Shift: TShiftState); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    function CreateColumns: TGridColumns; override;
    procedure DrawCellBitmap(RxColumn: TRxColumn; aRect: TRect;
      aState: TGridDrawState; AImageIndex: integer); virtual;
    procedure SetEditText(ACol, ARow: longint; const Value: string); override;
    procedure CheckNewCachedSizes(var AGCache: TGridDataCache); override;
    procedure ColRowMoved(IsColumn: boolean; FromIndex, ToIndex: integer); override;
    procedure Paint; override;
    procedure UpdateActive; override;
    procedure UpdateData; override;
    procedure MoveSelection; override;
    function  GetBufferCount: integer; override;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure FFilterListEditorOnChange(Sender: TObject);
    procedure FFilterListEditorOnCloseUp(Sender: TObject);
    procedure InternalOptimizeColumnsWidth(AColList: TList);
    function IsDefaultRowHeightStored: boolean;
    procedure VisualChange; override;
    procedure EditorWidthChanged(aCol,aWidth: Integer); override;

    procedure SetQuickUTF8Search(AValue: string);
    procedure BeforeDel(DataSet: TDataSet);
    procedure BeforePo(DataSet: TDataSet);
    procedure ErrorDel(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure ErrorPo(DataSet: TDataSet; E: EDatabaseError; var DataAction: TDataAction);
    procedure OnFind(Sender: TObject);
    procedure OnFilterBy(Sender: TObject);
    procedure OnFilter(Sender: TObject);
    procedure OnFilterClose(Sender: TObject);
    procedure OnSortBy(Sender: TObject);
    procedure OnChooseVisibleFields(Sender: TObject);
    procedure OnSelectAllRows(Sender: TObject);
    procedure OnCopyCellValue(Sender: TObject);
    procedure Loaded; override;
    procedure UpdateFooterRowOnUpdateActive;

    procedure DoEditorHide; override;
    procedure DoEditorShow; override;

    procedure EraseBackground(DC: HDC); override;
    property Editor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FilterRec(DataSet: TDataSet; var Accept: boolean);
    function EditorByStyle(Style: TColumnButtonStyle): TWinControl; override;
    procedure LayoutChanged; override;
    procedure SetFocus; override;
    procedure ShowFindDialog;
    procedure ShowColumnsDialog;
    procedure ShowSortDialog;
    procedure ShowFilterDialog;
    function ColumnByFieldName(AFieldName: string): TRxColumn;
    function ColumnByCaption(ACaption: string): TRxColumn;
    procedure CalcStatTotals;
    procedure OptimizeColumnsWidth(AColList: string);
    procedure OptimizeColumnsWidthAll;
    procedure UpdateTitleHight;
    procedure GetOnCreateLookup;
    procedure GetOnDisplayLookup;
    procedure SelectAllRows;
    procedure DeSelectAllRows;
    procedure InvertSelection;
    procedure CopyCellValue;

    procedure SetSort(AFields: array of String; ASortMarkers: array of TSortMarker; PreformSort: Boolean = False);

    property Canvas;
    property DefaultTextStyle;
    property EditorBorderStyle;
    property EditorMode;
    property ExtendedColSizing;
    property FastEditing;
    property FocusRectVisible;
    property SelectedRows;
    property QuickUTF8Search: string read FQuickUTF8Search write SetQuickUTF8Search;

    property SortField:string read GetSortField;
    property SortOrder:TSortMarker read GetSortOrder;

    property SortColumns:TRxDbGridColumnsSortList read FSortColumns;
    property MarkerUp : TBitmap read GetMarkerUp write SetMarkerUp;
    property MarkerDown : TBitmap read GetMarkerDown write SetMarkerDown;
  published
    property AfterQuickSearch: TRxQuickSearchNotifyEvent
      read FAfterQuickSearch write FAfterQuickSearch;
    property BeforeQuickSearch: TRxQuickSearchNotifyEvent
      read FBeforeQuickSearch write FBeforeQuickSearch;
    property OnGetBtnParams: TGetBtnParamsEvent
      read FOnGetBtnParams write FOnGetBtnParams;
    property TitleButtons: boolean read GetTitleButtons write SetTitleButtons;
    property AutoSort: boolean read FAutoSort write SetAutoSort;
    property OnGetCellProps: TGetCellPropsEvent
      read FOnGetCellProps write FOnGetCellProps;
    property Columns: TRxDbGridColumns
      read GetColumns write SetColumns stored IsColumnsStored;
    property KeyStrokes: TRxDBGridKeyStrokes read FKeyStrokes write SetKeyStrokes;
    property FooterOptions:TRxDBGridFooterOptions read FFooterOptions write SetFooterOptions;

    //storage
    property PropertyStorage: TCustomPropertyStorage
      read GetPropertyStorage write SetPropertyStorage;
    property Version: integer read FVersion write FVersion default 0;
    {$IFDEF RxDBGridDepricatedProps}
    property AllowedOperations: TRxDBGridAllowedOperations
      read GetAllowedOperations write SetAllowedOperations default [aoInsert, aoUpdate, aoDelete, aoAppend]; deprecated;
    {$ENDIF}
    property OptionsRx: TOptionsRx read FOptionsRx write SetOptionsRx;
    property FooterColor: TColor read GetFooterColor write SetFooterColor default clWindow; deprecated;
    property FooterRowCount: integer read GetFooterRowCount write SetFooterRowCount default 0; deprecated;

    property OnFiltred: TNotifyEvent read FOnFiltred write FOnFiltred;
//    property Constraints:TRxDBGridCollumnConstraints read GetConstraints write SetConstraints;

    property OnSortChanged: TNotifyEvent read FOnSortChanged write FOnSortChanged;

    //from DBGrid
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance default aaRightDown;
    property AutoFillColumns;
    property AutoEdit;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property BorderColor;
    property DrawFullLine: boolean read GetDrawFullLine write SetDrawFullLine;
    property FocusColor;
    property FixedHotColor;

    property SelectedColor;
    property GridLineColor;
    property GridLineStyle;

    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight; // stored IsDefaultRowHeightStored;

    property DefaultColWidth;

    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property Flat;
    property Font;
    property HeaderHotZones;
    property HeaderPushZones;
    //property ImeMode;
    //property ImeName;
    property Options;
    property OptionsExtra;
    property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Scrollbars default ssBoth;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;

    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnColumnSized;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawColumnCell;
    property OnDblClick;
    property OnEditButtonClick;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
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
    property OnSelectEditor;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
    property OnRxCalcFooterValues: TOnRxCalcFooterValues read FOnRxCalcFooterValues write FOnRxCalcFooterValues;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;

    property OnCreateLookup: TCreateLookup read F_CreateLookup write F_CreateLookup;
    property OnDisplayLookup: TDisplayLookup read F_DisplayLookup write F_DisplayLookup;
  end;

  { TRxDBGridAbstractTools }

  TRxDBGridAbstractTools = class(TComponent)
  private
    FOnAfterExecute: TNotifyEvent;
    FOnBeforeExecute: TNotifyEvent;
    FShowSetupForm: boolean;
    procedure ExecTools(Sender:TObject);
  protected
    FRxDBGrid: TRxDBGrid;
    FCaption:string;
    procedure SetRxDBGrid(AValue: TRxDBGrid);
    function DoExecTools:boolean; virtual;
    function DoSetupTools:boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute:boolean;
  published
    property RxDBGrid:TRxDBGrid read FRxDBGrid write SetRxDBGrid;
    property Caption:string read FCaption write FCaption;
    property ShowSetupForm:boolean read FShowSetupForm write FShowSetupForm default false;
    property OnBeforeExecute:TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute:TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
  end;

procedure RegisterRxDBGridSortEngine(RxDBGridSortEngineClass: TRxDBGridSortEngineClass;
  DataSetClassName: string);

implementation

uses Math, rxdconst, rxstrutils, rxdbgrid_findunit, rxdbgrid_columsunit,
  rxlookup, tooledit, LCLProc, Clipbrd, rxfilterby, rxsortby, variants;

const
  EditorCommandStrs: array[rxgcNone .. High(TRxDBGridCommand)] of TIdentMapEntry =
    (
    (Value: Ord(rxgcNone); Name: 'rxcgNone'),
    (Value: Ord(rxgcShowFindDlg); Name: 'rxgcShowFindDlg'),
    (Value: Ord(rxgcShowColumnsDlg); Name: 'rxgcShowColumnsDlg'),
    (Value: Ord(rxgcShowFilterDlg); Name: 'rxgcShowFilterDlg'),
    (Value: Ord(rxgcShowSortDlg); Name: 'rxgcShowSortDlg'),
    (Value: Ord(rxgcShowQuickFilter); Name: 'rxgcShowQuickFilter'),
    (Value: Ord(rxgcHideQuickFilter); Name: 'rxgcHideQuickFilter'),
    (Value: Ord(rxgcSelectAll); Name: 'rxgcSelectAll'),
    (Value: Ord(rxgcDeSelectAll); Name: 'rxgcDeSelectAll'),
    (Value: Ord(rxgcInvertSelection); Name: 'rxgcInvertSelection'),
    (Value: Ord(rxgcOptimizeColumnsWidth); Name: 'rxgcOptimizeColumnsWidth'),
    (Value: Ord(rxgcCopyCellValue); Name: 'rxgcCopyCellValue')
    );

var
  RxDBGridSortEngineList: TStringList;

procedure RegisterRxDBGridSortEngine(
  RxDBGridSortEngineClass: TRxDBGridSortEngineClass; DataSetClassName: string);
var
  Pos: integer;
  RxDBGridSortEngine: TRxDBGridSortEngine;
begin
  if not RxDBGridSortEngineList.Find(DataSetClassName, Pos) then
  begin
    RxDBGridSortEngine := RxDBGridSortEngineClass.Create;
    RxDBGridSortEngineList.AddObject(DataSetClassName, RxDBGridSortEngine);
  end;
end;

procedure GridInvalidateRow(Grid: TRxDBGrid; Row: longint);
var
  I: longint;
begin
  for I := 0 to Grid.ColCount - 1 do
    Grid.InvalidateCell(I, Row);
end;

type
  THackDataLink = class(TDataLink);
  THackDataSet = class(TDataSet);


type

  { TRxDBGridLookupComboEditor }

  TRxDBGridLookupComboEditor = class(TRxCustomDBLookupCombo)
  private
    FGrid: TRxDBGrid;
    FCol, FRow: integer;
    FLDS: TDataSource;
  protected
    procedure WndProc(var TheMessage: TLMessage); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure OnInternalClosePopup(AResult:boolean);override;
    procedure ShowList; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TRxDBGridDateEditor }
  TRxDBGridDateEditor = class(TCustomRxDateEdit)
  private
    FGrid: TRxDBGrid;
    FCol, FRow: integer;
  protected
{$IFDEF OLD_EDITBUTTON}
    procedure Change; override;
{$ELSE}
    procedure EditChange; override;
{$ENDIF}
    procedure KeyDown(var Key: word; Shift: TShiftState); override;

    procedure WndProc(var TheMessage: TLMessage); override;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;

  public
    constructor Create(Aowner : TComponent); override;
    //procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure EditingDone; override;
  end;

{ TRxDBGridAbstractTools }

procedure TRxDBGridAbstractTools.SetRxDBGrid(AValue: TRxDBGrid);
begin
  if FRxDBGrid=AValue then Exit;
  if Assigned(FRxDBGrid) then
    FRxDBGrid.RemoveTools(Self);
  FRxDBGrid:=AValue;
  if Assigned(FRxDBGrid) then
    FRxDBGrid.AddTools(Self);
end;

function TRxDBGridAbstractTools.DoExecTools: boolean;
begin
  //
end;

function TRxDBGridAbstractTools.DoSetupTools: boolean;
begin
  Result:=true;
end;

procedure TRxDBGridAbstractTools.ExecTools(Sender: TObject);
begin
  Execute;
end;

constructor TRxDBGridAbstractTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:=Name;
  FShowSetupForm:=false;
end;

function TRxDBGridAbstractTools.Execute: boolean;
begin
  Result:=true;
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  if FShowSetupForm then
    Result:=DoSetupTools;

  if Result then
    Result:=DoExecTools;

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;


{ TRxDBGridCollumnConstraint }

procedure TRxDBGridCollumnConstraints.SetMaxWidth(AValue: integer);
begin
  if FMaxWidth=AValue then Exit;
  if (FMinWidth<>0) and (AValue<>0) and (AValue < FMinWidth) then exit;
  FMaxWidth:=AValue;
end;

procedure TRxDBGridCollumnConstraints.SetMinWidth(AValue: integer);
begin
  if FMinWidth=AValue then Exit;
  if (FMaxWidth<>0) and (AValue<>0) and (AValue > FMaxWidth) then exit;
  FMinWidth:=AValue;
end;

procedure TRxDBGridCollumnConstraints.AssignTo(Dest: TPersistent);
begin
  if Dest is TRxDBGridCollumnConstraints then
  begin
    TRxDBGridCollumnConstraints(Dest).FMinWidth:=FMinWidth;
    TRxDBGridCollumnConstraints(Dest).FMaxWidth:=FMaxWidth;
  end
  else
  inherited AssignTo(Dest);
end;

constructor TRxDBGridCollumnConstraints.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
end;

{ TRxFilterItems }

function TRxFilterItems.AcceptRecord: boolean;
var
  i:integer;
begin
  Result:=true;
  for i:=0 to Count-1 do
  begin
    if not TRxFilterItem(Items[i]).TestValue then
    begin
      Result:=false;
      exit;
    end;
  end;
end;

{ TRxFilterItem }

function TRxFilterItem.TestValue: Boolean;
var
  ATestValue: string;
begin
  ATestValue:=FCol.Field.DisplayText;
  case OpCode of
    fopEQ:Result:=ATestValue = FVAlue;
    fopNotEQ:Result:=ATestValue <> FVAlue;
    fopStartFrom:Result:=UTF8Copy(ATestValue, 1, UTF8Length(FVAlue)) = FVAlue;
    fopEndTo:Result:=Copy(ATestValue, UTF8Length(ATestValue) - UTF8Length(FVAlue), UTF8Length(FVAlue)) = FVAlue;
    fopLike:Result:=UTF8Pos(FVAlue, ATestValue) > 0;
    fopNotLike:Result:=UTF8Pos(FVAlue, ATestValue) = 0;
  else
    Result:=false;
  end;
end;

{ TRxDbGridColumnsSortList }

function TRxDbGridColumnsSortList.GetCollumn(Index: Integer): TRxColumn;
begin
  Result:=TRxColumn(Items[Index]);
end;

{ TRxColumnEditButton }

function TRxColumnEditButton.GetGlyph: TBitmap;
begin
  Result:=FButton.Glyph;
end;

function TRxColumnEditButton.GetHint: String;
begin
  Result:=FButton.Hint;
end;

function TRxColumnEditButton.GetNumGlyphs: Integer;
begin
  Result:=FButton.NumGlyphs;
end;

function TRxColumnEditButton.GetOnButtonClick: TNotifyEvent;
begin
  Result:=FButton.OnClick;
end;

function TRxColumnEditButton.GetWidth: Integer;
begin
  Result:=FButton.Width;
end;

procedure TRxColumnEditButton.SetGlyph(AValue: TBitmap);
begin
  FButton.Glyph.Assign(AValue);
  if not (csLoading in TRxDBGrid(TRxColumnEditButtons(Collection).Owner).ComponentState) then
    FStyle:=ebsGlyphRx;
end;

procedure TRxColumnEditButton.SetHint(AValue: String);
begin
  FButton.Hint:=AValue;
  FSpinBtn.Hint:=AValue;
end;

procedure TRxColumnEditButton.SetNumGlyphs(AValue: Integer);
begin
  FButton.NumGlyphs:=AValue;
end;

procedure TRxColumnEditButton.SetOnButtonClick(AValue: TNotifyEvent);
begin
  FButton.OnClick:=AValue;
end;

procedure TRxColumnEditButton.SetStyle(AValue: TRxColumnEditButtonStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
end;

procedure TRxColumnEditButton.SetVisible(AValue: Boolean);
begin
//  if FVisible=AValue then Exit;
  FVisible:=AValue;

  if AValue then
  begin
    if Style = ebsUpDownRx then
    begin
      FSpinBtn.Visible:=AValue;
      FButton.Visible:=false;
    end
    else
    begin
      FButton.Visible:=AValue;
      FSpinBtn.Visible:=false;
    end;
  end
  else
  begin
    FButton.Visible:=AValue;
    FSpinBtn.Visible:=AValue;
  end;
end;

procedure TRxColumnEditButton.SetWidth(AValue: Integer);
begin
  FButton.Width:=AValue;
  FSpinBtn.Width:=AValue;
end;

procedure TRxColumnEditButton.DoBottomClick(Sender: TObject);
var
  F:TField;
  Col:TRxColumn;

  msg: TGridMessage;
begin
  Col:=TRxColumnEditButtons(Collection).FOwner as TRxColumn;
  F:=Col.Field;

  if Assigned(F) and (F.DataType in NumericDataTypes) then
  begin
    if not (F.DataSet.State in dsEditModes) then
      F.DataSet.Edit;
    F.Value:=F.Value - 1;

    Msg.LclMsg.msg:=GM_SETVALUE;
    Msg.Grid:=Col.Grid;
{    Msg.Col:=FCol;
    Msg.Row:=FRow;}
    Msg.Value:=F.DisplayText;
    TRxDBGrid(Col.Grid).Editor.Dispatch(Msg);

  end;
end;

procedure TRxColumnEditButton.DoTopClick(Sender: TObject);
var
  F:TField;
  Col:TRxColumn;

  msg: TGridMessage;
begin
  Col:=TRxColumnEditButtons(Collection).FOwner as TRxColumn;
  F:=Col.Field;

  if Assigned(F) and (F.DataType in NumericDataTypes) then
  begin
    if not (F.DataSet.State in dsEditModes) then
      F.DataSet.Edit;
    F.Value:=F.Value + 1;

    Msg.LclMsg.msg:=GM_SETVALUE;
    Msg.Grid:=Col.Grid;
{    Msg.Col:=FCol;
    Msg.Row:=FRow;}
    Msg.Value:=F.DisplayText;
    TRxDBGrid(Col.Grid).Editor.Dispatch(Msg);

  end;
end;

function TRxColumnEditButton.GetDisplayName: string;
begin
  if Hint<>'' then
    Result:=Hint
  else
    Result:='TRxColumnEditButton';
end;

constructor TRxColumnEditButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FButton:=TSpeedButton.Create(nil);
  FButton.Glyph:=LoadLazResBitmapImage('rx_markerdown');
  FSpinBtn:=TRxSpinButton.Create(nil);
  FSpinBtn.OnBottomClick:=@DoBottomClick;
  FSpinBtn.OnTopClick:=@DoTopClick;

  FVisible:=true;
  Width:=15;
end;

destructor TRxColumnEditButton.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FSpinBtn);
  inherited Destroy;
end;

{ TRxColumnEditButtons }

function TRxColumnEditButtons.GetItem(Index: integer): TRxColumnEditButton;
begin
  Result:= TRxColumnEditButton(inherited Items[Index]);
end;

procedure TRxColumnEditButtons.SetItem(Index: integer;
  AValue: TRxColumnEditButton);
begin
  inherited SetItem(Index, AValue);
end;

procedure TRxColumnEditButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

constructor TRxColumnEditButtons.Create(AOwner: TPersistent);
begin
  inherited Create(TRxColumnEditButton);
  FOwner:=AOwner;
end;

function TRxColumnEditButtons.Add: TRxColumnEditButton;
begin
  Result := TRxColumnEditButton(inherited Add);
end;

{ TRxDBGridFooterOptions }

procedure TRxDBGridFooterOptions.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;

  { TODO : Устаревший код - в следующей версии необходимо убрать }
  if FActive then
    FOwner.FOptionsRx:=FOwner.FOptionsRx + [rdgFooterRows]
  else
    FOwner.FOptionsRx:=FOwner.FOptionsRx - [rdgFooterRows];

  FOwner.VisualChange;
end;

procedure TRxDBGridFooterOptions.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FOwner.Invalidate;
end;

procedure TRxDBGridFooterOptions.SetRowCount(AValue: integer);
begin
  if FRowCount=AValue then Exit;
  FRowCount:=AValue;
  FOwner.VisualChange;
end;

procedure TRxDBGridFooterOptions.SetStyle(AValue: TTitleStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
end;

procedure TRxDBGridFooterOptions.AssignTo(Dest: TPersistent);
var
  FO:TRxDBGridFooterOptions absolute Dest;
begin
  if Dest is TRxDBGridFooterOptions then
  begin
    FO.Active:=Active;
    FO.Color:=Color;
    FO.RowCount:=RowCount;
    FO.Style:=Style;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TRxDBGridFooterOptions.Create(Owner: TRxDBGrid);
begin
  inherited Create;
  FOwner:=Owner;

  FColor := clWindow;
  FRowCount := 0;
  FStyle := tsLazarus;
end;

destructor TRxDBGridFooterOptions.Destroy;
begin
  inherited Destroy;
end;


{ TRxDBGridDateEditor }

{$IFDEF OLD_EDITBUTTON}
procedure TRxDBGridDateEditor.Change;
{$ELSE}
procedure TRxDBGridDateEditor.EditChange;
{$ENDIF}
var
  D:TDateTime;
begin
  {$IFDEF OLD_EDITBUTTON}
  inherited Change;
  {$ELSE}
  inherited EditChange;
  {$ENDIF}
  if Assigned(FGrid) and FGrid.DatalinkActive and not FGrid.EditorIsReadOnly then
  begin
    if not (FGrid.DataSource.DataSet.State in dsEditModes) then
      FGrid.DataSource.Edit;
    if Self.Text <> '' then
      FGrid.SelectedField.AsDateTime := Self.Date
    else
      FGrid.SelectedField.Clear;

    if FGrid <> nil then
    begin
      if TryStrToDate(Text, D) then
        FGrid.SetEditText(FCol, FRow, Text)
      else
        FGrid.SetEditText(FCol, FRow, '');
    end;
  end;
end;

procedure TRxDBGridDateEditor.KeyDown(var Key: word; Shift: TShiftState);

  function AllSelected: boolean;
  begin
    Result := (SelLength > 0) and (SelLength = UTF8Length(Text));
  end;

  function AtStart: boolean;
  begin
    Result := (SelStart = 0);
  end;

  function AtEnd: boolean;
  begin
    Result := ((SelStart + 1) > UTF8Length(Text)) or AllSelected;
  end;

  procedure doEditorKeyDown;
  begin
    if FGrid <> nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;

  procedure doGridKeyDown;
  begin
    if FGrid <> nil then
      FGrid.KeyDown(Key, shift);
  end;

  function GetFastEntry: boolean;
  begin
    if FGrid <> nil then
      Result := FGrid.FastEditing
    else
      Result := False;
  end;

  procedure CheckEditingKey;
  begin
    if (FGrid = nil) or FGrid.EditorIsReadOnly then
      Key := 0;
  end;

var
  IntSel: boolean;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_F2:
      if AllSelected then
      begin
        SelLength := 0;
        SelStart := Length(Text);
      end;
    VK_DELETE:
      CheckEditingKey;
    VK_UP, VK_DOWN:
      doGridKeyDown;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then
      begin
        IntSel :=
          ((Key = VK_LEFT) and not AtStart) or ((Key = VK_RIGHT) and not AtEnd);
        if not IntSel then
        begin
          doGridKeyDown;
        end;
      end;
    VK_END, VK_HOME:
      ;
    else
      doEditorKeyDown;
  end;
end;

procedure TRxDBGridDateEditor.WndProc(var TheMessage: TLMessage);
begin
  if FGrid<>nil then
    case TheMessage.Msg of
      LM_CLEAR,
      LM_CUT,
      LM_PASTE:
        begin
          if FGrid.EditorIsReadOnly then
            exit;
        end;
    end;

  if TheMessage.msg = LM_KILLFOCUS then
  begin
    if HWND(TheMessage.WParam) = HWND(Handle) then
    begin
      // lost the focus but it returns to ourselves
      // eat the message.
      TheMessage.Result := 0;
      exit;
    end;
  end;
  inherited WndProc(TheMessage);
end;

procedure TRxDBGridDateEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid := Msg.Grid as TRxDBGrid;
  Msg.Options := EO_AUTOSIZE or EO_SELECTALL
  {or EO_HOOKEXIT or EO_HOOKKEYPRESS or EO_HOOKKEYUP};
end;

procedure TRxDBGridDateEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Self.Date := FGrid.SelectedField.AsDateTime;
end;

procedure TRxDBGridDateEditor.msg_GetValue(var Msg: TGridMessage);
var
  sText: string;
begin
  sText := Text;
  Msg.Value := sText;
end;

procedure TRxDBGridDateEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

constructor TRxDBGridDateEditor.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  AutoSize := false;
  UpdateMask;
end;

                    {
procedure TRxDBGridDateEditor.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  Dec(aWidth, 25); //ButtonWidth);
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;               }


procedure TRxDBGridDateEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid <> nil then
    FGrid.EditingDone;
end;


{ TRxDBGridLookupComboEditor }

procedure TRxDBGridLookupComboEditor.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.msg = LM_KILLFOCUS then
  begin
    if HWND(TheMessage.WParam) = HWND(Handle) then
    begin
      // lost the focus but it returns to ourselves
      // eat the message.
      TheMessage.Result := 0;
      exit;
    end;
  end;
  inherited WndProc(TheMessage);
end;

procedure TRxDBGridLookupComboEditor.KeyDown(var Key: word; Shift: TShiftState);

  procedure doGridKeyDown;
  begin
    if Assigned(FGrid) then
//      FGrid.EditorkeyDown(Self, key, shift);
      FGrid.KeyDown(Key, shift);
  end;

  procedure doEditorKeyDown;
  begin
    if FGrid <> nil then
      FGrid.EditorkeyDown(Self, key, shift);
  end;

  function GetFastEntry: boolean;
  begin
    if FGrid <> nil then
      Result := FGrid.FastEditing
    else
      Result := False;
  end;
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or FGrid.EditorIsReadOnly then
      Key := 0;
  end;

begin
  CheckEditingKey;
  case Key of
    VK_UP,
    VK_DOWN:
      if (not PopupVisible) and (not (ssAlt in Shift)) then
      begin
        doGridKeyDown;
        Key:=0;
        exit;
      end;
    VK_LEFT, VK_RIGHT:
      if GetFastEntry then
      begin
        doGridKeyDown;
        exit;
      end;
    else
    begin
      inherited KeyDown(Key, Shift);
      doEditorKeyDown;
      exit;
    end;
  end;
  inherited KeyDown(Key, Shift);
{  if FGrid<>nil then
    FGrid.EditingDone;}
end;

procedure TRxDBGridLookupComboEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid := Msg.Grid as TRxDBGrid;
  Msg.Options := EO_AUTOSIZE;
end;

procedure TRxDBGridLookupComboEditor.msg_SetValue(var Msg: TGridMessage);
var
  F: TField;
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
  F := FGrid.SelectedField;
  DataSource := FGrid.DataSource;
  if Assigned(F) then
  begin
    //    DataField:=F.FieldName;
    DataField := F.KeyFields;
    LookupDisplay := F.LookupResultField;
    LookupField := F.LookupKeyFields;
    FLDS.DataSet := F.LookupDataSet;
    FGrid.GetOnCreateLookup;
  end;
end;

procedure TRxDBGridLookupComboEditor.msg_GetValue(var Msg: TGridMessage);
var
  sText: string;
  F:TField;
begin
  if Assigned(FGrid.SelectedField) and Assigned(FLDS.DataSet) then
  begin
    F:=FLDS.DataSet.FieldByName(LookupDisplay);
    if Assigned(F) then
    begin
      sText := F.DisplayText;
      Msg.Value := sText;
    end;
  end;
end;

procedure TRxDBGridLookupComboEditor.ShowList;
begin
  FGrid.GetOnDisplayLookup;
  inherited ShowList;
end;

procedure TRxDBGridLookupComboEditor.OnInternalClosePopup(AResult: boolean);
  procedure CheckEditingKey;
  begin
    if (FGrid=nil) or FGrid.EditorIsReadOnly then
//      Key := 0;
  end;
var
  F:TField;
begin
  inherited OnInternalClosePopup(AResult);
  CheckEditingKey;
  if (AResult) and Assigned(FGrid.SelectedField) and Assigned(FLDS.DataSet) then
  begin
    F:=FLDS.DataSet.FieldByName(LookupDisplay);
    if Assigned(F) then
    begin
      //FGrid.SelectedField.Assign(F);
      if (FGrid<>nil) and Visible then begin
        FGrid.SetEditText(FCol, FRow, F.DisplayText);
      end;
    end;
  end;
end;

constructor TRxDBGridLookupComboEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLDS := TDataSource.Create(nil);
  LookupSource := FLDS;
  AutoSize := false;
end;

destructor TRxDBGridLookupComboEditor.Destroy;
begin
  FreeAndNil(FLDS);
  inherited Destroy;
end;

{ TRxDBGrid }
const
  ALIGN_FLAGS: array[TAlignment] of integer =
    (DT_LEFT or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX,
    DT_RIGHT or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX,
    DT_CENTER or DT_SINGLELINE {or DT_EXPANDTABS} or DT_NOPREFIX);

const
  ALIGN_FLAGS_HEADER: array[TAlignment] of integer =
    (DT_LEFT or {DT_EXPANDTABS or} DT_NOPREFIX,
    DT_RIGHT or {DT_EXPANDTABS or }DT_NOPREFIX,
    DT_CENTER or {DT_EXPANDTABS or }DT_NOPREFIX);

{  TITLE_SUBHEADER = 2;
  TITLE_DEFAULT = 1;

const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);}

procedure WriteTextHeader(ACanvas: TCanvas; ARect: TRect; const Text: string;
  Alignment: TAlignment);
var
  DrawRect: TRect;
  W, CnvW: integer;
begin
  DrawRect := Rect(ARect.Left + 1, ARect.Top + 1, ARect.Right, ARect.Bottom);

  CnvW := Max(DrawRect.Right - DrawRect.Left, 1);
  W := (ACanvas.TextWidth(Text) div CnvW) + 1;

  DrawRect.Top := ((ARect.Top + ARect.Bottom) div 2) - W * ACanvas.TextHeight('Wg') div 2;
  if DrawRect.Top < ARect.Top + 1 then
    DrawRect.Top := ARect.Top + 1;

  SetBkMode(ACanvas.Handle, TRANSPARENT);
  DrawText(ACanvas.Handle, PChar(Text), Length(Text), DrawRect,
    //    DT_VCENTER or  DT_WORDBREAK or DT_CENTER
    ALIGN_FLAGS_HEADER[Alignment] {or DT_VCENTER or  DT_END_ELLIPSIS } or DT_WORDBREAK
    );
end;

procedure TRxDBGrid.SetTitleButtons(const AValue: boolean);
begin
  if AValue then
    Options := Options + [dgHeaderPushedLook]
  else
    Options := Options - [dgHeaderPushedLook];
end;

procedure TRxDBGrid.GetScrollbarParams(out aRange, aPage, aPos: Integer);
begin
  if (DataSource.DataSet<>nil) and DataSource.DataSet.Active then begin
    if DataSource.DataSet.IsSequenced then begin
      aRange := DataSource.DataSet.RecordCount + VisibleRowCount - 1;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      if DataSource.DataSet.BOF then aPos := 0 else
      if DataSource.DataSet.EOF then aPos := aRange
      else
        aPos := DataSource.DataSet.RecNo - 1; // RecNo is 1 based
      if aPos<0 then aPos:=0;
    end else begin
      aRange := 6;
      aPage := 2;
      if DataSource.DataSet.EOF then aPos := 4 else
      if DataSource.DataSet.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;
end;

procedure TRxDBGrid.RestoreEditor;
begin
  if EditorMode then
  begin
    EditorMode := False;
    EditorMode := True;
  end;
end;

procedure TRxDBGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos, aRange, aPage: Integer;
  DeltaRec: integer;

  function MaxPos: Integer;
  begin
    if IsSeq then
      result := DataSource.DataSet.RecordCount - 1
    else
      result := 4;
  end;

  procedure DsMoveBy(Delta: Integer);
  begin
    DataSource.DataSet.MoveBy(Delta);
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  procedure DsGoto(BOF: boolean);
  begin
    if BOF then DataSource.DataSet.First
    else        DataSource.DataSet.Last;
    GetScrollbarParams(aRange, aPage, aPos);
  end;

begin
  if not DatalinkActive then exit;
  IsSeq := DataSource.DataSet.IsSequenced;
  case Message.ScrollCode of
    SB_TOP:
      DsGoto(True);
    SB_BOTTOM:
      DsGoto(False);
    SB_PAGEUP:
      DsMoveBy(-VisibleRowCount);
    SB_LINEUP:
      DsMoveBy(-1);
    SB_LINEDOWN:
      DsMoveBy(1);
    SB_PAGEDOWN:
      DsMoveBy(VisibleRowCount);
    SB_THUMBPOSITION:
      DsMoveBy(Message.Pos - FOldPosition)
    else
      Exit;
  end;

  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;

  if EditorMode then
    RestoreEditor;
  {$ifdef dbgDBGrid}
  DebugLn('---- Diff=',dbgs(DeltaRec), ' FinalPos=',dbgs(aPos));
  {$endif}
end;

procedure TRxDBGrid.SetAutoSort(const AValue: boolean);
var
  S: string;
  Pos: integer;
begin
  if FAutoSort = AValue then
    exit;
  FAutoSort := AValue;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) and
    DataSource.DataSet.Active then
  begin
    S := DataSource.DataSet.ClassName;
    if RxDBGridSortEngineList.Find(S, Pos) then
      FSortEngine := RxDBGridSortEngineList.Objects[Pos] as TRxDBGridSortEngine
    else
      FSortEngine := nil;
{    FSortField := nil;
    FSortOrder := smNone;}
    FSortColumns.Clear;
  end;
end;

function TRxDBGrid.GetColumns: TRxDbGridColumns;
begin
  Result := TRxDbGridColumns(TCustomDrawGrid(Self).Columns);
end;

function TRxDBGrid.GetFooterColor: TColor;
begin
  Result:=FFooterOptions.FColor;
end;

function TRxDBGrid.GetFooterRowCount: integer;
begin
  Result:=FFooterOptions.RowCount;
end;

function TRxDBGrid.GetMarkerDown: TBitmap;
begin
  Result:=FMarkerDown;
end;

function TRxDBGrid.GetMarkerUp: TBitmap;
begin
  Result:=FMarkerUp;
end;

function TRxDBGrid.GetDrawFullLine: boolean;
begin
  Result := FDrawFullLine;
end;

procedure TRxDBGrid.SetDrawFullLine(Value: boolean);
begin
  FDrawFullLine := Value;
  VisualChange;
end;

procedure TRxDBGrid.DoCreateJMenu;

  procedure CreateMenuItem(ShortCut: char; const ACaption: string;
    MenuAction: TNotifyEvent);
  var
    R: TMenuItem;
  begin
    R := TMenuItem.Create(F_PopupMenu);
    F_PopupMenu.Items.Add(R);
    R.Caption := ACaption;
    if ShortCut <> #0 then
      R.ShortCut := KeyToShortCut(Ord(ShortCut), [ssCtrl]);
    R.OnClick := MenuAction;
  end;

begin
  F_PopupMenu := TPopupMenu.Create(Self);
  F_PopupMenu.Name := 'OptionsMenu';
  CreateMenuItem('F', sRxDBGridFind, @OnFind);
  CreateMenuItem('T', sRxDBGridFilter, @OnFilterBy);
  CreateMenuItem('E', sRxDBGridFilterSimple, @OnFilter);
  CreateMenuItem('Q', sRxDBGridFilterClear, @OnFilterClose);
  CreateMenuItem(#0, '-', nil);
  CreateMenuItem('C', sRxDBGridSortByColumns, @OnSortBy);
  CreateMenuItem('W', sRxDBGridSelectColumns, @OnChooseVisibleFields);
  CreateMenuItem('A', sRxDBGridSelectAllRows, @OnSelectAllRows);
  CreateMenuItem(#0, sRxDBGridCopyCellValue, @OnCopyCellValue);
end;

{$IFDEF RxDBGridDepricatedProps}
function TRxDBGrid.GetAllowedOperations: TRxDBGridAllowedOperations;
begin
  Result:=[];
  if dgDisableInsert in Options then
    Result:=Result - [aoInsert, aoAppend]
  else
    Result:=Result + [aoInsert, aoAppend]
    ;

  if dgDisableDelete in Options then
    Result:=Result - [aoDelete]
  else
    Result:=Result + [aoDelete]
    ;
end;
{$ENDIF}

function TRxDBGrid.GetPropertyStorage: TCustomPropertyStorage;
begin
  Result := FPropertyStorageLink.Storage;
end;

function TRxDBGrid.GetSortField: string;
begin
  if FSortColumns.Count > 0 then
    Result:=FSortColumns[0].GetSortFields
  else
    Result:='';
end;

function TRxDBGrid.GetSortOrder: TSortMarker;
begin
  if FSortColumns.Count > 0 then
    Result:=FSortColumns[0].SortOrder
  else
    Result:=smNone;
end;

function TRxDBGrid.GetTitleButtons: boolean;
begin
  Result := dgHeaderPushedLook in Options;
end;

function TRxDBGrid.IsColumnsStored: boolean;
begin
  Result := TRxDbGridColumns(TCustomDrawGrid(Self).Columns).Enabled;
end;

{$IFDEF RxDBGridDepricatedProps}
procedure TRxDBGrid.SetAllowedOperations(AValue: TRxDBGridAllowedOperations);
begin
//  FAllowedOperations:=AValue;
  if [aoInsert, aoAppend] * AValue <> [aoInsert, aoAppend] then
    Options:=Options - [dgDisableInsert]
  else
    Options:=Options + [dgDisableInsert];

  if aoDelete in AValue then
    Options:=Options - [dgDisableDelete]
  else
    Options:=Options + [dgDisableDelete]
end;
{$ENDIF}

procedure TRxDBGrid.SetColumns(const AValue: TRxDbGridColumns);
begin
  TRxDbGridColumns(TCustomDrawGrid(Self).Columns).Assign(Avalue);
end;

procedure TRxDBGrid.SetFooterColor(const AValue: TColor);
begin
  FFooterOptions.Color := AValue;
end;

procedure TRxDBGrid.SetFooterOptions(AValue: TRxDBGridFooterOptions);
begin
  FFooterOptions.AssignTo(AValue);
end;

procedure TRxDBGrid.SetFooterRowCount(const AValue: integer);
begin
  FFooterOptions.RowCount:=AValue;
end;

procedure TRxDBGrid.SetKeyStrokes(const AValue: TRxDBGridKeyStrokes);
begin
  if Assigned(AValue) then
    FKeyStrokes.Assign(AValue)
  else
    FKeyStrokes.Clear;

  UpdateJMenuKeys;
end;

procedure TRxDBGrid.SetMarkerDown(AValue: TBitmap);
begin
  FMarkerDown.Assign(AValue);
end;

procedure TRxDBGrid.SetMarkerUp(AValue: TBitmap);
begin
  FMarkerUp.Assign(AValue);
end;

procedure TRxDBGrid.SetOptionsRx(const AValue: TOptionsRx);
var
  OldOpt: TOptionsRx;
begin
  if FOptionsRx = AValue then
    exit;
  BeginUpdate;
  OldOpt := FOptionsRx;
  FOptionsRx := AValue;
  UseXORFeatures := rdgXORColSizing in AValue;
  if (rdgFilter in AValue) and not (rdgFilter in OldOpt) then
  begin
    LayoutChanged;
{    BeginUpdate;
    CalcTitle;
    EndUpdate;}
  end
  else
  if rdgFilter in OldOpt then
  begin
    FFilterListEditor.Hide;
    LayoutChanged;
{    BeginUpdate;
    CalcTitle;
    EndUpdate;}
  end;

  FFooterOptions.FActive:=rdgFooterRows in FOptionsRx;

  if (rdgWordWrap in OldOpt) and not (rdgWordWrap in FOptionsRx) then
    ResetRowHeght;

//  VisualChange;
  EndUpdate;
end;

procedure TRxDBGrid.SetPropertyStorage(const AValue: TCustomPropertyStorage);
begin
  FPropertyStorageLink.Storage := AValue;
end;

function TRxDBGrid.DatalinkActive: boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and
    DataSource.DataSet.Active;
end;

procedure TRxDBGrid.AdjustEditorBounds(NewCol, NewRow: Integer);
begin
  inherited AdjustEditorBounds(NewCol, NewRow);
  if EditorMode then
  begin
    DoSetColEdtBtn;
  end;
end;

procedure TRxDBGrid.TrackButton(X, Y: integer);
var
  Cell: TGridCoord;
  NewPressed: boolean;
  I, Offset: integer;
begin
  Cell := MouseCoord(X, Y);
  Offset := RowCount;//[0];
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
    (FPressedCol = TColumn(ColumnFromGridColumn(Cell.X))) and (Cell.Y < Offset);
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
  i, j: integer;
  H, H1, W, H2, W1: integer;
  rxCol, rxColNext: TRxColumn;
  rxTit, rxTitleNext: TRxColumnTitle;
  MLRec1, P: TMLCaptionItem;
  MLRec2: TMLCaptionItem;
  tmpCanvas: TCanvas;
  S: string;
begin
  { TODO -oalexs : need rewrite code - split to 2 step:
1. make links between column
2. calc title width for all linked column series }
  if RowCount = 0 then
    exit;
  tmpCanvas := GetWorkingCanvas(Canvas);
  try
    H := 1;
    ClearMLCaptionPointers;
    for i := 0 to Columns.Count - 1 do
    begin
      rxCol := TRxColumn(Columns[i]);
      if Assigned(rxCol) and rxCol.Visible then
      begin
        rxTit := TRxColumnTitle(rxCol.Title);
        if Assigned(rxTit) then
        begin
          if rxTit.Orientation in [toVertical270, toVertical90] then
            H := Max((tmpCanvas.TextWidth(Columns[i].Title.Caption) +
              tmpCanvas.TextWidth('W')*2) div DefaultRowHeight, H)
          else
          begin
            rxColNext := nil;
            rxTitleNext := nil;
            if i < Columns.Count - 1 then
            begin
              rxColNext := TRxColumn(Columns[i + 1]);
              rxTitleNext := TRxColumnTitle(rxColNext.Title);
            end;

            W := Max(rxCol.Width - 6, 1);
            if rxTit.CaptionLinesCount > 0 then
            begin
              H2 := 0;
              H1 := 0;
              for j := 0 to rxTit.CaptionLinesCount - 1 do
              begin
                MLRec1 := rxTit.CaptionLine(j);

                if Assigned(rxTitleNext) and (rxTitleNext.CaptionLinesCount > j) then
                begin
                  //make links to next column - and in the next column set link to prior-current
                  MLRec2 := rxTitleNext.CaptionLine(j);
                  if MLRec1.Caption = MLRec2.Caption then
                  begin
                    MLRec1.Next := MLRec2;
                    MLRec2.Prior := MLRec1;
                  end
                  else
                    break;
                end;

                MLRec1.Width := tmpCanvas.TextWidth(MLRec1.Caption) + 2;

                if W > MLRec1.Width then
                  H2 := 1
                else
                  H2 := MLRec1.Width div W + 1;

                if H2 > WordCount(MLRec1.Caption, [' ']) then
                  H2 := WordCount(MLRec1.Caption, [' ']);

                H1 := H1 + H2;
              end;
            end
            else
            begin
              H1 := Max((tmpCanvas.TextWidth(rxTit.Caption) + 2) div W + 1, H);
              if H1 > WordCount(rxTit.Caption, [' ']) then
                H1 := WordCount(rxTit.Caption, [' ']);
            end;
            H := Max(H1, H);
          end;

          for j := 0 to rxTit.CaptionLinesCount - 1 do
          begin
            MLRec1 := rxTit.CaptionLine(j);
            if MLRec1.Width < rxTit.Column.Width then
              MLRec1.Width := rxTit.Column.Width;
          end;

        end;
      end;
    end;

    //Тут расчёт высоты заголовка каждой колонки - надо обработать слитые заголовки
    H := 1;
    for i := 0 to Columns.Count - 1 do
    begin
      rxCol := TRxColumn(Columns[i]);
      rxTit := TRxColumnTitle(rxCol.Title);
      H1 := 0;
      //Не забудем про вертикальную ориентацию
      if Assigned(rxCol) and rxCol.Visible and Assigned(rxTit) then
      begin
        if rxTit.Orientation in [toVertical270, toVertical90] then
          H1 := Max((tmpCanvas.TextWidth(Columns[i].Title.Caption) +
            tmpCanvas.TextWidth('W')) div DefaultRowHeight, H)
        else
        begin
          if rxTit.CaptionLinesCount > H then
            H := rxTit.CaptionLinesCount;
          for j := 0 to rxTit.CaptionLinesCount - 1 do
          begin
            MLRec1 := rxTit.CaptionLine(j);
            S := MLRec1.Caption;
            if not Assigned(MLRec1.Prior) then
            begin
              W := rxCol.Width;
              P := MLRec1.Next;
              while Assigned(P) do
              begin
                Inc(W, P.Col.Width);
                P := P.Next;
              end;
              W1 := tmpCanvas.TextWidth(MLRec1.Caption) + 2;
              if W1 > W then
                MLRec1.Hegth := Min(W1 div Max(W, 1) + 1, UTF8Length(MLRec1.Caption))
              else
                MLRec1.Hegth := 1;

              P := MLRec1.Next;
              while Assigned(P) do
              begin
                P.Hegth := MLRec1.Hegth;
                P := P.Next;
              end;
            end;
            H1 := H1 + MLRec1.Hegth;
          end;
        end;
      end;
      if H1 > H then
        H := H1;
    end;

    RowHeights[0] := DefaultRowHeight * H;

    if rdgFilter in OptionsRx then
    begin
      if Assigned(FFilterListEditor) then
      begin
        RowHeights[0] := RowHeights[0] + FFilterListEditor.Height
      end
      else
      begin
        RowHeights[0] := RowHeights[0] + DefaultRowHeight;
      end;
    end;

  finally
    if TmpCanvas <> Canvas then
      FreeWorkingCanvas(tmpCanvas);
  end;
end;

procedure TRxDBGrid.ClearMLCaptionPointers;
var
  i, j: integer;
  rxCol: TRxColumn;
  rxTit: TRxColumnTitle;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    rxCol := TRxColumn(Columns[i]);
    if Assigned(rxCol) then
    begin
      rxTit := TRxColumnTitle(rxCol.Title);
      if Assigned(rxTit) then
      begin
        for j := 0 to rxTit.CaptionLinesCount - 1 do
        begin
          rxTit.CaptionLine(j).Next := nil;
          rxTit.CaptionLine(j).Prior := nil;
        end;
      end;
    end;
  end;
end;

function TRxDBGrid.getFilterRect(bRect: TRect): TRect;
begin
  Result := bRect;
  if Assigned(FFilterListEditor) then
    Result.Top := bRect.Bottom - FFilterListEditor.Height
  else
    Result.Top := bRect.Bottom - DefaultRowHeight;
end;

function TRxDBGrid.getTitleRect(bRect: TRect): TRect;
begin
  Result := bRect;
  if Assigned(FFilterListEditor) then
    Result.Bottom := bRect.Bottom - FFilterListEditor.Height
  else
    Result.Bottom := bRect.Bottom - DefaultRowHeight;
end;

procedure TRxDBGrid.OutCaptionCellText(aCol, aRow: integer; const aRect: TRect;
  aState: TGridDrawState; const ACaption: string);
var
  T1, T2: TTextStyle;
begin
  if (TitleStyle = tsNative) then
    DrawThemedCell(aCol, aRow, aRect, aState)
  else
  begin
    Canvas.FillRect(aRect);
    DrawCellGrid(aCol, aRow, aRect, aState);
  end;

  if ACaption <> '' then
  begin
{    T1:=Canvas.TextStyle;
    T2:=T1;
    T1.Wordbreak:=true;
    Canvas.TextStyle:=T1;
    DrawCellText(aCol, aRow, aRect, aState, ACaption);
    Canvas.TextStyle:=T2;     }
    WriteTextHeader(Canvas, aRect, ACaption, GetColumnAlignment(aCol, True));
  end;
end;

procedure TRxDBGrid.OutCaptionCellText90(aCol, aRow: integer;
  const aRect: TRect; aState: TGridDrawState; const ACaption: string;
  const TextOrient: TTextOrientation);
var
  dW, dY: integer;
begin
  if (TitleStyle = tsNative) then
    DrawThemedCell(aCol, aRow, aRect, aState)
  else
  begin
    Canvas.FillRect(aRect);
    DrawCellGrid(aCol, aRow, aRect, aState);
  end;


  if TextOrient in [toVertical90, toVertical270] then
  begin
    dW := ((aRect.Bottom - aRect.Top) - Canvas.TextWidth(ACaption)) div 2;
    dY := ((aRect.Right - aRect.Left) - Canvas.TextHeight(ACaption)) div 2;
  end
  else
  begin
    dW := 0;
    dY := 0;
  end;
  OutTextXY90(Canvas, aRect.Left + dY, aRect.Top + dw, ACaption, TextOrient);
end;

procedure TRxDBGrid.OutCaptionSortMarker(const aRect: TRect;
  ASortMarker: TSortMarker; ASortPosition: integer);
var
  X, Y, W: integer;
  S:string;
  F:TFont;
begin
  if (dgHeaderPushedLook in Options) then
  begin
    if (ASortMarker <> smNone) and (ASortPosition>0) then
    begin
      F:=TFont.Create;
      F.Assign(Font);

      if Font.Size = 0 then
        Font.Size:=7
      else
        Font.Size:=Font.Size-2;
      S:='('+IntToStr(ASortPosition)+')';
      W:=Canvas.TextWidth(S) + 10;
    end
    else
    begin
      W:=6;
      F:=nil;
    end;

    if ASortMarker = smDown then
    begin
      X := aRect.Right - FMarkerDown.Width - W;
      Y := Trunc((aRect.Top + aRect.Bottom - FMarkerDown.Height) / 2);
      Canvas.Draw(X, Y, FMarkerDown);
    end
    else
    if ASortMarker = smUp then
    begin
      X := aRect.Right - FMarkerUp.Width - W;
      Y := Trunc((aRect.Top + aRect.Bottom - FMarkerUp.Height) / 2);
      Canvas.Draw(X, Y, FMarkerUp);
    end;

    if Assigned(F) then
    begin
      Canvas.TextOut( X + FMarkerDown.Width, Y,  S);
      Font.Assign(F);
      FreeAndNil(F);
    end;

  end;
end;

procedure TRxDBGrid.OutCaptionMLCellText(aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState; MLI: TMLCaptionItem);
var
  MLINext: TMLCaptionItem;
  Rgn: HRGN;
begin
  MLINext := MLI.Next;
  while Assigned(MLINext) do
  begin
    aRect.Right := aRect.Right + MLINext.Col.Width;
    MLINext := MLINext.Next;
  end;

  //   OutCaptionCellText(aCol, aRow, aRect, aState, MLI.Caption);
  Rgn := CreateRectRgn(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
  SelectClipRgn(Canvas.Handle, Rgn);
  OutCaptionCellText(aCol, aRow, aRect, aState, MLI.Caption);
  SelectClipRgn(Canvas.Handle, 0);
  DeleteObject(Rgn);
end;

procedure TRxDBGrid.UpdateJMenuStates;
begin
  F_PopupMenu.Items[0].Visible := rdgAllowDialogFind in FOptionsRx;
  F_PopupMenu.Items[1].Visible := rdgAllowFilterForm in FOptionsRx;
  F_PopupMenu.Items[2].Visible := rdgAllowQuickFilter in FOptionsRx;
  F_PopupMenu.Items[3].Visible :=
    (rdgFilter in FOptionsRx) or (rdgAllowFilterForm in FOptionsRx);
  F_PopupMenu.Items[5].Visible := rdgAllowSortForm in FOptionsRx;
  F_PopupMenu.Items[6].Visible := rdgAllowColumnsForm in FOptionsRx;
  F_PopupMenu.Items[7].Visible := dgMultiselect in Options;
end;

procedure TRxDBGrid.UpdateJMenuKeys;

  function DoShortCut(Cmd: TRxDBGridCommand): TShortCut;
  var
    K: TRxDBGridKeyStroke;
  begin
    K := FKeyStrokes.FindRxKeyStrokes(Cmd);
    if Assigned(K) and K.Enabled then
      Result := K.ShortCut
    else
      Result := 0;
  end;

begin
  F_PopupMenu.Items[0].ShortCut := DoShortCut(rxgcShowFindDlg);
  F_PopupMenu.Items[1].ShortCut := DoShortCut(rxgcShowFilterDlg);
  F_PopupMenu.Items[2].ShortCut := DoShortCut(rxgcShowQuickFilter);
  F_PopupMenu.Items[3].ShortCut := DoShortCut(rxgcHideQuickFilter);
  F_PopupMenu.Items[5].ShortCut := DoShortCut(rxgcShowSortDlg);
  F_PopupMenu.Items[6].ShortCut := DoShortCut(rxgcShowColumnsDlg);
  F_PopupMenu.Items[7].ShortCut := DoShortCut(rxgcSelectAll);
end;

function TRxDBGrid.SortEngineOptions: TRxSortEngineOptions;
begin
  Result := [];
  if rdgCaseInsensitiveSort in FOptionsRx then
    Include(Result, seoCaseInsensitiveSort);
end;

procedure TRxDBGrid.OnIniSave(Sender: TObject);
var
  i: integer;
  S, S1: string;
  C: TRxColumn;
begin
  S := Owner.Name + '.' + Name;
  FPropertyStorageLink.Storage.WriteInteger(S + sVersion, FVersion);
  FPropertyStorageLink.Storage.WriteInteger(S + sCount, Columns.Count);
  S := S + sItem;
  for i := 0 to Columns.Count - 1 do
  begin
    S1 := S + IntToStr(i);
    C := TRxColumn(Columns[i]);
    FPropertyStorageLink.Storage.WriteString(S1 + sCaption,
      StrToHexText(C.Title.Caption));
    FPropertyStorageLink.Storage.WriteInteger(S1 + sWidth, C.Width);
    FPropertyStorageLink.Storage.WriteInteger(S1 + sIndex, C.Index);
    FPropertyStorageLink.Storage.WriteInteger(S1 + sVisible, Ord(C.Visible));
  end;

  { TODO : Необходимо подключить сохранение списка колонок сортировки }
{
  FSortColumns;
  if Assigned(FSortField) then
  begin
    FPropertyStorageLink.Storage.WriteInteger(S1 + sSortMarker, Ord(FSortOrder));
    FPropertyStorageLink.Storage.WriteString(S1 + sSortField, FSortField.FieldName);
  end
  else
    FPropertyStorageLink.Storage.WriteInteger(S1 + sSortMarker, Ord(smNone));
}
end;

procedure TRxDBGrid.OnIniLoad(Sender: TObject);
var
  i, ACount: integer;
  S, S1, ColumName: string;
  C: TRxColumn;

begin
  S := Owner.Name + '.' + Name;
  ACount := FPropertyStorageLink.Storage.ReadInteger(S + sVersion, FVersion);
  //Check cfg version
  if ACount = FVersion then
  begin
    ACount := FPropertyStorageLink.Storage.ReadInteger(S + sCount, 0);
    S := S + sItem;
    for i := 0 to ACount - 1 do
    begin
      S1 := S + IntToStr(i);
      ColumName := HexTextToStr(FPropertyStorageLink.Storage.ReadString(S1 +
        sCaption, ''));
      if ColumName <> '' then
      begin
        C := ColumnByCaption(ColumName);
        if Assigned(C) then
        begin
          C.Width := FPropertyStorageLink.Storage.ReadInteger(S1 + sWidth, C.Width);
          C.Visible := FPropertyStorageLink.Storage.ReadInteger(S1 +
            sVisible, Ord(C.Visible)) = 1;
          C.Index := Min(FPropertyStorageLink.Storage.ReadInteger(S1 + sIndex, C.Index),
            Columns.Count - 1);
        end;
      end;
    end;

    { TODO : Необходимо подключить востановление списка колонок сортировки }
{    FSortOrder := TSortMarker(FPropertyStorageLink.Storage.ReadInteger(
      S1 + sSortMarker, Ord(smNone)));
    if Assigned(FSortEngine) and (FSortOrder <> smNone) and DatalinkActive then
    begin
      ColumName := FPropertyStorageLink.Storage.ReadString(S1 + sSortField, '');
      if ColumName <> '' then
      begin
        FSortField := DataSource.DataSet.FindField(ColumName);
        if Assigned(FSortField) then
          FSortEngine.Sort(FSortField, DataSource.DataSet, FSortOrder = smUp,
            SortEngineOptions);
      end;
    end;}
  end;
end;

procedure TRxDBGrid.CleanDSEvent;
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    if DataSource.DataSet.OnPostError = @ErrorPo then
      DataSource.DataSet.OnPostError := F_EventOnPostError;

    if DataSource.DataSet.OnFilterRecord = @FilterRec then
      DataSource.DataSet.OnFilterRecord := F_EventOnFilterRec;

    if DataSource.DataSet.BeforeDelete = @BeforeDel then
      DataSource.DataSet.BeforeDelete := F_EventOnBeforeDelete;

    if DataSource.DataSet.BeforePost = @BeforePo then
      DataSource.DataSet.BeforePost:=F_EventOnBeforePost;

    if DataSource.DataSet.OnDeleteError = @ErrorDel then
      DataSource.DataSet.OnDeleteError:=F_EventOnDeleteError;

    if DataSource.DataSet.OnPostError = @ErrorPo then
      DataSource.DataSet.OnPostError:=F_EventOnPostError;

    F_EventOnPostError:=nil;
    F_EventOnFilterRec:=nil;
    F_EventOnBeforeDelete:=nil;
    F_EventOnBeforePost:=nil;
    F_EventOnDeleteError:=nil;
    F_EventOnPostError:=nil;
  end;
end;

procedure TRxDBGrid.CollumnSortListUpdate;
var
  i, J:integer;
  C:TRxColumn;

begin
  FSortColumns.Clear;
  for i:=0 to Columns.Count - 1 do
  begin
    C:=TRxColumn(Columns[i]);
    if C.SortOrder <> smNone then
    begin
      if FSortColumns.Count <> 0 then
      begin
        for j:=0 to FSortColumns.Count-1 do
          if FSortColumns[j].FSortPosition > C.FSortPosition then
          begin
            FSortColumns.Insert(j, C);
            C:=nil;
            Break;
          end;
      end;
      if C<>nil then
        FSortColumns.Add(C);
    end;
  end;

  for i:=0 to FSortColumns.Count - 1 do
    FSortColumns[i].FSortPosition:=i;
end;

procedure TRxDBGrid.CollumnSortListClear;
var
  i:integer;
begin
  FSortColumns.Clear;
  for i:=0 to Columns.Count - 1 do
  begin
    TRxColumn(Columns[i]).FSortOrder:=smNone;
    TRxColumn(Columns[i]).FSortPosition:=0;
  end;
end;

procedure TRxDBGrid.CollumnSortListApply;
var
  i:integer;
  S:string;
  Asc:array of boolean;
begin
  if (FSortColumns.Count = 0) then exit;
  FSortingNow:=true;
  if (FSortColumns.Count>1) or (Pos(';', FSortColumns[0].GetSortFields)>0) then
  begin
    SetLength(Asc, FSortColumns.Count);
    for i := 0 to FSortColumns.Count - 1 do
    begin
      Asc[i]:=FSortColumns[i].FSortOrder = smUp;
      if S<>'' then
          S:=S+';';
      S:=S + FSortColumns[i].GetSortFields;
    end;
    { TODO : Необходимо добавить опцию регистронезависимого поиска }
    FSortEngine.SortList(S, DataSource.DataSet, Asc, SortEngineOptions);
  end
  else
    FSortEngine.Sort(FSortColumns[0].GetSortFields, DataSource.DataSet, FSortColumns[0].FSortOrder = smUp, SortEngineOptions);
  FSortingNow:=false;
end;

procedure TRxDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Datalink) and (AComponent = DataSource) and (Operation = opRemove) then
  begin
    ShowMessage('i');
  end
  else
  if (Operation = opRemove) and (AComponent is TRxDBGridAbstractTools) then
    RemoveTools(TRxDBGridAbstractTools(AComponent));
end;

function TRxDBGrid.UpdateRowsHeight: integer;
var
  i, J, H, H1, H2:integer;
  B:boolean;
  F:TField;
  S:string;
  CurActiveRecord: Integer;
  R:TRxColumn;
begin
  Result:=0;
  if not (Assigned(DataLink) and DataLink.Active) then
    exit;

  CurActiveRecord:=DataLink.ActiveRecord;
  H2:=0;
  for i:=GCache.VisibleGrid.Top to GCache.VisibleGrid.Bottom do
  begin
    DataLink.ActiveRecord:=i - FixedRows;
    H:=1;
    for j:=0 to Columns.Count-1 do
    begin
      R:=Columns[j] as TRxColumn;;
      if R.WordWrap then
      begin
        F:=R.Field;
        if Assigned(F) then
          S:=F.DisplayText
        else
          S:='';

        H1 := Max((Canvas.TextWidth(S) + 2) div R.Width + 1, H);
        if H1 > WordCount(S, [' ']) then
          H1 := WordCount(S, [' ']);
      end
      else
        H1:=1;
      H:=Max(H, H1);
    end;

    if i<RowCount then
    begin
      RowHeights[i] := DefaultRowHeight * H;
      H2:=H2 + RowHeights[i];
      if H2<=ClientHeight  then
        Inc(Result);
    end;
  end;
  DataLink.ActiveRecord:=CurActiveRecord;
end;

procedure TRxDBGrid.ResetRowHeght;
var
  i:integer;
begin
  for i:=1 to RowCount-1 do
    RowHeights[i] := DefaultRowHeight;
end;

procedure TRxDBGrid.DoClearInvalidTitle;
var
  i, j:integer;
  FTitle:TRxColumnTitle;
begin
  for i:=0 to Columns.Count-1 do
  begin
    FTitle:=TRxColumnTitle(Columns[i].Title);
    for j:=0 to FTitle.CaptionLinesCount-1 do
      FTitle.CaptionLine(j).FInvalidDraw:=0;
  end;
end;

procedure TRxDBGrid.DoDrawInvalidTitle;
var
  {C, }i, j{, CB, CE}:integer;
  MLI:TMLCaptionItem;
  FTitle:TRxColumnTitle;
begin
  for i:=0 to Columns.Count-1 do
  begin
    FTitle:=TRxColumnTitle(Columns[i].Title);
    for j:=0 to FTitle.CaptionLinesCount - 1 do
    begin
      MLI:=FTitle.CaptionLine(j);
      if MLI.FInvalidDraw<0 then
      begin
        //InvalidateRow(0);
        exit;
      end;
    end;
  end;
end;

procedure TRxDBGrid.DoSetColEdtBtn;
var
  R:TRxColumn;
  i, w:integer;
  SB:TGraphicControl;
begin
  R:=SelectedColumn as TRxColumn;

  if Assigned(Editor) and Assigned(R) then
  begin
    W:=0;
    for i:=0 to R.EditButtons.Count-1 do
    begin
      if R.EditButtons[i].Visible then
        W:=W+R.EditButtons[i].Width;
    end;

    if W>0 then
    begin
      if Editor.Name = 'ButtonEditor' then
      begin
        Editor.Left:=Editor.Left - W;
        W:=Editor.Width + Editor.Left;
      end
      else
      begin
        Editor.Width:=Editor.Width - W;
        W:=Editor.Width + Editor.Left;
      end;

      for i:=0 to R.EditButtons.Count-1 do
      if R.EditButtons[i].Visible then
      begin
        if R.EditButtons[i].Style = ebsUpDownRx then
        begin
          SB:=R.EditButtons[i].FSpinBtn;
          TRxSpinButton(SB).FocusControl:=Editor;
        end
        else
          SB:=R.EditButtons[i].FButton;

        SB.Parent:=Self;
        SB.Left:=W;
        SB.Top:=Editor.Top;
        SB.Height:=Editor.Height;
        SB.Visible:=true;
{
        R.EditButtons[i].FButton.Parent:=Self;
        R.EditButtons[i].FButton.Left:=W;
        R.EditButtons[i].FButton.Top:=Editor.Top;
        R.EditButtons[i].FButton.Height:=Editor.Height;
        R.EditButtons[i].Visible:=true;
}
        W:=W+R.EditButtons[i].Width;
      end;
    end;
  end;
end;

procedure TRxDBGrid.AddTools(ATools: TRxDBGridAbstractTools);
var
  i:integer;
  R: TMenuItem;
begin
  for i:=8 to F_PopupMenu.Items.Count - 1 do
    if F_PopupMenu.Items[i].Tag = IntPtr(ATools) then
      exit;

  R := TMenuItem.Create(F_PopupMenu);
  F_PopupMenu.Items.Add(R);
  R.Caption := ATools.FCaption;
  R.OnClick := @(ATools.ExecTools);
  R.Tag:=IntPtr(ATools);
end;

procedure TRxDBGrid.RemoveTools(ATools: TRxDBGridAbstractTools);
var
  i:integer;
  R: TMenuItem;
begin
  for i:=8 to F_PopupMenu.Items.Count - 1 do
    if F_PopupMenu.Items[i].Tag = IntPtr(ATools) then
    begin
      R:=F_PopupMenu.Items[i];
      F_PopupMenu.Items.Delete(i);
      R.Free;
      exit;
    end;
end;


procedure TRxDBGrid.DefaultDrawCellA(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
begin
  PrepareCanvas(aCol, aRow, aState);
  if rdgFilter in OptionsRx then
  begin
    DefaultDrawFilter(aCol, aRow, getFilterRect(aRect), aState);
    DefaultDrawTitle(aCol, aRow, getTitleRect(aRect), aState);
  end
  else
    DefaultDrawTitle(aCol, aRow, aRect, aState);
end;

procedure TRxDBGrid.DefaultDrawTitle(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);

procedure DoClearMLIInvalid(MLI1: TMLCaptionItem);
begin
  while Assigned(MLI1) do
  begin
    inc(MLI1.FInvalidDraw);
    MLI1:=MLI1.Next;
  end;
end;

var
  ASortMarker: TSortMarker;
  ASortPosition: integer;

  Background: TColor;
  i: integer;
  Down: boolean;
  aRect2: TRect;

  FTitle: TRxColumnTitle;
  GrdCol: TRxColumn;

  MLI, MLINext, MLI1: TMLCaptionItem;

begin
  if (dgIndicator in Options) and (aCol = 0) then
  begin
    Canvas.FillRect(aRect);
    if F_Clicked then
      aState := aState + [gdPushed];

    if (TitleStyle = tsNative) then
      DrawThemedCell(aCol, aRow, aRect, aState)
    else
      DrawCellGrid(aCol, aRow, aRect, aState);

    if DatalinkActive and (rdgAllowToolMenu in FOptionsRx) then
      Canvas.Draw((ARect.Left + ARect.Right - F_MenuBMP.Width) div 2,
        (ARect.Top + ARect.Bottom - F_MenuBMP.Height) div 2, F_MenuBMP);
    exit;
  end;

  GrdCol := TRxColumn(ColumnFromGridColumn(aCol));

  Down := FPressed and (dgHeaderPushedLook in Options) and
    (FPressedCol = GrdCol);

  if Assigned(GrdCol) then
  begin
    ASortMarker := GrdCol.FSortOrder;
    if FSortColumns.Count>1 then
      ASortPosition:=GrdCol.FSortPosition
    else
      ASortPosition:=-1;
  end
  else
    ASortMarker := smNone;

  if Assigned(FOnGetBtnParams) and Assigned(GetFieldFromGridColumn(aCol)) then
  begin
    Background := Canvas.Brush.Color;
    FOnGetBtnParams(Self, GetFieldFromGridColumn(aCol), Canvas.Font,
      Background, ASortMarker, Down);
    Canvas.Brush.Color := Background;
  end;

  if (gdFixed in aState) and (aRow = 0) and (ACol >= FixedCols) then
  begin

    //GrdCol := ColumnFromGridColumn(aCol);
    if Assigned(GrdCol) then
      FTitle := TRxColumnTitle(GrdCol.Title)
    else
      FTitle := nil;

    if Assigned(FTitle) then
    begin
      if FTitle.Orientation <> toHorizontal then
      begin
        OutCaptionCellText90(aCol, aRow, aRect, aState, FTitle.Caption,
          FTitle.Orientation);
        if Down then
          aState := aState + [gdPushed];
      end
      else
      if (FTitle.CaptionLinesCount > 0) then
      begin
        aRect2.Left := aRect.Left;
        aRect2.Right := aRect.Right;
        aRect2.Top := aRect.Top;
        for i := 0 to FTitle.CaptionLinesCount - 1 do
        begin
          MLI := FTitle.CaptionLine(i);
          aRect2.Right := aRect.Right;

          if i = FTitle.CaptionLinesCount - 1 then
          begin
            aRect2.Bottom := aRect.Bottom;
            aRect.Top := ARect2.Top;
            if Down then
              aState := aState + [gdPushed]
            else
              aState := aState - [gdPushed]
              ;
          end
          else
          begin
            aRect2.Bottom := aRect2.Top + MLI.Hegth * DefaultRowHeight;
            aState := aState - [gdPushed];
          end;


          if Assigned(MLI.Next) then
          begin
            if Assigned(MLI.Prior) then
            begin
              if aCol = LeftCol then
              begin
                OutCaptionMLCellText(aCol, aRow, aRect2, aState, MLI);
                DoClearMLIInvalid(MLI);
              end
              else
                Dec(MLI.FInvalidDraw);
            end
            else
            begin
              OutCaptionMLCellText(aCol, aRow, aRect2, aState, MLI);
              DoClearMLIInvalid(MLI);
            end;
          end
          else
          begin
            if not Assigned(MLI.Prior) then
            begin
              OutCaptionCellText(aCol, aRow, aRect2, aState, MLI.Caption);
              DoClearMLIInvalid(MLI);
            end
            else
            begin
              if aCol = LeftCol then
              begin
                OutCaptionMLCellText(aCol, aRow, aRect2, aState, MLI);
                DoClearMLIInvalid(MLI);
              end
              else
                Dec(MLI.FInvalidDraw);
            end;
          end;
          aRect2.Top := aRect2.Bottom;
        end;
      end
      else
      begin
        if Down then
          aState := aState + [gdPushed];
        OutCaptionCellText(aCol, aRow, aRect, aState, FTitle.Caption);
      end;
    end
    else
    begin
      OutCaptionCellText(aCol, aRow, aRect, aState, GetDefaultColumnTitle(aCol));
    end;

    OutCaptionSortMarker(aRect, ASortMarker, ASortPosition+1);
  end
  else
  begin
    if Down then
      aState := aState + [gdPushed];
    OutCaptionCellText(aCol, aRow, aRect, aState, '');
  end;
end;

procedure TRxDBGrid.DefaultDrawFilter(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var
  bg: TColor;
  al: TAlignment;
  ft: TFont;
  MyCol: integer;
  TxS: TTextStyle;

begin
{  if (dgIndicator in Options) and (aCol = 0) then
  begin
    Canvas.FillRect(aRect);
    DrawCellGrid(aCol, aRow, aRect, aState);
    exit;
  end;

  DrawCellGrid(aCol, aRow, aRect, aState);}
  if (dgIndicator in Options) and (aCol = 0) then
  begin
    if (TitleStyle = tsNative) then
      DrawThemedCell(aCol, aRow, aRect, aState)
    else
    begin
      Canvas.FillRect(aRect);
      DrawCellGrid(aCol, aRow, aRect, aState);
    end;
    exit;
  end;

  if (TitleStyle = tsNative) then
    DrawThemedCell(aCol, aRow, aRect, aState)
  else
  begin
    Canvas.FillRect(aRect);
    DrawCellGrid(aCol, aRow, aRect, aState);
  end;

  Inc(aRect.Left, 1);
  Dec(aRect.Right, 1);
  Inc(aRect.Top, 1);
  Dec(aRect.Bottom, 1);

  if Columns.Count > (aCol - 1) then
  begin
    bg := Canvas.Brush.Color;
    al := Canvas.TextStyle.Alignment;
//    ft := Canvas.Font;
    ft:=TFont.Create;
    ft.Assign(Canvas.Font);
    TxS := Canvas.TextStyle;

    MyCol := Columns.RealIndex(aCol - 1);
    with TRxColumn(Columns[MyCol]).Filter do
    begin
//      Canvas.Brush.Color := Color;
//      Canvas.FillRect(aRect);
      if (TitleStyle <> tsNative) then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(aRect);
      end;

      if Value <> '' then
      begin
        Canvas.Font := Font;
        if (aRect.Right - aRect.Left) >= Canvas.TextWidth(Value) then
          TxS.Alignment := Alignment
        else
          TxS.Alignment := taLeftJustify;
        Canvas.TextStyle := TxS;
        DrawCellText(aCol, aRow, aRect, aState, Value);
      end
      else
      begin
        Canvas.Font := TRxColumn(Columns[MyCol]).Filter.EmptyFont;
        if (aRect.Right - aRect.Left) >= Canvas.TextWidth(Value) then
          TxS.Alignment := Alignment
        else
          TxS.Alignment := taLeftJustify;

        Canvas.TextStyle := TxS;
        DrawCellText(aCol, aRow, aRect, aState,
          TRxColumn(Columns[MyCol]).Filter.EmptyValue);
      end;
    end;

//    Canvas.Font := ft;
    Canvas.Font.Assign(ft);
    ft.Free;
    Canvas.Brush.Color := bg;
    //    Canvas.TextStyle.Alignment := al;
    TxS.Alignment := al;
    Canvas.TextStyle := TxS;
  end
  else
  begin
    bg := Canvas.Brush.Color;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(aRect);
    Canvas.Brush.Color := bg;
  end;
end;

procedure TRxDBGrid.DefaultDrawCellData(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var
  S: string;
  F: TField;
  C: TRxColumn;
  j: integer;
begin
  if Assigned(OnDrawColumnCell) and not (CsDesigning in ComponentState) then
    OnDrawColumnCell(Self, aRect, aCol, TColumn(ColumnFromGridColumn(aCol)), aState)
  else
  begin
    F := GetFieldFromGridColumn(aCol);
    C := ColumnFromGridColumn(aCol) as TRxColumn;
    if Assigned(C) and Assigned(C.FOnDrawColumnCell) then
      C.OnDrawColumnCell(Self, aRect, aCol, TColumn(ColumnFromGridColumn(aCol)), aState)
    else
    begin
      case ColumnEditorStyle(aCol, F) of
        cbsCheckBoxColumn: DrawCheckBoxBitmaps(aCol, aRect, F);
        else
          if F <> nil then
          begin
            if F.dataType <> ftBlob then
            begin
  {          if Assigned(F.LookupDataSet) and (F.LookupResultField<>'') then
              S := F.LookupDataSet.FieldByName(F.LookupResultField).DisplayText
            else}
              S := F.DisplayText;
              if Assigned(C) and (C.KeyList.Count > 0) and (C.PickList.Count > 0) then
              begin
                J := C.KeyList.IndexOf(S);
                if (J >= 0) and (J < C.PickList.Count) then
                  S := C.PickList[j];
              end;
            end
            else
              S := '(blob)';
          end
          else
            S := '';
          if (rdgWordWrap in FOptionsRx) and Assigned(C) and (C.WordWrap) then
            WriteTextHeader(Canvas, aRect, S, C.Alignment)
          else
            DrawCellText(aCol, aRow, aRect, aState, S);
      end;
    end;
  end;
end;

procedure TRxDBGrid.DrawCell(aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  RxColumn: TRxColumn;
  AImageIndex: integer;
  FBackground: TColor;
begin
  if (gdFixed in aState) and (aRow = 0) then
  begin
    DefaultDrawCellA(aCol, aRow, aRect, aState);
  end
  else
  if not ((gdFixed in aState) or ((aCol = 0) and (dgIndicator in Options)) or
    ((aRow = 0) and (dgTitles in Options))) then
  begin

    PrepareCanvas(aCol, aRow, aState);

    if Assigned(FOnGetCellProps) and not (gdSelected in aState) then
    begin
      FBackground := Canvas.Brush.Color;
      FOnGetCellProps(Self, GetFieldFromGridColumn(aCol), Canvas.Font, FBackground);
      Canvas.Brush.Color := FBackground;
    end;

    Canvas.FillRect(aRect);
    DrawCellGrid(aCol, aRow, aRect, aState);

    RxColumn := TRxColumn(ColumnFromGridColumn(aCol));
    if Assigned(RxColumn) and Assigned(RxColumn.Field) and
      Assigned(RxColumn.ImageList) then
    begin
      AImageIndex := StrToIntDef(RxColumn.KeyList.Values[RxColumn.Field.AsString],
        RxColumn.FNotInKeyListIndex);
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
  S: string;
  Pos: integer;
begin
  if Value then
  begin
    S := DataSource.DataSet.ClassName;
    if RxDBGridSortEngineList.Find(S, Pos) then
      FSortEngine := RxDBGridSortEngineList.Objects[Pos] as TRxDBGridSortEngine
    else
      FSortEngine := nil;
  end;

  inherited LinkActive(Value);
  if not Value then
{  begin
    S := DataSource.DataSet.ClassName;
    if RxDBGridSortEngineList.Find(S, Pos) then
      FSortEngine := RxDBGridSortEngineList.Objects[Pos] as TRxDBGridSortEngine
    else
      FSortEngine := nil;
  end
  else}
  begin
    FSortEngine := nil;
    if SelectedRows.Count > 0 then
      SelectedRows.Clear;
  end;

  if not FSortingNow then
    CollumnSortListClear;
{  begin
    FSortField := nil;
    FSortOrder := smNone;
  end;

  F_SortListField.Clear;
}
  if {not (csDestroying in ComponentState) and} not (csDesigning in ComponentState) then
    SetDBHandlers(Value);
end;

procedure TRxDBGrid.SetDBHandlers(Value: boolean);
begin
  if Value then
  begin
    if DataSource.DataSet.OnFilterRecord <> @FilterRec then
    begin
      F_EventOnFilterRec := DataSource.DataSet.OnFilterRecord;
      DataSource.DataSet.OnFilterRecord := @FilterRec;
    end;
    if DataSource.DataSet.BeforeDelete <> @BeforeDel then
    begin
      F_EventOnBeforeDelete := DataSource.DataSet.BeforeDelete;
      DataSource.DataSet.BeforeDelete := @BeforeDel;
    end;
    if DataSource.DataSet.BeforePost <> @BeforePo then
    begin
      F_EventOnBeforePost := DataSource.DataSet.BeforePost;
      DataSource.DataSet.BeforePost := @BeforePo;
    end;
    if DataSource.DataSet.OnDeleteError <> @ErrorDel then
    begin
      F_EventOnDeleteError := DataSource.DataSet.OnDeleteError;
      DataSource.DataSet.OnDeleteError := @ErrorDel;
    end;
    if DataSource.DataSet.OnPostError <> @ErrorPo then
    begin
      F_EventOnPostError := DataSource.DataSet.OnPostError;
      DataSource.DataSet.OnPostError := @ErrorPo;
    end;
    CalcStatTotals;
    if rdgFilter in OptionsRx then
       OnFilter(nil);
  end
  else
  begin
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    begin
      DataSource.DataSet.OnFilterRecord := F_EventOnFilterRec;
      F_EventOnFilterRec := nil;
      DataSource.DataSet.BeforeDelete := F_EventOnBeforeDelete;
      F_EventOnBeforeDelete := nil;
      DataSource.DataSet.BeforePost := F_EventOnBeforePost;
      F_EventOnBeforePost := nil;
      DataSource.DataSet.OnDeleteError := F_EventOnDeleteError;
      F_EventOnDeleteError := nil;
      DataSource.DataSet.OnPostError := F_EventOnPostError;
      F_EventOnPostError := nil;
      if rdgFilter in OptionsRx then
         OnFilter(nil);
    end;
    F_LastFilter.Clear;
  end;
end;

procedure TRxDBGrid.DrawFooterRows;
var
  FooterRect: TRect;
  R: TRect;
  TotalYOffs: integer;
  TotalWidth: integer;
  i: integer;
  C: TRxColumn;
  Background: TColor;
  ClipArea: Trect;
  TxS: TTextStyle;
begin
  TotalWidth := GetClientRect.Right;
{
if ScrollBarIsVisible(SB_HORZ) then
    TotalYOffs := GCache.ClientHeight - (GetSystemMetrics(SM_CYHSCROLL) + GetSystemMetrics(SM_SWSCROLLBARSPACING))
  else
    TotalYOffs := GCache.ClientHeight;
  }
  TotalYOffs := GCache.ClientHeight - (DefaultRowHeight * FFooterOptions.RowCount);

  FooterRect := Rect(0, TotalYOffs, TotalWidth, TotalYOffs + DefaultRowHeight * FFooterOptions.RowCount);

  Background := Canvas.Brush.Color;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(FooterRect);

  R.Top := TotalYOffs;
  R.Bottom := TotalYOffs + DefaultRowHeight * FFooterOptions.RowCount + 2;

  Canvas.Brush.Color := FFooterOptions.FColor;
  if (Columns.Count > 0) then
  begin
    TxS := Canvas.TextStyle;

    if FDrawFullLine then
    begin
      ColRowToOffset(True, True, 0, R.Left, R.Right);
      Canvas.Pen.Color := GridLineColor;
      Canvas.MoveTo(R.Right - 1, R.Top);
      Canvas.LineTo(R.Right - 1, RowHeights[0]);
    end;

    for i := GCache.VisibleGrid.Left to GCache.VisibleGrid.Right do
    begin
      ColRowToOffset(True, True, i, R.Left, R.Right);
      Canvas.FillRect(R);
      DrawCellGrid(i, 0, R, []);

      if FDrawFullLine then
      begin
        Canvas.MoveTo(R.Right - 1, R.Top);
        Canvas.LineTo(R.Right - 1, RowHeights[0]);
      end;

      C := ColumnFromGridColumn(i) as TRxColumn;
      if Assigned(C) then
      begin
        TxS.Alignment := C.Footer.Alignment;
        TxS.Layout := C.Footer.Layout;
        Canvas.TextStyle := TxS;
        DrawCellText(i, 0, R, [], C.Footer.DisplayText);
      end;
    end;
    if FDrawFullLine then
    begin
      Canvas.MoveTo(FooterRect.Left, FooterRect.Top);
      Canvas.LineTo(R.Right, FooterRect.Top);
    end;

    ClipArea := Canvas.ClipRect;
    for i := 0 to FixedCols - 1 do
    begin
      ColRowToOffset(True, True, i, R.Left, R.Right);
      if FFooterOptions.FStyle = tsNative then
        DrawThemedCell(i, 0, R, [gdFixed])
      else
        DrawCellGrid(i, 0, R, [gdFixed]);

      if ((R.Left < ClipArea.Right) and (R.Right > ClipArea.Left)) then
      begin
//        DrawCell(i, 0, R, [gdFixed]);

//        PrepareCanvas(i, 0, [gdFixed]);
        DefaultDrawTitle(i, 0, getTitleRect(R), [gdFixed]);
      end;
    end;
  end;
  Canvas.Brush.Color := Background;
end;

procedure TRxDBGrid.DoTitleClick(ACol: longint; ACollumn: TRxColumn;
  Shift: TShiftState);
begin
  if FAutoSort {and (FSortEngine <> nil)} and (ACollumn.Field <> nil) then
  begin
    if ssCtrl in Shift then
    begin
      if ACollumn.FSortOrder <> smNone then
      begin
        if ACollumn.FSortOrder = smUp then
          ACollumn.FSortOrder := smDown
        else
        begin
          ACollumn.FSortOrder := smNone;
          ACollumn.FSortPosition:=0;
        end;
      end
      else
      begin
        ACollumn.FSortOrder := smUp;
        ACollumn.FSortPosition:=FSortColumns.Count;
      end;
    end
    else
    begin
      if (FSortColumns.Count>0) and (FSortColumns[0] = ACollumn) then
      begin
        if Assigned(FSortEngine) then
        begin
          if FSortColumns[0].FSortOrder = smUp then
            FSortColumns[0].FSortOrder := smDown
          else
            FSortColumns[0].FSortOrder := smUp;
        end
        else
        begin
          case ACollumn.FSortOrder of
            smNone: ACollumn.FSortOrder := smUp;
            smUp: ACollumn.FSortOrder := smDown;
            smDown: ACollumn.FSortOrder := smNone;
          end;
        end;
      end
      else
      begin
        CollumnSortListClear;
        ACollumn.FSortOrder := smUp;
      end;
    end;

    CollumnSortListUpdate;
    if Assigned(FSortEngine) then
      CollumnSortListApply;
    if Assigned(FOnSortChanged) then
    begin
      FSortingNow := True;
      FOnSortChanged(Self);
      FSortingNow := False;
    end;
  end
  else
    HeaderClick(True, ACol);
end;

procedure TRxDBGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Cell: TGridCoord;
  Rect: TRect;
begin
  if FTracking then
    TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);

  if (rdgFilter in OptionsRx) and (dgColumnResize in Options) and
    (Cursor = crHSplit) then
  begin
    Cell := MouseCoord(X, Y);
    Rect := getFilterRect(CellRect(Cell.x, Cell.y));
    if (Cell.Y = 0) and (Cell.X >= Ord(dgIndicator in Options)) and (Rect.Top < Y) then
    begin
      Cursor := crDefault;
    end;
  end;

  if FColumnResizing and (MouseToGridZone(X, Y) = gzFixedCols) then
  begin
    CalcTitle;
    if FFooterOptions.Active and (dgColumnResize in Options) and (FFooterOptions.RowCount > 0) then
      DrawFooterRows;
  end;
end;

procedure TRxDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Cell: TGridCoord;
  Rect: TRect;
  C:TRxColumn;
begin
  QuickUTF8Search := '';

  Cell := MouseCoord(X, Y);
  if (DatalinkActive) and (DataSource.DataSet.State = dsBrowse) and
    (Button = mbLeft) and (Cell.X = 0) and (Cell.Y = 0) and
    (dgIndicator in Options) and (rdgAllowToolMenu in FOptionsRx) then
  begin
    F_Clicked := True;
    InvalidateCell(0, 0);
  end
  else
  if (Cell.Y = 0) and (Cell.X >= Ord(dgIndicator in Options)) then
  begin
    if (rdgFilter in OptionsRx) and DatalinkActive then
    begin
      Cell := MouseCoord(X, Y);
      Rect := getFilterRect(CellRect(Cell.x, Cell.y));
      if (Cell.Y = 0) and (Cell.X >= Ord(dgIndicator in Options)) and (Rect.Top < Y) then
      begin
        C:=TRxColumn (Columns[Columns.RealIndex(Cell.x - 1)]);
        if (C.Filter.Enabled) and (C.Filter.ValueList.Count > 0)  then
        begin
          FFilterListEditor.Style := csDropDownList;
          if C.Filter.DropDownRows>0 then
            FFilterListEditor.DropDownCount := C.Filter.DropDownRows;

          FFilterListEditor.Parent := Self;
          FFilterListEditor.Width := Rect.Right - Rect.Left;
          FFilterListEditor.Height := Rect.Bottom - Rect.Top;
          FFilterListEditor.BoundsRect := Rect;

          FFilterListEditor.Items.Assign(C.Filter.ValueList);

          FFilterListEditor.Text := C.Filter.Value;
          FFilterListEditor.Show(Self, Cell.x - 1);
        end;
        exit;
      end;
    end;

    if dgColumnResize in Options then
    begin
      FColumnResizing := True;
    end;

    if FAutoSort then
    begin
      Cell := MouseCoord(X, Y);
      if (Cell.Y = 0) and (Cell.X >= Ord(dgIndicator in Options)) then
      begin
        if (dgColumnResize in Options) and (Button = mbRight) then
        begin
//          Button := mbLeft;
//          FSwapButtons := True;
//          MouseCapture := True;
          Shift := Shift + [ssLeft];
          inherited MouseDown(Button, Shift, X, Y);
        end
        else
        if Button = mbLeft then
        begin
          if (MouseToGridZone(X, Y) = gzFixedCols) and
            (dgColumnResize in Options) and (Cursor = crHSplit) then
          begin
            if (ssDouble in Shift) and (rdgDblClickOptimizeColWidth in FOptionsRx) then
            begin
              if Assigned(ColumnFromGridColumn(Cell.X)) then
                TRxColumn(ColumnFromGridColumn(Cell.X)).OptimizeWidth;
            end
            else
              inherited MouseDown(Button, Shift, X, Y);
          end
          else
          begin
            MouseCapture := True;
            FTracking := True;
            FPressedCol := TRxColumn(ColumnFromGridColumn(Cell.X));
            TrackButton(X, Y);
            inherited MouseDown(Button, Shift, X, Y);
          end;
        end;
      end
      else
        inherited MouseDown(Button, Shift, X, Y);
    end
    else
      inherited MouseDown(Button, Shift, X, Y);
  end
  else
  begin
    if rdgMrOkOnDblClik in FOptionsRx then
    begin
      if (Cell.Y > 0) and (Cell.X >= Ord(dgIndicator in Options)) and
        (ssDouble in Shift) then
      begin
        if Owner is TCustomForm then
          TCustomForm(Owner).ModalResult := mrOk;
      end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TRxDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Cell: TGridCoord;
  ACol: longint;
  DoClick: boolean;

  ShowMenu: boolean;
  MPT: TPoint;
  Rct: TRect;
begin
  ShowMenu := False;

  FColumnResizing := False;

  if (dgHeaderPushedLook in Options) and FTracking and (FPressedCol <> nil) then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
      (Cell.Y < RowHeights[0]) and (FPressedCol = TRxColumn(ColumnFromGridColumn(Cell.X)));
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      if (dgIndicator in Options) then
        Dec(ACol);
      if DataLinkActive and (ACol >= 0) and (ACol < Columns.Count) then
      begin
        FPressedCol := TRxColumn(ColumnFromGridColumn(Cell.X));
        if Assigned(FPressedCol) then
          DoTitleClick(FPressedCol.Index, FPressedCol, Shift);
      end;
    end;
  end
  else
  if FSwapButtons then
  begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then
      Button := mbLeft;
  end;

  if (DatalinkActive) and (DataSource.DataSet.State = dsBrowse) and
    (rdgAllowToolMenu in FOptionsRx) then
  begin
    Cell := MouseCoord(X, Y);
    if ((Button = mbLeft) and (Cell.X = 0) and (Cell.Y = 0) and
      (dgIndicator in Options)) or (F_Clicked) then
    begin
      F_Clicked := False;
      InvalidateCell(0, 0);
      ShowMenu := True;
      Button := mbRight;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  if (DatalinkActive) and (DataSource.DataSet.State = dsBrowse) and (ShowMenu) then
  begin
    Rct := CellRect(0, 0);
    MPT.X := Rct.Left;
    if rdgFilter in FOptionsRx then
      MPT.Y := Rct.Bottom - DefaultRowHeight
    else
      MPT.Y := Rct.Bottom;
    MPT := ClientToScreen(MPT);

    UpdateJMenuStates;
    F_PopupMenu.Popup(MPT.X, MPT.Y);
  end;
end;

procedure TRxDBGrid.SetQuickUTF8Search(AValue: string);
var
  ClearSearchValue: boolean;
  OldSearchString: string;
begin
  if (rdgAllowQuickSearch in OptionsRx) then
  begin
    OldSearchString := Self.FQuickUTF8Search;
    if (OldSearchString <> AValue) and Assigned(Self.FBeforeQuickSearch) then
      Self.FBeforeQuickSearch(Self, SelectedField, AValue);
    if OldSearchString <> AValue then
    begin
      ClearSearchValue := True;
      if (Length(AValue) > 0) and (Self.DatalinkActive) then
      begin
        if (DataSource.DataSet.State = dsBrowse) and
          (not (DataSource.DataSet.EOF and DataSource.DataSet.BOF)) then
        begin
          //1.Вызываем процедурку поиска...
          if DataSetLocateThrough(Self.DataSource.DataSet,
            Self.SelectedField.FieldName, AValue, [loPartialKey, loCaseInsensitive]) then
            Self.FQuickUTF8Search := AValue;
          ClearSearchValue := False;
        end;
      end;
      if ClearSearchValue then
      begin
        Self.FQuickUTF8Search := '';
      end;
      if (OldSearchString <> Self.FQuickUTF8Search) and
        Assigned(Self.FAfterQuickSearch) then
        Self.FAfterQuickSearch(Self, SelectedField, OldSearchString);
    end;
  end;
  //TODO: сделать отображение ищущейся буквы/строки.
end;

procedure TRxDBGrid.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  CheckUp: boolean;
begin
  inherited UTF8KeyPress(UTF8Key);
  if ReadOnly then
  begin
    //0. Проверяем что это кнопка значащая, увеличиваем "строку поиска"
    if Length(UTF8Key) = 1 then
    begin
      //DebugLn('Ord Of Key:',IntToStr(Ord(UTF8Key[1])));
      CheckUp := not (Ord(UTF8Key[1]) in CBadQuickSearchSymbols);
    end
    else
      CheckUp := True;
    //  DebugLn('RxDBGrid.UTF8KeyPress check',IfThen(CheckUp,'True','False'),'INIT UTF8Key= ',UTF8Key,' Selected Field: ', Self.SelectedField.FieldName);
    if CheckUp then
      QuickUTF8Search := QuickUTF8Search + Trim(UTF8Key);
  end;
end;

procedure TRxDBGrid.KeyDown(var Key: word; Shift: TShiftState);
var
  FTmpReadOnly: boolean;

  procedure DoShowFindDlg;
  begin
    if not (rdgAllowDialogFind in OptionsRx) then
      exit;
    if Length(QuickUTF8Search) > 0 then
      QuickUTF8Search := '';
    ShowFindDialog;
  end;

  procedure DoShowColumnsDlg;
  begin
    if not (rdgAllowColumnsForm in OptionsRx) then
      exit;
    if Length(QuickUTF8Search) > 0 then
      QuickUTF8Search := '';
    ShowColumnsDialog;
  end;

  procedure DoShowQuickFilter;
  begin
    if not (rdgAllowQuickFilter in FOptionsRx) then
      exit;
    OnFilter(Self);
  end;

begin
  //DebugLn('RxDBGrid.KeyDown ',Name,' INIT Key= ',IntToStr(Key));
  if (Key in CCancelQuickSearchKeys) then
    if Length(QuickUTF8Search) > 0 then
      QuickUTF8Search := '';
(*  case Key of
{    VK_DELETE: if not (aoDelete in FAllowedOperations) then
        exit;
    VK_INSERT: if not (aoInsert in FAllowedOperations) then
        exit;
    VK_RETURN: if (aoAppend in FAllowedOperations) and (EditorMode) and
        (Col = ColCount - 1) and (Row = RowCount - 1) then
        if DataSource.DataSet.State = dsInsert then
        begin
          DataSource.DataSet.Post;
          Col := 0;
          Key := VK_DOWN;
          inherited KeyDown(Key, Shift);
          exit;
        end
        else
        begin
          Col := 0;
          Key := VK_DOWN;
          inherited KeyDown(Key, Shift);
          exit;
        end;

    VK_DOWN:
      if not (aoAppend in FAllowedOperations) then
      begin
        FTmpReadOnly := ReadOnly;
        ReadOnly := True;
        try
          inherited KeyDown(Key, Shift);
        finally
          ReadOnly := FTmpReadOnly;
        end;
        exit;
      end }
{      else
        UpdateRowsHeight;
    VK_UP:UpdateRowsHeight}
  end; *)
  inherited KeyDown(Key, Shift);
  if Key <> 0 then
  begin
    case FKeyStrokes.FindRxCommand(Key, Shift) of
      rxgcShowFindDlg: DoShowFindDlg;
      rxgcShowColumnsDlg: DoShowColumnsDlg;
      rxgcShowFilterDlg: OnFilterBy(Self);
      rxgcShowQuickFilter: DoShowQuickFilter;
      rxgcHideQuickFilter: OnFilterClose(Self);
      rxgcShowSortDlg: OnSortBy(Self);
      rxgcSelectAll: SelectAllRows;
      rxgcDeSelectAll: DeSelectAllRows;
      rxgcInvertSelection:InvertSelection;
      rxgcOptimizeColumnsWidth:OptimizeColumnsWidthAll;
      rxgcCopyCellValue:OnCopyCellValue(Self);
    else
      exit;
    end;
    Key := 0;
  end;
end;

function TRxDBGrid.CreateColumns: TGridColumns;
begin
  Result := TRxDbGridColumns.Create(Self, TRxColumn);
end;

procedure TRxDBGrid.DrawCellBitmap(RxColumn: TRxColumn; aRect: TRect;
  aState: TGridDrawState; AImageIndex: integer);
var
  ClientSize: TSize;
  H, W: integer;
begin
  InflateRect(aRect, -1, -1);

  H := RxColumn.ImageList.Height;
  W := RxColumn.ImageList.Width;

  ClientSize.cx := Min(aRect.Right - aRect.Left, W);
  ClientSize.cy := Min(aRect.Bottom - aRect.Top, H);

  if ClientSize.cx = W then
  begin
    aRect.Left := (aRect.Left + aRect.Right - W) div 2;
    aRect.Right := aRect.Left + W;
  end;

  if ClientSize.cy = H then
  begin
    aRect.Top := (aRect.Top + aRect.Bottom - H) div 2;
    aRect.Bottom := aRect.Top + H;
  end;

  RxColumn.ImageList.StretchDraw(Canvas, AImageIndex, aRect);
end;

procedure TRxDBGrid.SetEditText(ACol, ARow: longint; const Value: string);
var
  C: TRxColumn;
  j: integer;
  S: string;
begin
  C := ColumnFromGridColumn(aCol) as TRxColumn;
  S := Value;
  if Assigned(C) and (C.KeyList.Count > 0) and (C.PickList.Count > 0) then
  begin
    J := C.PickList.IndexOf(S);
    if (J >= 0) and (J < C.KeyList.Count) then
      S := C.KeyList[j];
  end;
  inherited SetEditText(ACol, ARow, S);
end;

procedure TRxDBGrid.CheckNewCachedSizes(var AGCache: TGridDataCache);
begin
  if FFooterOptions.Active and (FooterOptions.RowCount > 0) then
//    Dec(AGCache.ClientHeight, DefaultRowHeight * FooterOptions.RowCount);
  begin
      Dec(AGCache.ClientHeight, DefaultRowHeight * FooterOptions.RowCount);
      Dec(AGCache.ScrollHeight, DefaultRowHeight * FooterOptions.RowCount);
  end;
end;

procedure TRxDBGrid.ColRowMoved(IsColumn: boolean; FromIndex, ToIndex: integer);
begin
  inherited ColRowMoved(IsColumn, FromIndex, ToIndex);
  if IsColumn then
    CalcTitle;
end;

procedure TRxDBGrid.Paint;
begin
  Inc(FInProcessCalc);
  if rdgWordWrap in FOptionsRx then
    UpdateRowsHeight;

  DoClearInvalidTitle;

  inherited Paint;

  DoDrawInvalidTitle;

  if FFooterOptions.Active and (FFooterOptions.RowCount > 0) then
    DrawFooterRows;
  Dec(FInProcessCalc);
end;

procedure TRxDBGrid.UpdateActive;
begin
{  if FInProcessCalc > 0 then
    exit;}
  inherited UpdateActive;
{  if FInProcessCalc < 0 then
  begin
    FInProcessCalc := 0;
    UpdateFooterRowOnUpdateActive;
  end
{  else
  if FFooterOptions.Active and (FFooterOptions.RowCount > 0) then
    UpdateFooterRowOnUpdateActive;}
}
 // UpdateRowsHeight;
end;

procedure TRxDBGrid.UpdateData;
begin
  inherited UpdateData;
end;

procedure TRxDBGrid.MoveSelection;
begin
  inherited MoveSelection;
  if Assigned(FFooterOptions) and FFooterOptions.Active and (FFooterOptions.RowCount > 0) then
    DrawFooterRows;
//  UpdateRowsHeight;
end;

function TRxDBGrid.GetBufferCount: integer;
var
  H:integer;
begin
{  Result := ClientHeight div DefaultRowHeight;
  if rdgWordWrap in FOptionsRx then
  begin
    H:=UpdateRowsHeight;
//    if H>0 then
//      Result := H;
  end;
  if dgTitles in Options then
  begin
    Dec(Result, RowHeights[0] div DefaultRowHeight);
  end;
  if FFooterOptions.Active then
    Dec(Result, FFooterOptions.RowCount);}
  H:=ClientHeight - GCache.FixedHeight;
  if FFooterOptions.Active then
    H:=H - DefaultRowHeight * FFooterOptions.RowCount;

  Result := H div DefaultRowHeight;
//  result := (ClientHeight - GCache.FixedHeight - DefaultRowHeight) div DefaultRowHeight;
end;

procedure TRxDBGrid.CMHintShow(var Message: TLMessage);
var
  Cell: TGridCoord;
  tCol: TRxColumn;
begin
  if Assigned(TCMHintShow(Message).HintInfo) then
  begin
    with TCMHintShow(Message).HintInfo^ do
    begin
      Cell := MouseCoord(CursorPos.X, CursorPos.Y);
      if (Cell.Y = 0) and (Cell.X >= Ord(dgIndicator in Options)) then
      begin
        tCol := TRxColumn(ColumnFromGridColumn(Cell.X));
        if Assigned(tCol) and (TRxColumnTitle(tCol.Title).Hint <> '') and
          (TRxColumnTitle(tCol.Title).FShowHint) then
          HintStr := TRxColumnTitle(tCol.Title).Hint;
      end;
    end;
  end;
  inherited CMHintShow(Message);
end;

procedure TRxDBGrid.FFilterListEditorOnChange(Sender: TObject);
begin
  FFilterListEditor.Hide;
  with TRxColumn(Columns[Columns.RealIndex(FFilterListEditor.Col)]).Filter do
  begin
    if FFilterListEditor.Text = EmptyValue then
      Value := ''
    else
      Value := FFilterListEditor.Text;
  end;

  DataSource.DataSet.Refresh;
  CalcStatTotals;

  if Assigned(FOnFiltred) then
    FOnFiltred(Self);
end;

procedure TRxDBGrid.FFilterListEditorOnCloseUp(Sender: TObject);
begin
  FFilterListEditor.Hide;
  FFilterListEditor.Changed;
  SetFocus;
end;

procedure TRxDBGrid.InternalOptimizeColumnsWidth(AColList: TList);
var
  P: TBookmark;
  i, W, n: integer;
  WA: PIntegerArray;
  S: string;
begin
  GetMem(WA, SizeOf(integer) * AColList.Count);

  for I := 0 to AColList.Count - 1 do
  begin
    if TRxColumnTitle(TRxColumn(AColList[i]).Title).CaptionLinesCount > 1 then
      WA^[i] := Max(Canvas.TextWidth(
        TRxColumnTitle(TRxColumn(AColList[i]).Title).CaptionLine(
        TRxColumnTitle(TRxColumn(AColList[i]).Title).CaptionLinesCount -
        1).Caption) + 8, 20)
    else
      WA^[i] := Max(Canvas.TextWidth(TRxColumn(AColList[i]).Title.Caption) + 8, 20);
  end;

  with DataSource.DataSet do
  begin
    DisableControls;
    P := GetBookmark;
    First;
    try
      while not EOF do
      begin
        for I := 0 to AColList.Count - 1 do
        begin
          if Assigned(TRxColumn(AColList[i]).Field) then
            S := TRxColumn(AColList[i]).Field.DisplayText
          else
            S:='';
          with TRxColumn(AColList[i]) do
            if (KeyList.Count > 0) and (PickList.Count > 0) then
            begin
              n := KeyList.IndexOf(S);
              if (n <> -1) and (n < PickList.Count) then
                S := PickList.Strings[n];
            end;
          W := Canvas.TextWidth(S) + 6;
          if WA^[i] < W then
            WA^[i] := W;
        end;
        Next;
      end;
    finally
      GotoBookmark(p);
      FreeBookmark(p);
      EnableControls;
    end;
  end;

  for I := 0 to AColList.Count - 1 do
    if WA^[i] > 0 then
      TRxColumn(AColList[i]).Width := WA^[i];

  FreeMem(WA, SizeOf(integer) * AColList.Count);
end;

function TRxDBGrid.IsDefaultRowHeightStored: boolean;
begin
  Result := DefaultRowHeight = Canvas.TextHeight('Wg');
end;

procedure TRxDBGrid.VisualChange;
begin
  CalcTitle;
  inherited VisualChange;

{  if rdgWordWrap in FOptionsRx then
    UpdateRowsHeight;}
end;

procedure TRxDBGrid.EditorWidthChanged(aCol, aWidth: Integer);
var
  R:TRect;
begin
  inherited EditorWidthChanged(aCol, aWidth);
  if FFilterListEditor.Visible then
  begin
    R:=CellRect(FFilterListEditor.Col+1,0);
    FFilterListEditor.Width:=Columns[FFilterListEditor.Col].Width;
    FFilterListEditor.Left:=R.Left;
  end;
end;

function TRxDBGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;
var
  F: TField;
begin
  if Style = cbsAuto then
  begin
    F := SelectedField;
    if Assigned(F) then
    begin
      if Assigned(F.LookupDataSet) and (F.LookupKeyFields <> '') and
        (F.LookupResultField <> '') and (F.KeyFields <> '') then
      begin
        Result := FRxDbGridLookupComboEditor;
        exit;
      end
      else
      if F.DataType in [ftDate, ftDateTime] then
      begin
        Result := FRxDbGridDateEditor;
        exit;
      end;
    end;
  end;
  Result := inherited EditorByStyle(Style);

  if (Style = cbsPickList) and (Result is TCustomComboBox) then
  begin
    if TRxColumn(SelectedColumn).DirectInput then
      TCustomComboBox(Result).Style:=csDropDown
    else
      TCustomComboBox(Result).Style:=csDropDownList;
  end;

end;

procedure TRxDBGrid.CalcStatTotals;
var
  {$IFDEF NoAutomatedBookmark}
  P_26: TBookmark;
  {$ENDIF}
  P: TBookmark;
  i, cnt, K: integer;
  APresent: boolean;
  BB:Double;

  DHL:THackDataLink;
  DHS:THackDataSet;

  SaveState:TDataSetState;
  SavePos:integer;
  SaveActiveRecord:integer;

  SaveAfterScroll:TDataSetNotifyEvent;
  SaveBeforeScroll:TDataSetNotifyEvent;
  RCol:TRxColumn;
  AValue:Variant;

  FCList:TFPList;
begin
  if (not (FFooterOptions.Active and DatalinkActive)) or (Columns.Count = 0) or (gsAddingAutoColumns in GridStatus)  then
    Exit;
  //Дополнительно проверим - а стоит ли делать пробег по данным - есть ли агрегатные функции
  if Assigned(OnRxCalcFooterValues)then
  begin
    Inc(FInProcessCalc);
    for i := 0 to Columns.Count - 1 do
    begin
      RCol := TRxColumn(Columns[i]);
      RCol.Footer.ResetTestValue;
      AValue:=Null;
      OnRxCalcFooterValues(Self, RCol, AValue);
      if AValue<>null then RCol.Footer.FTestValue := AValue;
    end;
    Dec(FInProcessCalc);
    Exit;
  end;

  APresent := False;
  for i := 0 to Columns.Count - 1 do
  begin
    APresent := TRxColumn(Columns[i]).Footer.FValueType in
      [fvtSum, fvtAvg, fvtMax, fvtMin, fvtCount];
    if APresent then
      break;
  end;

  if not APresent then
    exit;


  Inc(FInProcessCalc);

  cnt:=0;
  for i := 0 to Columns.Count - 1 do
    TRxColumn(Columns[i]).Footer.ResetTestValue;

  if (DataSource.DataSet.RecordCount<=0) then
  begin
    Dec(FInProcessCalc);
    exit;
  end;

  DHL:=THackDataLink(Datalink);
  DHS:=THackDataSet(DataSource.DataSet);

  {$IFDEF NoAutomatedBookmark}
  P:=DataSource.DataSet.GetBookmark;
  {$ELSE}
  P := DHS.Bookmark;
  {$ENDIF}

  SaveState:=DHS.SetTempState(dsBrowse);

  SaveAfterScroll:=DHS.AfterScroll;
  SaveBeforeScroll:=DHS.BeforeScroll;
  DHS.AfterScroll:=nil;
  DHS.BeforeScroll:=nil;

  SaveActiveRecord:=DHL.ActiveRecord;
  DHL.ActiveRecord:=0;
  SavePos:=DHS.RecNo;

  FCList:=TFPList.Create;
  for i:=0 to Columns.Count-1 do
  begin
    RCol:=TRxColumn(Columns[i]);
    if (RCol.Footer.ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin]) and RCol.Visible then
    begin
      FCList.Add(RCol);
      RCol.Footer.FField:=DHS.FieldByName(RCol.Footer.FieldName);
    end;
  end;

  DHS.First;
  while not DHS.EOF do
  begin
    for i:=0 to FCList.Count-1 do
    begin
      RCol:=TRxColumn(FCList[i]);
      if RCol.FFooter.FValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
        RCol.FFooter.UpdateTestValueFromVar( RCol.FFooter.FField.AsVariant)
//        BB:=RCol.FFooter.FField.AsFloat;
//        RCol.FFooter.UpdateTestValueFromVar( RCol.FFooter.FField.AsFloat)
    end;
    inc(cnt);
    DHS.Next;
  end;

  FCList.Free;

  for i:=0 to Columns.Count-1 do
  begin
    RCol:=TRxColumn(Columns[i]);
    if RCol.Footer.ValueType = fvtCount then
        RCol.FFooter.FCountRec:=Cnt
    else
    if RCol.Footer.ValueType = fvtAvg then
      RCol.FFooter.FTestValue:=RCol.FFooter.FTestValue / Cnt;
  end;

  DHS.RecNo := Min(DHL.RecordCount + SavePos - 1, DHS.RecNo);
  K:=DHS.RecNo;

  while not DHS.BOF do
  begin
    if SavePos = DHS.RecNo then
      break;
    DHS.Prior;
  end;

  for i:=0 to Columns.Count-1 do
    TRxColumn(Columns[i]).Footer.FField:=nil;

  DHL.ActiveRecord:=SaveActiveRecord;
  DHS.RestoreState(SaveState);

  DHS.AfterScroll  := SaveAfterScroll;
  DHS.BeforeScroll := SaveBeforeScroll;

  {$IFDEF NoAutomatedBookmark}
  P_26:=DHS.GetBookmark;
  if DHS.CompareBookmarks(P_26, P)<>0 then
    DHS.GotoBookmark(P); //workaround for fix navigation problem
  DHS.FreeBookmark(P);
  DHS.FreeBookmark(P_26);
  {$ELSE}
  if DHS.CompareBookmarks(DHS.Bookmark, P)<>0 then
    DHS.Bookmark:=P; //workaround for fix navigation problem
  {$ENDIF}


  Dec(FInProcessCalc);
  if FInProcessCalc < 0 then
    FInProcessCalc := 0;
end;

procedure TRxDBGrid.OptimizeColumnsWidth(AColList: string);
var
  ColList: TList;

  procedure DoFillColList;
  var
    L: integer;
  begin
    L := Pos(';', AColList);
    while L > 0 do
    begin
      if AColList <> '' then
        ColList.Add(ColumnByFieldName(Copy(AColList, 1, L - 1)));
      Delete(AColList, 1, L);
      L := Pos(';', AColList);
    end;
    if AColList <> '' then
      ColList.Add(ColumnByFieldName(AColList));
  end;

begin
  if (not DatalinkActive) or (Columns.Count = 0) then
    Exit;
  ColList := TList.Create;
  DoFillColList;
  InternalOptimizeColumnsWidth(ColList);
  ColList.Free;

  if Assigned(OnColumnSized) then
    OnColumnSized(Self);
end;

procedure TRxDBGrid.OptimizeColumnsWidthAll;
var
  ColList: TList;
  i: integer;
begin
  if (not DatalinkActive) or (Columns.Count = 0) then
    Exit;
  ColList := TList.Create;
  for i := 0 to Columns.Count - 1 do
    ColList.Add(Columns[i]);
  InternalOptimizeColumnsWidth(ColList);
  ColList.Free;
end;

procedure TRxDBGrid.UpdateTitleHight;
begin
  CalcTitle;
end;

procedure TRxDBGrid.FilterRec(DataSet: TDataSet; var Accept: boolean);
var
  i: integer;
begin
  Accept := True;
  for i := 0 to Columns.Count - 1 do
  begin
    with TRxColumn(Columns[i]) do
    begin
      if (Filter.Value <> '') then
      begin
        if (Filter.Value <> Field.DisplayText) then
        begin
          Accept := False;
          break;
        end;
      end;
    end;
  end;
  if Assigned(F_EventOnFilterRec) then
    F_EventOnFilterRec(DataSet, Accept);
end;

procedure TRxDBGrid.BeforeDel(DataSet: TDataSet);
var
  i: integer;
begin
  if FFooterOptions.Active and (DatalinkActive) then
    for i := 0 to Columns.Count - 1 do
      if not TRxColumn(Columns[i]).Footer.DeleteTestValue then
      begin
        FInProcessCalc := -1;
        Break;
      end;
  if Assigned(F_EventOnBeforeDelete) then
    F_EventOnBeforeDelete(DataSet);
end;

procedure TRxDBGrid.BeforePo(DataSet: TDataSet);
var
  i: integer;
  C:TRxColumn;
begin
  if DatalinkActive then
  begin
    if FooterOptions.Active  then
    for i := 0 to Columns.Count - 1 do
    begin
      if not TRxColumn(Columns[i]).Footer.PostTestValue then
      begin
          FInProcessCalc := -1;
          Break;
      end;
    end;

    if rdgFilter in OptionsRx then
    for i := 0 to Columns.Count - 1 do
    begin
      C:=TRxColumn(Columns[i]);
      if Assigned(C.Field) and (C.Filter.ValueList.IndexOf(C.Field.DisplayText)<  0) then
        C.Filter.ValueList.Add(C.Field.DisplayText);
    end;
  end;

  if Assigned(F_EventOnBeforePost) then
    F_EventOnBeforePost(DataSet);
end;

procedure TRxDBGrid.ErrorDel(DataSet: TDataSet; E: EDatabaseError;
  var DataAction: TDataAction);
var
  i: integer;
begin
  if FFooterOptions.Active and (DatalinkActive) then
    for i := 0 to Columns.Count - 1 do
      if not TRxColumn(Columns[i]).Footer.ErrorTestValue then
      begin
        FInProcessCalc := -1;
        Break;
      end;
  if Assigned(F_EventOnDeleteError) then
    F_EventOnDeleteError(DataSet, E, DataAction);
end;

procedure TRxDBGrid.ErrorPo(DataSet: TDataSet; E: EDatabaseError;
  var DataAction: TDataAction);
var
  i: integer;
begin
  if FFooterOptions.Active and (DatalinkActive) then
    for i := 0 to Columns.Count - 1 do
      if not TRxColumn(Columns[i]).Footer.ErrorTestValue then
      begin
        FInProcessCalc := -1;
        Break;
      end;
  if Assigned(F_EventOnPostError) then
    F_EventOnPostError(DataSet, E, DataAction);
end;

procedure TRxDBGrid.OnFind(Sender: TObject);
begin
  if rdgAllowDialogFind in OptionsRx then
    ShowFindDialog;
end;

procedure TRxDBGrid.OnFilterBy(Sender: TObject);
var
  NewFilter: string;
begin
  if DataLinkActive then
  begin
    OptionsRx := OptionsRx - [rdgFilter];
    rxFilterByForm := TrxFilterByForm.Create(Application);
    NewFilter := DataSource.DataSet.Filter;
    if rxFilterByForm.Execute(Self, NewFilter, F_LastFilter) then
    begin
      if NewFilter <> '' then
      begin
        DataSource.DataSet.Filter := NewFilter;
        DataSource.DataSet.Filtered := True;
      end
      else
      begin
        DataSource.DataSet.Filtered := False;
      end;
      CalcStatTotals;
    end;
    FreeAndNil(rxFilterByForm);
  end;
end;

procedure TRxDBGrid.OnFilter(Sender: TObject);
var
  C: TRxColumn;
  i: integer;
  FBS, FAS:TDataSetNotifyEvent;
begin
  BeginUpdate;
  OptionsRx := OptionsRx + [rdgFilter];

  for i := 0 to Columns.Count - 1 do
  begin
    C := TRxColumn(Columns[i]);
    C.Filter.ValueList.Clear;
    C.Filter.Value := '';
    C.Filter.ItemIndex := -1;
    C.Filter.ValueList.Add(C.Filter.EmptyValue);
  end;

  if DatalinkActive then
  begin
    DataSource.DataSet.DisableControls;
    DataSource.DataSet.Filtered := True;
    FBS:=DataSource.DataSet.BeforeScroll;
    FAS:=DataSource.DataSet.AfterScroll;
    DataSource.DataSet.BeforeScroll:=nil;
    DataSource.DataSet.AfterScroll:=nil;
    DataSource.DataSet.First;
    while not DataSource.DataSet.EOF do
    begin
      for i := 0 to Columns.Count - 1 do
      begin
        C := TRxColumn(Columns[i]);
        if C.Filter.Enabled and (C.Field <> nil) and (C.Filter.ValueList.IndexOf(C.Field.DisplayText) < 0) then
          C.Filter.ValueList.Add(C.Field.DisplayText);
      end;
      DataSource.DataSet.Next;
    end;
    DataSource.DataSet.First;
    DataSource.DataSet.BeforeScroll:=FBS;
    DataSource.DataSet.AfterScroll:=FAS;
    DataSource.DataSet.EnableControls;
  end;

  EndUpdate;
end;

procedure TRxDBGrid.OnFilterClose(Sender: TObject);
var
  C: TRxColumn;
  i: integer;
begin
  OptionsRx := OptionsRx - [rdgFilter];
  DataSource.DataSet.Filtered := False;
  CalcStatTotals;
end;

procedure TRxDBGrid.OnSortBy(Sender: TObject);
var
  i: integer;
  S1: string;
  FSortListField:TStringList;
  FColumn:TRxColumn;
begin
  if DatalinkActive then
  begin
    FSortListField:=TStringList.Create;
    try
      rxSortByForm := TrxSortByForm.Create(Application);
      rxSortByForm.CheckBox1.Checked := rdgCaseInsensitiveSort in FOptionsRx;
      if rxSortByForm.Execute(Self, FSortListField) then
      begin
        for i := 0 to FSortListField.Count - 1 do
        begin
          S1:=FSortListField.Strings[i];
          FColumn:=TRxColumn(ColumnByFieldName(Copy(S1, 2, Length(S1))));
          if S1[1] = '1' then
            FColumn.FSortOrder := smUp
          else
            FColumn.FSortOrder := smDown;

          FColumn.FSortPosition:=i;
        end;

        CollumnSortListUpdate;

        if rxSortByForm.CheckBox1.Checked then
          Include(FOptionsRx, rdgCaseInsensitiveSort)
        else
          Exclude(FOptionsRx, rdgCaseInsensitiveSort);

        CollumnSortListApply;
        if Assigned(FOnSortChanged) then
        begin
          FSortingNow := True;
          FOnSortChanged(Self);
          FSortingNow := False;
        end;
      end;

    finally
      FreeAndNil(rxSortByForm);
      FreeAndNil(FSortListField);
    end;
    Invalidate;
  end;
end;

procedure TRxDBGrid.OnChooseVisibleFields(Sender: TObject);
begin
  if rdgAllowColumnsForm in OptionsRx then
    ShowColumnsDialog;
end;

procedure TRxDBGrid.OnSelectAllRows(Sender: TObject);
begin
  SelectAllRows;
end;

procedure TRxDBGrid.OnCopyCellValue(Sender: TObject);
var
  P:TBookMark;
  S:string;
  i, k:integer;
begin
  if DatalinkActive then
  begin
    if (dgMultiselect in Options) and (SelectedRows.Count>1) then
    begin
      S:='';
      DataSource.DataSet.DisableControls;
      {$IFDEF NoAutomatedBookmark}
      P:=DataSource.DataSet.GetBookmark;
      {$ELSE}
      P:=DataSource.DataSet.Bookmark;
      {$ENDIF}
      try
        DataSource.DataSet.First;
        while not DataSource.DataSet.EOF do
        begin
          if S<>'' then
            S:=S+LineEnding;
          K:=0;
          for i:=0 to Columns.Count-1 do
          begin
            if Assigned(Columns[i].Field) then
            begin
              if K<>0 then
                S:=S+#9;
              S:=S+Columns[i].Field.DisplayText;
              inc(K);
            end;
          end;
          DataSource.DataSet.Next;
        end;
      finally
      {$IFDEF NoAutomatedBookmark}
        DataSource.DataSet.GotoBookmark(P);
        DataSource.DataSet.FreeBookmark(P);
      {$ELSE}
        DataSource.DataSet.Bookmark:=P;
      {$ENDIF}
        DataSource.DataSet.EnableControls;
      end;
      Invalidate;
      if S<>'' then
      begin
        try
          Clipboard.Open;
          Clipboard.AsText:=S;
        finally
          Clipboard.Close;
        end;
      end;
    end
    else
    if Assigned(SelectedField) then
    try
      Clipboard.Open;
      Clipboard.AsText:=SelectedField.DisplayText;
    finally
      Clipboard.Close;
    end;
  end;
end;

procedure TRxDBGrid.Loaded;
begin
  inherited Loaded;
  UpdateJMenuKeys;
end;

procedure TRxDBGrid.UpdateFooterRowOnUpdateActive;
begin
  if Assigned(DataSource) then
  begin
    if DataSource.State <> FOldDataSetState then
    begin
      if (FOldDataSetState in dsEditModes) and (DataSource.State = dsBrowse) then
        CalcStatTotals;
      FOldDataSetState:=DataSource.State;
    end;
  end
  else
    FOldDataSetState:=dsInactive;
end;

procedure TRxDBGrid.DoEditorHide;
var
  R:TRxColumn;
  i, w:integer;
begin
  inherited DoEditorHide;
  R:=SelectedColumn as TRxColumn;

  if Assigned(Editor) and Assigned(R) then
  for i:=0 to R.EditButtons.Count-1 do
  begin
//    R.EditButtons[i].Visible:=false;
    if R.EditButtons[i].Style = ebsUpDownRx then
      R.EditButtons[i].FSpinBtn.Visible:=false
    else
      R.EditButtons[i].FButton.Visible:=false;
  end;
end;

procedure TRxDBGrid.DoEditorShow;
begin
  inherited DoEditorShow;
  DoSetColEdtBtn;
end;

procedure TRxDBGrid.GetOnCreateLookup;
begin
  if Assigned(F_CreateLookup) then
    F_CreateLookup(FRxDbGridLookupComboEditor);
end;

procedure TRxDBGrid.GetOnDisplayLookup;
begin
  if Assigned(F_DisplayLookup) then
    F_DisplayLookup(FRxDbGridLookupComboEditor);
end;

procedure TRxDBGrid.SelectAllRows;
var
  P:TBookMark;
begin
  if DatalinkActive then
  begin
    DataSource.DataSet.DisableControls;
{$IFDEF NoAutomatedBookmark}
    P:=DataSource.DataSet.GetBookmark;
{$ELSE}
    P:=DataSource.DataSet.Bookmark;
{$ENDIF}
    try
      DataSource.DataSet.First;
      while not DataSource.DataSet.EOF do
      begin
        SelectedRows.CurrentRowSelected:=true;
        DataSource.DataSet.Next;
      end;
    finally
{$IFDEF NoAutomatedBookmark}
      DataSource.DataSet.GotoBookmark(P);
      DataSource.DataSet.FreeBookmark(P);
{$ELSE}
      DataSource.DataSet.Bookmark:=P;
{$ENDIF}
      DataSource.DataSet.EnableControls;
    end;
    Invalidate;
  end;
end;

procedure TRxDBGrid.DeSelectAllRows;
var
  P:TBookMark;
begin
  if DatalinkActive then
  begin
    DataSource.DataSet.DisableControls;
{$IFDEF NoAutomatedBookmark}
    P:=DataSource.DataSet.GetBookmark;
{$ELSE}
    P:=DataSource.DataSet.Bookmark;
{$ENDIF}
    try
      DataSource.DataSet.First;
      while not DataSource.DataSet.EOF do
      begin
        SelectedRows.CurrentRowSelected:=false;
        DataSource.DataSet.Next;
      end;
    finally
{$IFDEF NoAutomatedBookmark}
      DataSource.DataSet.GotoBookmark(P);
      DataSource.DataSet.FreeBookmark(P);
{$ELSE}
      DataSource.DataSet.Bookmark:=P;
{$ENDIF}
      DataSource.DataSet.EnableControls;
    end;
    Invalidate;
  end;
end;

procedure TRxDBGrid.InvertSelection;
var
  P:TBookMark;
begin
  if DatalinkActive then
  begin
    DataSource.DataSet.DisableControls;
    {$IFDEF NoAutomatedBookmark}
    P:=DataSource.DataSet.GetBookmark;
    {$ELSE}
    P:=DataSource.DataSet.Bookmark;
    {$ENDIF}
    try
      DataSource.DataSet.First;
      while not DataSource.DataSet.EOF do
      begin
        SelectedRows.CurrentRowSelected:=not SelectedRows.CurrentRowSelected;
        DataSource.DataSet.Next;
      end;
    finally
      {$IFDEF NoAutomatedBookmark}
      DataSource.DataSet.GotoBookmark(P);
      DataSource.DataSet.FreeBookmark(P);
      {$ELSE}
      DataSource.DataSet.Bookmark:=P;
      {$ENDIF}
      DataSource.DataSet.EnableControls;
    end;
    Invalidate;
  end;
end;

procedure TRxDBGrid.CopyCellValue;
begin
  OnCopyCellValue(Self);
end;

procedure TRxDBGrid.SetSort(AFields: array of String;
  ASortMarkers: array of TSortMarker; PreformSort: Boolean);
var
  I: Integer;
  C: TRxColumn;
begin
  CollumnSortListClear;
  if (Length(AFields) > 0) and (Length(AFields) = Length(ASortMarkers)) then
  begin
    for I := 0 to Length(AFields) - 1 do
    begin
      C := ColumnByFieldName(AFields[I]);
      if C <> nil then
      begin
        C.SortOrder := ASortMarkers[I];
        C.FSortPosition := I;
      end;
    end;
    CollumnSortListUpdate;
  end;
  if PreformSort then
  begin
    if Assigned(FSortEngine) then
      CollumnSortListApply;
    if Assigned(FOnSortChanged) then
    begin
      FSortingNow := True;
      FOnSortChanged(Self);
      FSortingNow := False;
    end;
  end;
end;

//!!!
constructor TRxDBGrid.Create(AOwner: TComponent);
begin
  FFooterOptions:=TRxDBGridFooterOptions.Create(Self);
  inherited Create(AOwner);
{$IFDEF RXDBGRID_OPTIONS_WO_CANCEL_ON_EXIT}
  Options := Options - [dgCancelOnExit];
{$ENDIF}

  FSortColumns:=TRxDbGridColumnsSortList.Create;

  FMarkerUp := LoadLazResBitmapImage('rx_markerup');
  FMarkerDown := LoadLazResBitmapImage('rx_markerdown');

  Options := Options - [dgTabs];
  OptionsRx := OptionsRx + [rdgAllowColumnsForm, rdgAllowDialogFind,
    rdgAllowQuickFilter];

  FAutoSort := True;
  //  FTitleButtons:=True;

  F_Clicked := False;
  F_MenuBMP := LoadLazResBitmapImage('menu_grid');

  DoCreateJMenu;

  FKeyStrokes := TRxDBGridKeyStrokes.Create(Self);
  FKeyStrokes.ResetDefaults;

  F_LastFilter := TStringList.Create;
  //F_SortListField := TStringList.Create;

  FPropertyStorageLink := TPropertyStorageLink.Create;
  FPropertyStorageLink.OnSave := @OnIniSave;
  FPropertyStorageLink.OnLoad := @OnIniLoad;

//  FAllowedOperations := [aoInsert, aoUpdate, aoDelete, aoAppend];

  FFilterListEditor := TFilterListCellEditor.Create(nil);
  with FFilterListEditor do
  begin
    Name := 'FilterListEditor';
    Visible := False;
    Items.Append('');
    ReadOnly := True;
    AutoComplete := True;
    OnChange := @FFilterListEditorOnChange;
    OnCloseUp := @FFilterListEditorOnCloseUp;
  end;
  FColumnResizing := False;

  FRxDbGridLookupComboEditor := TRxDBGridLookupComboEditor.Create(nil);
  FRxDbGridLookupComboEditor.Name := 'RxDBGridLookupComboEditor';
  FRxDbGridLookupComboEditor.Visible := False;

  FRxDbGridDateEditor := TRxDBGridDateEditor.Create(nil);
  FRxDbGridDateEditor.Name := 'RxDbGridDateEditor';
  FRxDbGridDateEditor.Visible := False;

  UpdateJMenuKeys;

end;

destructor TRxDBGrid.Destroy;
begin
  CleanDSEvent;

  FreeAndNil(FFooterOptions);

  FreeAndNil(FRxDbGridLookupComboEditor);
  FreeAndNil(FRxDbGridDateEditor);
  FreeAndNil(FMarkerDown);
  FreeAndNil(FMarkerUp);
  FreeAndNil(FPropertyStorageLink);
  FreeAndNil(FFilterListEditor);

  FreeAndNil(F_PopupMenu);
  FreeAndNil(F_MenuBMP);
  FreeAndNil(F_LastFilter);

  FreeAndNil(FKeyStrokes);
  inherited Destroy;
  FreeAndNil(FSortColumns);
end;

procedure TRxDBGrid.LayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;

  inherited LayoutChanged;
  if DatalinkActive and (FInProcessCalc = 0) and (Datalink.DataSet.State = dsBrowse) then
    CalcStatTotals;
end;

procedure TRxDBGrid.SetFocus;
begin
  inherited SetFocus;
  if FFilterListEditor.Visible then
    FFilterListEditor.Hide;
end;

procedure TRxDBGrid.ShowFindDialog;
begin
  ShowRxDBGridFindForm(Self);
end;

procedure TRxDBGrid.ShowColumnsDialog;
begin
  ShowRxDBGridColumsForm(Self);
end;

procedure TRxDBGrid.ShowSortDialog;
begin
  OnSortBy(nil);
end;

procedure TRxDBGrid.ShowFilterDialog;
begin
  OnFilterBy(nil);
end;

function TRxDBGrid.ColumnByFieldName(AFieldName: string): TRxColumn;
var
  i: integer;
begin
  Result := nil;
  AFieldName := UpperCase(AFieldName);
  for i := 0 to Columns.Count - 1 do
  begin
    if UpperCase(Columns[i].FieldName) = AFieldName then
    begin
      Result := Columns[i] as TRxColumn;
      exit;
    end;
  end;
end;

function TRxDBGrid.ColumnByCaption(ACaption: string): TRxColumn;
var
  i: integer;
begin
  Result := nil;
  ACaption := UpperCase(ACaption);
  for i := 0 to Columns.Count - 1 do
    if ACaption = UpperCase(Columns[i].Title.Caption) then
    begin
      Result := TRxColumn(Columns[i]);
      exit;
    end;
end;

procedure TRxDBGrid.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
end;

procedure TRxDbGridColumns.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  TRxDBGrid(Grid).CollumnSortListUpdate;
end;

{ TRxDbGridColumns }
function TRxDbGridColumns.Add: TRxColumn;
begin
  Result := TRxColumn(inherited Add);
end;

{ TRxColumn }

function TRxColumn.GetKeyList: TStrings;
begin
  if FKeyList = nil then
    FKeyList := TStringList.Create;
  Result := FKeyList;
end;

function TRxColumn.GetSortFields: string;
begin
  if FSortFields = '' then
    Result:=FieldName
  else
    Result:=FSortFields;
end;

procedure TRxColumn.SetConstraints(AValue: TRxDBGridCollumnConstraints);
begin
  FConstraints.Assign(AValue);
end;

procedure TRxColumn.SetEditButtons(AValue: TRxColumnEditButtons);
begin
  FEditButtons.Assign(AValue);
end;

procedure TRxColumn.SetFilter(const AValue: TRxColumnFilter);
begin
  FFilter.Assign(AValue);
end;

function TRxColumn.GetFooter: TRxColumnFooter;
begin
  Result := FFooter;
end;

function TRxColumn.GetConstraints: TRxDBGridCollumnConstraints;
begin
  Result:=FConstraints;
end;

procedure TRxColumn.SetFooter(const AValue: TRxColumnFooter);
begin
  FFooter.Assign(AValue);
end;

procedure TRxColumn.SetImageList(const AValue: TImageList);
begin
  if FImageList = AValue then
    exit;
  FImageList := AValue;
  if Grid <> nil then
    Grid.Invalidate;
end;

procedure TRxColumn.SetKeyList(const AValue: TStrings);
begin
  if AValue = nil then
  begin
    if FKeyList <> nil then
      FKeyList.Clear;
  end
  else
    KeyList.Assign(AValue);
end;

procedure TRxColumn.SetNotInKeyListIndex(const AValue: integer);
begin
  if FNotInKeyListIndex = AValue then
    exit;
  FNotInKeyListIndex := AValue;
  if Grid <> nil then
    Grid.Invalidate;
end;

procedure TRxColumn.SetWordWrap(AValue: boolean);
begin
  if FWordWrap=AValue then Exit;
  FWordWrap:=AValue;
end;

function TRxColumn.CreateTitle: TGridColumnTitle;
begin
  Result := TRxColumnTitle.Create(Self);
end;

procedure TRxColumn.ColumnChanged;
begin
  inherited ColumnChanged;
  if Assigned(FConstraints) and (FConstraints.MinWidth <> 0) and (FConstraints.MinWidth > Width) then
    Width:=FConstraints.MinWidth;

  if Assigned(FConstraints) and (FConstraints.MaxWidth <> 0) and (FConstraints.MaxWidth < Width) then
    Width:=FConstraints.MaxWidth;
end;

constructor TRxColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FNotInKeyListIndex := -1;
  FConstraints:=TRxDBGridCollumnConstraints.Create(Self);
  FFooter := TRxColumnFooter.Create(Self);
  FFilter := TRxColumnFilter.Create(Self);
  FDirectInput := true;
  FEditButtons:=TRxColumnEditButtons.Create(Self);
  FOptions:=[coCustomizeVisible, coCustomizeWidth];
end;

destructor TRxColumn.Destroy;
begin
  FreeAndNil(FEditButtons);
  if FKeyList <> nil then
  begin
    FKeyList.Free;
    FKeyList := nil;
  end;
  FreeAndNil(FFooter);
  FreeAndNil(FFilter);
  FreeAndNil(FConstraints);
  inherited Destroy;
end;

procedure TRxColumn.OptimizeWidth;
begin
  if Grid <> nil then
    TRxDBGrid(Grid).OptimizeColumnsWidth(FieldName);
end;

{ TRxColumnTitle }
procedure TRxColumnTitle.SetOrientation(const AValue: TTextOrientation);
begin
  if FOrientation = AValue then
    exit;
  FOrientation := AValue;
  TRxDBGrid(TRxColumn(Column).Grid).CalcTitle;
  TRxColumn(Column).ColumnChanged;
end;

function TRxColumnTitle.GetCaptionLinesCount: integer;
begin
  if Assigned(FCaptionLines) then
    Result := FCaptionLines.Count
  else
    Result := 0;
end;

function TRxColumnTitle.CaptionLine(ALine: integer): TMLCaptionItem;
begin
  if Assigned(FCaptionLines) and (FCaptionLines.Count > 0) and
    (ALine >= 0) and (FCaptionLines.Count > ALine) then
    Result := TMLCaptionItem(FCaptionLines[ALine])
  else
    Result := nil;
end;

procedure TRxColumnTitle.ClearCaptionML;
var
  i: integer;
  R: TMLCaptionItem;
begin
  for i := 0 to FCaptionLines.Count - 1 do
  begin
    R := TMLCaptionItem(FCaptionLines[i]);
    R.Free;
  end;
  FCaptionLines.Clear;
end;

procedure TRxColumnTitle.SetCaption(const AValue: TCaption);
var
  c: integer;
  s: string;

  procedure AddMLStr(AStr: string);
  var
    R: TMLCaptionItem;
  begin
    R := TMLCaptionItem.Create;
    R.Caption := AStr;
    R.Col := Column;
    FCaptionLines.Add(R);
  end;

begin
  inherited SetCaption(AValue);
  ClearCaptionML;
  c := Pos('|', AValue);
  if C > 0 then
  begin
    S := AValue;
    while C > 0 do
    begin
      AddMLStr(Copy(S, 1, C - 1));
      System.Delete(S, 1, C);
      c := Pos('|', S);
    end;
    if S <> '' then
      AddMLStr(S);
  end;
  if not (csLoading in Column.Grid.ComponentState) and Column.Grid.HandleAllocated then
    TRxDBGrid(Column.Grid).CalcTitle;
end;

constructor TRxColumnTitle.Create(TheColumn: TGridColumn);
begin
  inherited Create(TheColumn);
{$IFDEF NEW_STYLE_TITLE_ALIGNMENT_RXDBGRID}
  Alignment := taCenter;
{$ENDIF}
  FCaptionLines := TFPList.Create;
end;

destructor TRxColumnTitle.Destroy;
begin
  ClearCaptionML;
  FreeAndNil(FCaptionLines);
  inherited Destroy;
end;

{ TRxColumnFooter }

procedure TRxColumnFooter.SetValue(const AValue: string);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFooter.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat = AValue then
    exit;
  FDisplayFormat := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFooter.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then
    exit;
  FAlignment := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFooter.SetFieldName(const AValue: string);
begin
  if FFieldName = AValue then
    exit;
  FFieldName := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFooter.SetLayout(const AValue: TTextLayout);
begin
  if FLayout = AValue then
    exit;
  FLayout := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFooter.SetValueType(const AValue: TFooterValueType);
begin
  if FValueType = AValue then
    exit;
  FValueType := AValue;
  if FValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
    TRxDBGrid(FOwner.Grid).CalcStatTotals;
  FOwner.ColumnChanged;
end;

function TRxColumnFooter.DisplayText: string;
begin
  case FValueType of
    fvtSum,
    fvtAvg,
    fvtMax,
    fvtMin: Result := GetStatTotal;
    fvtCount: Result := GetRecordsCount;
    fvtFieldValue: Result := GetFieldValue;
    fvtStaticText: Result := FValue;
    fvtRecNo: Result := GetRecNo;
    else
      Result := '';
  end;
end;

function TRxColumnFooter.GetFieldValue: string;
begin
  if (FFieldName <> '') and TRxDBGrid(FOwner.Grid).DatalinkActive then
    Result := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName).AsString
  else
    Result := '';
end;

function TRxColumnFooter.GetRecordsCount: string;
begin
  if TRxDBGrid(FOwner.Grid).DatalinkActive then
  begin
    if DisplayFormat <> '' then
      Result := Format(DisplayFormat,
        [{TRxDBGrid(FOwner.Grid).DataSource.DataSet.RecordCount} FCountRec])
    else
      Result := IntToStr(FCountRec); //TRxDBGrid(FOwner.Grid).DataSource.DataSet.RecordCount);
  end
  else
    Result := '';
end;

function TRxColumnFooter.GetRecNo: string;
begin
  if TRxDBGrid(FOwner.Grid).DatalinkActive then
  begin
    if DisplayFormat <> '' then
      Result := Format(DisplayFormat, [TRxDBGrid(FOwner.Grid).DataSource.DataSet.RecNo])
    else
      Result := IntToStr(TRxDBGrid(FOwner.Grid).DataSource.DataSet.RecNo);
  end
  else
    Result := '';
end;

function TRxColumnFooter.GetStatTotal: string;
var
  F: TField;
begin
  if (FFieldName <> '') and TRxDBGrid(FOwner.Grid).DatalinkActive and
    (TRxDBGrid(FOwner.Grid).DataSource.DataSet.RecordCount <> 0) then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName);
    if Assigned(F) then
    begin
      if F.DataType in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
        ftDate, ftTime, ftDateTime, ftTimeStamp, ftLargeint, ftBCD] then
      begin
        if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
        begin
          if FValueType in [fvtSum, fvtAvg] then
            Result := ''
          else
          if FTestValue = 0 then
            Result := ''
          else
          if FDisplayFormat = '' then
            Result := DateToStr(FTestValue)
          else
            Result := FormatDateTime(FDisplayFormat, FTestValue);
        end
        else
        if F.DataType in [ftSmallint, ftInteger, ftWord, ftLargeint] then
        begin
          if FDisplayFormat = '' then
            Result := IntToStr(Round(FTestValue))
          else
            Result := Format(FDisplayFormat, [Round(FTestValue)]);
        end
        else
        begin
          if FDisplayFormat <> '' then
            Result := FormatFloat(FDisplayFormat, FTestValue)
          else
          if F.DataType = ftCurrency then
            Result := FloatToStrF(FTestValue, ffCurrency, 12, 2)
          else
            Result := FloatToStr(FTestValue);
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TRxColumnFooter.ResetTestValue;
var
  F: TField;
begin
  FTestValue := 0;
  FCountRec:=0;

  if (ValueType in [fvtMin, fvtMax]) and (TRxDBGrid(
    FOwner.Grid).DataSource.DataSet.RecordCount <> 0) then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName);
    if (Assigned(F)) and not (F.IsNull) then
      if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
        FTestValue := F.AsDateTime
      else
        FTestValue := F.AsFloat;
  end;
end;

procedure TRxColumnFooter.UpdateTestValue;
var
  F: TField;
begin
  if ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FindField(FFieldName);
    if Assigned(F) then
    begin
      if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
      begin
        case FValueType of
          fvtMax: FTestValue := Max(FTestValue, F.AsDateTime);
          fvtMin: FTestValue := Min(FTestValue, F.AsDateTime);
        end;
      end
      else
      begin
        case FValueType of
          fvtSum: FTestValue := FTestValue + F.AsFloat;
          //        fvtAvg:
          fvtMax: FTestValue := Max(FTestValue, F.AsFloat);
          fvtMin: FTestValue := Min(FTestValue, F.AsFloat);
        end;
      end;
    end;
  end;
end;

function TRxColumnFooter.DeleteTestValue: boolean;
var
  F: TField;
begin
  Result := True;
  if ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName);
    if (Assigned(F)) and not (F.IsNull) then
      if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
        Result := not ((FValueType in [fvtMax, fvtMin]) and (FTestValue = F.AsDateTime))
      else
      if FValueType in [fvtMax, fvtMin] then
        Result := (FTestValue <> F.AsFloat)
      else
        FTestValue := FTestValue - F.AsFloat;
  end;
end;

function TRxColumnFooter.PostTestValue: boolean;
var
  F: TField;
begin
  Result := True;
  if ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName);
    if Assigned(F) then
      if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
      begin
        if FValueType in [fvtMax, fvtMin] then
          if F.DataSet.State = dsinsert then
          begin
            if not (F.IsNull) then
              case FValueType of
                fvtMax: FTestValue := Max(FTestValue, F.AsDateTime);
                fvtMin: FTestValue := Min(FTestValue, F.AsDateTime);
              end;
          end
          else
          if (F.OldValue <> null) and (FTestValue = TDateTime(F.OldValue)) then
            Result := False
          else
          if not F.IsNull then
            case FValueType of
              fvtMax: FTestValue := Max(FTestValue, F.AsDateTime);
              fvtMin: FTestValue := Min(FTestValue, F.AsDateTime);
            end;
      end
      else
      if F.DataSet.State = dsinsert then
      begin
        if not F.IsNull then
          case FValueType of
            fvtSum: FTestValue := FTestValue + F.AsFloat;
            fvtMax: FTestValue := Max(FTestValue, F.AsFloat);
            fvtMin: FTestValue := Min(FTestValue, F.AsFloat);
          end;
      end
      else
      if (FValueType in [fvtMax, fvtMin]) and (F.OldValue <> null) and
        (FTestValue = Float(F.OldValue)) then
        Result := False
      else
        case FValueType of
          fvtSum:
          begin
            if not F.IsNull then
            begin
              if F.OldValue <> null then
                FTestValue := FTestValue - Float(F.OldValue);
              FTestValue := FTestValue + F.AsFloat;
            end;
          end;
          fvtMax: if not F.IsNull then
              FTestValue := Max(FTestValue, F.AsFloat);
          fvtMin: if not F.IsNull then
              FTestValue := Min(FTestValue, F.AsFloat);
        end;
  end;
end;

function TRxColumnFooter.ErrorTestValue: boolean;
var
  F: TField;
begin
  Result := True;
  if ValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
  begin
    F := TRxDBGrid(FOwner.Grid).DataSource.DataSet.FieldByName(FFieldName);
    if Assigned(F) then
    begin
      if F.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
      begin
        if (FValueType in [fvtMax, fvtMin]) and not (F.IsNull) then
        begin
          if not (F.IsNull) and (FTestValue = F.AsDateTime) then
            Result := False
          else
          if (F.DataSet.RecordCount <> 0) and (F.OldValue <> null) then
          begin
            case FValueType of
              fvtMax: FTestValue := Max(FTestValue, TDateTime(F.OldValue));
              fvtMin: FTestValue := Min(FTestValue, TDateTime(F.OldValue));
            end;
          end;
        end;
      end
      else
      if (FValueType in [fvtMax, fvtMin]) and not (F.IsNull) and (FTestValue = F.AsFloat) then
        Result := False
      else
      begin
        case FValueType of
          fvtSum:
            if F.DataSet.RecordCount = 0 then
            begin
{              if not F.IsNull then
                FTestValue := FTestValue - F.AsFloat;}
              { TODO -oalexs : need rewrite this code - where difficult! }
            end
            else
            begin
              if F.OldValue <> null then
                FTestValue := FTestValue + Float(F.OldValue);
              if not F.IsNull then
                FTestValue := FTestValue - F.AsFloat;
            end;
          fvtMax:
            if (F.DataSet.RecordCount <> 0) and (F.OldValue <> null) then
              FTestValue := Max(FTestValue, Float(F.OldValue));
          fvtMin:
            if (F.DataSet.RecordCount <> 0) and (F.OldValue <> null) then
              FTestValue := Min(FTestValue, Float(F.OldValue));
        end;
      end;
    end;
  end;
end;

procedure TRxColumnFooter.UpdateTestValueFromVar(AValue: Variant);
begin
  if FValueType in [fvtSum, fvtAvg, fvtMax, fvtMin] then
  begin
    if (not VarIsEmpty(AValue)) and (AValue <> null) and Assigned(FField) then
    begin
      if FField.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp] then
      begin
        case FValueType of
          fvtMax: FTestValue := Max(FTestValue, AValue);
          fvtMin: FTestValue := Min(FTestValue, AValue);
        end;
      end
      else
      begin
        case FValueType of
          fvtSum,
          fvtAvg: FTestValue := FTestValue + AValue;
          fvtMax: FTestValue := Max(FTestValue, AValue);
          fvtMin: FTestValue := Min(FTestValue, AValue);
        end;
      end;
    end;
  end;
end;

///!
constructor TRxColumnFooter.Create(Owner: TRxColumn);
begin
  inherited Create;
  FOwner := Owner;
  FTestValue := 0;
  FLayout := tlCenter;
end;

{ TFilterListCellEditor }

procedure TFilterListCellEditor.WndProc(var TheMessage: TLMessage);
begin

  if TheMessage.msg = LM_KILLFOCUS then
  begin
    Change;
    Hide;
    if HWND(TheMessage.WParam) = HWND(Handle) then
    begin
      // lost the focus but it returns to ourselves
      // eat the message.
      TheMessage.Result := 0;
      exit;
    end;
  end;
  inherited WndProc(TheMessage);
end;

procedure TFilterListCellEditor.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_RETURN:
    begin
      DroppedDown := False;
      Change;
      Hide;
    end;
  end;
end;

procedure TFilterListCellEditor.Show(Grid: TCustomGrid; Col: integer);
begin
  FGrid := Grid;
  FCol := Col;
  Visible := True;
  //  Text:=TRxColumn(TRxDBGrid(Grid).SelectedColumn).Filter.Value;
  SetFocus;
  //  DroppedDown := true;
end;


{ TRxColumnFilter }

function TRxColumnFilter.GetItemIndex: integer;
begin
  Result := FValueList.IndexOf(FValue);
end;

procedure TRxColumnFilter.SetColor(const AValue: TColor);
begin
  if FColor = AValue then
    exit;
  FColor := AValue;
  FOwner.ColumnChanged;
end;

procedure TRxColumnFilter.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
  FOwner.ColumnChanged;
end;

procedure TRxColumnFilter.SetItemIndex(const AValue: integer);
begin
  if (AValue >= -1) and (AValue < FValueList.Count) then
  begin
    if AValue = -1 then
      FValue := ''
    else
      FValue := FValueList[AValue];
    FOwner.ColumnChanged;
  end;
end;

constructor TRxColumnFilter.Create(Owner: TRxColumn);
begin
  inherited Create;
  FOwner := Owner;
  FFont := TFont.Create;
  FEmptyFont := TFont.Create;
  FValueList := TStringList.Create;
  FValueList.Sorted := True;
  FColor := clWhite;

  FEmptyFont.Style := [fsItalic];
  FEmptyValue := sRxDBGridEmptiFilter;
  FEnabled:=true;
end;

destructor TRxColumnFilter.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FEmptyFont);
  FreeAndNil(FValueList);
  inherited Destroy;
end;

{ TExDBGridSortEngine }

function TRxDBGridSortEngine.EnabledFooterRowsCalc: boolean;
begin
  Result:=false;
end;

procedure TRxDBGridSortEngine.SortList(ListField: string; ADataSet: TDataSet;
  Asc: array of boolean; SortOptions: TRxSortEngineOptions);
begin

end;

{ TRxDBGridKeyStroke }

procedure TRxDBGridKeyStroke.SetCommand(const AValue: TRxDBGridCommand);
begin
  if FCommand = AValue then
    exit;
  FCommand := AValue;
  Changed(False);
end;

procedure TRxDBGridKeyStroke.SetShortCut(const AValue: TShortCut);
begin
  if FShortCut = AValue then
    exit;
  FShortCut := AValue;
  Menus.ShortCutToKey(FShortCut, FKey, FShift);
  Changed(False);
end;

function TRxDBGridKeyStroke.GetDisplayName: string;
begin
  IntToIdent(Ord(FCommand), Result, EditorCommandStrs);
  Result := Result + ' - ' + ShortCutToText(FShortCut);
end;

procedure TRxDBGridKeyStroke.Assign(Source: TPersistent);
begin
  if Source is TRxDBGridKeyStroke then
  begin
    Command := TRxDBGridKeyStroke(Source).Command;
    ShortCut := TRxDBGridKeyStroke(Source).ShortCut;
    Enabled := TRxDBGridKeyStroke(Source).Enabled;
  end
  else
    inherited Assign(Source);
end;

{ TRxDBGridKeyStrokes }

function TRxDBGridKeyStrokes.GetItem(Index: integer): TRxDBGridKeyStroke;
begin
  Result := TRxDBGridKeyStroke(inherited GetItem(Index));
end;

procedure TRxDBGridKeyStrokes.SetItem(Index: integer; const AValue: TRxDBGridKeyStroke);
begin
  inherited SetItem(Index, AValue);
end;

procedure TRxDBGridKeyStrokes.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if (UpdateCount = 0) and Assigned(Owner) and Assigned(TRxDBGrid(Owner).FKeyStrokes) then
    TRxDBGrid(Owner).UpdateJMenuKeys;
end;


constructor TRxDBGridKeyStrokes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRxDBGridKeyStroke);
end;

procedure TRxDBGridKeyStrokes.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TRxDBGridKeyStrokes then
  begin
    Clear;
    for i := 0 to TRxDBGridKeyStrokes(Source).Count-1 do
    begin
      with Add do
        Assign(TRxDBGridKeyStrokes(Source)[i]);
    end;
  end else
    inherited Assign(Source);
end;

function TRxDBGridKeyStrokes.Add: TRxDBGridKeyStroke;
begin
  Result := TRxDBGridKeyStroke(inherited Add);
  Result.Enabled := True;
end;

function TRxDBGridKeyStrokes.AddE(ACommand: TRxDBGridCommand;
  AShortCut: TShortCut): TRxDBGridKeyStroke;
begin
  Result := nil;
  Result := Add;
  Result.FShortCut := AShortCut;
  Result.FCommand := ACommand;
end;

procedure TRxDBGridKeyStrokes.ResetDefaults;
begin
  Clear;
  AddE(rxgcShowFindDlg, Menus.ShortCut(Ord('F'), [ssCtrl]));
  AddE(rxgcShowColumnsDlg, Menus.ShortCut(Ord('W'), [ssCtrl]));
  AddE(rxgcShowFilterDlg, Menus.ShortCut(Ord('T'), [ssCtrl]));
  AddE(rxgcShowSortDlg, Menus.ShortCut(Ord('S'), [ssCtrl]));
  AddE(rxgcShowQuickFilter, Menus.ShortCut(Ord('Q'), [ssCtrl]));
  AddE(rxgcHideQuickFilter, Menus.ShortCut(Ord('H'), [ssCtrl]));
  AddE(rxgcSelectAll, Menus.ShortCut(Ord('A'), [ssCtrl]));
  AddE(rxgcDeSelectAll, Menus.ShortCut(Ord('-'), [ssCtrl]));
  AddE(rxgcInvertSelection, Menus.ShortCut(Ord('*'), [ssCtrl]));
  AddE(rxgcOptimizeColumnsWidth, Menus.ShortCut(Ord('+'), [ssCtrl]));
  AddE(rxgcCopyCellValue, Menus.ShortCut(Ord('C'), [ssCtrl]));
end;

function TRxDBGridKeyStrokes.FindRxCommand(AKey: word;
  AShift: TShiftState): TRxDBGridCommand;
var
  i: integer;
  K: TRxDBGridKeyStroke;
begin
  Result := rxgcNone;
  for i := 0 to Count - 1 do
  begin
    K := Items[i];
    if (K.FKey = AKey) and (K.FShift = AShift) and (K.FEnabled) then
    begin
      Result := K.FCommand;
      exit;
    end;
  end;
end;

function TRxDBGridKeyStrokes.FindRxKeyStrokes(ACommand: TRxDBGridCommand):
TRxDBGridKeyStroke;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Command = ACommand) then
    begin
      Result := Items[i];
      exit;
    end;
  end;
end;

initialization
  {$IFNDEF RxDBGridDepricatedProps}
  RegisterPropertyToSkip( TRxDBGrid, 'AllowedOperations', 'This property duplicated standart DBGrid.Options', '');
  {$ENDIF}

  {$I rxdbgrid.lrs}
  //  {$I rx_markerdown.lrs}

  RxDBGridSortEngineList := TStringList.Create;
  RxDBGridSortEngineList.Sorted := True;

finalization

  while (RxDBGridSortEngineList.Count > 0) do
  begin
    RxDBGridSortEngineList.Objects[0].Free;
    RxDBGridSortEngineList.Delete(0);
  end;
  RxDBGridSortEngineList.Free;

end.

