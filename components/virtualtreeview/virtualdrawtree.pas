unit VirtualDrawTree;

interface

uses
  Classes, Types,SysUtils,Controls,
  Graphics,virtualtrees;

type
  TVTDrawHintEvent = procedure(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode; R: TRect;
    Column: TColumnIndex) of object;
  TVTDrawNodeEvent = procedure(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo) of object;
  TVTGetNodeWidthEvent = procedure(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; var NodeWidth: Integer) of object;
  TVTGetHintSizeEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    var R: TRect) of object;

  // Tree descendant to let an application draw its stuff itself.
  TCustomVirtualDrawTree = class(TBaseVirtualTree)
  private
    FOnDrawNode: TVTDrawNodeEvent;
    FOnGetNodeWidth: TVTGetNodeWidthEvent;
    FOnGetHintSize: TVTGetHintSizeEvent;
    FOnDrawHint: TVTDrawHintEvent;
  protected
    procedure DoDrawHint(xCanvas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex);
    procedure DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R: TRect); virtual;
    function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; xCanvas: TCanvas = nil): Integer; override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;

    property OnDrawHint: TVTDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnDrawNode: TVTDrawNodeEvent read FOnDrawNode write FOnDrawNode;
    property OnGetHintSize: TVTGetHintSizeEvent read FOnGetHintSize write FOnGetHintSize;
    property OnGetNodeWidth: TVTGetNodeWidthEvent read FOnGetNodeWidth write FOnGetNodeWidth;
  end;

  TVirtualDrawTree = class(TCustomVirtualDrawTree)
  protected
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    property Canvas;
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BorderStyle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DragCursor;
    property DragMode;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintAnimation;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    {$ifdef COMPILER_7_UP}
      property ParentBackground;
    {$endif COMPILER_7_UP}
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions;//: TVirtualTreeOptions read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    {$ifdef COMPILER_5_UP}
      property OnContextPopup;
    {$endif COMPILER_5_UP}
//    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
//    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawHint;
    property OnDrawNode;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetHelpContext;
    property OnGetHintSize;
    property OnGetImageIndex;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetNodeWidth;
    property OnGetPopupMenu;
//    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDraw;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;

implementation

//----------------- TCustomVirtualDrawTree -----------------------------------------------------------------------------

procedure TCustomVirtualDrawTree.DoDrawHint(xCanvas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex);

begin
  if Assigned(FOnDrawHint) then
    FOnDrawHint(Self, xCanvas, Node, R, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualDrawTree.DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R: TRect);

begin
  if Assigned(FOnGetHintSize) then
    FOnGetHintSize(Self, Node, Column, R);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualDrawTree.DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; xCanvas: TCanvas = nil): Integer;

begin
  Result := 2 * FTextMargin;
  if xCanvas = nil then
    xCanvas := Self.Canvas;

  if Assigned(FOnGetNodeWidth) then
    FOnGetNodeWidth(Self, xCanvas, Node, Column, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualDrawTree.DoPaintNode(var PaintInfo: TVTPaintInfo);

begin
  if Assigned(FOnDrawNode) then
    FOnDrawNode(Self, PaintInfo);
end;

//----------------- TVirtualDrawTree -----------------------------------------------------------------------------------

{function TVirtualDrawTree.GetOptions: TVirtualTreeOptions;

begin
  Result := Options as TVirtualTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualDrawTree.SetOptions(const Value: TVirtualTreeOptions);

begin
  FOptions.Assign(Value);
end;}

//----------------------------------------------------------------------------------------------------------------------

function TVirtualDrawTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TVirtualTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------
end.
