unit VirtualStringTree;

interface

uses
  Classes, Types,SysUtils,StdCtrls,LMessages,Forms,LCLType,LCLProc,LCLIntf,
  Graphics,virtualtrees,Controls;


type
  // Options regarding strings (useful only for the string tree and descentants):
  TVTStringOption = (
    toSaveCaptions,          // If set then the caption is automatically saved with the tree node, regardless of what is
                             // saved in the user data.
    toShowStaticText,        // Show static text in a caption which can be differently formatted than the caption
                             // but cannot be edited.
    toAutoAcceptEditChange   // Automatically accept changes during edit if the user finishes editing other then
                             // VK_RETURN or ESC. If not set then changes are cancelled.
  );
const
  DefaultStringOptions = [toSaveCaptions, toAutoAcceptEditChange];
  AlignmentToDrawFlag: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);

  CaptionChunk = 3;     // used by the string tree to store a node's caption

type

  TVTStringOptions = set of TVTStringOption;

  TCustomStringTreeOptions = class(TVirtualTreeOptions)
  private
    FStringOptions: TVTStringOptions;
    procedure SetStringOptions(const Value: TVTStringOptions);
  protected
    property StringOptions: TVTStringOptions read FStringOptions write SetStringOptions default DefaultStringOptions;
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
  public
    constructor Create(AOwner: TBaseVirtualTree); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TStringTreeOptions = class(TCustomStringTreeOptions)
  published
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
  end;

  TCustomVirtualStringTree = class;

  // Edit support classes.
  TStringEditLink = class;

    TVTEdit = class(TCustomEdit)
  private
    FRefLink: IVTEditLink;
    FLink: TStringEditLink;
    procedure CMAutoAdjust(var Message: TLMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
    procedure CMRelease(var Message: TLMessage); message CM_RELEASE;
    procedure CNCommand(var Message: TLMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMGetDlgCode(var Message: TLMNoParams {TWMGetDlgCode}); message LM_GETDLGCODE;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  protected
    procedure AutoAdjustSize;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TStringEditLink); reintroduce;

    procedure Release; virtual;

    //property AutoSelect; todo test, maybe it will come
    property AutoSize;
    property BorderStyle;
    property CharCase;
    //property HideSelection;
    property MaxLength;
    //property OEMConvert;
    property PasswordChar;
  end;

  TStringEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TVTEdit;                  // A normal custom edit control.
    FTree: TCustomVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FAlignment: TAlignment;
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
    procedure SetEdit(const Value: TVTEdit);
  public
    constructor Create;
    destructor Destroy; override;

    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    property Edit: TVTEdit read FEdit write SetEdit;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
  end;

  // Describes the type of text to return in the text and draw info retrival events.
  TVSTTextType = (
    ttNormal,      // normal label of the node, this is also the text which can be edited
    ttStatic       // static (non-editable) text after the normal text
  );

  // Describes the source to use when converting a string tree into a string for clipboard etc.
  TVSTTextSourceType = (
    tstAll,             // All nodes are rendered. Initialization is done on the fly.
    tstInitialized,     // Only initialized nodes are rendered.
    tstSelected,        // Only selected nodes are rendered.
    tstCutCopySet,      // Only nodes currently marked as being in the cut/copy clipboard set are rendered.
    tstVisible          // Only visible nodes are rendered.
  );

  TVTPaintText = procedure(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType) of object;
  TVSTGetTextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: WideString) of object;
  // New text can only be set for variable caption.
  TVSTNewTextEvent = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    NewText: WideString) of object;
  TVSTShortenStringEvent = procedure(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
    Column: TColumnIndex; const S: WideString; TextSpace: Integer; RightToLeft: Boolean; var Result: WideString;
    var Done: Boolean) of object;

  { TCustomVirtualStringTree }

  TCustomVirtualStringTree = class(TBaseVirtualTree)
  private
    FDefaultText: WideString;                    // text to show if there's no OnGetText event handler (e.g. at design time)
    FTextHeight: Integer;                        // true size of the font
    FEllipsisWidth: Integer;                     // width of '...' for the current font
    FInternalDataOffset: Cardinal;               // offset to the internal data of the string tree

    FOnPaintText: TVTPaintText;                  // triggered before either normal or fixed text is painted to allow
                                                 // even finer customization (kind of sub cell painting)
    FOnGetText,                                  // used to retrieve the string to be displayed for a specific node
    FOnGetHint: TVSTGetTextEvent;                // used to retrieve the hint to be displayed for a specific node
    FOnNewText: TVSTNewTextEvent;                // used to notify the application about an edited node caption
    FOnShortenString: TVSTShortenStringEvent;    // used to allow the application a customized string shortage

    procedure GetRenderStartValues(Source: TVSTTextSourceType; var Node: PVirtualNode;
      var NextNodeProc: TGetNextNodeProc);
    function GetOptions: TStringTreeOptions;
    function GetText(Node: PVirtualNode; Column: TColumnIndex): WideString;
    procedure InitializeTextProperties(var PaintInfo: TVTPaintInfo);
    procedure PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer; xText: WideString);
    procedure PaintStaticText(const PaintInfo: TVTPaintInfo; TextOutFlags: Integer; const xText: WideString);
    procedure ReadText(Reader: TReader);
    procedure SetDefaultText(const Value: WideString);
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure SetText(Node: PVirtualNode; Column: TColumnIndex; const Value: WideString);
    procedure WriteText(Writer: TWriter);
  protected
    procedure AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex); override;
    function CalculateTextWidth(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; xText: WideString): Integer; virtual;
    function ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex): WideString; override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex): WideString; override;
    function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; xCanvas: TCanvas = nil): Integer; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var xText: WideString); virtual;
    function DoIncrementalSearch(Node: PVirtualNode; const xText: WideString): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; xText: WideString); virtual;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure DoPaintText(Node: PVirtualNode; const xCanvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); virtual;
    function DoShortenString(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const S: WideString; xWidth: Integer;
      RightToLeft: Boolean; EllipsisWidth: Integer = 0): WideString; virtual;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
      var xText: WideString); override;
    function InternalData(Node: PVirtualNode): Pointer;
    procedure MainColumnChanged; override;
    function ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
      ChunkSize: Integer): Boolean; override;
    procedure WriteChunks(Stream: TStream; Node: PVirtualNode); override;

    property DefaultText: WideString read FDefaultText write SetDefaultText stored False;
    property EllipsisWidth: Integer read FEllipsisWidth;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;

    property OnGetHint: TVSTGettextEvent read FOnGetHint write FOnGetHint;
    property OnGetText: TVSTGetTextEvent read FOnGetText write FOnGetText;
    property OnNewText: TVSTNewTextEvent read FOnNewText write FOnNewText;
    property OnPaintText: TVTPaintText read FOnPaintText write FOnPaintText;
    property OnShortenString: TVSTShortenStringEvent read FOnShortenString write FOnShortenString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function ComputeNodeHeight(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex): Integer; virtual;
    function ContentToClipboard(Format: Word; Source: TVSTTextSourceType): HGLOBAL;
    function ContentToHTML(Source: TVSTTextSourceType; xCaption: WideString = ''): string;
    function ContentToRTF(Source: TVSTTextSourceType): string;
    function ContentToText(Source: TVSTTextSourceType; Separator: Char): string;
    function ContentToUnicode(Source: TVSTTextSourceType; Separator: WideChar): WideString;
    function InvalidateNode(Node: PVirtualNode): TRect; override;
    function Path(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Delimiter: WideChar): WideString;
    procedure ReinitNode(Node: PVirtualNode; Recursive: Boolean); override;

    property Text[Node: PVirtualNode; Column: TColumnIndex]: WideString read GetText write SetText;
  end;

  TVirtualStringTree = class(TCustomVirtualStringTree)
  private
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
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
    property DefaultText;
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
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
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
    property OnEditCancelled;
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
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
//    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
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
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;


implementation


//----------------- TCustomStringTreeOptions ---------------------------------------------------------------------------

constructor TCustomStringTreeOptions.Create(AOwner: TBaseVirtualTree);

begin
  inherited;
  FStringOptions := DefaultStringOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomStringTreeOptions.SetStringOptions(const Value: TVTStringOptions);

var
  ChangedOptions: TVTStringOptions;

begin
  if FStringOptions <> Value then
  begin
    // Exclusive ORing to get all entries wich are in either set but not in both.
    ChangedOptions := FStringOptions + Value - (FStringOptions * Value);
    FStringOptions := Value;
    with Owner do
      if (toShowStaticText in ChangedOptions) and not (csLoading in ComponentState) and HandleAllocated then
        Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomStringTreeOptions.AssignTo(Dest: TPersistent);

begin
  if Dest is TCustomStringTreeOptions then
  begin
    with Dest as TCustomStringTreeOptions do
      StringOptions := Self.StringOptions;
  end;

  // Let ancestors assign their options to the destination class.
  inherited;
end;


constructor TVTEdit.Create(Link: TStringEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMAutoAdjust(var Message: TLMessage);

begin
  AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMExit(var Message: TLMessage);

begin
  if Assigned(FLink) and not FLink.FStopping then
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        DoEndEdit
      else
        DoCancelEdit;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMRelease(var Message: TLMessage);

begin
  Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CNCommand(var Message: TLMCommand);

begin
{todo  if Assigned(FLink) and Assigned(FLink.FTree) and (Message.NotifyCode = EN_UPDATE) and
    not (toGridExtensions in FLink.FTree.FOptions.FMiscOptions) and
    not (vsMultiline in FLink.FNode.States) then
    // Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
    // and eventual resizing. Hence we use a message to accomplish that.
    if false and IsWinNT then
      AutoAdjustSize
    else
      PostMessage(Handle, CM_AUTOADJUST, 0, 0);}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMChar(var Message: TLMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMDestroy(var Message: TLMDestroy);

begin
  // If editing stopped by other means than accept or cancel then we have to do default processing for
  // pending changes.
  if Assigned(FLink) and not FLink.FStopping then
  begin
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FNode, FColumn] := FEdit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMGetDlgCode(var Message: TLMNoParams {TWMGetDlgCode});

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMKeyDown(var Message: TLMKeyDown);

// Handles some control keys.

var
  Shift: TShiftState;
  EndEdit: Boolean;
  Tree: TBaseVirtualTree;

begin
  case Message.CharCode of
    // Pretend these keycodes were send to the tree.
    VK_ESCAPE:
      begin
        Tree := FLink.FTree;
        FLink.FTree.DoCancelEdit;
        Tree.SetFocus;
      end;
    VK_RETURN:
      begin
        EndEdit := not (vsMultiline in FLink.FNode^.States);
        if not EndEdit then
        begin
          // If a multiline node is being edited the finish editing only if Ctrl+Enter was pressed,
          // otherwise allow to insert line breaks into the text.
          Shift := KeyDataToShiftState(Message.KeyData);
          EndEdit := ssCtrl in Shift;
        end;
        if EndEdit then
        begin
          Tree := FLink.FTree;
          FLink.FTree.InvalidateNode(FLink.FNode);
          FLink.FTree.DoEndEdit;
          Tree.SetFocus;
        end;
      end;
    VK_UP:
      begin
        if not (vsMultiline in FLink.FNode^.States) then
          Message.CharCode := VK_LEFT;
        inherited;
      end;
    VK_DOWN:
      begin
        if not (vsMultiline in FLink.FNode^.States) then
          Message.CharCode := VK_RIGHT;
        inherited;
      end;
  else
    inherited;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.AutoAdjustSize;

// Changes the size of the edit to accomodate as much as possible of its text within its container window.
// NewChar describes the next character which will be added to the edit's text.

var
  DC: HDC;
  Size: TSize;
  LastFont: THandle;

begin
  if not (vsMultiline in FLink.FNode^.States) then
  begin
    // avoid flicker
//todowin    SendMessage(Handle, WM_SETREDRAW, 0, 0);

    DC := GetDC(Handle);
    LastFont := SelectObject(DC, Font.Handle);
    try
      // Read needed space for the current text.
      {$ifdef UNICODE}
        GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Size);
      {$else}
        GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
      {$endif}
      Inc(Size.cx, 2 * FLink.FTree.FTextMargin);

      // Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.FTree.InvalidateNode(FLink.FNode);

      if FLink.FAlignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Height))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Height));
    finally
      SelectObject(DC, LastFont);
      ReleaseDC(Handle, DC);
//todowin      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CreateParams(var Params: TCreateParams);

begin
  inherited;

  // Only with multiline style we can use the text formatting rectangle.
  // This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    Style := Style or 4 {todoES_MULTILINE};
    if vsMultiline in FLink.FNode^.States then
      Style := Style and not ({todoES_AUTOHSCROLL or} WS_HSCROLL) or WS_VSCROLL {todoor ES_AUTOVSCROLL};
    if tsUseThemes in FLink.FTree.FStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.Release;

begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TStringEditLink ------------------------------------------------------------------------------------

constructor TStringEditLink.Create;

begin
  inherited;
  FEdit := TVTEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TStringEditLink.Destroy;

begin
  FEdit.Release;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.BeginEdit: Boolean; stdcall;

// Notifies the edit link that editing can start now. Descentants may cancel node edit
// by returning False.

begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    FEdit.SelectAll;
    FEdit.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.SetEdit(const Value: TVTEdit);

begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.CancelEdit: Boolean; stdcall;

begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FEdit.Hide;
    FTree.CancelEditNode;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.EndEdit: Boolean; stdcall;

begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    if FEdit.Modified then
      FTree.Text[FNode, FColumn] := FEdit.Text;
    FEdit.Hide;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  except
    FStopping := False;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.GetBounds: TRect; stdcall;

begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;

// Retrieves the true text bounds from the owner tree.

var
  Text: WideString;

begin
  Result := Tree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TCustomVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, Text);
    FEdit.Font.Color := clBlack;
    FEdit.Parent := Tree;
    RecreateWnd(FEdit);
    FEdit.HandleNeeded;
    FEdit.Text := Text;

    if Column <= NoColumn then
    begin
//b      FEdit.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
//b      FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

//b    if FEdit.BidiMode <> bdLeftToRight then
//b      ChangeBidiModeAlignment(FAlignment);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.ProcessMessage(var Message: TLMessage); stdcall;

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.SetBounds(R: TRect); stdcall;

// Sets the outer bounds of the edit control and the actual edit area in the control.

var
  Offset: Integer;

begin
  if not FStopping then
  begin
    with R do
    begin
      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FEdit.BoundsRect := R;

      // The selected text shall exclude the text margins and be centered vertically.
      // We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
      // control leaves around the (selected) text.
      R := FEdit.ClientRect;
      Offset := 2;
      if tsUseThemes in FTree.FStates then
        Inc(Offset);
      InflateRect(R, -FTree.FTextMargin + Offset, Offset);
      if not (vsMultiline in FNode^.States) then
        OffsetRect(R, 0, FTextBounds.Top - FEdit.Top);

//todowin      SendMessage(FEdit.Handle, EM_SETRECTNP, 0, Integer(@R));
    end;
  end;
end;

//----------------- TCustomVirtualString -------------------------------------------------------------------------------

constructor TCustomVirtualStringTree.Create(AOwner: TComponent);

begin
  inherited;
  FDefaultText := 'Node';
  FInternalDataOffset := AllocateInternalDataArea(SizeOf(Cardinal));
end;

destructor TCustomVirtualStringTree.Destroy;
begin
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.GetRenderStartValues(Source: TVSTTextSourceType; var Node: PVirtualNode;
  var NextNodeProc: TGetNextNodeProc);

begin
  case Source of
    tstInitialized:
      begin
        Node := GetFirstInitialized;
        NextNodeProc := @GetNextInitialized;
      end;
    tstSelected:
      begin
        Node := GetFirstSelected;
        NextNodeProc := @GetNextSelected;
      end;
    tstCutCopySet:
      begin
        Node := GetFirstCutCopy;
        NextNodeProc := @GetNextCutCopy;
      end;
    tstVisible:
      begin
        Node := GetFirstVisible;
        NextNodeProc := @GetNextVisible;
      end;
  else // tstAll
    Node := GetFirst;
    NextNodeProc := @GetNext;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetOptions: TStringTreeOptions;

begin
  Result := FOptions as TStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetText(Node: PVirtualNode; Column: TColumnIndex): WideString;

begin
  Assert(Assigned(Node), 'Node must not be nil.');

  if not (vsInitialized in Node^.States) then
    InitNode(Node);
  Result := FDefaultText;

  DoGetText(Node, Column, ttNormal, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.InitializeTextProperties(var PaintInfo: TVTPaintInfo);

// Initializes default values for customization in PaintNormalText.

begin
  with PaintInfo do
  begin
    // Set default font values first.
    Canvas.Font := Font;

{TODO    if (toHotTrack in FOptions.PaintOptions) and (Node = FCurrentHotNode) then
    begin
      Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
      Canvas.Font.Color := FColors.HotColor;
    end;}

    // Change the font color only if the node also is drawn in selected style.
    if poDrawSelection in PaintOptions then
    begin
      if (Column = FocusedColumn) or (toFullRowSelect in FOptions.SelectionOptions) then
      begin
          if vsSelected in Node^.States then
          begin
            if Focused or (toPopupMode in FOptions.PaintOptions) then
              Canvas.Font.Color := clHighlightText;
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer;
  xText: WideString);

// This method is responsible for painting the given test to target canvas (under consideration of the given rectangles).
// The text drawn here is considered as the normal text in a node.
// Note: NodeWidth is the actual width of the text to be drawn. This does not necessarily correspond to the width of
//       the node rectangle. The clipping rectangle comprises the entire node (including tree lines, buttons etc.).

var
  TripleWidth: Integer;
  R: TRect;
  DrawFormat: Cardinal;
  Size: TSize;

begin
  InitializeTextProperties(PaintInfo);
  with PaintInfo do
  begin
    R := ContentRect;
//todo    Canvas.TextFlags := 0;

    // Multiline nodes don't need special font handling or text manipulation.
    // Note: multiline support requires the Unicode version of DrawText, which is able to do word breaking.
    //       The emulation in this unit does not support this so we have to use the OS version. However
    //       DrawTextW is only available on NT/2000/XP and up. Hence there is only partial multiline support
    //       for 9x/Me.
    if vsMultiline in Node^.States then
    begin
      InflateRect(R, -FTextMargin, 0);
      DoPaintText(Node, Canvas, Column, ttNormal);
      // Disabled node color overrides all other variants.
      if (vsDisabled in Node^.States) or not Enabled then
        Canvas.Font.Color := FColors.DisabledColor;

      // The edit control flag will ensure that no partial line is displayed, that is, only lines
      // which are (vertically) fully visible are drawn.
      DrawFormat := DT_NOPREFIX or DT_WORDBREAK {todoor DT_END_ELLIPSIS} or DT_EDITCONTROL or AlignmentToDrawFlag[Alignment];
//b      if BidiMode <> bdLeftToRight then
//b        DrawFormat := DrawFormat or DT_RTLREADING;
    end
    else
    begin
      InflateRect(R, -FTextMargin, 0);
      FFontChanged := False;
      TripleWidth := FEllipsisWidth;
      DoPaintText(Node, Canvas, Column, ttNormal);
      if FFontChanged then
      begin
        // If the font has been changed then the ellipsis width must be recalculated.
        TripleWidth := 0;
        // Recalculate also the width of the normal text.
        GetTextExtentPoint32W(Canvas.Handle, PWideChar(xText), Length(xText), Size);
        NodeWidth := Size.cx + 2 * FTextMargin;
      end;

      // Disabled node color overrides all other variants.
      if (vsDisabled in Node^.States) or not Enabled then
        Canvas.Font.Color := FColors.DisabledColor;

      DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
//b      if BidiMode <> bdLeftToRight then
//b        DrawFormat := DrawFormat or DT_RTLREADING;
      // Check if the text must be shortend.
      if (Column > -1) and ((NodeWidth - 2 * FTextMargin) > R.Right - R.Left) then
      begin
        xText := DoShortenString(Canvas, Node, Column, xText, R.Right - R.Left, False{bBidiMode <> bdLeftToRight}, TripleWidth);
        if Alignment = taRightJustify then
          DrawFormat := DrawFormat or DT_RIGHT
        else
          DrawFormat := DrawFormat or DT_LEFT;
      end
      else
        DrawFormat := DrawFormat or AlignmentToDrawFlag[Alignment];
    end;

    if not Canvas.TextStyle.Opaque then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);
    DrawTextW(Canvas, PWideChar(xText), R, DrawFormat, False);   //theo
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.PaintStaticText(const PaintInfo: TVTPaintInfo; TextOutFlags: Integer;
  const xText: WideString);

// This method retrives and draws the static text bound to a particular node.

var
  R: TRect;
  DrawFormat: Cardinal;

begin
  with PaintInfo do
  begin
    Canvas.Font := Font;
    if toFullRowSelect in FOptions.SelectionOptions then
    begin
        if vsSelected in Node^.States then
        begin
          if Focused or (toPopupMode in FOptions.PaintOptions) then
            Canvas.Font.Color := clHighlightText
          else
            Canvas.Font.Color := Font.Color;
        end;
    end;

    DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
//todo    Canvas.TextFlags := 0;
    DoPaintText(Node, Canvas, Column, ttStatic);

    // Disabled node color overrides all other variants.
    if (vsDisabled in Node^.States) or not Enabled then
      Canvas.Font.Color := FColors.DisabledColor;

    R := ContentRect;
    if Alignment = taRightJustify then
      Dec(R.Right, NodeWidth + FTextMargin)
    else
      Inc(R.Left, NodeWidth + FTextMargin);

    if not Canvas.TextStyle.Opaque then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);
    DrawTextW(Canvas, PWideChar(xText),R, DrawFormat, False); //theo
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ReadText(Reader: TReader);

begin
  case Reader.NextValue of
    vaLString, vaString:
      SetDefaultText(Reader.ReadString);
  else
    SetDefaultText(Reader.ReadWideString);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetDefaultText(const Value: WideString);

begin
  if FDefaultText <> Value then
  begin
    FDefaultText := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetOptions(const Value: TStringTreeOptions);

begin
  FOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.SetText(Node: PVirtualNode; Column: TColumnIndex; const Value: WideString);

begin
  DoNewText(Node, Column, Value);
  InvalidateNode(Node);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.WriteText(Writer: TWriter);

begin
  Writer.WriteWideString(FDefaultText);
end;

//----------------------------------------------------------------------------------------------------------------------

{procedure TCustomVirtualStringTree.WMSetFont(var Msg: TWMSetFont);

// Whenever a new font is applied to the tree some default values are determined to avoid frequent
// determination of the same value.

var
  MemDC: HDC;
  Run: PVirtualNode;
  TM: TTextMetric;
  Size: TSize;

begin
  inherited;

  MemDC := CreateCompatibleDC(0);
  try
    SelectObject(MemDC, Msg.Font);
    GetTextMetrics(MemDC, TM);
    FTextHeight := TM.tmHeight;

    GetTextExtentPoint32W(MemDC, '...', 3, Size);
    FEllipsisWidth := Size.cx;
  finally
    DeleteDC(MemDC);
  end;

  // Have to reset all node widths.
  Run := FRoot.FirstChild;
  while Assigned(Run) do
  begin
    PInteger(InternalData(Run))^ := 0;
    Run := GetNextNoInit(Run);
  end;
end;}

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.AdjustPaintCellRect(var PaintInfo: TVTPaintInfo; var NextNonEmpty: TColumnIndex);

// In the case a node spans several columns (if enabled) we need to determine how many columns.
// Note: the autospan feature can only be used with left-to-right layout.

begin
  if (toAutoSpanColumns in FOptions.AutoOptions) and Header.UseColumns and True{b(PaintInfo.BidiMode = bdLeftToRight)} then
    with Header.Columns, PaintInfo do
    begin
      // Start with the directly following column.
      NextNonEmpty := GetNextVisibleColumn(Column);

      // Auto spanning columns can only be used for left-to-right directionality because the tree is drawn
      // from left to right. For RTL directionality it would be necessary to draw it from right to left.
      // While this could be managed, it becomes impossible when directionality is mixed.
      repeat
        if (NextNonEmpty = InvalidColumn) or not ColumnIsEmpty(Node, NextNonEmpty) or
          False{b(Items[NextNonEmpty].BidiMode <> bdLeftToRight)} then
          Break;
        Inc(CellRect.Right, Items[NextNonEmpty].Width);
        NextNonEmpty := GetNextVisibleColumn(NextNonEmpty);
      until False;
    end
    else
      inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.CalculateTextWidth(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  xText: WideString): Integer;

// determines the width of the given text

var
  Size: TSize;

begin
  Result := 2 * FTextMargin;
  if Length(xText) > 0 then
  begin
    Canvas.Font := Font;
    DoPaintText(Node, xCanvas, Column, ttNormal);

    GetTextExtentPoint32W(xCanvas.Handle, PWideChar(xText), Length(xText), Size);
    Inc(Result, Size.cx);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;

// For hit tests it is necessary to consider cases where columns are empty and automatic column spanning is enabled.
// This method simply checks the given column's text and if this is empty then the column is considered as being empty.

begin
  Result := Length(Text[Node, Column]) = 0;
  // If there is no text then let the ancestor decide if the column is to be considered as being empty
  // (e.g. by asking the application). If there is text then the column is never be considered as being empty.
  if Result then
    Result := inherited ColumnIsEmpty(Node, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DefineProperties(Filer: TFiler);

begin
  inherited;

  // Delphi still cannot handle wide strings properly while streaming
  Filer.DefineProperty('WideDefaultText', @ReadText, @WriteText, FDefaultText <> 'Node');
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;

begin
  Result := inherited DoCreateEditor(Node, Column);
  // Enable generic label editing support if the application does not have own editors.
  if Result = nil then
    Result := TStringEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex): WideString;

begin
  Result := inherited DoGetNodeHint(Node, Column);
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, Node, Column, ttNormal, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex): WideString;

begin
  Result := Text[Node, Column];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; xCanvas: TCanvas = nil): Integer;

// Returns the text width of the given node in pixels.
// This width is stored in the node's data member to increase access speed.

var
  Data: PInteger;

begin
  if (Column > NoColumn) and (vsMultiline in Node^.States) then
    Result := Header.Columns[Column].Width
  else
  begin
    if xCanvas = nil then
      xCanvas := Self.Canvas;

    if Column = Header.MainColumn then
    begin
      // primary column or no columns
      Data := InternalData(Node);
      Result := Data^;
      if Result = 0 then
      begin
        Data^ := CalculateTextWidth(xCanvas, Node, Column, Text[Node, Column]);
        Result := Data^;
      end;
    end
    else
      // any other column
      Result := CalculateTextWidth(xCanvas, Node, Column, Text[Node, Column]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var xText: WideString);

begin
  if Assigned(FOnGetText) then
    FOnGetText(Self, Node, Column, TextType, xText);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoIncrementalSearch(Node: PVirtualNode; const xText: WideString): Integer;

// Since the string tree has access to node text it can do incremental search on its own. Use the event to
// override the default behavior.

begin
  Result := 0;
  if Assigned(FOnIncrementalSearch) then
    FOnIncrementalSearch(Self, Node, xText, Result)
  else
    // Default behavior is to match the search string with the start of the node text.
    if Pos(xText, GetText(Node, FocusedColumn)) <> 1 then
      Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; xText: WideString);

begin
  if Assigned(FOnNewText) then
    FOnNewText(Self, Node, Column, xText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoPaintNode(var PaintInfo: TVTPaintInfo);

// Main output routine to print the text of the given node using the space provided in PaintInfo.ContentRect.

var
  S: WideString;
  TextOutFlags: Integer;

begin
  // Set a new OnChange event for the canvas' font so we know if the application changes it in the callbacks.
  // This long winded procedure is necessary because font changes (as well as brush and pen changes) are
  // unfortunately not announced via the Canvas.OnChange event.
  RedirectFontChangeEvent(PaintInfo.Canvas);

  // Determine main text direction as well as other text properties.
  TextOutFlags := ETO_CLIPPED {bor RTLFlag[PaintInfo.BidiMode <> bdLeftToRight]};
  S := Text[PaintInfo.Node, PaintInfo.Column];

  // Paint the normal text first...
  if Length(S) > 0 then
    PaintNormalText(PaintInfo, TextOutFlags, S);

  // ... and afterwards the static text if not centered and the node is not multiline enabled.
  if (Alignment <> taCenter) and not (vsMultiline in PaintInfo.Node^.States) and (toShowStaticText in TreeOptions.FStringOptions) then
  begin
    S := '';
    with PaintInfo do
      DoGetText(Node, Column, ttStatic, S);
    if Length(S) > 0 then
      PaintStaticText(PaintInfo, TextOutFlags, S);
  end;
  RestoreFontChangeEvent(PaintInfo.Canvas);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.DoPaintText(Node: PVirtualNode; const xCanvas: TCanvas; Column: TColumnIndex;
  TextType: TVSTTextType);

begin
  if Assigned(FOnPaintText) then
    FOnPaintText(Self, xCanvas, Node, Column, TextType);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.DoShortenString(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const S: WideString; xWidth: Integer; RightToLeft: Boolean; EllipsisWidth: Integer = 0): WideString;

var
  Done: Boolean;

begin
  Done := False;
  if Assigned(FOnShortenString) then
    FOnShortenString(Self, xCanvas, Node, Column, S, xWidth, RightToLeft, Result, Done);
  if not Done then
    Result := ShortenString(xCanvas.Handle, S, xWidth, RightToLeft, EllipsisWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TCustomStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.GetTextInfo(Node: PVirtualNode; Column: TColumnIndex; const AFont: TFont; var R: TRect;
  var xText: WideString);

// Returns the font, the text and its bounding rectangle to the caller. R is returned as the closest
// bounding rectangle around Text.

var
  NewHeight: Integer;
  TM: TTextMetric;

begin
  // Get default font and initialize the other parameters.
  inherited GetTextInfo(Node, Column, AFont, R, xText);

  Canvas.Font := AFont;

  FFontChanged := False;
  RedirectFontChangeEvent(Canvas);
  DoPaintText(Node, Canvas, Column, ttNormal);
  if FFontChanged then
  begin
    AFont.Assign(Canvas.Font);
    GetTextMetrics(Canvas.Handle, TM);
    NewHeight := TM.tmHeight;
  end
  else // Otherwise the correct font is already there and we only need to set the correct height.
    NewHeight := FTextHeight;
  RestoreFontChangeEvent(Canvas);

  // Alignment to the actual text.
  xText := Self.Text[Node, Column];
  R := GetDisplayRect(Node, Column, True, not (vsMultiline in Node^.States));
  if toShowHorzGridLines in TreeOptions.PaintOptions then
    Dec(R.Bottom);
  InflateRect(R, 0, -(R.Bottom - R.Top - NewHeight) div 2);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.InternalData(Node: PVirtualNode): Pointer;

begin
  if (Node = RootNode) or (Node = nil) then
    Result := nil
  else
    Result := PChar(Node) + FInternalDataOffset;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.MainColumnChanged;

var
  Run: PVirtualNode;

begin
  inherited;

  // Have to reset all node widths.
  Run := RootNode^.FirstChild;
  while Assigned(Run) do
  begin
    PInteger(InternalData(Run))^ := 0;
    Run := GetNextNoInit(Run);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ReadChunk(Stream: TStream; Version: Integer; Node: PVirtualNode; ChunkType,
  ChunkSize: Integer): Boolean;

// read in the caption chunk if there is one

var
  NewText: WideString;

begin
  case ChunkType of
    CaptionChunk:
      begin
        NewText := '';
        if ChunkSize > 0 then
        begin
          SetLength(NewText, ChunkSize div 2);
          Stream.Read(PWideChar(NewText)^, ChunkSize);
        end;
        // Do a new text event regardless of the caption content to allow removing the default string.
        Text[Node, Header.MainColumn] := NewText;
        Result := True;
      end;
  else
    Result := inherited ReadChunk(Stream, Version, Node, ChunkType, ChunkSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.WriteChunks(Stream: TStream; Node: PVirtualNode);

// Adds another sibling chunk for Node storing the label if the node is initialized.
// Note: If the application stores a node's caption in the node's data member (which will be quite common) and needs to
//       store more node specific data then it should use the OnSaveNode event rather than the caption autosave function
//       (take out soSaveCaption from StringOptions). Otherwise the caption is unnecessarily stored twice.

var
  xHeader: TChunkHeader;
  S: WideString;
  Len: Integer;

begin
  inherited;
  if (toSaveCaptions in TreeOptions.FStringOptions) and (Node <> RootNode) and
    (vsInitialized in Node^.States) then
    with Stream do
    begin
      // Read the node's caption (primary column only).
      S := Text[Node, Header.MainColumn];
      Len := 2 * Length(S);
      if Len > 0 then
      begin
        // Write a new sub chunk.
        xHeader.ChunkType := CaptionChunk;
        xHeader.ChunkSize := Len;
        Write(xHeader, SizeOf(xHeader));
        Write(PWideChar(S)^, Len);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ComputeNodeHeight(xCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex): Integer;

// Default node height calculation for multi line nodes. This method can be used by the application to delegate the
// quite expensive computation to the string tree.

var
  R: TRect;
  S: WideString;
  DrawFormat: Cardinal;
  xBidiMode: Classes.TBidiMode;
  xAlignment: TAlignment;
  PaintInfo: TVTPaintInfo;
  Dummy: TColumnIndex;

begin
  Result := Node^.NodeHeight;
  if vsMultiLine in Node^.States then
  begin
    S := Text[Node, Column];
    R := GetDisplayRect(Node, Column, True);
    DrawFormat := DT_TOP or DT_NOPREFIX or DT_CALCRECT or DT_WORDBREAK;
    if Column <= NoColumn then
    begin
      xBidiMode := Self.BidiMode;
      xAlignment := Self.Alignment;
    end
    else
    begin
      BidiMode := Header.Columns[Column].BidiMode;
      xAlignment := Header.Columns[Column].Alignment;
    end;

//    if xBidiMode <> bdLeftToRight then
//      ChangeBidiModeAlignment(Alignment);

    // Allow for autospanning.
    PaintInfo.Node := Node;
    PaintInfo.BidiMode := xBidiMode;
    PaintInfo.Column := Column;
    PaintInfo.CellRect := R;
    AdjustPaintCellRect(PaintInfo, Dummy);

    if xBidiMode <> bdLeftToRight then
      DrawFormat := DrawFormat or DT_RIGHT or DT_RTLREADING
    else
      DrawFormat := DrawFormat or DT_LEFT;
    DrawTextW(xCanvas, PWideChar(S), PaintInfo.CellRect, DrawFormat, False);  //theo
    Result := PaintInfo.CellRect.Bottom - PaintInfo.CellRect.Top;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToClipboard(Format: Word; Source: TVSTTextSourceType): HGLOBAL;

// This method constructs a shareable memory object filled with string data in the required format. Supported are:
// CF_TEXT - plain ANSI text (Unicode text is converted using the user's current locale)
// CF_UNICODETEXT - plain Unicode text
// CF_CSV - comma separated plain ANSI text
// CF_VRTF + CF_RTFNOOBS - rich text (plain ANSI)
// CF_HTML - HTML text encoded using UTF-8
//
// Result is the handle to a globally allocated memory block which can directly be used for clipboard and drag'n drop
// transfers. The caller is responsible for freeing the memory. If for some reason the content could not be rendered
// the Result is 0.

  //--------------- local function --------------------------------------------

  procedure MakeFragment(var HTML: string);

  // Helper routine to build a properly-formatted HTML fragment.

  const
    Version = 'Version:1.0'#13#10;
    StartHTML = 'StartHTML:';
    EndHTML = 'EndHTML:';
    StartFragment = 'StartFragment:';
    EndFragment = 'EndFragment:';
    DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">';
    HTMLIntro = '<html><head><META http-equiv=Content-Type content="text/html; charset=utf-8">' +
      '</head><body><!--StartFragment-->';
    HTMLExtro = '<!--EndFragment--></body></html>';
    NumberLengthAndCR = 10;

    // Let the compiler determine the description length.
    DescriptionLength = Length(Version) + Length(StartHTML) + Length(EndHTML) + Length(StartFragment) +
      Length(EndFragment) + 4 * NumberLengthAndCR;

  var
    Description: string;
    StartHTMLIndex,
    EndHTMLIndex,
    StartFragmentIndex,
    EndFragmentIndex: Integer;

  begin
    // The HTML clipboard format is defined by using byte positions in the entire block where HTML text and
    // fragments start and end. These positions are written in a description. Unfortunately the positions depend on the
    // length of the description but the description may change with varying positions.
    // To solve this dilemma the offsets are converted into fixed length strings which makes it possible to know
    // the description length in advance.
    StartHTMLIndex := DescriptionLength;              // position 0 after the description
    StartFragmentIndex := StartHTMLIndex + Length(DocType) + Length(HTMLIntro);
    EndFragmentIndex := StartFragmentIndex + Length(HTML);
    EndHTMLIndex := EndFragmentIndex + Length(HTMLExtro);

    Description := Version +
      SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10;
    HTML := Description + DocType + HTMLIntro + HTML + HTMLExtro;
  end;

  //--------------- end local function ----------------------------------------

var
  Data: Pointer;
  DataSize: Cardinal;
  S: string;
  WS: WideString;

begin
  Result := 0;
  case Format of
    CF_TEXT:
      begin
        S := ContentToText(Source, #9) + #0;
        Data := PChar(@S);
        DataSize := Length(S);
      end;
    CF_UNICODETEXT:
      begin
        WS := ContentToUnicode(Source, #9) + #0;
        Data := PWideChar(WS);
        DataSize := 2 * Length(WS);
      end;
  else
    if Format = CF_CSV then
      S := ContentToText(Source, ';'{todoListSeparator}) + #0
    else
      if (Format = CF_VRTF) or (Format = CF_VRTFNOOBJS) then
        S := ContentToRTF(Source) + #0
      else
        if Format = CF_HTML then
        begin
          S := ContentToHTML(Source);
          // Build a valid HTML clipboard fragment.
          MakeFragment(S);
          S := S + #0;
        end;
    Data := PChar(@S);
    DataSize := Length(S);
  end;

  if DataSize > 0 then
  begin
//x    Result := GlobalAlloc(GHND or GMEM_SHARE, DataSize);
//x    P := GlobalLock(Result);
//x    Move(Data^, P^, DataSize);
//x    GlobalUnlock(Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToHTML(Source: TVSTTextSourceType; xCaption: WideString = ''): string;

// Renders the current tree content (depending on Source) as HTML text encoded in UTF-8.
// If Caption is not empty then it is used to create and fill the header for the table built here.
// Based on ideas and code from Frank van den Bergh and Andreas H�stemeier.

type
  UCS2 = Word;
  UCS4 = Cardinal;

const
  MaximumUCS4: UCS4 = $7FFFFFFF;
  ReplacementCharacter: UCS4 = $0000FFFD;

var
  Buffer: TBufferedString;

  //--------------- local functions -------------------------------------------

  function ConvertSurrogate(S1, S2: UCS2): UCS4;

  // Converts a pair of high and low surrogate into the corresponding UCS4 character.

  const
    SurrogateOffset = ($D800 shl 10) + $DC00 - $10000;

  begin
    Result := Word(S1) shl 10 + Word(S2) - SurrogateOffset;
  end;

  //---------------------------------------------------------------------------

  function UTF16ToUTF8(const S: WideString): string;

  // Converts the given Unicode text (which may contain surrogates) into
  // the UTF-8 encoding used for the HTML clipboard format.

  const
    FirstByteMark: array[0..6] of Byte = ($00, $00, $C0, $E0, $F0, $F8, $FC);

  var
    Ch: UCS4;
    I, J, T: Integer;
    BytesToWrite: Cardinal;

  begin
    if Length(S) = 0 then
      Result := ''
    else
    begin
      // Make room for the result. Assume worst case, there are only short texts to convert.
      SetLength(Result, 6 * Length(S));
      T := 1;
      I := 1;
      while I <= Length(S) do
      begin
        Ch := UCS4(S[I]);

        // Is the character a surrogate?
        if (Ch and $FFFFF800) = $D800 then
        begin
          Inc(I);
          // Check the following char whether it forms a valid surrogate pair with the first character.
          if (I <= Length(S)) and ((UCS4(S[I]) and $FFFFFC00) = $DC00) then
            Ch := ConvertSurrogate(UCS2(Ch), UCS2(S[I]))
          else // Skip invalid surrogate value.
            Continue;
        end;

        if Ch < $80 then
          BytesToWrite := 1
        else
          if Ch < $800 then
            BytesToWrite := 2
          else
            if Ch < $10000 then
              BytesToWrite := 3
            else
              if Ch < $200000 then
                BytesToWrite := 4
              else
                if Ch < $4000000 then
                  BytesToWrite := 5
                else
                  if Ch <= MaximumUCS4 then
                    BytesToWrite := 6
                  else
                  begin
                    BytesToWrite := 2;
                    Ch := ReplacementCharacter;
                  end;

        for J := BytesToWrite downto 2 do
        begin
          Result[T + J - 1] := Char((Ch or $80) and $BF);
          Ch := Ch shr 6;
        end;
        Result[T] := Char(Ch or FirstByteMark[BytesToWrite]);
        Inc(T, BytesToWrite);

        Inc(I);
      end;
      SetLength(Result, T - 1); // set to actual length
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteColorAsHex(Color: TColor);

  var
    WinColor: COLORREF;
    I: Integer;
    Component,
    Value: Byte;

  begin
    Buffer.Add('#');
    WinColor := ColorToRGB(Color);
    I := 1;
    while I <= 6 do
    begin
      Component := WinColor and $FF;

      Value := 48 + (Component shr 4);
      if Value > $39 then
        Inc(Value, 7);
      Buffer.Add(Char(Value));
      Inc(I);

      Value := 48 + (Component and $F);
      if Value > $39 then
        Inc(Value, 7);
      Buffer.Add(Char(Value));
      Inc(I);

      WinColor := WinColor shr 8;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteStyle(Name: string; Font: TFont);

  // Creates a CSS style entry with the given name for the given font.
  // If Name is empty then the entry is created as inline style.

  begin
    if Length(Name) = 0 then
      Buffer.Add(' style="{font:')
    else
    begin
      Buffer.Add('.');
      Buffer.Add(Name);
      Buffer.Add('{font:');
    end;
    if fsUnderline in Font.Style then
      Buffer.Add(' underline');
    if fsItalic in Font.Style then
      Buffer.Add(' italic');
    if fsBold in Font.Style then
      Buffer.Add(' bold');
    Buffer.Add(Format(' %dpt "%s";', [Font.Size, Font.Name]));
    Buffer.Add('color:');
    WriteColorAsHex(Font.Color);
    Buffer.Add(';}');
    if Length(Name) = 0 then
      Buffer.Add('"');
  end;

  //--------------- end local functions ---------------------------------------

var
  I, J : Integer;
  Level, MaxLevel: Cardinal;
  AddHeader: string;
  Save, Run: PVirtualNode;
  GetNextNode: TGetNextNodeProc;
  xText: WideString;

  RenderColumns: Boolean;
  Columns: TColumnsArray;
  ColumnColors: array of string;
  Index: Integer;
  IndentWidth,
  LineStyleText: string;
  xAlignment: TAlignment;
//  BidiMode: TBidiMode;

  CellPadding: string;

begin
  GetNextNode := nil;
  Run := nil;
  Buffer := TBufferedString.Create;
  try
    // For customization by the application or descentants we use again the redirected font change event.
    RedirectFontChangeEvent(Canvas);

    CellPadding := Format('padding-left:%dpx;padding-right:%0:dpx;', [FMargin]);

    IndentWidth := IntToStr(FIndent);
    AddHeader := ' ';
    // Add title if adviced so by giving a caption.
    if Length(xCaption) > 0 then
      AddHeader := AddHeader + 'caption="' + UTF16ToUTF8(xCaption) + '"';
    if Borderstyle <> bsNone then
      AddHeader := AddHeader + Format('border="%d" frame=box', [BorderWidth + 1]);

    // Create HTML table based on the tree structure. To simplify formatting we use styles defined in a small CSS area.
    Buffer.Add('<style type="text/css">');
    Buffer.AddnewLine;
    WriteStyle('default', Font);
    Buffer.AddNewLine;
    WriteStyle('header', Header.Font);
    Buffer.AddNewLine;

    // Determine grid/table lines and create CSS for it.
    // Vertical and/or horizontal border to show.
    if LineStyle = lsSolid then
      LineStyleText := 'solid;'
    else
      LineStyleText := 'dotted;';
    if toShowHorzGridLines in FOptions.PaintOptions then
    begin
      Buffer.Add('.noborder{border-style:');
      Buffer.Add(LineStyleText);
      Buffer.Add(' border-bottom:1;border-left:0;border-right:0; border-top:0;');
      Buffer.Add(CellPadding);
      Buffer.Add('}');
    end
    else
    begin
      Buffer.Add('.noborder{border-style:none;');
      Buffer.Add(CellPadding);
      Buffer.Add('}');
    end;
    Buffer.AddNewLine;

    Buffer.Add('.normalborder {border-top:none; border-left:none; ');
    if toShowVertGridLines in FOptions.PaintOptions then
      Buffer.Add('border-right:1 ' + LineStyleText)
    else
      Buffer.Add('border-right:none;');
    if toShowHorzGridLines in FOptions.PaintOptions then
      Buffer.Add('border-bottom:1 ' + LineStyleText)
    else
      Buffer.Add('border-bottom:none;');
    Buffer.Add(CellPadding);
    Buffer.Add('}');
    Buffer.Add('</style>');
    Buffer.AddNewLine;

    // General table properties.
    Buffer.Add('<table class="default" bgcolor=');
    WriteColorAsHex(Color);
    Buffer.Add(AddHeader);
    Buffer.Add(' cellspacing="0" cellpadding=');
    Buffer.Add(IntToStr(FMargin) + '>');
    Buffer.AddNewLine;

    Columns := nil;
    ColumnColors := nil;
    RenderColumns := Header.UseColumns;
    if RenderColumns then
    begin
      Columns := Header.Columns.GetVisibleColumns;
      SetLength(ColumnColors, Length(Columns));
    end;

    GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    MaxLevel := 0;
    // The table consists of visible columns and rows as used in the tree, but the main tree column is splitted
    // into several HTML columns to accomodate the indentation.
    while Assigned(Run) do
    begin
      Level := GetNodeLevel(Run);
      If Level > MaxLevel then
        MaxLevel := Level;
      Run := GetNextNode(Run);
    end;

    if RenderColumns then
    begin
      Buffer.Add('<tr class="header" style="');
      Buffer.Add(CellPadding);
      Buffer.Add('">');
      Buffer.AddNewLine;
      // Make the first row in the HTML table an image of the tree header.
      for I := 0 to High(Columns) do
      begin
        Buffer.Add('<th height="');
        Buffer.Add(IntToStr(Header.Height));
        Buffer.Add('px"');
        xAlignment := Columns[I].Alignment;
        // Consider directionality.
//b        if Columns[I].FBiDiMode <> bdLeftToRight then
//b        begin
//b          ChangeBidiModeAlignment(xAlignment);
//b          Buffer.Add(' dir="rtl"');
//b        end;

          // Consider aligment.
        case xAlignment of
          taRightJustify:
            Buffer.Add(' align=right');
          taCenter:
            Buffer.Add(' align=center');
        else
          Buffer.Add(' align=left');
        end;

        Index := Columns[I].Index;
        // Merge cells of the header emulation in the main column.
        if (MaxLevel > 0) and (Index = Header.MainColumn) then
        begin
          Buffer.Add(' colspan="');
          Buffer.Add(IntToStr(MaxLevel + 1));
          Buffer.Add('"');
        end;

        // The color of the header is usually clBtnFace.
        Buffer.Add(' bgcolor=');
        WriteColorAsHex(clBtnFace);

        // Set column width in pixels.
        Buffer.Add(' width="');
        Buffer.Add(IntToStr(Columns[I].Width));
        Buffer.Add('px">');

        if Length(Columns[I].Text) > 0 then
          Buffer.Add(UTF16ToUTF8(Columns[I].Text));
        Buffer.Add('</th>');
      end;
      Buffer.Add('</tr>');
      Buffer.AddNewLine;
    end;

    // Now go through the tree.
    Run := Save;
    while Assigned(Run) do
    begin
      Level := GetNodeLevel(Run);
      Buffer.Add(' <tr class="default">');
      Buffer.AddNewLine;

      I := 0;
      while (I < Length(Columns)) or not RenderColumns do
      begin
        if RenderColumns then
          Index := Columns[I].Index
        else
          Index := NoColumn;

        if not RenderColumns or (coVisible in Columns[I].Options) then
        begin
          // Call back the application to know about font customization.
          Canvas.Font := Font;
          FFontChanged := False;
          DoPaintText(Run, Canvas, Index, ttNormal);

          if Index = Header.MainColumn then
          begin
            // Create a cell for each indentation level.
            if RenderColumns and not (coParentColor in Columns[I].Options) then
            begin
              for J := 1 to Level do
              begin
                Buffer.Add('<td class="noborder" width="');
                Buffer.Add(IndentWidth);
                Buffer.Add('" height="');
                Buffer.Add(IntToStr(NodeHeight[Run]));
                Buffer.Add('px"');
                if not (coParentColor in Columns[I].Options) then
                begin
                  Buffer.Add(' bgcolor=');
                  WriteColorAsHex(Columns[I].Color);
                end;
                Buffer.Add('>&nbsp;</td>');
              end;
            end
            else
            begin
              for J := 1 to Level do
                if J = 1 then
                begin
                  Buffer.Add(' <td height="');
                  Buffer.Add(IntToStr(NodeHeight[Run]));
                  Buffer.Add('px">&nbsp;</td>');
                end
                else
                  Buffer.Add(' <td>&nbsp;</td>');
            end;
          end;

          if FFontChanged then
          begin
            Buffer.Add(' <td class="normalborder" ');
            WriteStyle('', Canvas.Font);
            Buffer.Add(' height="');
            Buffer.Add(IntToStr(NodeHeight[Run]));
            Buffer.Add('px"');
          end
          else
          begin
            Buffer.Add(' <td class="normalborder"  height="');
            Buffer.Add(IntToStr(NodeHeight[Run]));
            Buffer.Add('px"');
          end;

          if RenderColumns then
          begin
            xAlignment := Columns[I].Alignment;
//b            BidiMode := Columns[I].BidiMode;
          end
          else
          begin
            xAlignment := Self.Alignment;
//b            BidiMode := Self.BidiMode;
          end;
          // Consider directionality.
//b          if BiDiMode <> bdLeftToRight then
//b          begin
//b            ChangeBidiModeAlignment(xAlignment);
//b            Buffer.Add(' dir="rtl"');
//b          end;

          // Consider aligment.
          case xAlignment of
            taRightJustify:
              Buffer.Add(' align=right');
            taCenter:
              Buffer.Add(' align=center');
          else
            Buffer.Add(' align=left');
          end;
          // Merge cells in the main column.
          if (MaxLevel > 0) and (Index = Header.MainColumn) and (Level < MaxLevel) then
          begin
            Buffer.Add(' colspan="');
            Buffer.Add(IntToStr(MaxLevel - Level + 1));
            Buffer.Add('"');
          end;
          if RenderColumns and not (coParentColor in Columns[I].Options) then
          begin
            Buffer.Add(' bgcolor=');
            WriteColorAsHex(Columns[I].Color);
          end;
          Buffer.Add('>');
          xText := Self.Text[Run, Index];
          if Length(xText) > 0 then
          begin
            xText := UTF16ToUTF8(xText);
            Buffer.Add(xText);
          end;
          Buffer.Add('</td>');
        end;

        if not RenderColumns then
          Break;
        Inc(I);
      end;
      Run := GetNextNode(Run);
      Buffer.Add(' </tr>');
      Buffer.AddNewLine;
    end;
    Buffer.Add('</table>');

    RestoreFontChangeEvent(Canvas);

    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToRTF(Source: TVSTTextSourceType): string;

// Renders the current tree content (depending on Source) as RTF (rich text).
// Based on ideas and code from Frank van den Bergh and Andreas H�stemeier.

var
  Fonts: TStringList;
  xColors: TList;
  CurrentFontIndex,
  CurrentFontColor,
  CurrentFontSize: Integer;
  Buffer: TBufferedString;

  //--------------- local functions -------------------------------------------

  procedure SelectFont(Font: string);

  var
    I: Integer;

  begin
    I := Fonts.IndexOf(Font);
    if I > -1 then
    begin
      // Font has already been used
      if I <> CurrentFontIndex then
      begin
        Buffer.Add('\f');
        Buffer.Add(IntToStr(I));
        CurrentFontIndex := I;
      end;
    end
    else
    begin
      I := Fonts.Add(Font);
      Buffer.Add('\f');
      Buffer.Add(IntToStr(I));
      CurrentFontIndex := I;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure SelectColor(Color: TColor);

  var
    I: Integer;

  begin
    I := xColors.IndexOf(Pointer(@Color));
    if I > -1 then
    begin
      // Color has already been used
      if I <> CurrentFontColor then
      begin
        Buffer.Add('\cf');
        Buffer.Add(IntToStr(I + 1));
        CurrentFontColor := I;
      end;
    end
    else
    begin
      I := xColors.Add(Pointer(@Color));
      Buffer.Add('\cf');
      Buffer.Add(IntToStr(I + 1));
      CurrentFontColor := I;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure TextPlusFont(Text: WideString; Font: TFont);

  var
    UseUnderline,
    UseItalic,
    UseBold: Boolean;
    I: Integer;

  begin
    if Length(Text) > 0 then
    begin
      UseUnderline := fsUnderline in Font.Style;
      if UseUnderline then
        Buffer.Add('\ul');
      UseItalic := fsItalic in Font.Style;
      if UseItalic then
        Buffer.Add('\i');
      UseBold := fsBold in Font.Style;
      if UseBold then
        Buffer.Add('\b');
      SelectFont(Font.Name);
      SelectColor(Font.Color);
      if Font.Size <> CurrentFontSize then
      begin
        // Font size must be given in half points.
        Buffer.Add('\fs');
        Buffer.Add(IntToStr(2 * Font.Size));
        CurrentFontSize := Font.Size;
      end;
      // Use escape sequences to note Unicode text.
      Buffer.Add(' ');
      // Note: Unicode values > 32767 must be expressed as negative numbers. This is implicitly done
      //       by interpreting the wide chars (word values) as small integers.
      for I := 1 to Length(Text) do
        Buffer.Add(Format('\u%d\''3f', [SmallInt(Text[I])]));
      if UseUnderline then
        Buffer.Add('\ul0');
      if UseItalic then
        Buffer.Add('\i0');
      if UseBold then
        Buffer.Add('\b0');
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Level, LastLevel: Integer;
  I, J: Integer;
  Save, Run: PVirtualNode;
  GetNextNode: TGetNextNodeProc;
  S, Tabs : string;
  xText: WideString;
  Twips: Integer;

  RenderColumns: Boolean;
  Columns: TColumnsArray;
  Index: Integer;
  xAlignment: TAlignment;
//  BidiMode: TBidiMode;

begin
  Run := nil;
  GetNextNode := nil;
  Buffer := TBufferedString.Create;
  try
    // For customization by the application or descentants we use again the redirected font change event.
    RedirectFontChangeEvent(Canvas);

    Fonts := TStringList.Create;
    xColors := TList.Create;
    CurrentFontIndex := -1;
    CurrentFontColor := -1;
    CurrentFontSize := -1;

    Columns := nil;
    Tabs := '';
    LastLevel := 0;

    RenderColumns := Header.UseColumns;
    if RenderColumns then
      Columns := Header.Columns.GetVisibleColumns;

    GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    // First make a table structure. The \rtf and other header stuff is included
    // when the font and color tables are created.
    Buffer.Add('\uc1\trowd\trgaph70');
    J := 0;
    if RenderColumns then
    begin
      for I := 0 to High(Columns) do
      begin
        Inc(J, Columns[I].Width);
        // This value must be expressed in twips (1 inch = 1440 twips).
        Twips := Round(1440 * J / Screen.PixelsPerInch);
        Buffer.Add('\cellx');
        Buffer.Add(IntToStr(Twips));
      end;
    end
    else
    begin
      Twips := Round(1440 * ClientWidth / Screen.PixelsPerInch);
      Buffer.Add('\cellx');
      Buffer.Add(IntToStr(Twips));
    end;

    // Fill table header.
    if RenderColumns then
    begin
      Buffer.Add('\pard\intbl');
      for I := 0 to High(Columns) do
      begin
        xAlignment := Columns[I].Alignment;
//b        BidiMode := Columns[I].BidiMode;

        // Alignment is not supported with older RTF formats, however it will be ignored.
//b        if BidiMode <> bdLeftToRight then
//b          ChangeBidiModeAlignment(xAlignment);
        case xAlignment of
          taRightJustify:
            Buffer.Add('\qr');
          taCenter:
            Buffer.Add('\qc');
        end;

        TextPlusFont(Columns[I].Text, Header.Font);
        Buffer.Add('\cell');
      end;
      Buffer.Add('\row');
    end;

    // Now write the contents.
    Run := Save;
    while Assigned(Run) do
    begin
      I := 0;
      while not RenderColumns or (I < Length(Columns)) do
      begin
        if RenderColumns then
        begin
          Index := Columns[I].Index;
          xAlignment := Columns[I].Alignment;
//b          BidiMode := Columns[I].BidiMode;
        end
        else
        begin
          Index := NoColumn;
          xAlignment := Alignment;
//b          BidiMode := Self.BidiMode;
        end;

        if not RenderColumns or (coVisible in Columns[I].Options) then
        begin
          xText := Self.Text[Run, Index];
          Buffer.Add('\pard\intbl');

          // Alignment is not supported with older RTF formats, however it will be ignored.
//b          if BidiMode <> bdLeftToRight then
//b            ChangeBidiModeAlignment(xAlignment);
          case xAlignment of
            taRightJustify:
              Buffer.Add('\qr');
            taCenter:
              Buffer.Add('\qc');
          end;

          // Call back the application to know about font customization.
          Canvas.Font := Font;
          FFontChanged := False;
          DoPaintText(Run, Canvas, Index, ttNormal);

          if Index = Header.MainColumn then
          begin
            Level := GetNodeLevel(Run);
            if Level <> LastLevel then
            begin
              LastLevel := Level;
              Tabs := '';
              for J := 0 to Level - 1 do
                Tabs := Tabs + '\tab';
            end;
            if Level > 0 then
            begin
              Buffer.Add(Tabs);
              Buffer.Add(' ');
              TextPlusFont(xText, Canvas.Font);
              Buffer.Add('\cell');
            end
            else
            begin
              TextPlusFont(xText, Canvas.Font);
              Buffer.Add('\cell');
            end;
          end
          else
          begin
            TextPlusFont(xText, Canvas.Font);
            Buffer.Add('\cell');
          end;
        end;

        if not RenderColumns then
          Break;
        Inc(I);
      end;
      Buffer.Add('\row');
      Run := GetNextNode(Run);
    end;

    Buffer.Add('\pard\par');

    // Build lists with fonts and colors. They have to be at the start of the document.
    S := '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl';
    for I := 0 to Fonts.Count - 1 do
      S := S + Format('{\f%d %s;}', [I, Fonts[I]]);
    S := S + '}';

    S := S + '{\colortbl;';
    for I := 0 to xColors.Count - 1 do
    begin
      J := ColorToRGB(TColor(xColors[I]^));
      S := S + Format('\red%d\green%d\blue%d;', [J and $FF, (J shr 8) and $FF, (J shr 16) and $FF]);
    end;
    S := S + '}';

    Result := S + Buffer.AsString + '}';
    Fonts.Free;
    xColors.Free;

    RestoreFontChangeEvent(Canvas);
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToText(Source: TVSTTextSourceType; Separator: Char): string;

// Renders the current tree content (depending on Source) as plain ANSI text.
// If an entry contains the separator char or double quotes then it is wrapped with double quotes
// and existing double quotes are duplicated.
// Note: Unicode strings are implicitely converted to ANSI strings based on the currently active user locale.

var
  RenderColumns: Boolean;
  Tabs: string;
  GetNextNode: TGetNextNodeProc;
  Run, Save: PVirtualNode;
  Level, MaxLevel: Cardinal;
  Columns: TColumnsArray;
  LastColumn: TVirtualTreeColumn;
  Index,
  I: Integer;
  xText: string;
  Buffer: TBufferedString;

begin
  Columns := nil;
  Run := nil;
  GetNextNode := nil;
  Buffer := TBufferedString.Create;
  try
    RenderColumns := Header.UseColumns;
    if RenderColumns then
      Columns := Header.Columns.GetVisibleColumns;

    GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    // The text consists of visible groups representing the columns, which are separated by one or more separator
    // characters. There are always MaxLevel separator chars in a line (main column only). Either before the caption
    // to ident it or after the caption to make the following column aligned.
    MaxLevel := 0;
    while Assigned(Run) do
    begin
      Level := GetNodeLevel(Run);
      If Level > MaxLevel then
        MaxLevel := Level;
      Run := GetNextNode(Run);
    end;

    SetLength(Tabs, MaxLevel);
    FillChar(PChar(@Tabs)^, MaxLevel, Separator);

    // First line is always the header if used.
    if RenderColumns then
    begin
      LastColumn := Columns[High(Columns)];
      for I := 0 to High(Columns) do
      begin
        Buffer.Add(Columns[I].Text);
        if Columns[I] <> LastColumn then
        begin
          if Columns[I].Index = Header.MainColumn then
          begin
            Buffer.Add(Tabs);
            Buffer.Add(Separator);
          end
          else
            Buffer.Add(Separator);
        end;
      end;
      Buffer.AddNewLine;
    end
    else
      LastColumn := nil;

    Run := Save;
    if RenderColumns then
    begin
      while Assigned(Run) do
      begin
        for I := 0 to High(Columns) do
        begin
          if coVisible in Columns[I].Options then
          begin
            Index := Columns[I].Index;
            // This line implicitly converts the Unicode text to ANSI.
            xText := Self.Text[Run, Index];
            if Index = Header.MainColumn then
            begin
              Level := GetNodeLevel(Run);
              Buffer.Add(Copy(Tabs, 1, Level));
              // Wrap the text with quotation marks if it contains the separator character.
              if (Pos(Separator, xText) > 0) or (Pos('"', xText) > 0) then
                Buffer.Add(AnsiQuotedStr(xText, '"'))
              else
                Buffer.Add(xText);
              Buffer.Add(Copy(Tabs, 1, MaxLevel - Level));
            end
            else
              if (Pos(Separator, xText) > 0) or (Pos('"', xText) > 0) then
                Buffer.Add(AnsiQuotedStr(xText, '"'))
              else
                Buffer.Add(xText);

            if Columns[I] <> LastColumn then
              Buffer.Add(Separator);
          end;
        end;
        Run := GetNextNode(Run);
        Buffer.AddNewLine;
      end;
    end
    else
    begin
      while Assigned(Run) do
      begin
        // This line implicitly converts the Unicode text to ANSI.
        xText := Self.Text[Run, NoColumn];
        Level := GetNodeLevel(Run);
        Buffer.Add(Copy(Tabs, 1, Level));
        Buffer.Add(xText);
        Buffer.AddNewLine;

        Run := GetNextNode(Run);
      end;
    end;

    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.ContentToUnicode(Source: TVSTTextSourceType; Separator: WideChar): WideString;

// Renders the current tree content (depending on Source) as Unicode text.
// If an entry contains the separator char then it is wrapped with double quotation marks.
// Note: There is no QuotedStr function for Unicode in the VCL (like AnsiQuotedStr) so we have the limitation here
//       that an entry must not contain double quotation marks, otherwise import into other programs might fail!

var
  RenderColumns: Boolean;
  Tabs: WideString;
  GetNextNode: TGetNextNodeProc;
  Run, Save: PVirtualNode;

  Columns: TColumnsArray;
  LastColumn: TVirtualTreeColumn;
  Level, MaxLevel: Cardinal;
  Index,
  I: Integer;
  xText: WideString;
  Buffer: TWideBufferedString;

begin
  Columns := nil;
  Run := nil;
  GetNextNode := nil;

  Buffer := TWideBufferedString.Create;
  try
    RenderColumns := Header.UseColumns;
    if RenderColumns then
      Columns := Header.Columns.GetVisibleColumns;

    GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    // The text consists of visible groups representing the columns, which are separated by one or more separator
    // characters. There are always MaxLevel separator chars in a line (main column only). Either before the caption
    // to ident it or after the caption to make the following column aligned.
    MaxLevel := 0;
    while Assigned(Run) do
    begin
      Level := GetNodeLevel(Run);
      If Level > MaxLevel then
        MaxLevel := Level;
      Run := GetNextNode(Run);
    end;

    SetLength(Tabs, MaxLevel);
    for I := 1 to MaxLevel do
      Tabs[I] := Separator;

    // First line is always the header if used.
    if RenderColumns then
    begin
      LastColumn := Columns[High(Columns)];
      for I := 0 to High(Columns) do
      begin
        Buffer.Add(Columns[I].Text);
        if Columns[I] <> LastColumn then
        begin
          if Columns[I].Index = Header.MainColumn then
          begin
            Buffer.Add(Tabs);
            Buffer.Add(Separator);
          end
          else
            Buffer.Add(Separator);
        end;
      end;
      Buffer.AddNewLine;
    end
    else
      LastColumn := nil;

    Run := Save;
    if RenderColumns then
    begin
      while Assigned(Run) do
      begin
        for I := 0 to High(Columns) do
        begin
          if coVisible in Columns[I].Options then
          begin
            Index := Columns[I].Index;
            xText := Self.Text[Run, Index];
            if Index = Header.MainColumn then
            begin
              Level := GetNodeLevel(Run);
              Buffer.Add(Copy(Tabs, 1, Level));
              // Wrap the text with quotation marks if it contains the separator character.
              if Pos(Separator, xText) > 0 then
              begin
                Buffer.Add('"');
                Buffer.Add(xText);
                Buffer.Add('"');
              end
              else
                Buffer.Add(xText);
              Buffer.Add(Copy(Tabs, 1, MaxLevel - Level));
            end
            else
              if Pos(Separator, xText) > 0 then
              begin
                Buffer.Add('"');
                Buffer.Add(xText);
                Buffer.Add('"');
              end
              else
                Buffer.Add(xText);

            if Columns[I] <> LastColumn then
              Buffer.Add(Separator);
          end;
        end;
        Run := GetNextNode(Run);
        Buffer.AddNewLine;
      end;
    end
    else
    begin
      while Assigned(Run) do
      begin
        xText := Self.Text[Run, NoColumn];
        Level := GetNodeLevel(Run);
        Buffer.Add(Copy(Tabs, 1, Level));
        Buffer.Add(xText);
        Buffer.AddNewLine;

        Run := GetNextNode(Run);
      end;
    end;
    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.InvalidateNode(Node: PVirtualNode): TRect;

begin
  Result := inherited InvalidateNode(Node);
  // Reset node width so changed text attributes are applied correctly.
  if Assigned(Node) then
  begin
    PInteger(InternalData(Node))^ := 0;
    // Reset height measured flag too to cause a re-issue of the OnMeasureItem event.
    Exclude(Node^.States, vsHeightMeasured);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomVirtualStringTree.Path(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  Delimiter: WideChar): WideString;

// Constructs a string containing the node and all its parents. The last character in the returned path is always the
// given delimiter.

var
  S: WideString;

begin
  S := '';
  if (Node = nil) or (Node = RootNode) then
    Result := Delimiter
  else
  begin
    Result := '';
    while Node <> RootNode do
    begin
      DoGetText(Node, Column, TextType, S);
      Result := S + Delimiter + Result;
      Node := Node^.Parent;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomVirtualStringTree.ReinitNode(Node: PVirtualNode; Recursive: Boolean);

begin
  inherited;
  // Reset node width so changed text attributes are applied correctly.
  if Assigned(Node) and (Node <> RootNode) then
  begin
    PInteger(InternalData(Node))^ := 0;
    // Reset height measured flag too to cause a re-issue of the OnMeasureItem event.
    Exclude(Node^.States, vsHeightMeasured);
  end;
end;

//----------------- TVirtualStringTree ---------------------------------------------------------------------------------

function TVirtualStringTree.GetOptions: TStringTreeOptions;

begin
  Result := FOptions as TStringTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualStringTree.SetOptions(const Value: TStringTreeOptions);

begin
  FOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualStringTree.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TStringTreeOptions;
end;

end.

