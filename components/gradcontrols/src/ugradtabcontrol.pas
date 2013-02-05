unit ugradtabcontrol; 

{-------------------------------------------------------------------------------
 @name GradTabControl
 @author Eugen Bolz
 @lastchange 13.12.2009 (DD.MM.YYYY)
 @version 0.2
 @comments TGradTabControl is based on TNotebook/TPageControl/TTabControl
 @license http://creativecommons.org/licenses/LGPL/2.1/
 ------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

{.$DEFINE DEBUGTAB}

interface

uses
  Classes, LResources, SysUtils, Menus, LCLType, ComCtrls,
  LCLProc, LCLIntf, ExtCtrls, Graphics, ugradbtn, Controls,
  uRotateBitmap, Buttons, Forms, ImgList, gradtabstyle
  {$IFDEF DEBUGTAB}
  , sharedloggerlcl
  {$ELSE}
  , DummyLogger
  {$ENDIF};

type
  TGradTabControl = class;
  TGradTabPage = class;

  { TGradTabCloseButton }

  TGradTabCloseButton = class(TGradButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TGradTabPageButton }

  TGradTabPageButton = class( TGradButton )
  private
    FCloseButton : TGradTabCloseButton;
    FPage: TGradTabPage;
    FShowCloseButton : Boolean;
    FShowCloseButtonOnMouseOver: Boolean;
    procedure AlignCloseButton;
    procedure SetShowCloseButton(AValue: Boolean);
    procedure SetShowCloseButtonOnMouseOver(const AValue: Boolean);
  protected
    procedure SetRotateDirection(const Value: TRotateDirection); override;
    procedure RealSetText(const Value: TCaption); override;
    procedure SetAutoHeightBorderSpacing(const AValue: Integer); override;
    procedure SetAutoWidthBorderSpacing(const AValue: Integer); override;
    procedure SetShowGlyph(const Value: Boolean); override;
    procedure CloseBtnBackgroundPaint(Sender: TGradButton;
      TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
    procedure GetContentRect(var TheRect: TRect); override;
    procedure SetBaseColor(const Value: TColor); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CloseButtonLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
    property ShowCloseButtonOnMouseOver : Boolean read FShowCloseButtonOnMouseOver write SetShowCloseButtonOnMouseOver default false;
    property Page : TGradTabPage read FPage write FPage;
  published
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

  TGradTabPageButtonClickEvent = procedure(GradTabControl : TGradTabControl;AIndex : Integer) of object;
  TGradTabPageButtonMouseDownUpEvent = procedure(GradTabControl : TGradTabControl;Button: TMouseButton;
      Shift: TShiftState; X, Y, AIndex: Integer) of object;
  TGradTabPageButtonMouseMoveEvent = procedure(GradTabControl : TGradTabControl; Shift: TShiftState;
  X, Y, AIndex: Integer) of object;

  //Properties of the Tab should be accessable from here
  TGradTabPage = class(TCustomControl)
  private
    FActiveTabColor: TColor;
    FButton : TGradTabPageButton;
    FCaption: TCaption;
    FGradTabControl : TGradTabControl;
    FFlags: TPageFlags;
    FImageIndex: Integer;
    FNormalTabColor: TColor;
    FOwnerTabColor: Boolean;
    FShowCloseButtonOnMouseOver: Boolean;
    FTabVisible,FCurrentlyDestroying,FShowCloseButton : Boolean;
    function GetTabButtonLayout: TButtonLayout;
    function GetTabColor: TColor;
    function GetTabGlyph: TBitmap;
    function GetTabShowGlyph: Boolean;
    function GetTabTextAlignment: TTextAlignment;
    function GetTabPopupMenu : TPopupMenu;
    function GetText : TCaption;
    procedure SetImageIndex(const AValue: Integer);
    procedure SetShowCloseButtonOnMouseOver(const AValue: Boolean);
    procedure SetTabButtonLayout(const AValue: TButtonLayout);
    procedure SetTabColor(const AValue: TColor);
    procedure SetTabGlyph(const AValue: TBitmap);
    procedure SetTabPopupMenu(Value : TPopupMenu);
    procedure SetTabShowGlyph(const AValue: Boolean);
    procedure SetTabTextAlignment(const AValue: TTextAlignment);
    procedure SetText(const Value: TCaption);
    procedure SetEnabled(Value: Boolean); override;
  protected
    function GetPageIndex: integer;
    procedure SetPageIndex(AValue: Integer);
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandle; override;
    procedure Paint; override;
    property TabButton : TGradTabPageButton read FButton;
    function VisibleIndex: integer;
    procedure UpdateImage;
  published
    property TabVisible : Boolean read FTabVisible write SetTabVisible default true;
    property PageIndex : Integer read GetPageIndex write SetPageIndex;
    property Caption : TCaption read GetText write SetText;
    property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
    property ShowCloseButtonOnMouseOver : Boolean read FShowCloseButtonOnMouseOver write SetShowCloseButtonOnMouseOver default false;
    property TabPopupMenu : TPopupMenu read GetTabPopupMenu write SetTabPopupMenu;
    property Color;
    property TabColor : TColor read GetTabColor write SetTabColor;
    property TabTextAlignment : TTextAlignment read GetTabTextAlignment write SetTabTextAlignment default taCenter;
    property TabGlyph : TBitmap read GetTabGlyph write SetTabGlyph;
    property TabShowGlyph : Boolean read GetTabShowGlyph write SetTabShowGlyph;
    property TabButtonLayout : TButtonLayout read GetTabButtonLayout write SetTabButtonLayout;
    property ImageIndex : Integer read FImageIndex write SetImageIndex default 0;
    property Enabled;
    property PopupMenu;
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property ActiveTabColor : TColor read FActiveTabColor write FActiveTabColor default clGreen;
    property NormalTabColor : TColor read FNormalTabColor write FNormalTabColor default clBlue;
    property OwnerTabColor : Boolean read FOwnerTabColor write FOwnerTabColor default false;
  end;
  
  { TFormPage }

  TFormPage = class(TGradTabPage)
  private
    FDestroyPageAtDestroy: Boolean;
    FOldForm : TCustomForm;
    FShowPageAtDestroy: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FormToPage(TheForm : TCustomForm);
    procedure PageToForm(AShow : Boolean);
  published
    property TheForm : TCustomForm read FOldForm;
    property ShowPageAtDestroy : Boolean read FShowPageAtDestroy write FShowPageAtDestroy default false;
    property DestroyPageAtDestroy : Boolean read FDestroyPageAtDestroy write FDestroyPageAtDestroy default false;
  end;

  { TGradTabPagesBar }

  TTabs = Array of Integer;

  {
      @name TGradTabPagesBar
      @comments Shows and Order the TabButtons
  }
  TGradTabPagesBar = class(TCustomControl)
  private
    FActiveTabColor: TColor;
    FNeedOrderButtons: Boolean;
    FNormalTabColor: TColor;
    FPageList : TListWithEvent;
    FTabControl : TGradTabControl;
    FShowFromButton, FMovedTo : Integer;
    FTabPosition : TTabPosition;
    FTabHeight,FLongWidth : Integer;
    FActiveIndex: Integer;
  protected
    procedure InsertButton(AButton: TGradTabPageButton; Index: Integer);
    procedure OrderButtons;
    procedure UnFocusButton(Index: Integer);
    procedure FocusButton(Index: Integer);
    procedure SetTabPosition(Value: TTabPosition);
    function IsVisible(Index: Integer) : Boolean;
    procedure ChangeLeftTop(LastTabPosition : TTabPosition);
    function GetViewedTabs : TTabs;
    function GetViewableTabs(FromIndex : Integer) : TTabs;
    function GetTabsOfSide(FromIndex : Integer; FromLeftSide : Boolean) : TTabs;
    procedure ScrollToTab(PIndex : Integer);
    procedure UpdateAllButtons;
    procedure NewStyle;
  public
    constructor Create(AOwner: TComponent; var thePageList: TListWithEvent;
     TheTabControl : TGradTabControl);
    procedure Paint; override;
    procedure Resize; override;
    procedure MoveToNext;
    procedure MoveToPrior;
    procedure MoveTo(Num: Integer);
    procedure MoveToNorm;
    property TabPosition : TTabPosition read FTabPosition write SetTabPosition;
    property NeedOrderButtons : Boolean read FNeedOrderButtons;
  published
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property ActiveTabColor : TColor read FActiveTabColor write FActiveTabColor default clGreen;
    property NormalTabColor : TColor read FNormalTabColor write FNormalTabColor default clBlue;
  end;
  
  // Is parent of the Next/Prev Buttons
  {
      @name TGradTabBar
      @description
  }
  TGradTabBar = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;
  
  TGradTabPages = class(TStrings)
  private
    FPageList: TListWithEvent;
    FGradTabControl : TGradTabControl;
    procedure PageListChange(Ptr: Pointer; AnAction: TListNotification);
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(var thePageList: TListWithEvent;
       theGradTabControl: TGradTabControl);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

  TGradTabControlPaintEvent = procedure(Sender: TCustomControl;
    TargetCanvas: TCanvas) of object;

  { TGradTabControl }

  TGradTabControl = class(TCustomControl)
  private
    FAutoShowScrollButton: Boolean;
    FCustomDraw: Boolean;
    FImages: TImageList;
    FIsUpdating: Boolean;
    FMoveIncrement: Integer;
    FLeftButton, FRightButton : TGradButton;
    FOnCustomDraw: TGradTabControlPaintEvent;
    FOnTabCloseButtonClick: TGradTabPageButtonClickEvent;
    FOnPageChanged: TNotifyEvent;
    FShowLeftTopScrollButton: Boolean;
    FShowRightBottomScrollButton: Boolean;
    FStyle: TGradTabStyleBase;
    FTabStrings : TStrings; //TGradTabPages
    FPageList: TList; //Is Managed by TGradTabPages
    FOnTabButtonClick : TGradTabPageButtonClickEvent;
    FOnTabButtonMouseDown,
     FOnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent;
    FOnTabButtonMouseMove : TGradTabPageButtonMouseMoveEvent;
    FPageIndex, fPageIndexOnLastChange, fPageIndexOnLastShow,
    FTabHeight, FLongWidth : Integer;
    FBar : TGradTabBar;
    FImageChangeLink : TChangeLink;
    FPagesBar: TGradTabPagesBar;
    FPagesPopup : TPopupMenu;
    FTabPosition : TTabPosition;
    FLongTabs : Boolean;
    procedure AssignEvents(TheControl : TCustomControl);
    procedure AlignPage(APage : TGradTabPage; ARect : TRect);
    procedure AlignPages;
    function GetActiveTabColor: TColor;
    function GetNormalTabColor: TColor;
    function GetTabPopupMenu: TPopupMenu;
    procedure ImageListChange(Sender: TObject);
    procedure SetActiveTabColor(const AValue: TColor);
    procedure SetCustomDraw(const AValue: Boolean);
    procedure SetNormalTabColor(const AValue: TColor);
    procedure SetStyle(const AValue: TGradTabStyleBase);
    procedure SetTabPopupMenu(const AValue: TPopupMenu);
    procedure UpdateTabImages;
    function GetCurrentPage : TGradTabPage;
    function GetPage(AIndex: Integer) : TGradTabPage;
    function GetCount : Integer;
    function GetPagesBarDragOver: TDragOverEvent;
    procedure MoveTab(Sender: TObject; NewIndex: Integer);
    function FindVisiblePage(Index: Integer): Integer;
    procedure PageButtonMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure PageButtonMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure PageButtonMouseClick(Sender: TObject);
    procedure PageCloseButtonMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure PopupMouseClick(Sender: TObject);
    procedure PageButtonMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
    procedure SetAutoShowScrollButtons(const AValue: Boolean);
    procedure SetImages(const AValue: TImageList);
    procedure SetLongWidth(const AValue: Integer);
    procedure SetShowLeftTopScrollButton(const AValue: Boolean);
    procedure SetShowRightBottomScrollButton(const AValue: Boolean);

    //SubControl Events
    procedure SubMouseWheel(Sender: TObject; Shift: TShiftState;
     WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SubMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure SubMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure SubMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure SubMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure SubMouseClick(Sender: TObject);
    procedure SubMouseDblClick(Sender: TObject);
    procedure SubMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
    procedure SubDragOver(Sender, Source: TObject;
           X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SubDragDrop(Sender, Source: TObject; X,Y: Integer);
    //End

    // Style Event Wrapper
    procedure StyleTabButton(Sender: TGradButton;
      TargetCanvas: TCanvas; R: TRect; BState : TButtonState);
    procedure StyleTabCloseButton(Sender: TGradButton;
      TargetCanvas: TCanvas; R: TRect; BState : TButtonState);
    procedure StyleTabButtonBorder(Sender: TGradButton;
      TargetCanvas: TCanvas; R: TRect; BState : TButtonState);

    // End

    procedure PopupTabs(Sender: TObject);
    procedure MoveLeftTopClick(Sender: TObject);
    procedure MoveRightBottomClick(Sender: TObject);
    procedure PageRemoved(Index: Integer);
    procedure SetCurrentPage(Value : TGradTabPage);
    procedure SetCurrentPageNum(Value: Integer);
    procedure SetPagesBarDragOver(const AValue: TDragOverEvent);
    procedure ShowPage(Index: Integer);
    procedure ShowCurrentPage;
    procedure UnShowPage(Index: Integer);
  protected
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure InsertPage(APage: TGradTabPage; Index: Integer);
    procedure AddRemovePageHandle(APage: TGradTabPage);
    procedure RemovePage(Index: Integer);
    procedure InvPaint;
    procedure SetTabHeight(Value: Integer);
    procedure SetTabs(Value: TStrings);
    procedure SetTabPosition(Value : TTabPosition);
    procedure SetLongTabs(Value : Boolean);
    procedure Change;
    procedure UpdateTabProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetTabRect(AIndex : Integer) : TRect;
    function AddPage(AName: String) : Integer;
    function AddPage(APage: TGradTabPage) : Integer;
    function GetTabBarSize(TabPos : TTabPosition) : Integer;
    function GetClientRect: TRect; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);

    property IsUpdating : Boolean read FIsUpdating;
    property Page[Index: Integer] : TGradTabPage read GetPage;
    property Bar : TGradTabBar read FBar;
    property PagesBar : TGradTabPagesBar read FPagesBar;
    property PageList: TList read FPageList;
    property Tabs : TStrings read FTabStrings write SetTabs;
    property PageCount : Integer read GetCount;
  published
    property Align;
    property BorderSpacing;
    property ActivePage : TGradTabPage read GetCurrentPage write SetCurrentPage;
    property OnTabButtonClick : TGradTabPageButtonClickEvent read FOnTabButtonClick write FOnTabButtonClick;
    property OnTabButtonMouseDown : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseDown write FOnTabButtonMouseDown;
    property OnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseUp write FOnTabButtonMouseUp;
    property OnTabButtonMouseMove : TGradTabPageButtonMouseMoveEvent read FOnTabButtonMouseMove write FOnTabButtonMouseMove;
    property OnTabCloseButtonClick : TGradTabPageButtonClickEvent read FOnTabCloseButtonClick write FOnTabCloseButtonClick;
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnClick;
    property OnDblClick;
    property TabPopupMenu : TPopupMenu read GetTabPopupMenu write SetTabPopupMenu;
    property PopupMenu;
    property PageIndex : Integer read FPageIndex write SetCurrentPageNum;
    property TabHeight : Integer read FTabHeight write SetTabHeight;
    property TabPosition : TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property LongTabs : Boolean read FLongTabs write SetLongTabs;
    property LongWidth: Integer read FLongWidth write SetLongWidth;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property AutoShowScrollButtons : Boolean read FAutoShowScrollButton write SetAutoShowScrollButtons default true;
    property ShowLeftTopScrollButton : Boolean read FShowLeftTopScrollButton write SetShowLeftTopScrollButton;
    property ShowRightBottomScrollButton : Boolean read FShowRightBottomScrollButton write SetShowRightBottomScrollButton;
    property Images : TImageList read FImages write SetImages;
    property NormalTabColor: TColor read GetNormalTabColor write SetNormalTabColor default clBlue;
    property ActiveTabColor: TColor read GetActiveTabColor write SetActiveTabColor default clGreen;
    property CustomDraw : Boolean read FCustomDraw write SetCustomDraw;
    property OnCustomDraw : TGradTabControlPaintEvent read FOnCustomDraw write FOnCustomDraw;
    property Style : TGradTabStyleBase read FStyle write SetStyle;
  end;
  
  procedure Register;
  function IsAssigned(var Obj : TObject) : String;
  function BoolStr(BV : Boolean) : String;
  function IncAr(var Ar : TTabs) : Integer;
  function Form2Page(theTabControl : TGradTabControl; theForm : TCustomForm) : TFormPage;

implementation

uses
  gradtabcontroleditor, ComponentEditors;

{-------------------------------------------------------------------------------
  Register
 ------------------------------------------------------------------------------}
procedure Register;
begin
   RegisterComponents('Misc',[TGradTabControl]);
   RegisterComponentEditor(TGradTabControl,TGradTabControlComponentEditor);
   RegisterComponentEditor(TGradTabPage,TGradTabPageComponentEditor);
   RegisterNoIcon([TGradTabPage]);
end;

function IsAssigned(var Obj: TObject): String;
begin
  Result := 'Assigned: '+BoolToStr(Assigned(Obj),true);
end;

function BoolStr(BV: Boolean): String;
begin
  Result := BoolToStr(BV,true);
end;

function ValueInArray(Needle : Integer; Stack : TTabs) : Boolean;
var
   i : Integer;
begin
   Result := false;

   DebugLn('ValueInArray: Needle=%d Low=%d High=%d',[Needle, Low(Stack), High(Stack)]);

   for i := Low(Stack) to High(Stack) do
   if Needle =Stack[i] then
   begin
      Result := true;
      Exit;
   end;
end;

function IncAr(var Ar : TTabs) : Integer;
begin
   SetLength(Ar, Length(Ar)+1);
   Result := Length(Ar)-1;
end;

function Form2Page(theTabControl: TGradTabControl; theForm: TCustomForm
  ): TFormPage;
begin
   Result := TFormPage.Create(theTabControl);
   Result.FormToPage(theForm);
   Result.ShowPageAtDestroy:=false;
   Result.Parent := theTabControl;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton Create(AOwner: TComponent
 ------------------------------------------------------------------------------}
constructor TGradTabPageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle+[csNoDesignSelectable,csDesignInteractive]-[csCaptureMouse];
  FCloseButton := TGradTabCloseButton.Create(AOwner);
  FCloseButton.Width:=14;
  FCloseButton.Height:=14;
  TextAlignment:=taCenter;
  FCloseButton.Left:=1;
  FCloseButton.Top:=1;
  FCloseButton.Caption:='';
  FCloseButton.OnMouseLeave:=@CloseButtonLeave;

  FShowCloseButton:=false;
  FShowCloseButtonOnMouseOver:=false;

  SetSubComponent(true);
end;

destructor TGradTabPageButton.Destroy;
begin
  FCloseButton.Free;

  inherited Destroy;
end;

procedure TGradTabPageButton.CloseButtonLeave(Sender: TObject);
begin
  if not FShowCloseButton then Exit;
  if not FShowCloseButtonOnMouseOver then Exit;

  // Hide Close Button Fix
  FCloseButton.Visible:=false;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton AlignCloseButton
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.AlignCloseButton;
var
  TheRect : TRect;
  DisplayWidth, DisplayHeight: Integer;
  NewTop,NewLeft : Integer;
begin
  if not HasParent then Exit;

  //GetBackgroundRect(TheRect);
  GetContentRect(TheRect);

  DisplayWidth:= TheRect.Right-TheRect.Left;
  DisplayHeight:=TheRect.Bottom-TheRect.Top;

  case RotateDirection of
    rdNormal: begin
      NewTop:=(DisplayHeight div 2)-(FCloseButton.Height div 2);
      NewLeft:=DisplayWidth-(FCloseButton.Width);
    end;
    rdRight: begin
      NewTop:=DisplayHeight-(FCloseButton.Height);
      NewLeft:=(DisplayWidth div 2)-(FCloseButton.Width div 2);
    end;
    rdLeft:  begin
      NewTop:=1;
      NewLeft:=(DisplayWidth div 2)-(FCloseButton.Width div 2);
    end;
  end;

  FCloseButton.Top:=NewTop+TheRect.Top;
  FCloseButton.Left:=NewLeft+TheRect.Left;

  FCloseButton.UpdateButton;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton SetShowCloseButton(AValue: Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.SetShowCloseButton(AValue: Boolean);
begin
  if AValue = FShowCloseButton then Exit;
  FShowCloseButton:=AValue;

  if AValue then
  begin
    AlignCloseButton;
    FCloseButton.Parent:=Self;
  end else begin
    FCloseButton.Parent:=nil;
  end;
end;

procedure TGradTabPageButton.SetShowCloseButtonOnMouseOver(const AValue: Boolean
  );
begin
  {*
    IF ShowCloseButton AND ShowCloseButtonOnMouseOver
    then the CloseButton is Invisible until the Mouse is Over the CloseButton
  *}

  if FShowCloseButtonOnMouseOver=AValue then exit;
  if not FShowCloseButton then Exit;

  FShowCloseButtonOnMouseOver:=AValue;

  FCloseButton.Visible:=false;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton Resize
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.Resize;
begin
  inherited Resize;

  AlignCloseButton;
end;

procedure TGradTabPageButton.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
var
   ParentControl : TGradTabControl;
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);

  AlignCloseButton;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton SetRotateDirection(const Value: TRotateDirection)
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.SetRotateDirection(const Value: TRotateDirection);
begin
  inherited;

  case Value of
    rdNormal: TextAlignment := taLeftJustify;
    rdLeft, rdRight: TextAlignment := taCenter;
  end;

  FCloseButton.RotateDirection:=Value;
  if FShowCloseButton then
     AlignCloseButton;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton.SetText(const Value: TCaption)
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.RealSetText(const Value: TCaption);
var
   NewCaption : TCaption;
begin
   Logger.EnterMethod(Self, 'RealSetText');

   NewCaption:=Value;

   inherited RealSetText(NewCaption);

   AlignCloseButton;

   Logger.Send('Parent Assigned', Assigned(Parent));
   Logger.Send('Parent is TGradTabPagesBar', Parent is TGradTabPagesBar);

   if Assigned(Parent) AND (Parent is TGradTabPagesBar) then
      (Parent as TGradTabPagesBar).OrderButtons;

   Logger.ExitMethod(Self, 'RealSetText');
end;

procedure TGradTabPageButton.SetAutoHeightBorderSpacing(const AValue: Integer);
begin
  if ShowGlyph then
    inherited SetAutoHeightBorderSpacing(AValue+Glyph.Height)
  else
    inherited SetAutoHeightBorderSpacing(AValue);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.SetAutoWidthBorderSpacing(const AValue: Integer);
begin
  if ShowGlyph then
    inherited SetAutoWidthBorderSpacing(AValue+Glyph.Width)
  else
    inherited SetAutoWidthBorderSpacing(AValue);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.SetShowGlyph(const Value: Boolean);
begin
  inherited SetShowGlyph(Value);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.CloseBtnBackgroundPaint(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  // nothing
end;

procedure TGradTabPageButton.GetContentRect(var TheRect: TRect);
begin
  inherited GetContentRect(TheRect);

  if ShowCloseButton then
  begin
    case RotateDirection of
      rdNormal: begin
        TheRect.Right  :=TheRect.Right-(FCloseButton.Glyph.Width);
      end;
      rdLeft: begin
        TheRect.Top    := TheRect.Top+(FCloseButton.Glyph.Height);
      end;
      rdRight: begin
        TheRect.Bottom := TheRect.Bottom-(FCloseButton.Glyph.Height);
      end;
    end;
  end;
end;

procedure TGradTabPageButton.SetBaseColor(const Value: TColor);
begin
  inherited SetBaseColor(Value);

  FCloseButton.Color:=Value;
end;

procedure TGradTabPageButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CloseBtnRect : TRect;
begin
  inherited MouseMove(Shift, X, Y);

  if not FShowCloseButton then Exit;
  if not FShowCloseButtonOnMouseOver then Exit;

  CloseBtnRect.Top:=FCloseButton.Top;
  CloseBtnRect.Left:=FCloseButton.Left;
  CloseBtnRect.Right:=FCloseButton.Left+FCloseButton.Width;
  CloseBtnRect.Bottom:=FCloseButton.Top+FCloseButton.Height;

  // Shows the Close Button
  FCloseButton.Visible:=PtInRect(CloseBtnRect, Point(X,Y));
end;

{-------------------------------------------------------------------------------
  TGradTabPage Create(AOwner: TGradTabPageButton)
 ------------------------------------------------------------------------------}
constructor TGradTabPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCurrentlyDestroying := false;
  fCompStyle := csPage;
  ControlStyle := ControlStyle + [csAcceptsControls,csDesignFixedBounds,csNoDesignVisible];

  Align := alClient;

  FButton := TGradTabPageButton.Create(Self);
  FButton.Page := Self;

  FTabVisible:=true;
  FShowCloseButton:=false;
  FImageIndex:=0;

  FActiveTabColor:=clGreen;
  FNormalTabColor:=clBlue;
  FOwnerTabColor:=false;
end;

{-------------------------------------------------------------------------------
  TGradTabPage Destroy
 ------------------------------------------------------------------------------}
destructor TGradTabPage.Destroy;
begin
     FCurrentlyDestroying := true;
     Parent := nil;

     FButton.Parent := nil;
     FButton.Free;

     inherited;
end;

{-------------------------------------------------------------------------------
  TGradTabPage DestroyHandle
 ------------------------------------------------------------------------------}
procedure TGradTabPage.DestroyHandle;
begin
     inherited DestroyHandle;
     Exclude(FFlags,pfAdded);
end;

{-------------------------------------------------------------------------------
  TGradTabPage GetPageIndex: integer
 ------------------------------------------------------------------------------}
function TGradTabPage.GetPageIndex: integer;
begin
   if (Parent<>nil) and (Parent is TGradTabControl) then
     Result:=TGradTabControl(Parent).PageList.IndexOf(Self)
   else
     Result := -1;
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetPageIndex(AValue: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetPageIndex(AValue: Integer);
begin
   if (Parent<>nil) and (Parent is TGradTabControl) then begin
     TGradTabControl(Parent).MoveTab(Self,AValue);
   end;
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetParent(NewParent: TWinControl)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetParent(NewParent: TWinControl);
var
  OldParent: TWinControl;
  ParentTabControl: TGradTabControl;
  i: integer;
begin
  {$IFDEF DEBUGTAB}
   DebugLn('TGradTabPage.SetParent');
  {$ENDIF}

  if (NewParent=Parent) or (pfInserting in FFlags) then Exit;
  //if ((Parent<>nil)) AND (NewParent=Parent) then exit;
  CheckNewParent(NewParent);
  OldParent:=Parent;

  {$IFDEF DEBUGTAB}
   DebugLn('OldParent: %s NewParent: %s',[DbgSName(OldParent),DbgSName(NewParent)]);
  {$ENDIF}

  if (OldParent<>NewParent) and (OldParent<>nil)
  and (OldParent is TGradTabControl)
  and (not (pfRemoving in FFlags))
  then begin
    // remove from old pagelist
    ParentTabControl := TGradTabControl(OldParent);
    i := PageIndex;
    if i >= 0 then
      ParentTabControl.RemovePage(i);

    {$IFDEF DEBUGTAB}
      DebugLn('Page removed from old TabControl');
    {$ENDIF}
  end;

  inherited SetParent(NewParent);

  {$IFDEF DEBUGTAB}
   DebugLn('New Parent set');
  {$ENDIF}

  if (OldParent<>NewParent) and (Parent<>nil)
  and (Parent is TGradTabControl) then begin
    // add to new pagelist
    ParentTabControl:=TGradTabControl(Parent);
    i:=ParentTabControl.PageList.IndexOf(Self);
    if i<0 then
      ParentTabControl.InsertPage(Self,ParentTabControl.PageCount);

    {$IFDEF DEBUGTAB}
      DebugLn('Insert Page in new Parent');
    {$ENDIF}
  end;

  FGradTabControl := TGradTabControl(NewParent);

  {$IFDEF DEBUGTAB}
   DebugLn('TGradTabPage.SetParent end');
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetShowCloseButton(Value: Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetShowCloseButton(Value: Boolean);
begin
    if FShowCloseButton=Value then Exit;

    FShowCloseButton:=Value;
    FButton.ShowCloseButton:=Value;

    if not Value then
       FButton.Caption:=FCaption
    else
       SetText(FCaption);
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetTabVisible(Value: Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetTabVisible(Value: Boolean);
begin
     if Value = FTabVisible then exit;
      FTabVisible := Value;

     if csDesigning in ComponentState then
        Exit;

      if Parent.HandleAllocated then
      begin
        TGradTabControl(Parent).AddRemovePageHandle(Self);
        if FTabVisible then
        begin
          // check if there was no visible tab
          if TGradTabControl(Parent).PageIndex = -1 then
            TGradTabControl(Parent).PageIndex:=PageIndex;
        end
        else
          // Check if the page is active and set a new pageindex
          TGradTabControl(Parent).PageRemoved(PageIndex);
      end;

    FButton.Visible:=FTabVisible;

    if FTabVisible then
       TGradTabControl(Parent).PagesBar.OrderButtons
    else begin
        FButton.Left:=-FButton.Width;
        FButton.Top:=-FButton.Height;
    end;


    //DoTabVisible; { TODO }
end;

{-------------------------------------------------------------------------------
  TGradTabPage GetTabPopupMenu : TPopupMenu
 ------------------------------------------------------------------------------}
function TGradTabPage.GetTabPopupMenu : TPopupMenu;
begin
    Result := FButton.PopupMenu;
end;

procedure TGradTabPage.SetTabTextAlignment(const AValue: TTextAlignment);
begin
   if FButton.TextAlignment = AValue then Exit;
   FButton.TextAlignment:= AValue;
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetTabPopupMenu(Value: TPopupMenu)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetTabPopupMenu(Value : TPopupMenu);
begin
    if not Assigned(FButton) then Exit;
    if not Assigned(Value) then Exit;

    FButton.PopupMenu:=Value;
end;

procedure TGradTabPage.SetTabShowGlyph(const AValue: Boolean);
begin
  FButton.ShowGlyph:= AValue;
end;

function TGradTabPage.GetTabColor: TColor;
begin
  Result := FButton.Color
end;

function TGradTabPage.GetTabButtonLayout: TButtonLayout;
begin
  Result := FButton.ButtonLayout;
end;

function TGradTabPage.GetTabGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

function TGradTabPage.GetTabShowGlyph: Boolean;
begin
  Result := FButton.ShowGlyph;
end;

function TGradTabPage.GetTabTextAlignment: TTextAlignment;
begin
  Result := FButton.TextAlignment;
end;

procedure TGradTabPage.SetImageIndex(const AValue: Integer);
begin
  if FGradTabControl=nil then Exit;
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;

  if (FGradTabControl.Images <> nil) AND (FImageIndex<>-1) then
  begin
      UpdateImage;
  end;
end;

procedure TGradTabPage.SetShowCloseButtonOnMouseOver(const AValue: Boolean);
begin
  if FShowCloseButtonOnMouseOver=AValue then exit;
  FShowCloseButtonOnMouseOver:=AValue;

  FButton.ShowCloseButtonOnMouseOver:=AValue;
end;

procedure TGradTabPage.SetTabButtonLayout(const AValue: TButtonLayout);
begin
  FButton.ButtonLayout:=AValue;
end;

procedure TGradTabPage.SetTabColor(const AValue: TColor);
begin
  if FButton.Color = AValue then Exit;

  FButton.Color:=AValue;
end;

procedure TGradTabPage.SetTabGlyph(const AValue: TBitmap);
begin
  if FButton.Glyph = AValue then Exit;

  FButton.Glyph.Assign(AValue);
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetText(Value: TCaption)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetText(const Value: TCaption);
begin
    FCaption := Value;

    if FShowCloseButton then
       FButton.Caption:=FCaption+'   '
    else
       FButton.Caption:={IntToStr(PageIndex)+': '+}FCaption;
end;

{-------------------------------------------------------------------------------
  TGradTabPage GetText : TCaption
 ------------------------------------------------------------------------------}
function TGradTabPage.GetText : TCaption;
begin
    Result := FCaption;
end;

procedure TGradTabPage.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);

  FButton.Enabled:=Value;
end;

{-------------------------------------------------------------------------------
  TGradTabPage Paint
 ------------------------------------------------------------------------------}
procedure TGradTabPage.Paint;
var
   i : Integer;
begin
    {for i := 0 to Height do
    begin
          Canvas.Pen.Color:=ColorBetween(Color,Parent.Color,i / (Height ));

       Canvas.Line(0,i,Width,i);
       //Canvas.FillRect(-1,-1,Width,Height);

    end;

    Canvas.TextOut(Width-Canvas.TextWidth(DbgSName(Self)),0,DbgSName(Self));
    }
    inherited;
end;

{-------------------------------------------------------------------------------
  TGradTabPage VisibleIndex
 ------------------------------------------------------------------------------}
function TGradTabPage.VisibleIndex: integer;
var
  List: TList;
  i: Integer;
begin
  if (Parent<>nil) and (Parent is TGradTabControl) then begin
    Result:=0;
    List:=TGradTabControl(Parent).PageList;
    i:=0;
    repeat
      if i=List.Count then exit(-1);
      if (TObject(List[i])=Self) then exit;
      if TGradTabPage(List[i]).TabVisible or (csDesigning in ComponentState)
      then inc(Result);
      inc(i);
    until false;
  end else
    Result := -1;
end;

procedure TGradTabPage.UpdateImage;
begin
  if FGradTabControl = nil then Exit;

  FButton.Glyph.Clear;

  if Assigned(FGradTabControl.Images) AND (FGradTabControl.Images.Count<>0)
  AND (ImageIndex < FGradTabControl.Images.Count) AND (ImageIndex<>-1) then begin
     FGradTabControl.Images.GetBitmap(ImageIndex,FButton.Glyph);
     FButton.ShowGlyph:=true;
  end;
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar Create(AOwner: TComponent; var theTabList: TListWithEvent;
       TheTabControl : TGradTabControl)
 ------------------------------------------------------------------------------}
constructor TGradTabPagesBar.Create(AOwner: TComponent; var thePageList: TListWithEvent;
       TheTabControl : TGradTabControl);
begin
     inherited Create(AOwner);

     FPageList := thePageList;
     FShowFromButton:=-1;
     ControlStyle := ControlStyle+[csNoDesignSelectable];
     FTabPosition:=tpTop;
     FMovedTo:=1;
     FTabControl := TheTabControl;
     SetSubComponent(true);

     FTabControl.FLeftButton.Visible:=false;
     FTabControl.FRightButton.Visible:=false;

     FActiveTabColor:=clGreen;
     FNormalTabColor:=clBlue;
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar Paint
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.Paint;
begin
     {Canvas.Brush.Color:=clBlue;
     Canvas.FillRect(0,0,Width,Height);
     }
     //WriteLn(Left, ' ' ,Top, ' ' ,Width, ' ' ,Height);

     inherited;
end;

procedure TGradTabPagesBar.Resize;
begin
  inherited Resize;

  OrderButtons;
end;

procedure TGradTabPagesBar.MoveToNext;
var
   TheTabs : TTabs;
   VIA, L : Boolean;
   FLastShowFrom : Integer;
begin
  {$IFDEF DEBUGTAB}DebugLn('MoveToNext Begin');{$ENDIF}
  //DebugLn(GetStackTrace(true));
  TheTabs:= GetViewedTabs;

  VIA := ValueInArray(FPageList.Count-1,TheTabs);
  L := (Length(TheTabs)>1);

  {$IFDEF DEBUGTAB}
  DebugLn('ValueInArray=%s Length=%s',[BoolToStr(VIA,true),BoolToStr(L,true)]);
  {$ENDIF}

  FLastShowFrom:= FShowFromButton;

  if VIA AND L then
     FShowFromButton := TheTabs[0]
  else
     Inc(FShowFromButton);

  if FShowFromButton>=FPageList.Count then
     FShowFromButton:= FPageList.Count-1;

  {if not FTabControl.Page[FShowFromButton].Enabled then
     FShowFromButton := FLastShowFrom;}

  {$IFDEF DEBUGTAB}DebugLn('New FShowFromButton: %d',[FShowFromButton]);{$ENDIF}
  OrderButtons;

  {$IFDEF DEBUGTAB}DebugLn('MoveToNext End');{$ENDIF}

  //GetViewedTabs;
end;

procedure TGradTabPagesBar.MoveToPrior;
var
   FLastShowFrom : Integer;
begin
  FLastShowFrom:= FShowFromButton;

  Dec(FShowFromButton);
  if FShowFromButton<0 then FShowFromButton := 0;

  {$IFDEF DEBUGTAB}DebugLn('New FShowFromButton: %d',[FShowFromButton]);{$ENDIF}
  OrderButtons;

  //GetViewedTabs;
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar InsertButton(AButton: TGradTabPageButton; Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.InsertButton(AButton: TGradTabPageButton; Index: Integer);
var
  LastLeft : Integer;
begin
  LastLeft := 0;

  Logger.EnterMethod(Self, 'InsertButton('+IntToStr(Index)+')');

  if (Index >= 1) AND (FPageList.Count>=1) then
    LastLeft := TGradTabPage(FPageList.Items[Index-1]).TabButton.Left;

  FTabControl.AssignEvents(AButton);

  with AButton do
  begin
    Left                   := -123;
    ShowFocusBorder        := false;
    TextAlignment          := taCenter;
    BorderSides            := [bsTopLine,bsRightLine,bsLeftLine];
    OnMouseDown            := @FTabControl.PageButtonMouseDown;
    OnMouseUp              := @FTabControl.PageButtonMouseUp;
    OnClick                := @FTabControl.PageButtonMouseClick;
    OnMouseMove            := @FTabControl.PageButtonMouseMove;
    FCloseButton.OnMouseUp := @FTabControl.PageCloseButtonMouseUp;

    FTabControl.Style.PrepareButton(AButton);

    if FTabControl.Style.HasTabButtonPaint then
    begin
      OnNormalBackgroundPaint   := @FTabControl.StyleTabButton;
      OnDisabledBackgroundPaint := @FTabControl.StyleTabButton;
      OnDownBackgroundPaint     := @FTabControl.StyleTabButton;
      OnHotBackgroundPaint      := @FTabControl.StyleTabButton;

      if FTabControl.Style.HasBorderButtonPaint then
        OnBorderBackgroundPaint := @FTabControl.StyleTabButtonBorder;

      OwnerBackgroundDraw := true;

      if FTabControl.Style.HasCloseButtonPaint then
      begin
        FCloseButton.OnNormalBackgroundPaint   := @FTabControl.StyleTabCloseButton;
        FCloseButton.OnDisabledBackgroundPaint := @FTabControl.StyleTabCloseButton;
        FCloseButton.OnDownBackgroundPaint     := @FTabControl.StyleTabCloseButton;
        FCloseButton.OnHotBackgroundPaint      := @FTabControl.StyleTabCloseButton;

        FCloseButton.OwnerBackgroundDraw := true;
      end;
    end else begin
      OwnerBackgroundDraw := false;
    end;
  end;

  if Assigned(PopupMenu) then
    AButton.PopupMenu := PopupMenu;

  {if (Index >= 1) AND (FPageList.Count>=1) then
    UnFocusButton(Index-1);

  FocusButton(Index);

  OrderButtons;}

  Logger.ExitMethod(Self, 'InsertButton('+IntToStr(Index)+')');
end;

procedure TGradTabPagesBar.MoveTo(Num: Integer);

   function GetLast : Integer;
   var
      Btn : TGradButton;
      i,l : Integer;
   begin
       Result := 1;
       if FPageList.Count = 0 then Exit;

       l := 1;

       for i := 0 to FPageList.Count-2 do
       begin
           Btn := TGradTabPage(FPageList.Items[i]).TabButton;

           L := L + 1;

           if TabPosition in [tpTop, tpBottom] then begin
              L := L + Btn.Width;
              //if Result > Width then Result := Result - Width;
           end else begin
              L := L + Btn.Height;
              //if Result > Height then Result := Result - Height;
           end;
       end;

       Result := l;

       //Result :=
   end;

var
   i : Integer;
begin
   {$IFDEF DEBUGTAB}DebugLn('MoveTo Max: %d, Current: %d',[GetLast+1,FMovedTo]);{$ENDIF}

   if FMovedTo > 1 then FMovedTo := 1;
   if FMovedTo < -GetLast then FMovedTo := -GetLast+2;

   //OrderButtons;
end;

procedure TGradTabPagesBar.MoveToNorm;
begin
  FMovedTo:=1;

  OrderButtons;
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar OrderButtons
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.OrderButtons;
var
  LastLeft, LastTop, i, BarWidth, BarHeight: Integer;
  NewDirection: TRotateDirection;
  NewBorderSides: TBorderSides;
  NewGradientType: TGradientType;
  B : TGradButton;
begin
  if csDestroying in FTabControl.ComponentState then Exit;
  if FPageList.Count=0 then Exit;

  if (FTabControl.IsUpdating) then
  begin
    FNeedOrderButtons := true;
    Exit;
  end;

  FNeedOrderButtons := false;

  Logger.EnterMethod(Self, 'OrderButtons');

  FMovedTo:=0;

  for i := FShowFromButton-1 downto 0 do
  begin
    if i >= FPageList.Count then Continue;
    B := TGradTabPage(FPageList.Items[i]).TabButton;
    if FTabPosition in [tpRight, tpLeft] then
       Dec(FMovedTo,B.Height+1);

    if FTabPosition in [tpTop, tpBottom] then
       Dec(FMovedTo,B.Width+1);
  end;

  Inc(FMovedTo);

  LastLeft:= FMovedTo;
  LastTop := FMovedTo;

 { if FTabPosition in [tpTop, tpBottom] then begin
      BarWidth:= Width;
      BarHeight:= 0;
  end else begin   }
  BarWidth := Width;
  BarHeight:= Height;
  //end;

  case FTabPosition of
    tpTop:
    begin
      NewBorderSides := [bsTopLine, bsRightLine, bsLeftLine];
      NewGradientType := gtHorizontal;
      NewDirection := rdNormal;
    end;
    tpBottom:
    begin
      NewBorderSides := [bsBottomLine, bsRightLine, bsLeftLine];
      NewGradientType := gtHorizontal;
      NewDirection := rdNormal;
    end;
    tpLeft:
    begin
      NewBorderSides := [bsTopLine, bsBottomLine, bsLeftLine];
      if FTabControl.LongTabs then begin
          NewGradientType := gtHorizontal;
          NewDirection := rdNormal;
      end else begin
          NewGradientType := gtVertical;
          NewDirection := rdLeft;
      end;
    end;
    tpRight:
    begin
      NewBorderSides := [bsTopLine, bsBottomLine, bsRightLine];
      if FTabControl.LongTabs then begin
          NewGradientType := gtHorizontal;
          NewDirection := rdNormal;
      end else begin
          NewGradientType := gtVertical;
          NewDirection := rdRight;
      end;
    end;
  end;

  FActiveIndex:=FTabControl.PageIndex;

  Logger.EnterMethod(Self, 'GROUP: foreach FPageList[i]');
  for i := 0 to FPageList.Count - 1 do
  begin
    B := TGradTabPage(FPageList.Items[i]).TabButton;

    Logger.EnterMethod(Self, 'GROUP: TabButton of Page');
    Logger.Send('Visible', B.Visible);

    if B.Visible then
    begin
      B.RotateDirection := NewDirection;
      B.BorderSides := NewBorderSides;
      B.GradientType := NewGradientType;

      Logger.Send('Before', 0);
      Logger.Send('i', i);
      Logger.Send('Width', B.Width);
      Logger.Send('Height', B.Height);
      Logger.Send('Left', B.Left);
      Logger.Send('Top', B.Top);
      Logger.Send('BarWidth', BarWidth);
      Logger.Send('BarHeight', BarHeight);

      case FTabPosition of
        tpTop:
        begin
          //if B.Width < B.GetAutoWidth then
          B.Width := B.GetAutoWidth;

          B.Left := LastLeft;
          LastLeft := LastLeft + B.Width + 1;

          if FActiveIndex = i then begin
            B.Top := 0;
            B.Height:= BarHeight;
          end else begin
            B.Top := 3;
            B.Height:= BarHeight-3;
          end;
        end;
        tpBottom:
        begin
          //if B.Width < B.GetAutoWidth then
          B.Width := B.GetAutoWidth;

          B.Left := LastLeft;
          LastLeft := LastLeft + B.Width + 1;

          B.Top := 0;

          if FActiveIndex = i then
            B.Height := BarHeight
          else
            B.Height := BarHeight-3;
        end;
        tpLeft:
        begin
          if FTabControl.LongTabs then begin
            if (B.GetAutoWidth > FTabControl.LongWidth) then
            begin
              FTabControl.LongWidth:=B.GetAutoWidth;
              Exit;
            end;

          end;

          B.Height:= B.GetAutoHeight;

          B.Top := LastTop;
          LastTop := LastTop + B.Height + 1;

          if FActiveIndex = i then begin
            B.Left := 0;
            B.Width:= FTabControl.GetTabBarSize(tpLeft);
          end else begin
            B.Left := 3;
            B.Width:= FTabControl.GetTabBarSize(tpLeft)-3;
          end;
        end;
        tpRight:
        begin
          if FTabControl.LongTabs then begin
            if (B.GetAutoWidth > FTabControl.LongWidth) then
            begin
              FTabControl.LongWidth:=B.GetAutoWidth;
              Exit;
            end;
          end;

          B.Height:= B.GetAutoHeight;

          B.Top := LastTop;
          LastTop := LastTop + B.Height + 1;

          B.Left := 0;

          if FActiveIndex = i then
            B.Width := FTabControl.GetTabBarSize(tpRight)
          else begin
            B.Width := FTabControl.GetTabBarSize(tpRight)-3;
          end;

          Logger.Send('Width', B.Width);
          Logger.Send('TabBarSize(tpRight)', FTabControl.GetTabBarSize(tpRight));
          Logger.Send('FActive', FActiveIndex);
        end;
      end;

      Logger.Send('After', 0);
      Logger.Send('i', i);
      Logger.Send('Width', B.Width);
      Logger.Send('Height', B.Height);
      Logger.Send('Left', B.Left);
      Logger.Send('Top', B.Top);
      Logger.Send('BarWidth', BarWidth);
      Logger.Send('BarHeight', BarHeight);
    end;

    if B.Parent <> Self then
      B.Parent := Self;

    Logger.ExitMethod(Self, 'GROUP: TabButton of Page');
  end;

  Logger.ExitMethod(Self, 'GROUP: foreach FPageList[i]');

  Logger.Send('BarWidth', BarWidth);
  Logger.Send('LastLeft', LastLeft);
  Logger.Send('FMovedTo', FMovedTo);
  Logger.Send('BarHeight', BarHeight);
  Logger.Send('LastTop', LastTop);

  Logger.Send('BarWidth < (LastLeft-FMovedTo)', BarWidth < (LastLeft-FMovedTo));
  Logger.Send('BarHeight < (LastTop-FMovedTo)', BarHeight < (LastTop-FMovedTo));

  Logger.Send('TabControl.AutoShowScrollButtons', FTabControl.AutoShowScrollButtons);

  if not FTabControl.AutoShowScrollButtons then
  begin
    Logger.ExitMethod(Self, 'OrderButtons');
    Exit;
  end;

  if ((BarWidth < (LastLeft-FMovedTo))
  OR (BarHeight < (LastTop-FMovedTo))
  OR (FMovedTo <> 1))
  AND ((BarHeight<>0)
  AND (BarWidth<>0)) then begin
    FTabControl.FLeftButton.Visible:=true;
    FTabControl.FRightButton.Visible:=true;
  end else begin
    FTabControl.FLeftButton.Visible:=false;
    FTabControl.FRightButton.Visible:=false;
  end;

  Logger.Send('FR', FTabControl.FRightButton.Visible);
  Logger.Send('FL', FTabControl.FLeftButton.Visible);

  Logger.ExitMethod(Self, 'OrderButtons');
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar UnFocusButton(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.UnFocusButton(Index: Integer);
begin
  if (Index < 0) or (Index >= FPageList.Count) then Exit;

  Logger.EnterMethod(Self, 'UnFocusButton('+IntToStr(Index)+')');

  with TGradTabPage(FPageList.Items[Index]).TabButton do
  begin
    case FTabPosition of
      tpTop : begin
        Top:=3;
        Height:=Self.Height-3;
      end;
      tpBottom: begin
        Top:=0;
        Height:=Self.Height-3;
      end;
      tpRight: begin
        Left := 0;
        Width:= Self.Width-3;
      end;
      tpLeft: begin
        Left := 3;
        Width:=Self.Width-3;
      end;
    end;

    if TGradTabPage(FPageList.Items[Index]).OwnerTabColor then
      Color := TGradTabPage(FPageList.Items[Index]).NormalTabColor
    else
      Color := NormalTabColor;

    UpdatePositions;
  end;

  Logger.ExitMethod(Self, 'UnFocusButton('+IntToStr(Index)+')');
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar FocusButton(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.FocusButton(Index: Integer);
var
  CurTabs : TTabs;
  DoNext : Boolean;
  c : Integer;
begin
  if csDestroying in FTabControl.ComponentState then Exit;
  if (Index < 0) or (Index >= FPageList.Count) then Exit;

  Logger.EnterMethod(Self, 'FocusButton('+IntToStr(Index)+')');

  with TGradTabPage(FPageList.Items[Index]).TabButton do
  begin
    case FTabPosition of
      tpTop, tpBottom : begin
        Top:=0;
        Height:=Self.Height;

        DoNext := ((Left+Width)>= Self.Width);
      end;
      tpRight, tpLeft: begin
        Left := 0;
        Width:= Self.Width;

        if FTabControl.LongTabs then
          Height := FTabControl.TabHeight;

        DoNext := ((Top+Height)>= Self.Height);
      end;
    end;

    if TGradTabPage(FPageList.Items[Index]).OwnerTabColor then
      Color := TGradTabPage(FPageList.Items[Index]).ActiveTabColor
    else
      Color := ActiveTabColor;

    UpdateButton;
    Invalidate;
  end;

  if not (FTabControl.FRightButton.Visible AND FTabControl.FLeftButton.Visible) then Exit;
    C := 0;

  ScrollToTab(Index);

  Logger.ExitMethod(Self, 'FocusButton('+IntToStr(Index)+')');
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar SetTabPosition(Value: TTabPosition)
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.SetTabPosition(Value: TTabPosition);

  function DbgsTabPosition(V : TTabPosition) : String;
  begin
    case V of
      tpTop : Result := 'tpTop';
      tpBottom: Result := 'tpBottom';
      tpRight: Result := 'tpRight';
      tpLeft: Result := 'tpLeft';
    end;
  end;

begin
  if FTabPosition = Value then Exit;

  {$IFDEF DEBUGTAB}
    DebugLn('Change TabPosition from %s to %s',[DbgsTabPosition(FTabPosition),DbgsTabPosition(Value)]);
  {$ENDIF}

  FTabPosition:=Value;

  OrderButtons;
end;

function TGradTabPagesBar.IsVisible(Index: Integer): Boolean;
var
   TheButton : TGradTabPageButton;
begin
   if (Index < 0) or (Index >= FPageList.Count) then Exit;

   TheButton := TGradTabPage(FPageList.Items[Index]).TabButton;

   if TabPosition in [tpTop, tpBottom] then
      Result := TheButton.Visible AND (TheButton.Left >= 0) AND (TheButton.Left+TheButton.Width <= Width+5)
   else
      Result := TheButton.Visible AND (TheButton.Top >= 0) AND (TheButton.Top+TheButton.Height <= Height+5);
end;

procedure TGradTabPagesBar.ChangeLeftTop(LastTabPosition: TTabPosition);
begin

end;

function TGradTabPagesBar.GetViewedTabs: TTabs;
var
  i,l : Integer;
begin
  {$IFDEF DEBUGTAB}
    DebugLn('GetViewedTabs');
    DebugLn('Width=%d Height=%d',[Width,Height]);
  {$ENDIF}
  for i := 0 to FPageList.Count-1 do
  begin
    with TGradTabPage(FPageList.Items[i]).TabButton do
    begin
      if ((TabPosition in [tpTop, tpBottom]) AND (Left >= 0) {AND (Left <=(Self.Width-10))} AND (Left+Width < Self.Width)) OR
        ((TabPosition in [tpLeft, tpRight]) AND (Top >= 0) {AND (Top <=(Self.Height-10))} AND (Top+Height < Self.Height)) then
      begin
        l := IncAr(Result);
        {$IFDEF DEBUGTAB}
          DebugLn('L=%d T=%d W=%d H=%d Caption=%s',[Left, Top, Width, Height, Caption]);
          DebugLn('%d. Value: %d',[l,i]);
        {$ENDIF}
        Result[l] := i;
      end;
    end;
  end;
  {$IFDEF DEBUGTAB}
    DebugLn('GetViewedTabs End');
  {$ENDIF}
end;

function TGradTabPagesBar.GetViewableTabs(FromIndex: Integer): TTabs;
var
   i,l, Last : Integer;
begin
   Last := 1;
   for i := FromIndex to FPageList.Count-1 do
   begin
      with TGradTabPage(FPageList.Items[i]).TabButton do
      begin
        case TabPosition of
             tpTop,tpBottom : begin
                 if Last + Width < Self.Width then
                 begin
                    l := IncAr(Result);
                    Result[l] := i;
                    Inc(Last, Width+1);
                 end;
             end;
             tpLeft,tpRight : begin
                 if Last + Height < Self.Height then
                 begin
                    l := IncAr(Result);
                    Result[l] := i;
                    Inc(Last, Height+1);
                 end;
             end;
        end;
      end;
   end;

end;

function TGradTabPagesBar.GetTabsOfSide(FromIndex: Integer; FromLeftSide: Boolean
  ): TTabs;
var
   i,l,fstart,fend : Integer;
begin
   if FromLeftSide then begin
       fstart := 0;
       fend:= FromIndex-1;
   end else begin
       fstart:= FromIndex+1;
       fend:= FPageList.Count-1;
   end;

   for i := fstart to fend do
   begin
      l := IncAr(Result);
      Result[l] := i;
   end;
end;

procedure TGradTabPagesBar.ScrollToTab(PIndex: Integer);
var
   CurTabs, TabsLeft, TabsRight : TTabs;
   C : Integer;
   DoNext : Boolean;
   IsInLeft, IsInRight : Boolean;
begin
   C := 0;

   if (FPageList.Count=0) OR (PIndex>=FPageList.Count) then Exit;
   if IsVisible(PIndex) then Exit;

   Logger.EnterMethod(Self, 'ScrollToTab('+IntToStr(PIndex)+')');

   CurTabs := GetViewedTabs;
   TabsLeft:= GetTabsOfSide(CurTabs[0],true);
   TabsRight:= GetTabsOfSide(CurTabs[High(CurTabs)],false);

   IsInLeft:= ValueInArray(PIndex,TabsLeft);
   IsInRight:= ValueInArray(PIndex,TabsRight);

   if IsInLeft then begin
      FShowFromButton := TabsLeft[0];
      OrderButtons;
      if IsVisible(PIndex) then
      begin
        Logger.Send('Tab is Visible', 0);
        Logger.ExitMethod(Self, 'ScrollToTab('+IntToStr(PIndex)+')');
        Exit;
      end;
   end;

   repeat
       Logger.Watch('RepeatCount', C);

       with TGradTabPage(FPageList.Items[PIndex]).TabButton do
       case FTabPosition of
           tpTop, tpBottom : DoNext := ((Left+Width)>= Self.Width);
           tpRight, tpLeft: DoNext := ((Top+Height)>= Self.Height);
       end;

       if DoNext then MoveToNext else MoveToPrior;

       Inc(C);

       Logger.Watch('IsVisible', IsVisible(PIndex));
    until(IsVisible(PIndex));

   Logger.ExitMethod(Self, 'ScrollToTab('+IntToStr(PIndex)+')');
end;

procedure TGradTabPagesBar.UpdateAllButtons;
var
  i : Integer;
begin
  for i:= 0 to FPageList.Count-1 do
  begin
    TGradTabPage(FPageList[i]).TabButton.UpdateButton;
  end;
end;

procedure TGradTabPagesBar.NewStyle;
var
  i : Integer;
begin
  for i:= 0 to FPageList.Count-1 do
  begin
    with TGradTabPage(FPageList[i]).TabButton do
    begin
      FTabControl.Style.PrepareButton(TGradTabPage(FPageList[i]).TabButton);
      if FTabControl.Style.HasTabButtonPaint then
      begin
        OnNormalBackgroundPaint:=@FTabControl.StyleTabButton;
        OnDisabledBackgroundPaint:=@FTabControl.StyleTabButton;
        OnDownBackgroundPaint:=@FTabControl.StyleTabButton;
        OnHotBackgroundPaint:=@FTabControl.StyleTabButton;

        if FTabControl.Style.HasBorderButtonPaint then
          OnBorderBackgroundPaint := @FTabControl.StyleTabButtonBorder;

        OwnerBackgroundDraw:=true;

        if FTabControl.Style.HasCloseButtonPaint then
        begin
          FCloseButton.OnNormalBackgroundPaint   := @FTabControl.StyleTabCloseButton;
          FCloseButton.OnDisabledBackgroundPaint := @FTabControl.StyleTabCloseButton;
          FCloseButton.OnDownBackgroundPaint     := @FTabControl.StyleTabCloseButton;
          FCloseButton.OnHotBackgroundPaint      := @FTabControl.StyleTabCloseButton;

          FCloseButton.OwnerBackgroundDraw := true;
          FCloseButton.UpdateButton;
        end;
      end else begin
        OwnerBackgroundDraw:=false;
      end;
    end;
  end;


end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar Create(AOwner: TComponent)
 ------------------------------------------------------------------------------}
constructor TGradTabBar.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     ControlStyle := ControlStyle+[csNoDesignSelectable];
     SetSubComponent(true);
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar Destroy
 ------------------------------------------------------------------------------}
destructor TGradTabBar.Destroy;
begin
     inherited;
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar Paint
 ------------------------------------------------------------------------------}
procedure TGradTabBar.Paint;
begin
     {Canvas.Brush.Color:=clFuchsia;
     Canvas.FillRect(0,0,Width,Height);
     }
     {$IFDEF DEBUGTAB}
        //WriteLn(Left, ' ' ,Top, ' ' ,Width, ' ' ,Height);
     {$ENDIF}

     inherited;
end;

{------------------------------------------------------------------------------
  TGradTabPages Constructor
 ------------------------------------------------------------------------------}
constructor TGradTabPages.Create(var thePageList: TListWithEvent;
theGradTabControl: TGradTabControl);
begin
  inherited Create;
  fPageList := thePageList;
  fPageList.OnChange:=@PageListChange;
  fGradTabControl:= theGradTabControl;
end;

{------------------------------------------------------------------------------
  procedure TGradTabPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
 ------------------------------------------------------------------------------}
procedure TGradTabPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
var
  APage: TGradTabPage;
begin
  if (AnAction=lnAdded) then begin
  APage:=TObject(Ptr) as TGradTabPage;
  if not (pfInserting in APage.FFlags) then
    APage.Parent:=fGradTabControl;
  end;
end;

{------------------------------------------------------------------------------
  TGradTabPages Get
 ------------------------------------------------------------------------------}
function TGradTabPages.Get(Index: Integer): String;
begin
  //Logger.Send('TGradTabPages.Get', Index);

  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TGradTabPages.Get Index out of bounds');

  Result := TGradTabPage(fPageList[Index]).Caption;
  //Logger.Send('TGradTabPages.Get', Result);
end;

{------------------------------------------------------------------------------
  TGradTabPages GetCount
 ------------------------------------------------------------------------------}
function TGradTabPages.GetCount: Integer;
begin
  Result := fPageList.Count;
end;

{------------------------------------------------------------------------------
  TGradTabPages GetObject
 ------------------------------------------------------------------------------}
function TGradTabPages.GetObject(Index: Integer): TObject;
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TGradTabPages.GetObject Index out of bounds');
  Result := TGradTabPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TGradTabPages Put
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Put(Index: Integer; const S: String);
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TGradTabPages.Put Index out of bounds');

  TGradTabPage(fPageList[Index]).Caption := S;
end;

{------------------------------------------------------------------------------
  TGradTabPages Clear
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Clear;
begin
  {$IFDEF DEBUGTAB}DebugLn('TGradTabPages.Clear Begin');{$ENDIF}
  while fPageList.Count>0 do
    Delete(fPageList.Count-1);
  {$IFDEF DEBUGTAB}DebugLn('TGradTabPages.Clear End');{$ENDIF}
end;

{------------------------------------------------------------------------------
  TGradTabPages Delete(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Delete(Index: Integer);
var
  APage: TGradTabPage;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Delete B ',FGradTabControl.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',FGradTabControl.PageIndex]);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    APage:=TGradTabPage(fPageList[Index]);

    {$IFDEF DEBUGTAB}
    DebugLn('B Parent = nil');
    {$ENDIF}
    // delete handle
    APage.Parent:=nil;
    {$IFDEF DEBUGTAB}
    DebugLn('B APage.Free');
    {$ENDIF}

    // free the page
    APage.Free;
  end;
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Delete END ',FGradTabControl.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',FGradTabControl.PageIndex]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TGradTabPages Insert(Inder: Integer; const S: String)
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Insert(Index: Integer; const S: String);
var
  NewPage: TGradTabPage;
  NewOwner: TComponent;
begin
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert A ',FGradTabControl.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  NewOwner:=FGradTabControl.Owner;
  if NewOwner=nil then
    NewOwner:=FGradTabControl;

  NewPage := TGradTabPage.Create(NewOwner);

  with NewPage do
  begin
      Caption := S;
  end;

  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert B ',FGradTabControl.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  {TODO}
  FGradTabControl.InsertPage(NewPage,Index);
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert END ',FGradTabControl.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TGradTabPages Move
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Move(CurIndex, NewIndex: Integer);
var
  APage: TGradTabPage;
  NewControlIndex, NewPageIndex: integer;
begin
  if CurIndex=NewIndex then exit;
  NewPageIndex:=NewIndex;

  APage:=TGradTabPage(fPageList[CurIndex]);

  // calculate new control index (i.e. ZOrderPosition)
  if NewIndex>=fPageList.Count-1 then
    NewControlIndex:=FGradTabControl.ControlCount-1
  else
    NewControlIndex:=FGradTabControl.GetControlIndex(TGradTabPage(fPageList[NewIndex]));

  // calculate new PageIndex
  {TODO}
  if FGradTabControl.PageIndex=CurIndex then
    NewPageIndex:=NewIndex
  else if FGradTabControl.PageIndex>CurIndex then begin
    if FGradTabControl.PageIndex<=NewIndex then
      NewPageIndex:=FGradTabControl.PageIndex-1;
  end else begin
    if FGradTabControl.PageIndex>=NewIndex then
      NewPageIndex:=FGradTabControl.PageIndex+1;
  end;

  // move Page in fPageList
  fPageList.Move(CurIndex, NewIndex);

  // move in wincontrol list
  FGradTabControl.SetControlIndex(APage,NewControlIndex);

  // update PageIndex
  FGradTabControl.PageIndex:=NewPageIndex;
end;

{------------------------------------------------------------------------------
  TGradTabControl Create(AOwner: TComponent)
 ------------------------------------------------------------------------------}
constructor TGradTabControl.Create(AOwner: TComponent);
begin
  inherited;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange:=@ImageListChange;

  FTabPosition:=tpTop;
  FAutoShowScrollButton:=true;
  fCompStyle := csNoteBook;

  ControlStyle := [{csAcceptsControls, }csDesignInteractive];
  TabStop:=true;

  FPageList := TListWithEvent.Create;
  FTabStrings := TGradTabPages.Create(TListWithEvent(FPageList), Self);
  FPageIndex:=-1;

  FTabHeight:=20;

  FPagesPopup := TPopupMenu.Create(Self);
  FPagesPopup.OnPopup:=@PopupTabs;

  FBar := TGradTabBar.Create(Self);
  FBar.Height:=FTabHeight;
  FBar.Top:=0;
  FBar.Left:=0;
  FBar.Width:=Width;
  FBar.Parent := Self;
  //FBar.Align:=alTop;

  FMoveIncrement:=1;

  FLeftButton := TGradButton.Create(Self);
  FLeftButton.Parent := FBar;
  FLeftButton.Align:= alLeft;
  FLeftButton.Caption:='<';
  FLeftButton.AutoWidth:=true;
  FLeftButton.Visible := false;
  FLeftButton.SetSubComponent(true);

  FRightButton := TGradButton.Create(Self);
  FRightButton.Parent := FBar;
  FRightButton.Align:= alRight;
  FRightButton.Caption:='>';
  FRightButton.AutoWidth:=true;
  FRightButton.Visible:= false;
  FRightButton.SetSubComponent(true);

  FPagesBar := TGradTabPagesBar.Create(Self,TListWithEvent(FPageList), Self);
  FPagesBar.Parent:=FBar;
  FPagesBar.Align:=alClient;
  FPagesBar.Left:=0;
  FPagesBar.Top:=0;
  FPagesBar.Width:=FBar.Width;
  FPagesBar.Height:=FBar.Height;

  AssignEvents(FBar);
  AssignEvents(FPagesBar);
  //AssignEvents(FRightButton);
  //AssignEvents(FLeftButton);

  FLeftButton.OnClick:=@MoveLeftTopClick;
  FRightButton.OnClick:=@MoveRightBottomClick;

  with FRightButton, FLeftButton do
  begin
    OnMouseUp:=nil;
    OnMouseDown:=nil;
    PopupMenu:=FPagesPopup;
  end;

  FRightButton.PopupMenu:=FPagesPopup;

  Height:=200;
  Width:=200;

  FStyle := TGradTabStandardStyle.Create;
end;

{------------------------------------------------------------------------------
  TGradTabControl Destroy
 ------------------------------------------------------------------------------}
destructor TGradTabControl.Destroy;
begin
    {$IFDEF DEBUGTAB}
      DebugLn('TGradTabControl.Destroy Start');
      DebugLn('B Tabs.Clear');
    {$ENDIF}
    Tabs.Clear;
    {$IFDEF DEBUGTAB}
    DebugLn('A Tabs.Clear');
    DebugLn('B FreeAndNil(FTabStrings)');
    {$ENDIF}
    FreeAndNil(FTabStrings);
    {$IFDEF DEBUGTAB}
    DebugLn('A FreeAndNil(FTabStrings)');
    DebugLn('B FreeAndNil(FPageList)');
    {$ENDIF}
    FreeAndNil(FPageList);
    {$IFDEF DEBUGTAB}
    DebugLn('A FreeAndNil(FPageList)');
    {$ENDIF}
    //FLeftButton.Free;
    //FRightButton.Free;
    {$IFDEF DEBUGTAB}
    DebugLn('TGradTabControl.Destroy End');
    {$ENDIF}

    FImageChangeLink.Destroy;

    inherited;
end;

procedure TGradTabControl.BeginUpdate;
begin
  Logger.EnterMethod(Self, 'BeginUpdate');
  FIsUpdating := True;
end;

procedure TGradTabControl.EndUpdate;
begin
  FIsUpdating:= False;

  if FPagesBar.NeedOrderButtons then
  begin
    Logger.Send('OrderButtonsNeeded', 0);
    FPagesBar.OrderButtons;
  end;
  Logger.ExitMethod(Self, 'EndUpdate');
end;

function TGradTabControl.GetTabRect(AIndex: Integer): TRect;
begin
  if (AIndex >= FPageList.Count) or (AIndex < 0) then Exit;
  with TGradTabPage(FPageList[AIndex]).TabButton do
  begin
      Result.Left:=Left;
      Result.Top:=Top;
      Result.Bottom:=Top+Height;
      Result.Right:=Left+Width;
  end;
end;

procedure TGradTabControl.AssignEvents(TheControl: TCustomControl);
begin
  if TheControl = nil then Exit;
  with TheControl do
  begin
      OnMouseWheel:=@SubMouseWheel;
      OnMouseWheelUp:=@SubMouseWheelUp;
      OnMouseWheelDown:=@SubMouseWheelDown;
      OnClick:=@SubMouseClick;
      OnDblClick:=@SubMouseDblClick;
      OnMouseMove:=@SubMouseMove;
      OnMouseDown:=@SubMouseDown;
      OnMouseUp:=@SubMouseUp;
      OnDragOver:=@SubDragOver;
      OnDragDrop:=@SubDragDrop;
  end;
end;

{------------------------------------------------------------------------------
  TGradTabControl AlignPage(APage : TGradTabPage; ARect : TRect)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.AlignPage(APage : TGradTabPage; ARect : TRect);
begin
     if APage <> nil then
     with APage do
     begin
         Left:=ARect.Left;
         Top :=ARect.Top;
         Width := ARect.Right;
         Height:= ARect.Bottom;
     end;
end;

procedure TGradTabControl.AlignPages;
var
   i : Integer;
begin
   for i := 0 to FPageList.Count-1 do
       AlignPage(TGradTabPage(FPageList.Items[i]),GetClientRect);

   UpdateAllDesignerFlags;
end;

function TGradTabControl.GetActiveTabColor: TColor;
begin
   Result := FPagesBar.ActiveTabColor;
end;

function TGradTabControl.GetNormalTabColor: TColor;
begin
   Result := FPagesBar.NormalTabColor;
end;

function TGradTabControl.GetTabPopupMenu: TPopupMenu;
begin
  Result := FPagesBar.PopupMenu;
end;

procedure TGradTabControl.ImageListChange(Sender: TObject);
begin
   UpdateTabImages;
end;

procedure TGradTabControl.SetActiveTabColor(const AValue: TColor);
begin
   FPagesBar.ActiveTabColor:= AValue;
end;

procedure TGradTabControl.SetCustomDraw(const AValue: Boolean);
begin
  if FCustomDraw=AValue then exit;
  FCustomDraw:=AValue;

  Invalidate;
end;

procedure TGradTabControl.SetNormalTabColor(const AValue: TColor);
begin
   FPagesBar.NormalTabColor:=AValue;
end;

procedure TGradTabControl.SetStyle(const AValue: TGradTabStyleBase);
begin
  if FStyle = AValue then Exit;
  if FStyle <> nil then
  begin
    FStyle.Free;
    FStyle := nil;
  end;

  FStyle := AValue;

  if FStyle = nil then
  begin
    FStyle := TGradTabStandardStyle.Create;
  end;

  // Update all Tab Paintings
  InvPaint;
  PagesBar.NewStyle;

  if FStyle.HasLeftRightButtonPaint then
  begin
    FLeftButton.OnNormalBackgroundPaint  := @FStyle.TabLeftRightButton;
    FLeftButton.OnDownBackgroundPaint    := @FStyle.TabLeftRightButton;
    FLeftButton.OnHotBackgroundPaint     := @FStyle.TabLeftRightButton;
    FLeftButton.OnBorderBackgroundPaint  := @FStyle.TabLeftRightBorderButton;

    FLeftButton.OwnerBackgroundDraw      := true;

    FRightButton.OnNormalBackgroundPaint := @FStyle.TabLeftRightButton;
    FRightButton.OnDownBackgroundPaint   := @FStyle.TabLeftRightButton;
    FRightButton.OnHotBackgroundPaint    := @FStyle.TabLeftRightButton;
    FRightButton.OnBorderBackgroundPaint := @FStyle.TabLeftRightBorderButton;

    FRightButton.OwnerBackgroundDraw     := true;
  end;
  PagesBar.UpdateAllButtons;
end;

procedure TGradTabControl.SetTabPopupMenu(const AValue: TPopupMenu);
var
  i : Integer;
begin
  FPagesBar.PopupMenu := AValue;

  for i := 0 to PageCount -1 do
    Page[i].TabButton.PopupMenu := AValue;
end;

procedure TGradTabControl.UpdateTabImages;
var
   i : Integer;
begin
  if FImages = nil then Exit;

  {$IFDEF DEBUGTAB}DebugLn('TGradTabControl.UpdateTabImages Images.Count: %d',[Images.Count]);{$ENDIF}

  for i := 0 to PageCount-1 do
      TGradTabPage(FPageList.Items[i]).UpdateImage;
end;

{------------------------------------------------------------------------------
  TGradTabControl GetCurrentPage : TGradTabPage
 ------------------------------------------------------------------------------}
function TGradTabControl.GetCurrentPage : TGradTabPage;
begin
    Result := nil;

    if FPageIndex <> -1 then
      Result := TGradTabPage(FPageList.Items[FPageIndex]);
end;

{------------------------------------------------------------------------------
  TGradTabControl GetPage(AIndex: Integer) : TGradTabPage
 ------------------------------------------------------------------------------}
function TGradTabControl.GetPage(AIndex: Integer) : TGradTabPage;
begin
  Result := nil;

  if (AIndex >= 0) AND (AIndex < FPageList.Count) then
    Result := TGradTabPage(FPageList.Items[AIndex]);
end;

{------------------------------------------------------------------------------
  TGradTabControl GetCount : Integer
 ------------------------------------------------------------------------------}
function TGradTabControl.GetCount : Integer;
begin
    Result := FPageList.Count;
end;

function TGradTabControl.GetPagesBarDragOver: TDragOverEvent;
begin
  Result := FPagesBar.OnDragOver;
end;

{------------------------------------------------------------------------------
  TGradTabControl MoveTab(Sender: TObject; NewIndex: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.MoveTab(Sender: TObject; NewIndex: Integer);
begin
   if (Sender <> nil) and (NewIndex < PageCount) then begin
    TGradTabPages(FTabStrings).Move(TGradTabPage(Sender).PageIndex,NewIndex);
   end
   else ; //raise exception?
   Change;
end;

{------------------------------------------------------------------------------
  TGradTabControl FindVisiblePage(Index: Integer): Integer
 ------------------------------------------------------------------------------}
function TGradTabControl.FindVisiblePage(Index: Integer): Integer;
begin
  for Result := Index to FPageList.Count - 1 do
    if TGradTabPage(FPageList[Result]).TabVisible then
      exit;
  // if arrived here no visible forward page was found, search backwards
  for Result := Index - 1 downto 0 do
    if TGradTabPage(FPageList[Result]).TabVisible then
      exit;
  Result := -1;
end;

{------------------------------------------------------------------------------
  TGradTabControl PageButtonMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.PageButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   AButton : TGradTabPageButton;
begin
   AButton := TGradTabPageButton(Sender);

   if Assigned(FOnTabButtonMouseDown) then
      FOnTabButtonMouseDown(Self, Button, Shift, X,Y, FPageList.IndexOf(AButton.Owner));
end;

{------------------------------------------------------------------------------
  TGradTabControl PageButtonMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.PageButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   AButton : TGradTabPageButton;
begin
   AButton := TGradTabPageButton(Sender);
   if Assigned(FOnTabButtonMouseUp) then
      FOnTabButtonMouseUp(Self, Button, Shift, X,Y, FPageList.IndexOf(AButton.Owner));
end;

{------------------------------------------------------------------------------
  TGradTabControl.PageButtonMouseClick(Sender: TObject)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.PageButtonMouseClick(Sender: TObject);
var
   AButton : TGradTabPageButton;
begin
   BeginUpdate;
   AButton := TGradTabPageButton(Sender);
   PageIndex:=FPageList.IndexOf(AButton.Owner);
   EndUpdate;

   if Assigned(FOnTabButtonClick) then
      FOnTabButtonClick(Self, FPageList.IndexOf(AButton.Owner));
end;

procedure TGradTabControl.PageCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   AButton : TGradTabPageButton;
begin
   AButton := TGradTabPageButton(TControl(Sender).Parent);

   if Assigned(FOnTabCloseButtonClick) then
      FOnTabCloseButtonClick(Self, FPageList.IndexOf(AButton.Owner));
end;

procedure TGradTabControl.PopupMouseClick(Sender: TObject);
var
   AButton : TGradTabPageButton;
begin
   AButton := Page[(Sender as TMenuItem).Tag].TabButton;
   PageButtonMouseClick(AButton);
end;

procedure TGradTabControl.PageButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   AButton : TGradTabPageButton;
begin
   AButton := TGradTabPageButton(Sender);

   if Assigned(FOnTabButtonMouseMove) then
      FOnTabButtonMouseMove(Self, Shift, X,Y, FPageList.IndexOf(AButton.Owner));
end;

procedure TGradTabControl.SetAutoShowScrollButtons(const AValue: Boolean);
begin
  if FAutoShowScrollButton=AValue then exit;
  FAutoShowScrollButton:=AValue;

  FPagesBar.OrderButtons;
end;

procedure TGradTabControl.SetImages(const AValue: TImageList);
begin
  {$IFDEF DEBUGTAB}DebugLn('TGradTabControl.SetImages FImages: ', IsAssigned(FImages));{$ENDIF}
  if FImages=AValue then exit;
  if (AValue = nil) AND (FImages<>nil) then
     FImages.UnRegisterChanges(FImageChangeLink);

  FImages:=AValue;

  if FImages <> nil then
     FImages.RegisterChanges(FImageChangeLink);

  UpdateTabImages;
end;

procedure TGradTabControl.SetLongWidth(const AValue: Integer);
begin
  {$IFDEF DEBUGTAB} DebugLn('SetLongWidth Old=%d New=%d',[FLongWidth, AValue]); {$ENDIF}
  if FLongWidth = AValue then Exit;
  {$IFDEF DEBUGTAB} DebugLn('NewLongWidth set'); {$ENDIF}
  FLongWidth:=AValue;
  SetTabPosition(TabPosition);
end;

procedure TGradTabControl.SetShowLeftTopScrollButton(const AValue: Boolean);
begin
  if FShowLeftTopScrollButton=AValue then exit;
  FShowLeftTopScrollButton:=AValue;

  FLeftButton.Visible:=AValue;
end;

procedure TGradTabControl.SetShowRightBottomScrollButton(const AValue: Boolean
  );
begin
  if FShowRightBottomScrollButton=AValue then exit;
  FShowRightBottomScrollButton:=AValue;

  FRightButton.Visible:=AValue;
end;

procedure TGradTabControl.SubMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheel) then
     OnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TGradTabControl.SubMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheelUp) then
     OnMouseWheelUp(Sender, Shift, MousePos, Handled);
end;

procedure TGradTabControl.SubMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheelDown) then
     OnMouseWheelDown(Sender, Shift, MousePos, Handled);
end;

procedure TGradTabControl.SubMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
     OnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TGradTabControl.SubMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
     OnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TGradTabControl.SubMouseClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Sender);
end;

procedure TGradTabControl.SubMouseDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Sender);
end;

procedure TGradTabControl.SubMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(OnMouseMove) then
     OnMouseMove(Sender, Shift, X, Y);
end;

procedure TGradTabControl.SubDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
     OnDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TGradTabControl.SubDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
     OnDragDrop(Sender, Source, X, Y);
end;

procedure TGradTabControl.StyleTabButton(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  if FStyle.HasTabButtonPaint then
  FStyle.TabButton(Self, FPageList.IndexOf(Sender.Owner), Sender,
    TargetCanvas, R, BState);
end;

procedure TGradTabControl.StyleTabCloseButton(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  if FStyle.HasCloseButtonPaint then
  FStyle.TabCloseButton(Self, FPageList.IndexOf(Sender.Owner), Sender,
    TargetCanvas, R, BState);
end;

procedure TGradTabControl.StyleTabButtonBorder(Sender: TGradButton;
  TargetCanvas: TCanvas; R: TRect; BState: TButtonState);
begin
  if FStyle.HasBorderButtonPaint then
  FStyle.TabButtonBorder(Self, FPageList.IndexOf(Sender.Owner), Sender,
    TargetCanvas, R, BState);
end;

procedure TGradTabControl.PopupTabs(Sender: TObject);
var
   tempMenu : TMenuItem;
   i : Integer;
begin
  with FPagesPopup.Items do begin
      Clear;

      for i := 0 to PageCount-1 do begin
          tempMenu := TMenuItem.Create(FPagesPopup);

          tempMenu.Caption:=Page[i].Caption;
          tempMenu.OnClick:=@PopupMouseClick;
          tempMenu.Tag:=i;

          //DebugLn('I=%d OnClick-Assigned=%s',[i,BoolStr(Assigned(Page[i].TabButton.OnClick))]);

          Add(tempMenu);
      end;
  end;
end;

procedure TGradTabControl.MoveLeftTopClick(Sender: TObject);
begin
  PagesBar.MoveToPrior;
end;

procedure TGradTabControl.MoveRightBottomClick(Sender: TObject);
begin
  PagesBar.MoveToNext;
end;

{------------------------------------------------------------------------------
  TGradTabControl PageRemoved(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.PageRemoved(Index: Integer);
var
  NewPageIndex: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    // if this page is showing, then show the next page before deleting it
    if Index = FPageIndex then
    begin
      NewPageIndex := FindVisiblePage(Index);
      {$IFDEF DEBUGTAB}
              DebugLn('TGradTabControl.PageRemoved Index: %d NewPageIndex: %d',[Index, NewPageIndex]);
      {$ENDIF}
      if NewPageIndex >= 0 then
        PageIndex := NewPageIndex
      else
        FPageIndex := NewPageIndex;
    end;

    FPagesBar.OrderButtons;
  end;
end;

{------------------------------------------------------------------------------
  TGradTabControl SetCurrentPage(Value : TGradTabPage)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetCurrentPage(Value : TGradTabPage);
begin
    if FPageList.IndexOf(Value) = -1 then Exit;

    SetCurrentPageNum(FPageList.IndexOf(Value));
end;

{------------------------------------------------------------------------------
  TGradTabControl SetCurrentPageNum(Value: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetCurrentPageNum(Value: Integer);
var
  Last : Integer;
begin
  FPagesBar.OrderButtons;

  if (Value<0) or (Value>=fPageList.Count) then Exit;
  if FPageIndex=Value then Exit;
  if not Page[Value].Enabled then Exit;
  if not Page[Value].TabVisible then Exit;

  Last := FPageIndex;

  // Set PageIndex here, that Un/FocusButton called by Un/ShowPage can
  // read the current PageIndex to draw the right Style :)
  FPageIndex := Value;

  if Last <> -1 then UnShowPage(Last);

  ShowPage(Value);

  UpdateAllDesignerFlags;

  if ([csDesigning, csLoading, csDestroying] * ComponentState = [])
  and Assigned(OnPageChanged) then
    OnPageChanged(Self);
end;

procedure TGradTabControl.SetPagesBarDragOver(const AValue: TDragOverEvent);
begin
  FPagesBar.OnDragOver:=AValue;
end;

{------------------------------------------------------------------------------
  TGradTabControl ShowPage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.ShowPage(Index: Integer);
begin
  Logger.EnterMethod(Self, 'ShowPage('+IntToStr(Index)+')');

  // Focus the TabButton
  FPagesBar.FocusButton(Index);

  // Enable Page
  with TGradTabPage(FPageList.Items[Index]) do
  begin
     Visible:=true;
     BringToFront;
  end;

  UpdateDesignerFlags(Index);

  AlignPage(TGradTabPage(FPageList.Items[Index]), GetClientRect);
  Logger.ExitMethod(Self, 'ShowPage('+IntToStr(Index)+')');
end;

{------------------------------------------------------------------------------
  TGradTabControl UnShowPage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.UnShowPage(Index: Integer);
begin
  // Disable Page

  if (Index<0) or (Index>=fPageList.Count) then Exit;

  Logger.EnterMethod(Self, 'UnShowPage('+IntToStr(Index)+')');

  // Unfocus the TabButton
  FPagesBar.UnFocusButton(Index);

  UpdateDesignerFlags(Index);

  with TGradTabPage(FPageList.Items[Index]) do
    Visible:=false;

  Logger.ExitMethod(Self, 'UnShowPage('+IntToStr(Index)+')');
end;

{------------------------------------------------------------------------------
  TGradTabControl function ChildClassAllowed(ChildClass: TClass): boolean;
 ------------------------------------------------------------------------------}
function TGradTabControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
    {$IFDEF DEBUGTAB}
            DebugLn('TGradTabControl.ChildClassAllowed ',ChildClass.ClassName);
    {$ENDIF}

    Result := (ChildClass<>nil);
    Result := ChildClass.InheritsFrom(TGradTabPage) AND Result;
    if Result then Exit;

    if GetCurrentPage=nil then begin
       Result := ChildClass.InheritsFrom(TGradTabBar);
    end else begin
       if (ChildClass.InheritsFrom(TControl)) AND (NOT ChildClass.InheritsFrom(TGradTabPage)) then begin
          TControl(ChildClass.ClassParent).Parent := GetCurrentPage;
          Result := true;
       end;
    end;
end;

{------------------------------------------------------------------------------
  TGradTabControl InsertPage(APage: TGradTabPage; Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.InsertPage(APage: TGradTabPage; Index: Integer);
var
  NewZPosition: integer;
begin
  if FPageList.IndexOf(APage)>=0 then exit;
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabControl.InsertPage A ',dbgsName(Self),' Index=',Index,' Name=',
    APage.Name,' Caption=',APage.Caption]);
  {$ENDIF}
  APage.DisableAlign;
  try
    if Index<FPageList.Count then
      NewZPosition:=GetControlIndex(TGradTabPage(fPageList[Index]))
    else
      NewZPosition:=-1;
    Include(APage.FFlags,pfInserting);
    FPageList.Insert(Index,APage);
    Exclude(APage.FFlags,pfInserting);
    APage.Parent := Self;

    if APage.Caption = '' then
       APage.Caption:=APage.Name;

    FPagesBar.InsertButton(APage.TabButton, Index);

    if NewZPosition>=0 then
      SetControlIndex(APage,NewZPosition);
    if PageIndex = -1 then
      FPageIndex := Index;

    {$IFDEF DEBUGTAB}DebugLn('APage.Name empty: %s',[BoolToStr(APage.Name='',true)]);{$ENDIF}

    if HandleAllocated and (not (csLoading in ComponentState)) then begin
      AddRemovePageHandle(APage);
      if PageIndex = Index then
        ShowCurrentPage;
    end;
  finally
    APage.EnableAlign;

    AlignPage(APage, GetClientRect);
    SetCurrentPageNum(Index);
  end;

  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabControl.InsertPage END ',dbgsName(Self),' Index=',
    Index,' Name=',APage.Name,' Caption=',APage.Caption]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TGradTabControl RemovePage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.AddRemovePageHandle(APage: TGradTabPage);
begin
   if (not (csDestroying in APage.ComponentState))
  and (APage.TabVisible or (csDesigning in ComponentState)) then begin
    {$IFDEF NOTEBOOK_DEBUG}
    DebugLn(['TGradTabControl.AddRemovePageHandle ADD ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
    {$ENDIF}
    if (pfAdded in APage.FFlags) then exit;
    Include(APage.FFlags,pfAdding);
    APage.FFlags:=APage.FFlags+[pfAdded]-[pfAdding];
    APage.AdjustSize;
  end else begin
    {$IFDEF NOTEBOOK_DEBUG}
    DebugLn(['TGradTabControl.AddRemovePageHandle REMOVE ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
    {$ENDIF}
    if not (pfAdded in APage.FFlags) or (pfRemoving in APage.FFlags) then
      exit;
    APage.FFlags := APage.FFlags - [pfAdded] + [pfRemoving];
    if APage.HandleAllocated then
      APage.DestroyHandle;
    Exclude(APage.FFlags, pfRemoving);
  end;
end;

{------------------------------------------------------------------------------
  TGradTabControl RemovePage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.RemovePage(Index: Integer);
var
  APage: TGradTabPage;
begin
  if (Index >= 0) and (Index < FPageList.Count) then
  begin
    APage:=TGradTabPage(fPageList[Index]);
    APage.FTabVisible:=false; { TODO }
    if HandleAllocated then
      AddRemovePageHandle(APage); { TODO?}

    PageRemoved(Index);
    FPageList.Delete(Index);

    APage.Parent:=nil;

    if FPageIndex >= Index then
      Dec(FPageIndex);

    FPagesBar.OrderButtons;
  end;
end;

{------------------------------------------------------------------------------
  TGradTabControl InvPaint
 ------------------------------------------------------------------------------}
procedure TGradTabControl.InvPaint;
begin
  if csDesigning in ComponentState then
    Invalidate
  else
    Paint;
end;

{------------------------------------------------------------------------------
  TGradTabControl SetTabHeight(Value: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetTabHeight(Value: Integer);
begin
    if FTabHeight = Value then Exit;

    FTabHeight:=Value;
    
    SetTabPosition(TabPosition);
end;

{------------------------------------------------------------------------------
  TGradTabControl SetTabs(Value: TStrings)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetTabs(Value: TStrings);
begin
    FTabStrings.Assign(Value);
end;

{------------------------------------------------------------------------------
  TGradTabControl SetTabPosition(Value : TTabPosition)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetTabPosition(Value : TTabPosition);
var
  tempSize : Integer;
begin
  FTabPosition:=Value;

  tempSize:=FTabHeight;

  {$IFDEF DEBUGTAB}
    DebugLn('TGradTabControl.SetTabPosition Before');
    DebugLn('FBar Left %d Top %d Height %d Width %d',[ FBar.Left, Fbar.Top, FBar.Height, FBar.Width]);
    DebugLn('FPagesBar Left %d Top %d Height %d Width %d',[ FPagesBar.Left, FPagesbar.Top, FPagesBar.Height, FPagesBar.Width]);
    DebugLn('Control Left %d Top %d Height %d Width %d',[ Left, Top, Height, Width]);
  {$ENDIF}
    
  FPagesBar.TabPosition:=Value;

  case Value of
    tpTop: begin
      FBar.Height:=FTabHeight;
      FBar.Top:=0;
      FBar.Left:=0;
      FBar.Width:=Width;
      //FBar.Align:=alTop;
    end;
    tpLeft:begin
      FBar.Height:=Height;
      FBar.Top:=0;
      FBar.Left:=0;
      FBar.Width:=GetTabBarSize(tpLeft);
      //FBar.Align:=alLeft;
    end;
    tpBottom:begin
      FBar.Height:=tempSize;
      FBar.Top:=Height-tempSize;
      FBar.Left:=0;
      FBar.Width:=Width;
      //FBar.Align := alBottom;
    end;
    tpRight:begin
      FBar.Left:=Width-GetTabBarSize(tpRight);
      FBar.Top:=0;
      FBar.Height:=Height;
      FBar.Width:=GetTabBarSize(tpRight);
      //FBar.Align:=alRight;
    end;
  end;

  //FPagesBar.Align:=alClient;

  case Value of
    tpTop: begin
      FLeftButton.Align:=alLeft;
      FLeftButton.RotateDirection:=rdNormal;
      FLeftButton.AutoWidth:=true;
      FLeftButton.AutoHeight:=false;
      FRightButton.Align:=alRight;
      FRightButton.RotateDirection:=rdNormal;
      FRightButton.AutoWidth:=true;
      FRightButton.AutoHeight:=false;
      //FBar.Align:=alTop;
    end;
    tpLeft:begin
      FLeftButton.Align:=alTop;
      FLeftButton.RotateDirection:=rdRight;
      FLeftButton.AutoWidth:=false;
      FLeftButton.AutoHeight:=true;
      FRightButton.Align:=alBottom;
      FRightButton.RotateDirection:=rdRight;
      FRightButton.AutoWidth:=false;
      FRightButton.AutoHeight:=true;
      //FBar.Align:=alLeft;
    end;
    tpBottom:begin
      FLeftButton.Align:=alLeft;
      FLeftButton.RotateDirection:=rdNormal;
      FLeftButton.AutoWidth:=true;
      FLeftButton.AutoHeight:=false;
      FRightButton.Align:=alRight;
      FRightButton.RotateDirection:=rdNormal;
      FRightButton.AutoWidth:=true;
      FRightButton.AutoHeight:=false;
      //FBar.Align := alBottom;
    end;
    tpRight:begin
      FLeftButton.Align:=alTop;
      FLeftButton.RotateDirection:=rdRight;
      FLeftButton.AutoWidth:=false;
      FLeftButton.AutoHeight:=true;
      FRightButton.Align:=alBottom;
      FRightButton.RotateDirection:=rdRight;
      FRightButton.AutoWidth:=false;
      FRightButton.AutoHeight:=true;
    //FBar.Align:=alRight;
    end;
  end;

  if not FPagesBar.IsVisible(FPageIndex) then
    FPagesBar.FocusButton(FPageIndex);

  {$IFDEF DEBUGTAB}
    DebugLn('TGradTabControl.SetTabPosition After');
    DebugLn('FBar Left %d Top %d Height %d Width %d',[ FBar.Left, Fbar.Top, FBar.Height, FBar.Width]);
    DebugLn('FPagesBar Left %d Top %d Height %d Width %d',[ FPagesBar.Left, FPagesbar.Top, FPagesBar.Height, FPagesBar.Width]);
    DebugLn('Control Left %d Top %d Height %d Width %d',[ Left, Top, Height, Width]);
  {$ENDIF}

  AlignPages;

  FPagesBar.UpdateAllButtons;
  InvPaint;
end;

{------------------------------------------------------------------------------
  TGradTabControl SetLongTabs(Value : Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetLongTabs(Value : Boolean);
begin
  if FLongTabs=Value then Exit;
  FLongTabs:=Value;

  SetTabPosition(TabPosition);
end;

{------------------------------------------------------------------------------
  TGradTabControl Update
 ------------------------------------------------------------------------------}
procedure TGradTabControl.Change;
begin
    ShowCurrentPage;
    fPageIndexOnLastChange := fPageIndex;
    if ([csLoading,csDestroying]*ComponentState=[])
    then begin
      if Assigned(fOnPageChanged) then fOnPageChanged(Self);
    end;
end;

procedure TGradTabControl.UpdateTabProperties;
begin

end;

{------------------------------------------------------------------------------
  TGradTabControl AddPage(AName: String) : Integer
 ------------------------------------------------------------------------------}
function TGradTabControl.AddPage(AName: String) : Integer;
begin
  Result := FTabStrings.Add(AName);
end;

{------------------------------------------------------------------------------
  TGradTabControl AddPage(APage: TGradTabPage) : Integer
 ------------------------------------------------------------------------------}
function TGradTabControl.AddPage(APage: TGradTabPage) : Integer;
begin
  Result := FPageList.Count;
  FPageList.Insert(Result, APage);
end;

function TGradTabControl.GetTabBarSize(TabPos: TTabPosition): Integer;
begin
   if TabPos in [tpTop,tpBottom] then
      Result := TabHeight
   else if LongTabs then
      Result := LongWidth
   else
      Result := TabHeight;
end;

{------------------------------------------------------------------------------
  TGradTabControl GetClientRect: TRect
 ------------------------------------------------------------------------------}
function TGradTabControl.GetClientRect: TRect;
var
   tempR : TRect;
begin
   tempR := Inherited;

   case FTabPosition of
      tpTop: begin
         tempR.Top:=FTabHeight+2;
         tempR.Left:=2;
         tempR.Right:=Width-2;
         tempR.Bottom:=Height-2;
      end;
      tpBottom: begin
         tempR.Top:=2;
         tempR.Left:=2;
         tempR.Right:=Width-2;
         tempR.Bottom:=Height-FTabHeight-2;
      end;
      tpRight: begin
         tempR.Top:=2;
         tempR.Left:=2;
         tempR.Right:=Width-GetTabBarSize(tpRight)-2;
         tempR.Bottom:=Height-2;
      end;
      tpLeft: begin
         tempR.Top:=2;
         tempR.Left:=GetTabBarSize(tpLeft)+2;
         tempR.Right:=Width-2;
         tempR.Bottom:=Height-2;
      end;
   end;

   Result := tempR;
end;

{------------------------------------------------------------------------------
  TGradTabControl Paint
 ------------------------------------------------------------------------------}
procedure TGradTabControl.Paint;
begin
  FStyle.TabControl(Self, Self.Canvas);
end;

{------------------------------------------------------------------------------
  TGradTabControl Resize
 ------------------------------------------------------------------------------}
procedure TGradTabControl.Resize;
begin
  inherited;

  {$IFDEF DEBUGTAB}
    DebugLn('TGradTabControl.Resize HasParent %s FPageList.Count %d',[BoolToStr(HasParent,true), FPageList.Count]);
  {$ENDIF}
     
  if HasParent and (FPageList.Count<>0) then
    AlignPage(GetCurrentPage, GetClientRect);

        
  case FTabPosition of
        tpTop: begin
            FBar.Width:=Width;
            FBar.Top:=0;
        end;
        tpBottom: begin
            FBar.Width:=Width;
            FBar.Top:=Height-FTabHeight;
        end;
        tpLeft: begin
            FBar.Height:=Height;
            FBar.Left:=0;
            FBar.Top:=0;
        end;
        tpRight: begin
            FBar.Height:=Height;
            FBar.Left:=Width-GetTabBarSize(tpRight);
            FBar.Top:=0;
        end;
     end;
end;

{------------------------------------------------------------------------------
  procedure TGradTabControl.UpdateAllDesignerFlags;
 ------------------------------------------------------------------------------}
procedure TGradTabControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i:=0 to PageCount-1 do
    UpdateDesignerFlags(i);
end;

{------------------------------------------------------------------------------
  TGradTabControl UpdateDesignerFlags(APageIndex: integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.UpdateDesignerFlags(APageIndex: integer);
begin
  {$IFDEF DEBUGTAB}
     DebugLn('TGradTabControl.UpdateDesignerFlags: Index: %d Current: %d Assigned: %s',[APageIndex, FPageIndex,BoolToStr(Assigned(Page[APageIndex]),true)]);
  {$ENDIF}

  if APageIndex<>FPageIndex then
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle+[csNoDesignVisible{,csNoDesignSelectable}]
  else
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle-[csNoDesignVisible{,csNoDesignSelectable}];

  {$IFDEF DEBUGTAB} DebugLn('TGradTabControl.UpdateDesignerFlags End'); {$ENDIF}

end;

{------------------------------------------------------------------------------
  TGradTabControl.ShowCurrentPage
 ------------------------------------------------------------------------------}
procedure TGradTabControl.ShowCurrentPage;
var
  CurPage: TGradTabPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < FPageList.Count) then
  begin
    CurPage:=Page[FPageIndex];
    // first make the new page visible
    {$IFDEF DEBUGTAB}DebugLn(['TGradTabControl.ShowCurrentPage ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible]);{$ENDIF}
    if CurPage.Visible then begin
      if FPageIndexOnLastShow<>fPageIndex then begin
        // some widgetsets like win32/64 do not send WM_SIZE messages for
        // hidden pages. Force resizing page (it is alClient).
        {$IFDEF DEBUGTAB}
         DebugLn(['TGradTabControl.ShowCurrentPage ',dbgsName(Self),' ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible,' BoundsRect=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CurPage.BoundsRect=',dbgs(CurPage.BoundsRect),' CurPage.ClientRect=',dbgs(CurPage.ClientRect)]);
        {$ENDIF}
        ReAlign;
        // TCustomPage.IsControlVisible is overriden
        // therefore AutoSizing of childs was skipped => do it now
        CurPage.ReAlign;
      end;
    end else begin
      CurPage.Visible := true;
    end;
  end;
end;

{ TFormPage }

constructor TFormPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FShowPageAtDestroy:=false;
  FDestroyPageAtDestroy:=false;
end;

destructor TFormPage.Destroy;
var
   FOld : TCustomForm;
begin
  if FDestroyPageAtDestroy AND (FOldForm <> nil) then begin
     FOld := FOldForm;
     PageToForm(false);
     FOld.Free;
     FOld := nil;
  end else
  PageToForm(FShowPageAtDestroy);

  inherited Destroy;
end;

procedure TFormPage.FormToPage(TheForm: TCustomForm);
var
   i : Integer;
begin
   if FOldForm = TheForm then Exit;
   FOldForm := TheForm;

   for i := TheForm.ControlCount-1 downto 0 do begin
       TheForm.Controls[i].Parent := Self;
   end;

   Caption:=TheForm.Caption;

   if TheForm.Visible then
      TheForm.Close;
end;

procedure TFormPage.PageToForm(AShow : Boolean);
var
   i : Integer;
begin
   for i := ControlCount-1 downto 0 do begin
       Controls[i].Parent := FOldForm;
   end;

   if AShow then
      FOldForm.Show;
   FOldForm := nil;
end;

{ TGradTabCloseButton }

constructor TGradTabCloseButton.Create(AOwner: TComponent);
var
  tempPic : TPicture;
begin

  //FOwnerBackgroundDraw:=true;

  inherited Create(AOwner);

  try
    tempPic := TPicture.Create;

    tempPic.LoadFromLazarusResource('btn_cancel');

    Glyph.Assign(tempPic.Graphic);
  finally
    tempPic.Free;
  end;

  ShowGlyph:=true;
  BorderSides:=[];
  //Color:=clRed;

  SetSubComponent(true);
end;

initialization
  {$I ugradtabcontrol.lrs}

end.

