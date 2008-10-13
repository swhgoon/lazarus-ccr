unit ugradtabcontrol; 

{-------------------------------------------------------------------------------
 @name GradTabControl
 @author Eugen Bolz
 @lastchange 07.10.2008
 @version 0.1
 @comments TGradTabControl is based on TNotebook/TPageControl/TTabControl
 @license http://creativecommons.org/licenses/LGPL/2.1/
 @todo:
    - If a Button isnt visible but focused the bar should move to the button - working
    - Close Button at Tabs
    - Drawer needed or first style wishes
    - Button Events accessable from Page-Events
    - TabBar Events accessable from TGradTabControl
    - Maybe rename TGradTabControl to TCustomPageControl and of these
       TGradTabControl and TGradPageControl
 ------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

{.$DEFINE DEBUGTAB}

interface

uses
  Classes,LResources, SysUtils, Menus, LCLType,
  LCLProc, ExtCtrls, Graphics, ugradbtn, Controls, uRotateBitmap,
  Buttons;

type
  TGradTabControl = class;

  { TGradTabPageButton }

  TGradTabPageButton = class( TGradButton )
  private
       FCloseButton : TGradButton;
       FShowCloseButton : Boolean;
       procedure AlignCloseButton;
       procedure SetShowCloseButton(AValue: Boolean);
  protected
       procedure SetRotateDirection(const Value: TRotateDirection); override;
       procedure RealSetText(const Value: TCaption); override;
       procedure SetAutoHeightBorderSpacing(const AValue: Integer); override;
       procedure SetAutoWidthBorderSpacing(const AValue: Integer); override;
       procedure SetShowGlyph(const Value: Boolean); override;
  public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Resize; override;
       procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
       property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
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
       FButton : TGradTabPageButton;
       FCaption: TCaption;
       FGradTabControl : TGradTabControl; //Maybe needed ^.^
       FFlags: TPageFlags;
       FImageIndex: Integer;
       FTabVisible,FCurrentlyDestroying,FShowCloseButton : Boolean;
       function GetTabButtonLayout: TButtonLayout;
       function GetTabColor: TColor;
       function GetTabGlyph: TBitmap;
       function GetTabShowGlyph: Boolean;
       function GetTabTextAlignment: TTextAlignment;
       function GetTabPopupMenu : TPopupMenu;
       function GetText : TCaption;
       procedure SetImageIndex(const AValue: Integer);
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
       procedure SetButton(Value : TGradTabPageButton); //Later dont needed
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
       //property ControlState;
       //property ControlStyle;
       property TabVisible : Boolean read FTabVisible write SetTabVisible default true;
       property PageIndex : Integer read GetPageIndex write SetPageIndex;
       property Caption : TCaption read GetText write SetText;
       property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
       property TabPopupMenu : TPopupMenu read GetTabPopupMenu write SetTabPopupMenu;
       property Color;
       property TabColor : TColor read GetTabColor write SetTabColor;
       property TabTextAlignment : TTextAlignment read GetTabTextAlignment write SetTabTextAlignment;
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
  end;
  
  { TGradTabPagesBar }

  TTabs = Array of Integer;

  {
      @name TGradTabPagesBar
      @comments Shows and Order the TabButtons
  }
  TGradTabPagesBar = class(TCustomControl)
  private
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
      procedure ScrollToTab(PIndex : Integer);
  public
      constructor Create(AOwner: TComponent; var thePageList: TListWithEvent;
       TheTabControl : TGradTabControl);
      procedure Paint; override;
      procedure MoveToNext;
      procedure MoveToPrior;
      procedure MoveTo(Num: Integer);
      procedure MoveToNorm;
      property TabPosition : TTabPosition read FTabPosition write SetTabPosition;
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
      //destructor Destroy; override;
  end;
  
  //Verwaltet die extra Buttons ( wie weiter/zurück )
  //Verschiebt die ansicht von TGradTabPagesBar
  //irgendwann mit effekt
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
    FTabList : TListWithEvent;
    FGradTabControl : TGradTabControl;
    procedure PageListChange(Ptr: Pointer; AnAction: TListNotification);
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(var thePageList: TListWithEvent;
       var theTabList: TListWithEvent;
       theGradTabControl: TGradTabControl);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    //function InsertPage(APage : TGradTabPage) : Integer;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;
  
  { TGradTabControl }

  TGradTabControl = class(TCustomControl)
  private
        FAutoShowScrollButton: Boolean;
        FImages: TImageList;
        FMoveIncrement: Integer;
        FLeftButton, FRightButton : TGradButton;
        FOnPageChanged: TNotifyEvent;
        FShowLeftTopScrollButton: Boolean;
        FShowRightBottomScrollButton: Boolean;
        FTabStrings : TStrings; //TGradTabPages
        FPageList: TList; //Is Managed by TGradTabPages
        FTabList : TList; //Also ^^
        FOnTabButtonClick : TGradTabPageButtonClickEvent;
        FOnTabButtonMouseDown,
         FOnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent;
        FOnTabButtonMouseMove : TGradTabPageButtonMouseMoveEvent;
        FPageIndex, fPageIndexOnLastChange, fPageIndexOnLastShow,
        FTabHeight, FLongWidth : Integer;
        FBar : TGradTabBar;
        FPagesBar: TGradTabPagesBar;
        FPagesPopup : TPopupMenu;
        FTabPosition : TTabPosition;
        FLongTabs : Boolean;
        procedure AssignEvents(TheControl : TCustomControl);
        procedure AlignPage(APage : TGradTabPage; ARect : TRect);
        procedure AlignPages;
        procedure ImageListChange(Sender: TObject);
        procedure UpdateTabImages;
        //procedure AddRemovePageHandle(APage: TGradTabPage);
        //procedure DoSendPageIndex;
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
        procedure SubMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
        procedure SubDragOver(Sender, Source: TObject;
               X,Y: Integer; State: TDragState; var Accept: Boolean);
        procedure SubDragDrop(Sender, Source: TObject; X,Y: Integer);
        //End
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
        function GetTabRect(AIndex : Integer) : TRect;
        //function GetTabAtPoint(TabPoint : TPoint) : T
        function AddPage(AName: String) : Integer;
        function AddPage(APage: TGradTabPage) : Integer;
        function GetTabBarSize(TabPos : TTabPosition) : Integer;
        function GetClientRect: TRect; override;
        procedure Paint; override;
        procedure Resize; override;
        procedure UpdateAllDesignerFlags;
        procedure UpdateDesignerFlags(APageIndex: integer);

        //Old - will be erased soon ^^
        property Pages[Index: Integer] : TGradTabPage read GetPage;
        property Page[Index: Integer] : TGradTabPage read GetPage;
        property Bar : TGradTabBar read FBar;
        property PagesBar : TGradTabPagesBar read FPagesBar;
        property PageList: TList read FPageList;
        property Tabs : TStrings read FTabStrings write SetTabs;
        property PageCount : Integer read GetCount;
  published
        property Align;
        property ControlState;
        property ControlStyle;
        property ActivePage : TGradTabPage read GetCurrentPage write SetCurrentPage;
        property OnTabButtonClick : TGradTabPageButtonClickEvent read FOnTabButtonClick write FOnTabButtonClick;
        property OnTabButtonMouseDown : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseDown write FOnTabButtonMouseDown;
        property OnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseUp write FOnTabButtonMouseUp;
        property OnTabButtonMouseMove : TGradTabPageButtonMouseMoveEvent read FOnTabButtonMouseMove write FOnTabButtonMouseMove;
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
        //On*- PagesBar Events
        property OnPagesBarDragOver : TDragOverEvent read GetPagesBarDragOver write SetPagesBarDragOver;
        //End

        property PageIndex : Integer read FPageIndex write SetCurrentPageNum;
        property TabHeight : Integer read FTabHeight write SetTabHeight;
        property TabPosition : TTabPosition read FTabPosition write SetTabPosition default tpTop;
        property LongTabs : Boolean read FLongTabs write SetLongTabs;
        property LongWidth: Integer read FLongWidth write SetLongWidth;
        property MoveIncrement : Integer read FMoveIncrement write FMoveIncrement;
        property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
        property AutoShowScrollButtons : Boolean read FAutoShowScrollButton write SetAutoShowScrollButtons;
        property ShowLeftTopScrollButton : Boolean read FShowLeftTopScrollButton write SetShowLeftTopScrollButton;
        property ShowRightBottomScrollButton : Boolean read FShowRightBottomScrollButton write SetShowRightBottomScrollButton;
        property Images : TImageList read FImages write SetImages;

        //property ShowTabs : Boolean; { TODO }
  end;
  
  procedure Register;
  function IsAssigned(var Obj : TObject) : String;
  function BoolStr(BV : Boolean) : String;

implementation

uses
    gradtabcontroleditor, ComponentEditors;

const
     FPageCount : Integer = 0;

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
   {$IFDEF DEBUGTAB}
   DebugLn('ValueInArray: Needle=%d Low=%d High=%d',[Needle, Low(Stack), High(Stack)]);
   {$ENDIF}
   for i := Low(Stack) to High(Stack) do
   if Needle =Stack[i] then
   begin
      Result := true;
      Exit;
   end;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton Create(AOwner: TComponent
 ------------------------------------------------------------------------------}
constructor TGradTabPageButton.Create(AOwner: TComponent);
var
   tempPic : TPicture;
begin
     inherited Create(AOwner);

     ControlStyle := ControlStyle+[csNoDesignSelectable,csDesignInteractive]-[csCaptureMouse];
     FCloseButton := TGradButton.Create(Self);
     FCloseButton.Width:=12;
     FCloseButton.Height:=12;
     TextAlignment:=taCenter;
     FCloseButton.Left:=1;
     FCloseButton.Top:=1;
     FCloseButton.Caption:='';
     //FCloseButton.Glyph := TBitmap(CreateBitmapFromLazarusResource('close_btn'));
     FCloseButton.ShowGlyph:=true;

     try
        tempPic := TPicture.Create;

        tempPic.LoadFromLazarusResource('close_btn');

        FCloseButton.Glyph.Assign(tempPic.Graphic);
     finally
        tempPic.Free;
     end;

     FCloseButton.BorderSides:=[];
     FCloseButton.Color:=clRed;
     //FCloseButton.Visible:=false;
     //FCloseButton.Parent := Self;

     FShowCloseButton:=false;

     SetSubComponent(true);
end;

destructor TGradTabPageButton.Destroy;
begin
  FCloseButton.Free;

  inherited Destroy;
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

   //FCloseButton.Color:=clRed;

   //FCloseButton.Top:=1;
   //FCloseButton.Left:=1;

   GetBackgroundRect(TheRect);

   DisplayWidth:= TheRect.Right-TheRect.Left;
   DisplayHeight:=TheRect.Bottom-TheRect.Top;

   case RotateDirection of
      rdNormal: begin
        NewTop:=(DisplayHeight div 2)-(FCloseButton.Height div 2)-1;
        NewLeft:=DisplayWidth-(FCloseButton.Width)+1;
      end;
      rdRight: begin
        NewTop:=DisplayHeight-(FCloseButton.Height)+1;
        NewLeft:=(DisplayWidth div 2)-(FCloseButton.Width div 2);
        //FCloseButton.Left:=TheRect.Left+((TheRect.Right-TheRect.Left) div 2)-(FCloseButton.Width div 2);
      end;
      rdLeft:  begin
        NewTop:=1;
        NewLeft:=(DisplayWidth div 2)-(FCloseButton.Width div 2);
        //FCloseButton.Top:=TheRect.Top;
      end;
   end;

   FCloseButton.Top:=NewTop+TheRect.Top;
   FCloseButton.Left:=NewLeft+TheRect.Left;
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton SetShowCloseButton(AValue: Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.SetShowCloseButton(AValue: Boolean);
begin
    if AValue = FShowCloseButton then Exit;
    FShowCloseButton:=AValue;

    //FCloseButton.Visible:=AValue;

    if AValue then
    begin
        AlignCloseButton;
        FCloseButton.Parent:=Self;
    end else begin
        //Caption:=Caption-'    ';
        FCloseButton.Parent:=nil;
    end;
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
  {if Parent = nil then Exit;
  ParentControl := TGradTabPagesBar(Parent).FTabControl;

  if ParentControl.TabPosition in [tpLeft, tpRight] then
  begin
       if ParentControl.LongTabs then
       begin
            if ParentControl.LongWidth <> aWidth then
            if ParentControl.LongWidth < aWidth then
            begin
                ParentControl.LongWidth:=aWidth;
            end;
       end;
  end;}
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;

{-------------------------------------------------------------------------------
  TGradTabPageButton SetRotateDirection(const Value: TRotateDirection)
 ------------------------------------------------------------------------------}
procedure TGradTabPageButton.SetRotateDirection(const Value: TRotateDirection);
begin
    inherited;

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
   NewCaption:=Value;

   {if FShowCloseButton then
      NewCaption := NewCaption+'    ';}

   inherited RealSetText(NewCaption);

   AlignCloseButton;

   {$IFDEF DEBUGTAB}
   DebugLn('SetText ',BoolToStr(Assigned(Parent),true),
     BoolToStr((Parent is TGradTabPagesBar),true) );
   {$ENDIF}
     
   if Assigned(Parent) AND (Parent is TGradTabPagesBar) then
      (Parent as TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.SetAutoHeightBorderSpacing(const AValue: Integer);
begin
  inherited SetAutoHeightBorderSpacing(AValue);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.SetAutoWidthBorderSpacing(const AValue: Integer);
begin
  inherited SetAutoWidthBorderSpacing(AValue);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
end;

procedure TGradTabPageButton.SetShowGlyph(const Value: Boolean);
begin
  inherited SetShowGlyph(Value);

  if Parent <> nil then (Parent AS TGradTabPagesBar).OrderButtons;
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

     FTabVisible:=true;
     FShowCloseButton:=false;
     FImageIndex:=0;
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

     {SetText(FCaption); }
   end;
end;

{-------------------------------------------------------------------------------
  TGradTabPage SetButton(Value: TGradTabPageButton)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetButton(Value : TGradTabPageButton);
begin
    //FButton := Value;
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
  DebugLn('TGradTabPage.SetParent');
  if (NewParent=Parent) or (pfInserting in FFlags) then Exit;
  //if ((Parent<>nil)) AND (NewParent=Parent) then exit;
  CheckNewParent(NewParent);
  OldParent:=Parent;

  DebugLn('OldParent: %s NewParent: %s',[DbgSName(OldParent),DbgSName(NewParent)]);

  if (OldParent<>NewParent) and (OldParent<>nil)
  and (OldParent is TGradTabControl)
  and (not (pfRemoving in FFlags))
  then begin
    // remove from old pagelist
    ParentTabControl := TGradTabControl(OldParent);
    i := PageIndex;
    if i >= 0 then
      ParentTabControl.RemovePage(i);

    DebugLn('Page removed from old TabControl');
  end;

  inherited SetParent(NewParent);

  DebugLn('New Parent set');

  if (OldParent<>NewParent) and (Parent<>nil)
  and (Parent is TGradTabControl) then begin
    // add to new pagelist
    ParentTabControl:=TGradTabControl(Parent);
    i:=ParentTabControl.PageList.IndexOf(Self);
    if i<0 then
      ParentTabControl.InsertPage(Self,ParentTabControl.PageCount);

    DebugLn('Insert Page in new Parent');
  end;

  FGradTabControl := TGradTabControl(NewParent);

  DebugLn('TGradTabPage.SetParent end');
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
    //Result := nil;
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
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;

  if FGradTabControl.Images <> nil then
  begin
      UpdateImage;
  end;
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

  if Assigned(FGradTabControl.Images) AND (FGradTabControl.Images.Count<>0) then begin
     FGradTabControl.Images.GetBitmap(ImageIndex,FButton.Glyph);
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

     {Enabled:=false;
     NotEnabledColor:=Color;
     BorderSides:=[];
      }
     //WriteLn('TGradTabPagesBar.Create');
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

  {if not FTabControl.Page[FShowFromButton].Enabled then
     FShowFromButton := FLastShowFrom;
  }
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

     if (Index >= 1) AND (FPageList.Count>=1) then
        LastLeft := TGradTabPage(FPageList.Items[Index-1]).TabButton.Left;

     FTabControl.AssignEvents(AButton);

     with AButton do
     begin
        Left:=-123;
        Parent := Self;
        ShowFocusBorder:=false;
        TextAlignment:=taCenter;
        BorderSides:=[bsTopLine,bsRightLine,bsLeftLine];
        OnMouseDown:=@FTabControl.PageButtonMouseDown;
        OnMouseUp:=@FTabControl.PageButtonMouseUp;
        OnClick:=@FTabControl.PageButtonMouseClick;
        OnMouseMove:=@FTabControl.PageButtonMouseMove;

       { if TabPosition in [tpTop, tpBottom] then begin
           AutoWidth := true
        else if FTabControl.LongTabs then
           AutoWidth := false
        else
           AutoWidth := true;
        }

     end;

     //FTabList.Insert(Index,AButton);

     //FTabList.Add(AButton);

     if (Index >= 1) AND (FPageList.Count>=1) then
        UnFocusButton(Index-1);

     FocusButton(Index);

     OrderButtons;

     //WriteLn('TGradTabPagesBar.InsertButton');
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
   //if Num = FMovedTo then Exit;

   {for i := 0 to FPageList.Count-1 do
   begin
       with TGradTabPage(FPageList.Items[i]).TabButton do
       if TabPosition in [tpTop, tpBottom] then begin
           Left := Left + Num;
       end else begin
           Top := Top + Num;
       end;
   end;}

   DebugLn('Max: %d, Current: %d',[GetLast+1,FMovedTo]);

   //FMovedTo:=FMovedTo + Num;

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

  DebugLn('OrderButton Start');

  FMovedTo:=0;

  if FPageList.Count=0 then Exit;

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


  //DebugLn('ActivePage: %d',[FActiveIndex]);
  FActiveIndex:=FTabControl.PageIndex;
  //DebugLn('ActivePage: %d',[FActiveIndex]);

  for i := 0 to FPageList.Count - 1 do
  begin
    B := TGradTabPage(FPageList.Items[i]).TabButton;

    if B.Visible then
    begin
      B.RotateDirection := NewDirection;
      B.BorderSides := NewBorderSides;
      B.GradientType := NewGradientType;

      //DebugLn('Begin I: %d W: %d H: %d L: %d T: %d, BW: %d, BH: %d',[i,B.Width,B.Height,B.Left,B.Top,BarWidth,BarHeight]);

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
           //DebugLn('B.Width=%d TabBarSize(tpRight)=%d FActive=%d',[B.Width, FTabControl.GetTabBarSize(tpRight), FActiveIndex]);
        end;
      end;

      //DebugLn('End I: %d W: %d H: %d L: %d T: %d, BW: %d, BH: %d',[i,B.Width,B.Height,B.Left,B.Top,BarWidth,BarHeight])
    end;
  end;

  {DebugLn('BarWidth=%d LastLeft=%d FMovedTo=%d BarHeight=%d LastTop=%d',[BarWidth,
      LastLeft, FMovedTo, BarHeight, LastTop]);

  DebugLn('BarWidth < LastLeft-FMovedTo = %s BarHeight < LastTop-FMovedTo=%s',[
      BoolStr(BarWidth < (LastLeft-FMovedTo)), BoolStr(BarHeight < (LastTop-FMovedTo))]);
  }
  if not FTabControl.AutoShowScrollButtons then Exit;

  if ((BarWidth < (LastLeft-FMovedTo)) OR (BarHeight < (LastTop-FMovedTo))) AND ((BarHeight<>0) AND (BarWidth<>0)) then begin
     FTabControl.FLeftButton.Visible:=true;
     FTabControl.FRightButton.Visible:=true;
  end else begin
     FTabControl.FLeftButton.Visible:=false;
     FTabControl.FRightButton.Visible:=false;
  end;

  DebugLn('FR=%s FL=%s',[BoolStr(FTabControl.FRightButton.Visible),BoolStr(FTabControl.FLeftButton.Visible)]);

  DebugLn('OrderButton End');
end;

{-------------------------------------------------------------------------------
  TGradTabPagesBar UnFocusButton(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPagesBar.UnFocusButton(Index: Integer);
begin
   if (Index < 0) or (Index >= FPageList.Count) then Exit;

   {$IFDEF DEBUGTAB}
        DebugLn('TGradTabPagesBar.UnFocusButton Index: %d Assigned %s', [Index,BoolToStr(Assigned(TGradTabPage(FPageList.Items[Index]).TabButton),true)]);
    {$ENDIF}

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

       Color := clBlue;

       UpdatePositions;
   end;
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
    //FShowFromButton:=Index;
    if csDestroying in FTabControl.ComponentState then Exit;
    if (Index < 0) or (Index >= FPageList.Count) then Exit;
    //FActiveIndex:=Index;
    //FShowFromButton:=Index;
    {$IFDEF DEBUGTAB}
        DebugLn('TGradTabPagesBar.FocusButton Index: %d Assigned %s', [Index,BoolToStr(Assigned(TGradTabPage(FPageList.Items[Index]).TabButton),true)]);
    {$ENDIF}
    
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
        Color := clGreen;
    end;

    DebugLn('FR=%s FL=%s',[BoolStr(FTabControl.FRightButton.Visible),BoolStr(FTabControl.FLeftButton.Visible)]);
    if not (FTabControl.FRightButton.Visible AND FTabControl.FLeftButton.Visible) then Exit;
    C := 0;

    DebugLn('Left=%d Width=%d Width=%d',[TGradTabPage(FPageList.Items[Index]).TabButton.Left,
       TGradTabPage(FPageList.Items[Index]).TabButton.Width, Width]);

    DebugLn('Before ScrollToTab');
    ScrollToTab(Index);
    DebugLn('After ScrollToTab');
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
    DebugLn('Change TabPosition from %s to %s',[DbgsTabPosition(FTabPosition),DbgsTabPosition(Value)]);
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
      Result := TheButton.Visible AND (TheButton.Left >= 0) AND (TheButton.Left <= Width)
   else
      Result := TheButton.Visible AND (TheButton.Top >= 0) AND (TheButton.Left <= Height);
end;

procedure TGradTabPagesBar.ChangeLeftTop(LastTabPosition: TTabPosition);
begin

end;

function TGradTabPagesBar.GetViewedTabs: TTabs;

   function IncAr(var Ar : TTabs) : Integer;
   begin
       SetLength(Ar, Length(Ar)+1);
       Result := Length(Ar)-1;
   end;

var
   i,l : Integer;
begin
   //DebugLn('GetViewedTabs');
   //DebugLn('Width=%d Height=%d',[Width,Height]);
   for i := 0 to FPageList.Count-1 do
   begin
      with TGradTabPage(FPageList.Items[i]).TabButton do
      begin
        if ((TabPosition in [tpTop, tpBottom]) AND (Left >= 0) {AND (Left <=(Self.Width-10))} AND (Left+Width < Self.Width)) OR
           ((TabPosition in [tpLeft, tpRight]) AND (Top >= 0) {AND (Top <=(Self.Height-10))} AND (Top+Height < Self.Height)) then
        begin
           l := IncAr(Result);
           //DebugLn('L=%d T=%d W=%d H=%d Caption=%s',[Left, Top, Width, Height, Caption]);
           //DebugLn('%d. Value: %d',[l,i]);
           Result[l] := i;
        end;
      end;
   end;
   //DebugLn('GetViewedTabs End');
end;

procedure TGradTabPagesBar.ScrollToTab(PIndex: Integer);
var
   CurTabs : TTabs;
   C : Integer;
   DoNext : Boolean;
begin
   C := 0;

   if (FPageList.Count=0) OR (PIndex>=FPageList.Count) then Exit;

   {$IFDEF DEBUGTAB} DebugLn('ScrollToTab=%d',[PIndex]); {$ENDIF}
   repeat
       {$IFDEF DEBUGTAB} DebugLn('Run=%d',[C]); {$ENDIF}
       CurTabs := GetViewedTabs;
       SetLength(CurTabs, Length(CurTabs)-2);

       with TGradTabPage(FPageList.Items[PIndex]).TabButton do
       case FTabPosition of
           tpTop, tpBottom : DoNext := ((Left+Width)>= Self.Width);
           tpRight, tpLeft: DoNext := ((Top+Height)>= Self.Height);
       end;

       if DoNext then MoveToNext else MoveToPrior;

       Inc(C);

       {$IFDEF DEBUGTAB} DebugLn('ValInAr=%s',[BoolStr(ValueInArray(PIndex,CurTabs))]); {$ENDIF}

    until(ValueInArray(PIndex,CurTabs) OR (C=10));

    if DoNext then MoveToNext else MoveToPrior;
   {$IFDEF DEBUGTAB} DebugLn('ScrollToTab End'); {$ENDIF}
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
var theTabList: TListWithEvent;
theGradTabControl: TGradTabControl);
begin
  inherited Create;
  fPageList := thePageList;
  fPageList.OnChange:=@PageListChange;
  FTabList := theTabList;
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
  //DebugLn('TGradTabPages.Get Index=',Index);
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TGradTabPages.Get Index out of bounds');
  Result := TGradTabPage(fPageList[Index]).Caption;
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
  DebugLn('TGradTabPages.Clear Begin');
  while fPageList.Count>0 do
    Delete(fPageList.Count-1);
  DebugLn('TGradTabPages.Clear End');
end;

{------------------------------------------------------------------------------
  TGradTabPages Delete(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabPages.Delete(Index: Integer);
var
  APage: TGradTabPage;
  AButton : TGradTabPageButton;
  CurrentPageNum,i : Integer;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF DEBUGTAB}
  //DebugLn('TGradTabPages.Delete A Index=',Index);
  DebugLn(['TGradTabPages.Delete B ',FGradTabControl.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',FGradTabControl.PageIndex]);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    APage:=TGradTabPage(fPageList[Index]);

    DebugLn('B Parent = nil');
    // delete handle
    APage.Parent:=nil;
    DebugLn('B APage.Free');
    // free the page
    APage.Free;
        {if Index = 0 then
    begin
        for i := 1 to FPageList.Count-1 do
        begin
            CurrentPageNum:=i;
        end;

    end else begin
        CurrentPageNum:=Index-1;
    end;

    FGradTabControl.CurrentPageNum:= CurrentPageNum;
    }
    //FGradTabControl.PageRemoved(Index);

    //APage:=TGradTabPage(fPageList[Index]);
    // delete handle
    //APage.Parent:=nil;
    // free the page
    //APage.Free;

    //FPageList.Delete(Index);

    {AButton:=TGradTabPageButton(fTabList[Index]);
    // delete handle
    AButton.Parent:=nil;
    // free the page
    AButton.Free;
    }

    //FTabList.Delete(Index);

    //FGradTabControl.PagesBar.OrderButtons;
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
  NewButton: TGradTabPageButton;
  NewOwner: TComponent;
begin
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert A ',FNoteBook.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  NewOwner:=FGradTabControl.Owner;
  if NewOwner=nil then
    NewOwner:=FGradTabControl;
  //NewPage := FGradTabControl.PageClass.Create(NewOwner);
  NewPage := TGradTabPage.Create(NewOwner);

  //NewButton := TGradTabPageButton.Create(NewOwner);

  //NewPage.SetButton(NewButton);

  //FPageList.Add(NewPage);
  //FTabList.Add(NewButton);

  //InsertPage(NewPage);

  with NewPage do
  begin
      Caption := S;
      //Name := 'gradtabpage_'+IntToStr(Index);
  end;

  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert B ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  {TODO}
  FGradTabControl.InsertPage(NewPage,Index);
  //FGradTabControl.PagesBar.InsertButton(NewButton, Index);
  {$IFDEF DEBUGTAB}
  DebugLn(['TGradTabPages.Insert END ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
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
  //fTabList.Move(CurIndex, NewIndex);

  //FGradTabControl.PagesBar.OrderButtons;

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

    FTabPosition:=tpTop;
    fCompStyle := csNoteBook;

    ControlStyle := [{csAcceptsControls, }csDesignInteractive];
    TabStop:=true;

    FPageList := TListWithEvent.Create;
    FTabList := TListWithEvent.Create;
    FTabStrings := TGradTabPages.Create(TListWithEvent(FPageList),
       TListWithEvent(FTabList) ,Self);
    FPageIndex:=-1;

    FTabHeight:=20;

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
    end;

    Height:=200;
    Width:=200;
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
    DebugLn('B FreeAndNil(FTabList)');
    {$ENDIF}
    FreeAndNil(FTabList);
    {$IFDEF DEBUGTAB}
    DebugLn('A FreeAndNil(FTabList)');
    DebugLn('B FreeAndNil(FPagesBar)');
    DebugLn(IsAssigned(FPagesBar));
    {$ENDIF}
    FLeftButton.Free;
    FRightButton.Free;
    {$IFDEF DEBUGTAB}
    //FPagesBar.Parent := nil;
    DebugLn('B Free');
    //FPagesBar.Free;
    DebugLn('A FreeAndNil(FPagesBar)');
    DebugLn('B FreeAndNil(FBar)');
    DebugLn(IsAssigned(FBar));
    //FBar.Parent := nil;
    DebugLn('B Free');
    //FBar.Free;
    DebugLn('A FreeAndNil(FBar)');
    DebugLn('TGradTabControl.Destroy End');
    {$ENDIF}

    inherited;
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

procedure TGradTabControl.ImageListChange(Sender: TObject);
begin
   UpdateTabImages;
end;

procedure TGradTabControl.UpdateTabImages;
var
   i : Integer;
begin
  if FImages = nil then Exit;

  DebugLn('Images.Count: %d',[Images.Count]);

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
var
   LastTabPage : TGradTabPage;
   LastTab : Integer;
begin
     Result := nil;

     if (AIndex >= 0) AND (AIndex < FPageList.Count) then
        Result := TGradTabPage(FPageList.Items[AIndex]);

     //WriteLn('AIndex: ', AIndex);
     //WriteLn('Last: ', FPageList.Count);


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
   //WriteLn('MouseDown: ', FTabList.IndexOf(Sender));
   
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
   //WriteLn('MouseUp: ', FTabList.IndexOf(Sender));
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
   //WriteLn('MouseClick: ', FTabList.IndexOf(Sender));

   AButton := TGradTabPageButton(Sender);
   PageIndex:=FPageList.IndexOf(AButton.Owner);
   
   if Assigned(FOnTabButtonClick) then
      FOnTabButtonClick(Self, FPageList.IndexOf(AButton.Owner));
end;

procedure TGradTabControl.PageButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   AButton : TGradTabPageButton;
begin
   AButton := TGradTabPageButton(Sender);

   //DebugLn('PageButtonMouseMove X=%d Y=%d',[X,Y]);

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
  DebugLn('FImages: ', IsAssigned(FImages));
  if FImages=AValue then exit;
  FImages:=AValue;

  DebugLn('FImages: ', IsAssigned(FImages));
  DebugLn('FImages: ', IsAssigned(Images));

  //DebugLn('AValue: ', IsAssigned(AValue));

  if FImages <> nil then
     FImages.OnChange := @ImageListChange;

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

procedure TGradTabControl.MoveLeftTopClick(Sender: TObject);
begin
    //PagesBar.MoveTo(FMoveIncrement);

    PagesBar.MoveToPrior;
end;

procedure TGradTabControl.MoveRightBottomClick(Sender: TObject);
begin
    //PagesBar.MoveTo(-FMoveIncrement);
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
begin
   if (Value<0) or (Value>=fPageList.Count) then Exit;
   if FPageIndex=Value then Exit;
   if not Page[Value].Enabled then Exit;
   if not Page[Value].TabVisible then Exit;

   if FPageIndex <> -1 then UnShowPage(FPageIndex);

   ShowPage(Value);
   FPagesBar.FocusButton(Value);

   FPageIndex := Value;

   UpdateAllDesignerFlags;
   FPagesBar.OrderButtons;
   //SetCurrentPage(TGradTabPage(FPageList.Items[Value]));
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
    //Tab größer machen
    FPagesBar.FocusButton(Index);

    //Page enablen
    with TGradTabPage(FPageList.Items[Index]) do
    begin
       Visible:=true;
       BringToFront;
    end;

    UpdateDesignerFlags(Index);

    AlignPage(TGradTabPage(FPageList.Items[Index]), GetClientRect);
end;

{------------------------------------------------------------------------------
  TGradTabControl UnShowPage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.UnShowPage(Index: Integer);
begin

    //Page disablen
    if (Index<0) or (Index>=fPageList.Count) then Exit;

    //Tab kleiner machen
    FPagesBar.UnFocusButton(Index);

    UpdateDesignerFlags(Index);

     with TGradTabPage(FPageList.Items[Index]) do
       Visible:=false;

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
  cRect : TRect;
  tempName : String;
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

    FPagesBar.InsertButton(APage.TabButton, Index);
    if APage.Caption = '' then
       APage.Caption:=APage.Name;

    //FPageList.Insert(Index,APage);
    //APage.Parent := Self;
    if NewZPosition>=0 then
      SetControlIndex(APage,NewZPosition);
    if PageIndex = -1 then
      FPageIndex := Index;

    //tempName := GetNewName;
    //APage.Name:='SOMENEWNOTUSEDNAME_XYZ'; //maybe it works xD

    DebugLn('APage.Name empty: %s',[BoolToStr(APage.Name='',true)]);

    {if (APage.Name = '') then
       APage.Name := tempName
    else
       if APage.Name = tempName then GetNewName;
    }

    //UpdateDesignerFlags(Index);

    //FPagesBar.InsertButton(APage.FButton,Index);

    if HandleAllocated and (not (csLoading in ComponentState)) then begin
      AddRemovePageHandle(APage);
      if PageIndex = Index then
        ShowCurrentPage;
    end;
  finally
    APage.EnableAlign;

    AlignPage(APage, GetClientRect);
    SetCurrentPageNum(Index);
    //FPagesBar.OrderButtons;
    //cRect := TGradTabControl(APage.Parent).GetClientRect;
    //APage.Color:=clBlue;
    //APage.ChangeBounds(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
  end;

  {FPagesBar.FocusButton(Index);
  FPagesBar.OrderButtons;  }

  //if Index = FPageList.Count-1 then FPagesBar.MoveToNext;

  //DebugLn(DbgSName(APage.Parent),' a');
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
    DebugLn(['TCustomNoteBook.AddRemovePageHandle ADD ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
    {$ENDIF}
    if (pfAdded in APage.FFlags) then exit;
    Include(APage.FFlags,pfAdding);
    //TWSCustomNotebookClass(WidgetSetClass).AddPage(Self, APage, APage.VisibleIndex);
    APage.FFlags:=APage.FFlags+[pfAdded]-[pfAdding];
    APage.ResizeDelayedAutoSizeChildren
  end else begin
    {$IFDEF NOTEBOOK_DEBUG}
    DebugLn(['TCustomNoteBook.AddRemovePageHandle REMOVE ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
    {$ENDIF}
    if not (pfAdded in APage.FFlags) or (pfRemoving in APage.FFlags) then
      exit;
    APage.FFlags := APage.FFlags - [pfAdded] + [pfRemoving];
    //TWSCustomNotebookClass(WidgetSetClass).RemovePage(Self, APage.VisibleIndex);
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
    //FTabList.Delete(Index);
    APage.Parent:=nil;
    //FTabStrings.Delete(Index);
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
    
    {if TabPosition in [tpTop,tpBottom] then
       FBar.Height := Value
    else if not LongTabs then
       FBar.Width := Value;

    InvPaint;

    FPagesBar.Align:=alNone;
    FPagesBar.Align:=alClient;
    FPagesBar.OrderButtons;

    if ActivePage <> nil then AlignPage(ActivePage,GetClientRect); }

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
         DebugLn('Before');
         DebugLn('FBar Left %d Top %d Height %d Width %d',[ FBar.Left, Fbar.Top, FBar.Height, FBar.Width]);
         DebugLn('FPagesBar Left %d Top %d Height %d Width %d',[ FPagesBar.Left, FPagesbar.Top, FPagesBar.Height, FPagesBar.Width]);
         DebugLn('Control Left %d Top %d Height %d Width %d',[ Left, Top, Height, Width]);
    {$ENDIF}
    
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

    if FPagesBar.TabPosition=Value then
       FPagesBar.OrderButtons
    else
       FPagesBar.TabPosition:=Value;

    {$IFDEF DEBUGTAB}
         DebugLn('After');
         DebugLn('FBar Left %d Top %d Height %d Width %d',[ FBar.Left, Fbar.Top, FBar.Height, FBar.Width]);
         DebugLn('FPagesBar Left %d Top %d Height %d Width %d',[ FPagesBar.Left, FPagesbar.Top, FPagesBar.Height, FPagesBar.Width]);
         DebugLn('Control Left %d Top %d Height %d Width %d',[ Left, Top, Height, Width]);
    {$ENDIF}

    AlignPages;
    //AlignPage(CurrentPage,GetClientRect);

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
   //FPagesBar.OrderButtons;
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

    //Result := FTabStrings.Count;
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
var
   i,j : Integer;
begin
   Canvas.Brush.Color:=Color;
   Canvas.FillRect(0,0,Width,Height);

   Canvas.Pen.Color:=clBlack;

   case FTabPosition of
      tpTop: begin
         Canvas.Line(0,FTabHeight,0,Height);
         Canvas.Line(0,FTabHeight,Width,FTabHeight);
         Canvas.Line(Width-1,FTabHeight,Width-1,Height);
         Canvas.Line(0,Height-1,Width,Height-1);
      end;
      tpBottom: begin
         Canvas.Line(0,0,0,Height-FTabHeight);
         Canvas.Line(0,0,Width,0); //Top
         Canvas.Line(Width-1,0,Width-1,Height-FTabHeight);
         Canvas.Line(0,Height-FTabHeight-1,Width,Height-FTabHeight-1);
      end;
      tpRight: begin
         Canvas.Line(0,0,0,Height); //Left
         Canvas.Line(0,0,Width-GetTabBarSize(tpRight),0); //Top
         Canvas.Line(Width-1-GetTabBarSize(tpRight),0,Width-1-GetTabBarSize(tpRight),Height); //Right
         Canvas.Line(0,Height-1,Width-GetTabBarSize(tpRight),Height-1);//Bottom
      end;
      tpLeft: begin
         Canvas.Line(GetTabBarSize(tpLeft),0,GetTabBarSize(tpLeft),Height); //Left
         Canvas.Line(GetTabBarSize(tpLeft),0,Width,0); //Top
         Canvas.Line(Width-1,0,Width-1,Height); //Right
         Canvas.Line(GetTabBarSize(tpLeft),Height-1,Width,Height-1);//Bottom
      end;
   end;
   
end;

{------------------------------------------------------------------------------
  TGradTabControl Resize
 ------------------------------------------------------------------------------}
procedure TGradTabControl.Resize;
begin
     inherited;

     //WriteLn('Resize', ' ', BoolToStr(HasParent,true), ' ', FPageList.Count);
     //Current Page resizing
     
     {$IFDEF DEBUGTAB}
        DebugLn('GradTabControl.Resize HasParent %s FPageList.Count %d',[BoolToStr(HasParent,true), FPageList.Count]);
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
     DebugLn('UpdateDesignerFlags: Index: %d Current: %d Assigned: %s',[APageIndex, FPageIndex,BoolToStr(Assigned(Page[APageIndex]),true)]);
  {$ENDIF}

  if APageIndex<>FPageIndex then
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle+[csNoDesignVisible{,csNoDesignSelectable}]
  else
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle-[csNoDesignVisible{,csNoDesignSelectable}];

  {$IFDEF DEBUGTAB} DebugLn('UpdateDesignerFlags End'); {$ENDIF}

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
    //DebugLn(['TCustomNotebook.ShowCurrentPage ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible]);
    if CurPage.Visible then begin
      if FPageIndexOnLastShow<>fPageIndex then begin
        // some widgetsets like win32/64 do not send WM_SIZE messages for
        // hidden pages. Force resizing page (it is alClient).
        //DebugLn(['TCustomNotebook.ShowCurrentPage ',dbgsName(Self),' ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible,' BoundsRect=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CurPage.BoundsRect=',dbgs(CurPage.BoundsRect),' CurPage.ClientRect=',dbgs(CurPage.ClientRect)]);
        ReAlign;
        // TCustomPage.IsControlVisible is overriden
        // therefore AutoSizing of childs was skipped => do it now
        CurPage.ReAlign;
      end;
    end else begin
      CurPage.Visible := true;
    end;
    //FPageIndexOnLastShow:=fPageIndex;
   // CurPage.DoShow;
    {if (FPageIndexOnLastChange >= 0) and (FPageIndexOnLastChange < PageCount) and
       (FPageIndexOnLastChange <> FPageIndex) then
    begin
      // Page[FPageIndexOnLastChange].Visible := False; <-- this will be better,
      // but this is not work on gtk (tab hides too)
      Page[FPageIndexOnLastChange].DoHide;
    end; }
  end;
end;

initialization
  {$I ugradtabcontrol.lrs}

end.

