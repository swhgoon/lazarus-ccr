unit ugradtabcontrol; 

{-------------------------------------------------------------------------------
 @name GradTabControl
 @author Eugen Bolz
 @lastchange 28.07.2008
 @version 0.1
 @comments TGradTabControl is based on TNotebook/TPageControl
 @license http://creativecommons.org/licenses/LGPL/2.1/
 @todo:

 ------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

{$DEFINE DEBUGTAB}

interface

uses
  Classes,LResources, SysUtils, Menus, LCLType,
    LCLProc, ExtCtrls, Graphics, ugradbtn,Controls, urotatebitmap;

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
  public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure Resize; override;
       property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
  end;

  TGradTabPageButtonClickEvent = procedure(GradTabControl : TGradTabControl;AIndex : Integer) of object;
  TGradTabPageButtonMouseDownUpEvent = procedure(GradTabControl : TGradTabControl;Button: TMouseButton;
      Shift: TShiftState; X, Y, AIndex: Integer) of object;
  
  //Eigenschaft des Tabs hier miteinbauen
  TGradTabPage = class(TCustomControl)
  private
        FButton : TGradTabPageButton;
        FCaption: TCaption;
        FGradTabControl : TGradTabControl; //Maybe needed ^.^
        FFlags: TPageFlags;
        FTabVisible,FCurrentlyDestroying,FShowCloseButton : Boolean;
        procedure SetTabPopupMenu(Value : TPopupMenu);
        function GetTabPopupMenu : TPopupMenu;
        procedure SetText(const Value: TCaption);
        function GetText : TCaption;
  protected
        function GetPageIndex: integer;
        procedure SetPageIndex(AValue: Integer);
        procedure SetButton(Value : TGradTabPageButton); //Spaeter nicht mehr nötig
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
  published
        //property ControlState;
        //property ControlStyle;
        property TabVisible : Boolean read FTabVisible write SetTabVisible default true;
        property PageIndex : Integer read GetPageIndex write SetPageIndex;
        property Caption : TCaption read GetText write SetText;
        property ShowCloseButton : Boolean read FShowCloseButton write SetShowCloseButton default false;
        property TabPopupMenu : TPopupMenu read GetTabPopupMenu write SetTabPopupMenu;
  end;
  
  //Zeigt die Tabs an
  //Dafür extra eine Komponente damit die Tabs auch "abgehackt"
  //angezeigt werden können
  //ohne ihre größe zu ändern^^
  //Die Tab wechsel Effekte werden nur auf diesem Control ausgeführt

  { TGradTabPagesBar }

  TGradTabPagesBar = class(TCustomControl)
  private
      FPageList : TListWithEvent;
      FShowFromButton, FMovedTo : Integer;
      FTabPosition : TTabPosition;
      FTabHeight,FLongWidth : Integer;
      FActiveIndex: Integer;
  protected
      procedure InsertButton(AButton: TGradTabPageButton; Index: Integer);
      procedure OrderButtons;
      procedure UnFocusButton(Index: Integer);
      procedure FocusButton(Index: Integer);
      procedure SetTabPosition(Value:
       TTabPosition);
      function IsVisible(Index: Integer) : Boolean;
      procedure ChangeLeftTop(LastTabPosition : TTabPosition);
  public
      constructor Create(AOwner: TComponent; var thePageList: TListWithEvent);
      procedure Paint; override;
      procedure MoveTo(Num: Integer);
      procedure MoveToNorm;
      property TabPosition : TTabPosition read FTabPosition write SetTabPosition;
      //destructor Destroy; override;
  end;
  
  //Verwaltet die extra Buttons ( wie weiter/zurück )
  //Verschiebt die ansicht von TGradTabPagesBar
  //irgendwann mit effekt
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
        FMoveIncrement: Integer;
        FLeftButton, FRightButton : TGradButton;
        FTabStrings : TStrings; //TGradTabPages
        FPageList: TList; //Is Managed by TGradTabPages
        FTabList : TList; //Also ^^
        FOnTabButtonClick : TGradTabPageButtonClickEvent;
        FOnTabButtonMouseDown,
         FOnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent;
        FPageIndex, fPageIndexOnLastChange, fPageIndexOnLastShow,
        FTabHeight, FLongWidth : Integer;
        FBar : TGradTabBar;
        FPagesBar: TGradTabPagesBar;
        FTabPosition : TTabPosition;
        FLongTabs : Boolean;
        procedure AlignPage(APage : TGradTabPage; ARect : TRect);
        //procedure AddRemovePageHandle(APage: TGradTabPage);
        //procedure DoSendPageIndex;
        function GetCurrentPage : TGradTabPage;
        function GetPage(AIndex: Integer) : TGradTabPage;
        function GetCount : Integer;
        procedure MoveTab(Sender: TObject; NewIndex: Integer);
        function FindVisiblePage(Index: Integer): Integer;
        procedure PageButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
        procedure PageButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
        procedure PageButtonMouseClick(Sender: TObject);
        procedure MoveLeftTopClick(Sender: TObject);
        procedure MoveRightBottomClick(Sender: TObject);
        procedure PageRemoved(Index: Integer);
        procedure SetCurrentPage(Value : TGradTabPage);
        procedure SetCurrentPageNum(Value: Integer);
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
  public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function AddPage(AName: String) : Integer;
        function AddPage(APage: TGradTabPage) : Integer;
        function GetClientRect: TRect; override;
        procedure Paint; override;
        procedure Resize; override;
        procedure UpdateAllDesignerFlags;
        procedure UpdateDesignerFlags(APageIndex: integer);

        //Old - will be erased soon ^^
        property CurrentPageNum : Integer read FPageIndex write SetCurrentPageNum;
        property CurrentPage : TGradTabPage read GetCurrentPage write SetCurrentPage;
        property Pages[Index: Integer] : TGradTabPage read GetPage;
        property Page[Index: Integer] : TGradTabPage read GetPage;
        property Bar : TGradTabBar read FBar;
        property PagesBar : TGradTabPagesBar read FPagesBar;
        property PageList: TList read FPageList;
        property Tabs : TStrings read FTabStrings write SetTabs;
  published
        property Align;
        property ControlState;
        property ControlStyle;
        property ActivePage : TGradTabPage read GetCurrentPage write SetCurrentPage;
        property OnTabButtonClick : TGradTabPageButtonClickEvent read FOnTabButtonClick write FOnTabButtonClick;
        property OnTabButtonMouseDown : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseDown write FOnTabButtonMouseDown;
        property OnTabButtonMouseUp : TGradTabPageButtonMouseDownUpEvent read FOnTabButtonMouseUp write FOnTabButtonMouseUp;
        property PageIndex : Integer read FPageIndex write SetCurrentPageNum;
        property TabHeight : Integer read FTabHeight write SetTabHeight;
        property TabPosition : TTabPosition read FTabPosition write SetTabPosition default tpTop;
        property PageCount : Integer read GetCount;
        property LongTabs : Boolean read FLongTabs write SetLongTabs;
        property LongWidth: Integer read FLongWidth write FLongWidth;
        property MoveIncrement : Integer read FMoveIncrement write FMoveIncrement;
        //property ShowTabs : Boolean; { TODO }
  end;
  
  procedure Register;
  function IsAssigned(var Obj : TObject) : String;

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

{-------------------------------------------------------------------------------
  TGradTabPageButton Create(AOwner: TComponent
 ------------------------------------------------------------------------------}
constructor TGradTabPageButton.Create(AOwner: TComponent);
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
     FCloseButton.Glyph.Assign(CreateBitmapFromLazarusResource('close_btn'));
     FCloseButton.ShowGlyph:=true;

     FCloseButton.BorderSides:=[];
     FCloseButton.Color:=clRed;
     //FCloseButton.Visible:=false;
     //FCloseButton.Parent := Self;

     FShowCloseButton:=true;

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

{-------------------------------------------------------------------------------
  TGradTabPage SetTabPopupMenu(Value: TPopupMenu)
 ------------------------------------------------------------------------------}
procedure TGradTabPage.SetTabPopupMenu(Value : TPopupMenu);
begin
    if not Assigned(FButton) then Exit;
    if not Assigned(Value) then Exit;

    FButton.PopupMenu:=Value;
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
       FButton.Caption:=FCaption;
end;

{-------------------------------------------------------------------------------
  TGradTabPage GetText : TCaption
 ------------------------------------------------------------------------------}
function TGradTabPage.GetText : TCaption;
begin
    Result := FCaption;
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

{-------------------------------------------------------------------------------
  TGradTabPagesBar Create(AOwner: TComponent; var theTabList: TListWithEvent)
 ------------------------------------------------------------------------------}
constructor TGradTabPagesBar.Create(AOwner: TComponent; var thePageList: TListWithEvent);
begin
     inherited Create(AOwner);

     FPageList := thePageList;
     FShowFromButton:=0;
     ControlStyle := ControlStyle+[csNoDesignSelectable];
     FTabPosition:=tpTop;
     FMovedTo:=1;

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

     with AButton do
     begin
        Left := LastLeft;
        Parent := Self;
        ShowFocusBorder:=false;
        AutoWidth:=true;
        TextAlignment:=taCenter;
        BorderSides:=[bsTopLine,bsRightLine,bsLeftLine];
        OnMouseDown:=@((Self.Owner as TGradTabControl).PageButtonMouseDown);
        OnMouseUp:=@((Self.Owner as TGradTabControl).PageButtonMouseUp);
        OnClick:=@((Self.Owner as TGradTabControl).PageButtonMouseClick);
     end;

     //FTabList.Insert(Index,AButton);

     //FTabList.Add(AButton);

     if (Index >= 1) AND (FPageList.Count>=1) then
        UnFocusButton(Index-1);

     FocusButton(Index);

     //OrderButtons;

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

           L := L +1;

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

   FMovedTo:=FMovedTo + Num;

   if FMovedTo > 1 then FMovedTo := 1;
   if FMovedTo < -GetLast then FMovedTo := -GetLast+2;

   OrderButtons;
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
  DebugLn('OrderButton Start');

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
      NewGradientType := gtVertical;
      NewDirection := rdLeft;
    end;
    tpRight:
    begin
      NewBorderSides := [bsTopLine, bsBottomLine, bsRightLine];
      NewGradientType := gtVertical;
      NewDirection := rdRight;
    end;
  end;

  for i := 0 to FPageList.Count - 1 do
  begin
    B := TGradTabPage(FPageList.Items[i]).TabButton;

    if B.Visible then
    begin
      B.RotateDirection := NewDirection;
      B.BorderSides := NewBorderSides;
      B.GradientType := NewGradientType;

      DebugLn('Begin I: %d W: %d H: %d L: %d T: %d, BW: %d, BH: %d',[i,B.Width,B.Height,B.Left,B.Top,BarWidth,BarHeight]);

      case FTabPosition of
        tpTop:
        begin
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
           B.Top := LastTop;
           LastTop := LastTop + B.Height + 1;

           if FActiveIndex = i then begin
              B.Left := 0;
              B.Width:= BarWidth;
           end else begin
              B.Left := 3;
              B.Width:= BarWidth-3;
           end;
        end;
        tpRight:
        begin
           B.Top := LastTop;
           LastTop := LastTop + B.Height + 1;

           B.Left := 0;

           if FActiveIndex = i then
              B.Width := BarWidth
           else
              B.Width := BarWidth-3;
        end;
      end;





      {if FActiveIndex = i then
       FocusButton(i)
      else
       UnFocusButton(i);
      }


      {if FTabPosition in [tpTop, tpBottom] then
      begin
        B.Left := LastLeft;
        LastLeft := LastLeft + B.Width + 1;

        if FTabPosition = tpBottom then begin




        end else begin

        end;
      end
      else
      begin

        if FTabPosition = tpRight then begin


        end else
      end; }

      DebugLn('End I: %d W: %d H: %d L: %d T: %d, BW: %d, BH: %d',[i,B.Width,B.Height,B.Left,B.Top,BarWidth,BarHeight])
    end;
  end;
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
begin
    //FShowFromButton:=Index;
    if (Index < 0) or (Index >= FPageList.Count) then Exit;
    FActiveIndex:=Index;
    FShowFromButton:=Index;
    {$IFDEF DEBUGTAB}
        DebugLn('TGradTabPagesBar.FocusButton Index: %d Assigned %s', [Index,BoolToStr(Assigned(TGradTabPage(FPageList.Items[Index]).TabButton),true)]);
    {$ENDIF}
    
    with TGradTabPage(FPageList.Items[Index]).TabButton do
    begin
        case FTabPosition of
           tpTop, tpBottom : begin
                Top:=0;
                Height:=Self.Height;
           end;
           tpRight, tpLeft: begin
                Left := 0;
                Width:= Self.Width;
           end;
        end;
        Color := clGreen;
    end;
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

{-------------------------------------------------------------------------------
  TGradTabPagesBar Create(AOwner: TComponent)
 ------------------------------------------------------------------------------}
constructor TGradTabBar.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     ControlStyle := ControlStyle+[csNoDesignSelectable];
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
     Canvas.Brush.Color:=clFuchsia;
     Canvas.FillRect(0,0,Width,Height);

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
  {$IFDEF NOTEBOOK_DEBUG}
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

  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TGradTabPages.Insert B ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  {TODO}
  FGradTabControl.InsertPage(NewPage,Index);
  //FGradTabControl.PagesBar.InsertButton(NewButton, Index);
  {$IFDEF NOTEBOOK_DEBUG}
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

    ControlStyle := [csAcceptsControls];
    TabStop:=true;

    FPageList := TListWithEvent.Create;
    FTabList := TListWithEvent.Create;
    FTabStrings := TGradTabPages.Create(TListWithEvent(FPageList),
       TListWithEvent(FTabList) ,Self);
    FPageIndex:=-1;

    FTabHeight:=30;

    FBar := TGradTabBar.Create(Self);
    FBar.Height:=FTabHeight;
    FBar.Top:=0;
    FBar.Left:=0;
    FBar.Width:=Width;
    FBar.Parent := Self;
    //FBar.Align:=alTop;

    FMoveIncrement:=1;

    FPagesBar := TGradTabPagesBar.Create(Self,TListWithEvent(FPageList));
    FPagesBar.Parent:=FBar;
    FPagesBar.Align:=alClient;
    FPagesBar.Left:=0;
    FPagesBar.Top:=0;
    FPagesBar.Width:=FBar.Width;
    FPagesBar.Height:=FBar.Height;

    FLeftButton := TGradButton.Create(Self);
    FLeftButton.Parent := FBar;
    FLeftButton.Align:= alLeft;
    FLeftButton.Caption:='<';
    FLeftButton.AutoWidth:=true;
    FLeftButton.OnClick:=@MoveLeftTopClick;

    FRightButton := TGradButton.Create(Self);
    FRightButton.Parent := FBar;
    FRightButton.Align:= alRight;
    FRightButton.Caption:='>';
    FRightButton.AutoWidth:=true;
    FRightButton.OnClick:=@MoveRightBottomClick;

    Height:=200;
    Width:=200;
end;

{------------------------------------------------------------------------------
  TGradTabControl Destroy
 ------------------------------------------------------------------------------}
destructor TGradTabControl.Destroy;
begin
    DebugLn('TGradTabControl.Destroy Start');
    DebugLn('B Tabs.Clear');
    Tabs.Clear;
    DebugLn('A Tabs.Clear');
    DebugLn('B FreeAndNil(FTabStrings)');
    FreeAndNil(FTabStrings);
    DebugLn('A FreeAndNil(FTabStrings)');
    DebugLn('B FreeAndNil(FPageList)');
    FreeAndNil(FPageList);
    DebugLn('A FreeAndNil(FPageList)');
    DebugLn('B FreeAndNil(FTabList)');
    FreeAndNil(FTabList);
    DebugLn('A FreeAndNil(FTabList)');
    DebugLn('B FreeAndNil(FPagesBar)');
    DebugLn(IsAssigned(FPagesBar));

    FLeftButton.Free;
    FRightButton.Free;

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


    inherited;
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
   CurrentPageNum:=FPageList.IndexOf(AButton.Owner);
   
   if Assigned(FOnTabButtonClick) then
      FOnTabButtonClick(Self, FPageList.IndexOf(AButton.Owner));
end;

procedure TGradTabControl.MoveLeftTopClick(Sender: TObject);
begin
    PagesBar.MoveTo(FMoveIncrement);
end;

procedure TGradTabControl.MoveRightBottomClick(Sender: TObject);
begin
    PagesBar.MoveTo(-FMoveIncrement);
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
      DebugLn('TGradTabControl.PageRemoved Index: %d NewPageIndex: %d',[Index, NewPageIndex]);
      if NewPageIndex >= 0 then
        PageIndex := NewPageIndex
      else
        FPageIndex := NewPageIndex;
    end;
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

   if FPageIndex <> -1 then UnShowPage(FPageIndex);

   ShowPage(Value);
   FPagesBar.FocusButton(Value);

   FPageIndex := Value;

   UpdateAllDesignerFlags;
   FPagesBar.OrderButtons;
   //SetCurrentPage(TGradTabPage(FPageList.Items[Value]));
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


    //UpdateDesignerFlags(Index);

    AlignPage(TGradTabPage(FPageList.Items[Index]), GetClientRect);
end;

{------------------------------------------------------------------------------
  TGradTabControl UnShowPage(Index: Integer)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.UnShowPage(Index: Integer);
begin
    //Tab kleiner machen

    //Page disablen
    if (Index<0) or (Index>=fPageList.Count) then Exit;

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
    DebugLn('TGradTabControl.ChildClassAllowed ',ChildClass.ClassName);
    Result := (ChildClass<>nil);
    if ChildClass.InheritsFrom(TGradTabPage) then Result := true;
    if GetCurrentPage=nil then begin
       if ChildClass.InheritsFrom(TGradTabBar) then Result := true;
    end else begin
       if (ChildClass.InheritsFrom(TControl)) AND (NOT ChildClass.InheritsFrom(TGradTabPage)) then
          TControl(ChildClass.ClassParent).Parent := GetCurrentPage;
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

    with APage.TabButton do
    begin
       OnClick:=@PageButtonMouseClick;
       OnMouseDown:=@PageButtonMouseDown;
       OnMouseUp:=@PageButtonMouseUp;
       Left:=-123;
       ShowFocusBorder:=false;
       AutoWidth:=true;
       BorderSides:=[bsTopLine,bsRightLine,bsLeftLine];
       Parent := PagesBar;
       Caption:=APage.Name;
    end;

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
    FPagesBar.OrderButtons;
    //cRect := TGradTabControl(APage.Parent).GetClientRect;
    //APage.Color:=clBlue;
    //APage.ChangeBounds(cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
  end;

  FPagesBar.OrderButtons;
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
    FTabHeight:=Value;
    
    if TabPosition in [tpTop,tpBottom] then
       FBar.Height := Value
    else
       FBar.Width := Value;

    InvPaint;

    if ActivePage <> nil then AlignPage(ActivePage,GetClientRect);
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

    if FLongTabs then
       tempSize := FLongWidth;

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
         FBar.Width:=FTabHeight;
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
         FBar.Left:=Width-tempSize;
         FBar.Top:=0;
         FBar.Height:=Height;
         FBar.Width:=tempSize;
         //FBar.Align:=alRight;
      end;
    end;

    //FPagesBar.Align:=alClient;

    case Value of
      tpTop: begin
         FLeftButton.Align:=alLeft;
         FLeftButton.RotateDirection:=rdNormal;
         FRightButton.Align:=alRight;
         FRightButton.RotateDirection:=rdNormal;
         //FBar.Align:=alTop;
      end;
      tpLeft:begin
         FLeftButton.Align:=alTop;
         FLeftButton.RotateDirection:=rdRight;
         FRightButton.Align:=alBottom;
         FRightButton.RotateDirection:=rdRight;
         //FBar.Align:=alLeft;
      end;
      tpBottom:begin
         FLeftButton.Align:=alLeft;
         FLeftButton.RotateDirection:=rdNormal;
         FRightButton.Align:=alRight;
         FRightButton.RotateDirection:=rdNormal;
         //FBar.Align := alBottom;
      end;
      tpRight:begin
         FLeftButton.Align:=alTop;
         FLeftButton.RotateDirection:=rdRight;
         FRightButton.Align:=alBottom;
         FRightButton.RotateDirection:=rdRight;
         //FBar.Align:=alRight;
      end;
    end;

    FPagesBar.TabPosition:=Value;

    {$IFDEF DEBUGTAB}
         DebugLn('After');
         DebugLn('FBar Left %d Top %d Height %d Width %d',[ FBar.Left, Fbar.Top, FBar.Height, FBar.Width]);
         DebugLn('FPagesBar Left %d Top %d Height %d Width %d',[ FPagesBar.Left, FPagesbar.Top, FPagesBar.Height, FPagesBar.Width]);
         DebugLn('Control Left %d Top %d Height %d Width %d',[ Left, Top, Height, Width]);
    {$ENDIF}
    
    AlignPage(CurrentPage,GetClientRect);

    InvPaint;
end;

{------------------------------------------------------------------------------
  TGradTabControl SetLongTabs(Value : Boolean)
 ------------------------------------------------------------------------------}
procedure TGradTabControl.SetLongTabs(Value : Boolean);
begin
   FLongTabs:=Value;
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
      //if Assigned(fOnPageChanged) then fOnPageChanged(Self);
    end;
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
    FPageList.Insert(FPageList.Count, APage);

    //Result := FTabStrings.Count;
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
         tempR.Right:=Width-FTabHeight-2;
         tempR.Bottom:=Height-2;
      end;
      tpLeft: begin
         tempR.Top:=2;
         tempR.Left:=FTabHeight+2;
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
         Canvas.Line(0,0,Width-FTabHeight,0); //Top
         Canvas.Line(Width-1-FTabHeight,0,Width-1-FTabHeight,Height); //Right
         Canvas.Line(0,Height-1,Width-FTabHeight,Height-1);//Bottom
      end;
      tpLeft: begin
         Canvas.Line(FTabHeight,0,FTabHeight,Height); //Left
         Canvas.Line(FTabHeight,0,Width,0); //Top
         Canvas.Line(Width-1,0,Width-1,Height); //Right
         Canvas.Line(FTabHeight,Height-1,Width,Height-1);//Bottom
         {tempR.Top:=0;
         tempR.Left:=0;
         tempR.Right:=FTabHeight;
         tempR.Bottom:=Height;}
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
            FBar.Left:=Width-FTabHeight;
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
  DebugLn('UpdateDesignerFlags: Index: %d Assigned: %s',[APageIndex,BoolToStr(Assigned(Page[APageIndex]),true)]);

  if APageIndex<>FPageIndex then
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle+[csNoDesignVisible,csNoDesignSelectable]
  else
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle-[csNoDesignVisible,csNoDesignSelectable];

  DebugLn('UpdateDesignerFlags End');

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

