unit TDIClass ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Menus,
  ExtendedNotebook, Buttons, Graphics ;

type

  ETDIError = class( Exception ) ;

  TTDICloseTabButtom = (tbNone, tbMenu, tbButtom ) ;
  TTDIBackgroundCorner = (coTopLeft, coTopRight, coBottomLeft, coBottomRight);

  { TTDIAction }

  TTDIAction = class( TPersistent )
  private
    FCaption : String ;
    FImageIndex : Integer ;
  public
    Constructor Create ;
  published
    property Caption    : String  read FCaption    write FCaption ;
    property ImageIndex : Integer read FImageIndex write FImageIndex ;
  end ;

  { TTDIActions }

  TTDIActions = Class( TPersistent )
  private
    FCloseAllTabs : TTDIAction ;
    FCloseTab : TTDIAction ;
    FTabsMenu : TTDIAction ;
  public
    Constructor Create ;
    Destructor Destroy ; override;
  published
    property TabsMenu     : TTDIAction read FTabsMenu     write FTabsMenu ;
    property CloseTab     : TTDIAction read FCloseTab     write FCloseTab ;
    property CloseAllTabs : TTDIAction read FCloseAllTabs write FCloseAllTabs ;
  end ;

  { TTDIPage }

  TTDIPage = class(TTabSheet)
  private
    fsFormInPage : TForm ;
    fsLastActiveControl: TWinControl;

    procedure OnResizeTDIPage(Sender : TObject) ;

    procedure SetFormInPage(AValue : TForm) ;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckFormAlign ;

  public
    constructor Create(TheOwner: TComponent );  override;

    procedure RestoreLastFocusedControl ;

    property FormInPage : TForm read fsFormInPage write SetFormInPage ;
    property LastActiveControl : TWinControl read fsLastActiveControl write fsLastActiveControl ;
  end ;

  { TTDINoteBook }

  TTDINoteBook = class(TExtendedNotebook)
  private
    FBackgroundImage : TImage ;
    FCloseTabButtom : TTDICloseTabButtom ;
    FFixedPages : Integer ;
    FMainMenu : TMainMenu ;
    FBackgroundCorner : TTDIBackgroundCorner ;
    FTDIActions : TTDIActions ;

    procedure SetBackgroundImage(AValue : TImage) ;
    procedure SetBackgroundCorner(AValue : TTDIBackgroundCorner) ;
    procedure SetCloseTabButtom(AValue : TTDICloseTabButtom) ;
    procedure SetMainMenu(AValue : TMainMenu) ;
    procedure SetFixedPages(AValue : Integer) ;
  private
    FCloseBitBtn : TBitBtn ;
    FCloseMenuItem : TMenuItem ;
    FCloseMenuItem2 : TMenuItem ;
    FCloseAllTabsMenuItem : TMenuItem ;
    FRestoreActiveControl : Boolean ;
    FTabsMenuItem : TMenuItem ;
    FTimerRestoreLastControl : TTimer;
    FVerifyIfCanChangePage : Boolean ;

    procedure CreateCloseBitBtn ;
    procedure CreateCloseMenuItem ;
    procedure CreateTabsMenuItem ;

    procedure ShowCloseButtom ;
    procedure HideCloseButtom ;
    procedure DrawBackgroundImage ;

    procedure CloseTabClicked( Sender: TObject );
    procedure CloseAllTabsClicked( Sender: TObject );
    procedure SelectTabByMenu( Sender: TObject );
    procedure DropDownTabsMenu( Sender: TObject );

    procedure TimerRestoreLastFocus( Sender: TObject );

  protected
    function CanChange: Boolean; override;
    procedure DoChange; override;
    procedure Loaded; override;
    procedure RemovePage(Index: Integer); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(TheOwner: TComponent );  override;
    destructor Destroy ; override;

    procedure CreateFormInNewPage( AFormClass: TFormClass; ImageIndex : Integer = -1 ) ;
    procedure ShowForInNewPage( AForm: TForm; ImageIndex : Integer = -1 );

    Function CanCloseAllPages: Boolean ;
    Function CanCloseAPage( APageIndex: Integer): Boolean;
    procedure CheckInterface;

  published
    property BackgroundImage : TImage read FBackgroundImage
      write SetBackgroundImage  ;
    property BackgroundCorner : TTDIBackgroundCorner read FBackgroundCorner
        write SetBackgroundCorner default coBottomRight ;
    property MainMenu : TMainMenu read FMainMenu write SetMainMenu ;
    property CloseTabButtom : TTDICloseTabButtom read FCloseTabButtom
      write SetCloseTabButtom default tbMenu ;

    property TDIActions : TTDIActions read FTDIActions write FTDIActions ;

    property RestoreActiveControl : Boolean read FRestoreActiveControl
      write FRestoreActiveControl default True;
    property VerifyIfCanChangePage : Boolean read FVerifyIfCanChangePage
      write FVerifyIfCanChangePage default True;
    property FixedPages : Integer read FFixedPages write SetFixedPages default 0;
  end ;


implementation

Uses TDIConst ;

{ TTDIAction }

constructor TTDIAction.Create ;
begin
  FCaption    := '';
  FImageIndex := -1;
end ;

{ TTDIActions }

constructor TTDIActions.Create ;
begin
  FCloseAllTabs := TTDIAction.Create;
  FCloseAllTabs.Caption := sActionCloseAllTabs;

  FCloseTab := TTDIAction.Create;
  FCloseTab.Caption := sActionCloseTab;

  FTabsMenu := TTDIAction.Create;
  FTabsMenu.Caption := sActionTabsMenu;
end ;

destructor TTDIActions.Destroy ;
begin
  FCloseAllTabs.Free;
  FCloseTab.Free;
  FTabsMenu.Free;

  inherited Destroy;
end ;

{ TTDIPage }

constructor TTDIPage.Create(TheOwner : TComponent) ;
begin
  inherited Create(TheOwner) ;

  Self.Parent   := TWinControl( TheOwner ) ;
  Self.OnResize := @OnResizeTDIPage ;

  fsLastActiveControl := nil ;
end ;

procedure TTDIPage.RestoreLastFocusedControl ;
begin
  if Assigned( fsLastActiveControl ) then
  begin
    if fsLastActiveControl <> Screen.ActiveControl then
    begin
      if fsLastActiveControl.Visible and fsLastActiveControl.Enabled then
      begin
        try
           fsLastActiveControl.SetFocus ;
           //FormInPage.ActiveControl := fsLastActiveControl;
        except
        end ;
      end ;
    end
  end
  else
  begin
    { No LastActiveControle ? Ok, if current Screen control isn't in TabSheet,
      go to first Control on TabSheet... }
    if not Self.ContainsControl( Screen.ActiveControl ) then
      Self.SelectNext( Self, True, True);
  end
end ;

procedure TTDIPage.SetFormInPage(AValue : TForm) ;
begin
  fsFormInPage := AValue ;

  // Adjusting Page Caption and Color as the Form //
  Caption := fsFormInPage.Caption;
  //Color := fsFormInPage.Color;

  // Adjusting AForm Border Style and Align //
  fsFormInPage.BorderStyle := bsNone ;
  //fsFormInPage.Align     := alClient ;   // This will be done in OnResizeTDIPage

  // Change Form Parent to the Page //
  fsFormInPage.Parent := Self;
  //fsFormInPage.FreeNotification(Self);  // This cause a SIGSEGV, when Form is Closed from inside

  // Show the Form //
  fsFormInPage.Visible := True ;

  // Saving the current ActiveControl in the Form //
  fsLastActiveControl := fsFormInPage.ActiveControl;
end ;

procedure TTDIPage.Notification(AComponent : TComponent ; Operation : TOperation
  ) ;
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) and (AComponent = fsFormInPage) then
    fsFormInPage := nil;
end ;

procedure TTDIPage.CheckFormAlign ;
begin
  if not Assigned(fsFormInPage) then exit ;

  { If Form has MaxConstrains and doesn't fill all the Screen, Centralize on
    TabSheet }
  if (fsFormInPage.Width < Width) and (fsFormInPage.Height < Height) then
  begin
    fsFormInPage.Align := alNone;

    if (fsFormInPage.Width < Width) then
      fsFormInPage.Left  := Trunc( (Width - fsFormInPage.Width) / 2 );

    if (fsFormInPage.Height < Height) then
      fsFormInPage.Top   := Trunc( (Height - fsFormInPage.Height) / 2 );
  end
  else
    fsFormInPage.Align := alClient;
end ;

procedure TTDIPage.OnResizeTDIPage(Sender : TObject) ;
begin
  CheckFormAlign;
end ;

{ TTDINoteBook }

constructor TTDINoteBook.Create(TheOwner : TComponent) ;
begin
  inherited Create(TheOwner) ;

  FCloseTabButtom        := tbMenu;
  FBackgroundCorner      := coBottomRight;
  FFixedPages            := 0;
  FRestoreActiveControl  := True;
  FVerifyIfCanChangePage := True;
  FBackgroundImage       := nil;
  FCloseBitBtn           := nil;
  FCloseMenuItem         := nil;
  FCloseMenuItem2        := nil;
  FCloseAllTabsMenuItem  := nil;
  FTabsMenuItem          := nil;
  FTDIActions            := TTDIActions.Create;

  { This is ugly, I know... but I didn't found a best solution to restore Last
    Focused Control of TDIPage }
  FTimerRestoreLastControl := TTimer.Create(Self);
  FTimerRestoreLastControl.Enabled  := False;
  FTimerRestoreLastControl.Interval := 10;
  FTimerRestoreLastControl.OnTimer  := @TimerRestoreLastFocus;
end ;

destructor TTDINoteBook.Destroy ;
begin
  if Assigned( FCloseBitBtn )  then
    FCloseBitBtn.Free ;

  { // Don't Destroy Menu Items... They will be destroyed by MainMenu //

  if Assigned( FCloseMenuItem )  then
    FCloseMenuItem.Free ;

  if Assigned( FTabsMenuItem )  then
  begin
    FTabsMenuItem.Free ;
    FCloseMenuItem2.Free;
    FCloseAllTabsMenuItem.Free;
  end ;
  }

  FTDIActions.Free;

  FTimerRestoreLastControl.Free;

  inherited Destroy;
end ;

procedure TTDINoteBook.CreateCloseBitBtn ;
begin
  if FCloseBitBtn <> nil then exit;

  FCloseBitBtn := TBitBtn.Create( Self ) ;
  with FCloseBitBtn do
  begin
    Name     := 'CloseBitBtn';
    Caption  := 'X';
    Visible  := False ;
    Parent   := Nil;
    Height   := 22;
    Width    := 22;
    Layout   := blGlyphTop;
    OnClick  := @CloseTabClicked;
    TabStop  := False;
    AnchorSideTop.Control   := Self;
    AnchorSideRight.Control := Self;
    AnchorSideRight.Side    := asrBottom;
    Anchors                 := [akTop, akRight]
  end ;

  if Self.Owner is TWinControl then
    FCloseBitBtn.Parent := TWinControl(Self.Owner) ;

  // Setting Image to FCloseBitBtn //;
  if Assigned( Images ) and (FTDIActions.CloseTab.ImageIndex > -1) then
  begin
     Images.GetBitmap( FTDIActions.CloseTab.ImageIndex, FCloseBitBtn.Glyph );
     FCloseBitBtn.Caption := '';
  end ;
end ;

procedure TTDINoteBook.CreateCloseMenuItem ;
begin
  if FCloseMenuItem <> nil then exit;

  if not Assigned( FMainMenu ) then
     raise ETDIError.Create( sMainMenuNotAssigned );

  FCloseMenuItem := TMenuItem.Create( FMainMenu );
  with FCloseMenuItem do
  begin
    Name := 'miTDICloseButtom';
    if (TDIActions.CloseTab.ImageIndex < 0) or
       (not Assigned( FMainMenu.Images )) or
       (TDIActions.CloseTab.ImageIndex >= Images.Count)  then
      Caption := 'X'
    else
    begin
      Caption := '' ;
      ImageIndex := TDIActions.CloseTab.ImageIndex;
    end ;

    RightJustify := True ;
    OnClick      := @CloseTabClicked;
  end ;

  FMainMenu.Items.Add( FCloseMenuItem );
end ;

procedure TTDINoteBook.CreateTabsMenuItem ;
Var
  NewMenuItem : TMenuItem;
begin
  if FTabsMenuItem <> nil then exit;

  if not Assigned( FMainMenu ) then
     raise ETDIError.Create( sMainMenuNotAssigned );

  // Creating entry on MainMenu //
  FTabsMenuItem := TMenuItem.Create( FMainMenu );
  with FTabsMenuItem do
  begin
    Name         := 'miTDITabsMenuItem';
    Caption      := TDIActions.TabsMenu.Caption;
    ImageIndex   := TDIActions.TabsMenu.ImageIndex;
    RightJustify := True ;
    OnClick      := @DropDownTabsMenu;

  end ;
  FMainMenu.Items.Add( FTabsMenuItem );

  // Creating Sub-Menu options //

  // Creating a Separator //
  NewMenuItem := TMenuItem.Create( FTabsMenuItem );
  with NewMenuItem do
  begin
    Name    := 'miTDISeparator';
    Caption := '-';
  end ;
  FTabsMenuItem.Add(NewMenuItem);

  // Creating Close Tab MenuItem //
  FCloseMenuItem2 := TMenuItem.Create( FTabsMenuItem );
  with FCloseMenuItem2 do
  begin
    Name       := 'miTDICloseTab';
    Caption    := TDIActions.CloseTab.Caption;
    ImageIndex := TDIActions.CloseTab.ImageIndex;
    OnClick    := @CloseTabClicked;
  end ;
  FTabsMenuItem.Add(FCloseMenuItem2);

  // Creating Close All Tabs MenuItem //
  FCloseAllTabsMenuItem := TMenuItem.Create( FTabsMenuItem );
  with FCloseAllTabsMenuItem do
  begin
    Name       := 'miTDICloseAllTabs';
    Caption    := TDIActions.CloseAllTabs.Caption;
    ImageIndex := TDIActions.CloseAllTabs.ImageIndex;
    OnClick    := @CloseAllTabsClicked;
  end ;
  FTabsMenuItem.Add(FCloseAllTabsMenuItem);
end ;

procedure TTDINoteBook.SetFixedPages(AValue : Integer) ;
begin
  if FFixedPages = AValue then Exit ;
  FFixedPages := AValue ;

  CheckInterface;
end ;

procedure TTDINoteBook.SetBackgroundImage(AValue : TImage) ;
begin
  if FBackgroundImage = AValue then Exit ;
  FBackgroundImage := AValue ;

  if Visible then
    DrawBackgroundImage;
end ;

procedure TTDINoteBook.SetBackgroundCorner(AValue : TTDIBackgroundCorner) ;
begin
  if FBackgroundCorner = AValue then Exit ;
  FBackgroundCorner := AValue ;

  if Visible then
    DrawBackgroundImage;
end ;

procedure TTDINoteBook.SetCloseTabButtom(AValue : TTDICloseTabButtom) ;
begin
  if FCloseTabButtom = AValue then Exit ;

  if (AValue = tbButtom) and  (not (Owner is TWinControl)) then
    raise ETDIError.Create( sOwnerIsNotWinControl ) ;

  FCloseTabButtom := AValue ;
end ;

procedure TTDINoteBook.SetMainMenu(AValue : TMainMenu) ;
begin
  if FMainMenu = AValue then Exit ;
  FMainMenu := AValue ;
end ;

procedure TTDINoteBook.CreateFormInNewPage(AFormClass : TFormClass ;
  ImageIndex : Integer) ;
Var
  NewForm : TForm ;
begin
  NewForm := AFormClass.Create(nil);

  ShowForInNewPage( NewForm, ImageIndex );
end ;

procedure TTDINoteBook.ShowForInNewPage(AForm : TForm ; ImageIndex : Integer) ;
Var
  NewPage : TTDIPage ;
begin
  // Create a new Page
  NewPage := TTDIPage.Create(Self);
  NewPage.ImageIndex := ImageIndex;

  Visible := True;

  // This will call TTDIPage.SetFormInPage, who does the magic //
  NewPage.FormInPage := AForm;

  // Activate the new Page
  ActivePage := NewPage;

  // First Page always need a little help for align form inside //
  if PageCount = 1 then
  begin
    NewPage.CheckFormAlign ;
    CheckInterface;
  end ;
end ;

procedure TTDINoteBook.CheckInterface ;
begin
  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  Visible := (PageCount > 0);

  // Checking for Close Button visibility //
  if (FCloseTabButtom <> tbNone) then
  begin
    if Visible then
      ShowCloseButtom
    else
      HideCloseButtom;
  end ;

  // Checking for Tabs Menu visibility //
  if FTabsMenuItem <> nil then
     FTabsMenuItem.Visible := Visible ;

  // Drawing Background Image //
  if Visible then
    DrawBackgroundImage;
end ;

procedure TTDINoteBook.ShowCloseButtom ;
begin
  case FCloseTabButtom of
    tbButtom :
      begin
        if FCloseBitBtn = nil then
          CreateCloseBitBtn;

        if not FCloseBitBtn.Visible then
        begin
          FCloseBitBtn.Visible := True ;
          FCloseBitBtn.BringToFront;
        end ;
        FCloseBitBtn.Enabled := ( ActivePageIndex >= FFixedPages );
      end ;

    tbMenu :
      begin
        if FCloseMenuItem = nil then
          CreateCloseMenuItem;

        FCloseMenuItem.Visible := True ;
        FCloseMenuItem.Enabled   := ( ActivePageIndex >= FFixedPages );
      end ;
  end ;

end ;

procedure TTDINoteBook.HideCloseButtom ;
begin
  if FCloseBitBtn <> nil then
    FCloseBitBtn.Visible := False;
  if FCloseMenuItem <> nil then
    FCloseMenuItem.Visible := False;
end ;

procedure TTDINoteBook.CloseTabClicked(Sender : TObject) ;
begin
  RemovePage( ActivePageIndex );
end ;

procedure TTDINoteBook.CloseAllTabsClicked(Sender : TObject) ;
Var
  LastPageCount : Integer ;
begin
   if PageCount < 1 then exit ;

   LastPageCount := -1 ;
   PageIndex     := PageCount-1;  // Go to Last page
   // Close while have pages, and Pages still being closed //
   while (PageCount > FFixedPages) and (LastPageCount <> PageCount) do
   begin
     LastPageCount := PageCount ;
     RemovePage( ActivePageIndex );
     Application.ProcessMessages;
   end;
end ;

function TTDINoteBook.CanCloseAllPages : Boolean ;
Var
  I : Integer ;
begin
  Result := True;
  if PageCount < 1 then exit ;

  I := 0;
  while Result and ( I < PageCount ) do
  begin
    Result := CanCloseAPage( I );
    Inc(I)
  end ;
end ;

function TTDINoteBook.CanCloseAPage(APageIndex : Integer) : Boolean ;
begin
  Result := True;

  if Pages[APageIndex] is TTDIPage then
    with TTDIPage(Pages[APageIndex]) do
    begin
      if Assigned( FormInPage ) then
        Result := FormInPage.CloseQuery;
    end ;
end ;


procedure TTDINoteBook.SelectTabByMenu(Sender : TObject) ;
begin
  if Sender is TMenuItem then
    ActivePageIndex := TMenuItem(Sender).Tag;
end ;

procedure TTDINoteBook.DropDownTabsMenu(Sender : TObject) ;
Var
  I : Integer ;
  NewMenuItem : TMenuItem ;
begin
  // Removing Menu Items until find Separator '-' //
  NewMenuItem := FTabsMenuItem.Items[0] ;
  while (NewMenuItem.Caption <> '-') do
  begin
     FTabsMenuItem.Remove(NewMenuItem);
     NewMenuItem.Free ;
     NewMenuItem := FTabsMenuItem.Items[0] ;
  end ;

  // Inserting on Menu Items for existing Tabs //
  for I := PageCount-1 downto 0 do
  begin
     NewMenuItem := TMenuItem.Create(FTabsMenuItem);
     NewMenuItem.Caption    := Page[I].Caption ;
     NewMenuItem.ImageIndex := Page[I].ImageIndex ;
     NewMenuItem.OnClick    := @SelectTabByMenu ;
     NewMenuItem.Tag        := I ;
     NewMenuItem.Checked    := (I = PageIndex ) ;

     FTabsMenuItem.Insert(0,NewMenuItem);
  end ;

  FCloseMenuItem2.Enabled       := (PageCount > 0) and
                                   (ActivePageIndex >= FFixedPages);
  FCloseAllTabsMenuItem.Enabled := (PageCount > 0);
end ;

procedure TTDINoteBook.TimerRestoreLastFocus(Sender : TObject) ;
begin
  FTimerRestoreLastControl.Enabled := False;

  if Assigned( ActivePage ) then
    if ActivePage is TTDIPage then
      TTDIPage( ActivePage ).RestoreLastFocusedControl;
end ;

function TTDINoteBook.CanChange : Boolean ;
Var
  AWinControl : TWinControl ;
begin
  Result := True;

  if ([csDesigning, csDestroying] * ComponentState = []) then
  begin
    if Assigned( ActivePage ) then
    begin
      // Saving Last Active Control in Page //
      AWinControl := Screen.ActiveControl;

      if ActivePage is TTDIPage then
      begin
        if ActivePage.ContainsControl( AWinControl ) then
        begin
          TTDIPage( ActivePage ).LastActiveControl := AWinControl;

          if FVerifyIfCanChangePage then
          begin
            { Try to detect if occurs some exception when leaving current
              control focus. This may occurs in TWinControl.OnExit Validation }
            Self.SetFocus;

            { If still on same ActiveControl, maybe Focus Control was trapped on
              some OnExit Validation }
            Result := ( AWinControl <> Screen.ActiveControl );
          end ;
        end ;
      end ;
    end ;
  end ;

  Result := Result and (inherited CanChange) ;
end ;

procedure TTDINoteBook.DoChange ;
begin
  inherited DoChange;

  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  CheckInterface;

  {
  // This doesn't work on Win32, Focus always go to first control on Page //
  if FRestoreActiveControl then
    if (ActivePage is TTDIPage) then
      TTDIPage( ActivePage ).RestoreLastFocusedControl;
  }

  // This is a ugly workaround.. but it works :) //
  FTimerRestoreLastControl.Enabled := True;
end ;

procedure TTDINoteBook.Loaded ;
begin
  inherited Loaded ;

  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  if Assigned( FMainMenu ) then
     CreateTabsMenuItem;

  CheckInterface;
end ;

procedure TTDINoteBook.RemovePage(Index : Integer) ;
Var
  CanRemovePage : Boolean ;
begin
  CanRemovePage := True;

  if ([csDesigning, csDestroying] * ComponentState = []) then
    if Pages[Index] is TTDIPage then
      with TTDIPage(Pages[Index]) do
      begin
        if Assigned( FormInPage ) then
        begin
          CanRemovePage := FormInPage.CloseQuery ;
          if CanRemovePage then
            FormInPage.Close ;
        end ;
      end ;

  if CanRemovePage then
  begin
    inherited RemovePage(Index) ;

    if PageCount < 1 then  // On this case, DoChange is not fired //
      CheckInterface;
  end ;
end ;

procedure TTDINoteBook.Notification(AComponent : TComponent ;
  Operation : TOperation) ;
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) then
  begin
     if (AComponent = FBackgroundImage) then
       FBackgroundImage := nil ;

     if (AComponent = FMainMenu) then
       FMainMenu := nil ;


     if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

     if (AComponent is TForm) then
       if TForm(AComponent).Parent is TTDIPage then
         RemovePage( TTDIPage(TForm(AComponent).Parent).PageIndex  );
  end ;
end ;

procedure TTDINoteBook.DrawBackgroundImage ;
begin
  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  if not Assigned( FBackgroundImage ) then exit ;

  if not Assigned( ActivePage ) then exit ;

  FBackgroundImage.Parent  := ActivePage;
  FBackgroundImage.Anchors := [];
  FBackgroundImage.AnchorSideBottom.Control := nil;
  FBackgroundImage.AnchorSideTop.Control    := nil;
  FBackgroundImage.AnchorSideRight.Control  := nil;
  FBackgroundImage.AnchorSideLeft.Control   := nil;

  if FBackgroundCorner in [coBottomRight, coBottomLeft] then
  begin
     FBackgroundImage.AnchorSideBottom.Control := ActivePage;
     FBackgroundImage.AnchorSideBottom.Side    := asrBottom;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akBottom];
  end
  else
  begin
     FBackgroundImage.AnchorSideTop.Control := ActivePage;
     FBackgroundImage.AnchorSideTop.Side    := asrTop;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akTop];
  end ;

  if FBackgroundCorner in [coBottomRight, coTopRight] then
  begin
     FBackgroundImage.AnchorSideRight.Control := ActivePage;
     FBackgroundImage.AnchorSideRight.Side    := asrBottom;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akRight];
  end
  else
  begin
     FBackgroundImage.AnchorSideLeft.Control := ActivePage;
     FBackgroundImage.AnchorSideLeft.Side    := asrTop;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akLeft];
  end ;

  FBackgroundImage.Visible := True ;
end ;

end.

