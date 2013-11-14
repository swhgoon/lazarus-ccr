(*
  TDI - Tabbed Document Interface for Lazarus - Show multiple forms in Tabs
  Copyright (C) 2012  Daniel Simões de Almeida

  You can get the latest version of this file in Lazarus CCR, located in:
  https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/tdi

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
  You can also get a copy of the license accessing the address:
  http://www.opensource.org/licenses/lgpl-license.php

  Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br
       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170
*)

unit TDIClass ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Menus,
  ExtendedNotebook, Buttons, Graphics, LMessages, LCLVersion  ;

const
  TDIM_CLOSEPAGE = LM_INTERFACELAST + 500;

type

  ETDIError = class( Exception ) ;

  TTDICloseTabButtom = (tbNone, tbMenu, tbButtom ) ;
  TTDIBackgroundCorner = (coTopLeft, coTopRight, coBottomLeft, coBottomRight);

  { TTDIAction }

  TTDIAction = class( TPersistent )
  private
    FCaption : String ;
    FImageIndex : Integer ;
    FVisible : Boolean ;
  public
    Constructor Create ;
  published
    property Caption    : String  read FCaption    write FCaption ;
    property ImageIndex : Integer read FImageIndex write FImageIndex ;
    property Visible    : Boolean read FVisible    write FVisible;
  end ;

  { TTDIActions }

  TTDIActions = Class( TPersistent )
  private
    FCloseAllTabs : TTDIAction ;
    FCloseTab : TTDIAction ;
    FNextTab : TTDIAction ;
    FPreviousTab : TTDIAction ;
    FTabsMenu : TTDIAction ;
  public
    Constructor Create ;
    Destructor Destroy ; override;
  published
    property TabsMenu     : TTDIAction read FTabsMenu     write FTabsMenu ;
    property CloseTab     : TTDIAction read FCloseTab     write FCloseTab ;
    property CloseAllTabs : TTDIAction read FCloseAllTabs write FCloseAllTabs ;
    property NextTab      : TTDIAction read FNextTab      write FNextTab ;
    property PreviousTab  : TTDIAction read FPreviousTab  write FPreviousTab ;
  end ;

  { TTDIPage }

  TTDIPage = class(TTabSheet)
  private
    fsFormInPage : TForm ;
    fsFormOldParent: TWinControl;
    fsFormOldCloseEvent : TCloseEvent;
    fsFormOldAlign : TAlign;
    fsFormOldClientRect : TRect;
    fsFormOldBorderStyle : TFormBorderStyle;
    fsLastActiveControl: TWinControl;

    procedure OnResizeTDIPage(Sender : TObject) ;
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure SaveFormProperties ;
    procedure RestoreFormProperties ;

    procedure SetFormInPage(AValue : TForm) ;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckFormAlign ;

  public
    constructor Create(TheOwner: TComponent );  override;
    destructor Destroy ; override;

    procedure RestoreLastFocusedControl ;

    property FormInPage : TForm read fsFormInPage write SetFormInPage ;
    property LastActiveControl : TWinControl read fsLastActiveControl write fsLastActiveControl ;
  end ;


  TTDIOption = ( tdiMiddleButtomClosePage, tdiRestoreLastActiveControl,
                 tdiVerifyIfCanChangePage ) ;
  TTDIOptions = set of TTDIOption ;
  { TTDINoteBook }

  TTDINoteBook = class(TExtendedNotebook)
  private
    FBackgroundImage : TImage ;
    FCloseTabButtom : TTDICloseTabButtom ;
    FFixedPages : Integer ;
    FMainMenu : TMainMenu ;
    FBackgroundCorner : TTDIBackgroundCorner ;
    FTDIActions : TTDIActions ;
    FTDIOptions : TTDIOptions ;
    FShortCutClosePage: TShortCut;

    procedure SetBackgroundImage(AValue : TImage) ;
    procedure SetBackgroundCorner(AValue : TTDIBackgroundCorner) ;
    procedure SetCloseTabButtom(AValue : TTDICloseTabButtom) ;
    procedure SetMainMenu(AValue : TMainMenu) ;
    procedure SetFixedPages(AValue : Integer) ;
  private
    FCloseBitBtn : TBitBtn ;
    FNextMenuItem : TMenuItem;
    FPreviousMenuItem : TMenuItem;
    FCloseMenuItem : TMenuItem ;
    FCloseMenuItem2 : TMenuItem ;
    FCloseAllTabsMenuItem : TMenuItem ;
    FTabsMenuItem : TMenuItem ;
    FTimerRestoreLastControl : TTimer;
    FIsRemovingAPage : Boolean;

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
    procedure NextPageClicked( Sender: TObject );
    procedure PreviousPageClicked( Sender: TObject );

    procedure TimerRestoreLastFocus( Sender: TObject );

    procedure RemoveInvalidPages ;
  protected
    function CanChange: Boolean;
       {$if (lcl_major > 0) or (lcl_release > 30)} override; {$endif}
    procedure DoChange; override;
    procedure Loaded; override;
    procedure RemovePage(Index: Integer);
       {$if (lcl_major > 0) or (lcl_release > 30)} override; {$endif}

    procedure msg_ClosePage(var Msg: TLMessage); message TDIM_CLOSEPAGE;

    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(TheOwner: TComponent);  override;
    destructor Destroy ; override;
    procedure DoCloseTabClicked(APage: TCustomPage); override;

    function CreateFormInNewPage( AFormClass: TFormClass;
      ImageIndex : Integer = -1 ) : TForm;
    procedure ShowFormInPage( AForm: TForm; ImageIndex : Integer = -1 );
    Function FindFormInPages( AForm: TForm): Integer ;

    Function CanCloseAllPages: Boolean ;
    Function CanCloseAPage( APageIndex: Integer): Boolean;

    procedure RestoreLastFocusedControl ;
    procedure ScrollPage( ToForward: Boolean );
    procedure CheckInterface;

    procedure UpdateTabsMenuItem ;

  published
    property BackgroundImage : TImage read FBackgroundImage
      write SetBackgroundImage  ;
    property BackgroundCorner : TTDIBackgroundCorner read FBackgroundCorner
        write SetBackgroundCorner default coBottomRight ;
    property MainMenu : TMainMenu read FMainMenu write SetMainMenu ;
    property CloseTabButtom : TTDICloseTabButtom read FCloseTabButtom
      write SetCloseTabButtom default tbMenu ;

    property TDIActions : TTDIActions read FTDIActions write FTDIActions ;

    property TDIOptions : TTDIOptions read FTDIOptions write FTDIOptions
      default [ tdiMiddleButtomClosePage, tdiRestoreLastActiveControl,
                tdiVerifyIfCanChangePage ];
    property ShortCutClosePage: TShortCut read FShortCutClosePage
      write FShortCutClosePage default 16499;  // Ctrl+F4

    property FixedPages : Integer read FFixedPages write SetFixedPages default 0;
  end ;


implementation

Uses LCLType, TDIConst;

{ TTDIAction }

constructor TTDIAction.Create ;
begin
  FCaption    := '';
  FImageIndex := -1;
  FVisible    := True;
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

  FNextTab := TTDIAction.Create;
  FNextTab.Caption := sActionNextTab;
  FNextTab.Visible := False;

  FPreviousTab := TTDIAction.Create;
  FPreviousTab.Caption := sActionPreviousTab;
  FPreviousTab.Visible := False;
end ;

destructor TTDIActions.Destroy ;
begin
  FCloseAllTabs.Free;
  FCloseTab.Free;
  FTabsMenu.Free;
  FNextTab.Free;
  FPreviousTab.Free;

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

destructor TTDIPage.Destroy ;
begin
  inherited Destroy ;
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

  // Saving Form Properties //
  SaveFormProperties ;

  // Adjusting Page Caption and Color as the Form //
  Caption := fsFormInPage.Caption;
  //Color := fsFormInPage.Color;

  // HiJacking the Form.OnClose Event, to detect Form Closed from Inside //
  fsFormInPage.OnClose := @OnFormClose;

  // Adjusting AForm Border Style and Align //
  fsFormInPage.BorderStyle := bsNone ;
  fsFormInPage.Align       := alClient ; 

  // Change Form Parent to the Page //
  fsFormInPage.Parent := Self;
end ;

procedure TTDIPage.Notification(AComponent : TComponent ; Operation : TOperation
  ) ;
begin
  inherited Notification(AComponent, Operation) ;

  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  if (Operation = opRemove) and (AComponent = fsFormInPage) then
  begin
    fsFormInPage := nil;
  end ;
end ;

procedure TTDIPage.CheckFormAlign ;
Var
  Maximize: Boolean ;
begin
  if not Assigned(fsFormInPage) then exit ;

  Maximize := not (( fsFormInPage.Constraints.MaxWidth <> 0 ) and (fsFormInPage.Width < Width)) ;
  if Maximize then
     Maximize := not (( fsFormInPage.Constraints.MaxHeight <> 0 ) and (fsFormInPage.Height < Height));

  { If Form has MaxConstrains and doesn't fill all the Screen, Centralize on
    TabSheet }
  if not Maximize then
  begin
    fsFormInPage.Align := alNone;

    if (fsFormInPage.Width < Width) then
      fsFormInPage.Left := Trunc( (Width - fsFormInPage.Width) / 2 )
    else
      fsFormInPage.Left := 0 ;

    if (fsFormInPage.Height < Height) then
      fsFormInPage.Top := Trunc( (Height - fsFormInPage.Height) / 2 )
    else
      fsFormInPage.Top := 0 ;
  end
  else
    fsFormInPage.Align := alClient;
end ;

procedure TTDIPage.OnResizeTDIPage(Sender : TObject) ;
begin
  CheckFormAlign;
end ;

procedure TTDIPage.OnFormClose(Sender : TObject ; var CloseAction : TCloseAction
  ) ;
var
  Msg: TLMessage;
begin
  if Assigned( fsFormOldCloseEvent ) then
     fsFormOldCloseEvent( Sender, CloseAction );

  if {(CloseAction <> caFree) and} Assigned( fsFormInPage ) then
    RestoreFormProperties;

  fsFormInPage := nil;

  if Assigned( Parent ) then
  begin
    Msg.msg    := TDIM_CLOSEPAGE;
    Msg.lParam := PageIndex;

    Parent.Dispatch( Msg );
  end ;
end ;

procedure TTDIPage.SaveFormProperties ;
begin
  if not Assigned( fsFormInPage ) then exit ;

  fsFormOldParent            := fsFormInPage.Parent;
  fsFormOldCloseEvent        := fsFormInPage.OnClose;
  fsFormOldAlign             := fsFormInPage.Align;
  fsFormOldBorderStyle       := fsFormInPage.BorderStyle;
  fsFormOldClientRect.Top    := fsFormInPage.Top;
  fsFormOldClientRect.Left   := fsFormInPage.Left;
  fsFormOldClientRect.Right  := fsFormInPage.Width;
  fsFormOldClientRect.Bottom := fsFormInPage.Height;
end ;

procedure TTDIPage.RestoreFormProperties ;
begin
  if not Assigned( fsFormInPage ) then exit ;

{  if ([csDesigning, csDestroying] * fsFormInPage.ComponentState <> []) then
     exit ;}

  fsFormInPage.Visible     := False;  // This prevent OnFormShow be fired
  fsFormInPage.Parent      := fsFormOldParent;
  fsFormInPage.Align       := fsFormOldAlign;
  fsFormInPage.BorderStyle := fsFormOldBorderStyle;
  fsFormInPage.Top         := fsFormOldClientRect.Top;
  fsFormInPage.Left        := fsFormOldClientRect.Left;
  fsFormInPage.Width       := fsFormOldClientRect.Right;
  fsFormInPage.Height      := fsFormOldClientRect.Bottom;
  fsFormInPage.OnClose     := fsFormOldCloseEvent;
end ;

{ TTDINoteBook }

constructor TTDINoteBook.Create(TheOwner : TComponent) ;
begin
  inherited Create(TheOwner) ;

  FCloseTabButtom        := tbMenu;
  FBackgroundCorner      := coBottomRight;
  FFixedPages            := 0;
  FIsRemovingAPage       := False;
  FShortCutClosePage     := 16499;
  FBackgroundImage       := nil;
  FCloseBitBtn           := nil;
  FCloseMenuItem         := nil;
  FCloseMenuItem2        := nil;
  FCloseAllTabsMenuItem  := nil;
  FTabsMenuItem          := nil;
  FNextMenuItem          := nil;
  FPreviousMenuItem      := nil;
  FTDIActions            := TTDIActions.Create;
  FTDIOptions            := [ tdiMiddleButtomClosePage,
                              tdiRestoreLastActiveControl,
                              tdiVerifyIfCanChangePage ] ;

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

procedure TTDINoteBook.DoCloseTabClicked(APage: TCustomPage);
var
  LastPageCount: Integer;
begin
  LastPageCount := PageCount;

  inherited DoCloseTabClicked(APage);

  if Assigned( APage ) and (LastPageCount = PageCount) then  // If Page was not closed...
  begin
    PageIndex := APage.PageIndex;

    if PageIndex >= FixedPages then
      RemovePage( APage.PageIndex );
  end;
end;

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
    RightJustify := True ;
    OnClick      := @DropDownTabsMenu;
  end ;
  FMainMenu.Items.Add( FTabsMenuItem );

  // Creating Sub-Menu options //

  // Creating a Separator //
  NewMenuItem := TMenuItem.Create( FTabsMenuItem );
  with NewMenuItem do
  begin
    Name    := 'miTDISeparator1';
    Caption := '-';
  end ;
  FTabsMenuItem.Add(NewMenuItem);

  if {$if (lcl_major > 0) or (lcl_release > 30)}
      (nboKeyboardTabSwitch in Options)
     {$else}
      True
     {$endif} then
  begin
    FNextMenuItem := TMenuItem.Create( FTabsMenuItem );
    with FNextMenuItem do
    begin
      Name       := 'miTDINextPage';
      ShortCut   := Menus.ShortCut(VK_TAB, [ssCtrl] );
      OnClick    := @NextPageClicked;
    end ;
    FTabsMenuItem.Add(FNextMenuItem);

    FPreviousMenuItem := TMenuItem.Create( FTabsMenuItem );
    with FPreviousMenuItem do
    begin
      Name       := 'miTDIPreviousPage';
      ShortCut   := Menus.ShortCut(VK_TAB, [ssCtrl,ssShift] );
      OnClick    := @PreviousPageClicked;
    end ;
    FTabsMenuItem.Add(FPreviousMenuItem);

    if TDIActions.NextTab.Visible or TDIActions.PreviousTab.Visible then
    begin
      // Creating a Separator //
      NewMenuItem := TMenuItem.Create( FTabsMenuItem );
      with NewMenuItem do
      begin
        Name    := 'miTDISeparator2';
        Caption := '-';
      end ;
      FTabsMenuItem.Add(NewMenuItem);
    end;
  end ;

  // Creating Close Tab MenuItem //
  FCloseMenuItem2 := TMenuItem.Create( FTabsMenuItem );
  with FCloseMenuItem2 do
  begin
    Name       := 'miTDICloseTab';
    OnClick    := @CloseTabClicked;
    ShortCut   := FShortCutClosePage;
  end ;
  FTabsMenuItem.Add(FCloseMenuItem2);

  // Creating Close All Tabs MenuItem //
  FCloseAllTabsMenuItem := TMenuItem.Create( FTabsMenuItem );
  with FCloseAllTabsMenuItem do
  begin
    Name       := 'miTDICloseAllTabs';
    OnClick    := @CloseAllTabsClicked;
  end ;
  FTabsMenuItem.Add(FCloseAllTabsMenuItem);

  UpdateTabsMenuItem;
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

function TTDINoteBook.CreateFormInNewPage(AFormClass: TFormClass;
   ImageIndex: Integer): TForm;
begin
  Result := AFormClass.Create(Application);

  ShowFormInPage( Result, ImageIndex );
end ;

procedure TTDINoteBook.ShowFormInPage(AForm : TForm ; ImageIndex : Integer) ;
Var
  NewPage : TTDIPage ;
  AlreadyExistingPage : Integer ;
begin
  if not Assigned( AForm ) then
     raise ETDIError.Create( sFormNotAssigned ) ;

  // Looking for a Page with same AForm Object //
  AlreadyExistingPage := FindFormInPages( AForm );
  if AlreadyExistingPage >= 0 then
  begin
    PageIndex := AlreadyExistingPage;
    exit ;
  end ;

  // Create a new Page
  NewPage := TTDIPage.Create(Self);
  NewPage.ImageIndex := ImageIndex;

  Visible := True;

  // This will call TTDIPage.SetFormInPage, who does the magic //
  NewPage.FormInPage := AForm;

  // Activate the new Page
  ActivePage := NewPage;

  // Show the Form //
  AForm.Visible := True ;

  // Saving the current ActiveControl in the Form //
  NewPage.LastActiveControl := AForm.ActiveControl;

  // Checking Form alignment //
  if (AForm.Constraints.MaxHeight <= 0) or
     (AForm.Constraints.MaxWidth <= 0) then
    AForm.Align := alClient;                   // Try to expand the Form
  NewPage.CheckFormAlign ;

  if PageCount = 1 then
    CheckInterface;
end ;

function TTDINoteBook.FindFormInPages(AForm : TForm) : Integer ;
var
  I : Integer ;
begin
  Result := -1;

  I := 0;
  while (Result < 0) and (I < PageCount) do
  begin
     if Pages[I] is TTDIPage then
       with TTDIPage( Pages[I] ) do
       begin
         if AForm = FormInPage then
           Result := I;
       end ;

     Inc( I ) ;
  end ;
end ;

procedure TTDINoteBook.CheckInterface ;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

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
  if Visible and (FTabsMenuItem <> nil) then
  begin
    with FTabsMenuItem do
    begin
      Caption    := TDIActions.TabsMenu.Caption;
      Visible    := TDIActions.TabsMenu.Visible;
      ImageIndex := TDIActions.TabsMenu.ImageIndex;
    end ;
  end ;

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
        FCloseMenuItem.Enabled := ( ActivePageIndex >= FFixedPages );
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

procedure TTDINoteBook.RestoreLastFocusedControl ;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  FTimerRestoreLastControl.Enabled := True;
end ;

procedure TTDINoteBook.ScrollPage(ToForward : Boolean) ;
var
  NewPage : Integer ;
begin
  if ToForward then
  begin
    NewPage := PageIndex + 1 ;
    if NewPage >= PageCount then
      NewPage := 0;
  end
  else
  begin
    NewPage := PageIndex - 1 ;
    if NewPage < 0 then
      NewPage := PageCount-1 ;
  end ;

  PageIndex := NewPage;
end ;


procedure TTDINoteBook.SelectTabByMenu(Sender : TObject) ;
begin
  if Sender is TMenuItem then
    ActivePageIndex := TMenuItem(Sender).Tag;
end ;

procedure TTDINoteBook.DropDownTabsMenu(Sender : TObject) ;
begin
  UpdateTabsMenuItem;
end ;

procedure TTDINoteBook.UpdateTabsMenuItem ;
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

  // Updating already existing MenuItems //
  with FCloseMenuItem2 do
  begin
    Enabled    := (PageCount > 0) and (ActivePageIndex >= FFixedPages);
    Caption    := TDIActions.CloseTab.Caption;
    Visible    := TDIActions.CloseTab.Visible;
    ImageIndex := TDIActions.CloseTab.ImageIndex;
  end ;

  with FCloseAllTabsMenuItem do
  begin
    Enabled    := (PageCount > FFixedPages);
    Caption    := TDIActions.CloseAllTabs.Caption;
    Visible    := TDIActions.CloseAllTabs.Visible;
    ImageIndex := TDIActions.CloseAllTabs.ImageIndex;
  end ;

  if FNextMenuItem <> nil then
    with FNextMenuItem do
    begin
      Enabled    := (PageCount > 1);
      Caption    := TDIActions.NextTab.Caption;
      Visible    := TDIActions.NextTab.Visible;
      ImageIndex := TDIActions.NextTab.ImageIndex;
    end ;

  if FPreviousMenuItem <> nil then
    with FPreviousMenuItem do
    begin
      Enabled    := (PageCount > 1);
      Caption    := TDIActions.PreviousTab.Caption;
      Visible    := TDIActions.PreviousTab.Visible;
      ImageIndex := TDIActions.PreviousTab.ImageIndex;
    end ;
end ;

procedure TTDINoteBook.NextPageClicked(Sender : TObject) ;
begin
  ScrollPage( True );
end ;

procedure TTDINoteBook.PreviousPageClicked(Sender : TObject) ;
begin
  ScrollPage( False );
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

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
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

          if tdiVerifyIfCanChangePage in FTDIOptions then
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

  Result := Result
    {$if (lcl_major > 0) or (lcl_release > 30)}
      and (inherited CanChange)
    {$endif};

  // Emulate FormInPage.OnDeactivate //
  (*
  if Result and (not FIsRemovingAPage) and
     ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
  begin
    if (ActivePage is TTDIPage) then
    begin
      with TTDIPage(ActivePage) do
      begin
        if Assigned( FormInPage ) then
          if ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
            if Assigned( FormInPage.OnDeactivate ) then
              if FormInPage.Visible then
                FormInPage.OnDeactivate( Self );
      end ;
    end ;
  end ;
  *)
end ;

procedure TTDINoteBook.DoChange ;
begin
  inherited DoChange;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  // Emulate FormInPage.OnActivate //
  (*
  if (not FIsRemovingAPage) and (ActivePage is TTDIPage) then
  begin
    with TTDIPage(ActivePage) do
    begin
      if Assigned( FormInPage ) then
        if ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
          if Assigned( FormInPage.OnActivate ) then
            if FormInPage.Visible then
              FormInPage.OnActivate( Self );
    end;
  end ;
  *)

  CheckInterface;

  {
  // This doesn't work on Win32, Focus always go to first control on Page //
  if FRestoreActiveControl then
    if (ActivePage is TTDIPage) then
      TTDIPage( ActivePage ).RestoreLastFocusedControl;
  }

  // This is a ugly workaround.. but it works :) //
  if tdiRestoreLastActiveControl in FTDIOptions then
    RestoreLastFocusedControl;
end ;

procedure TTDINoteBook.Loaded ;
begin
  inherited Loaded ;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  if Assigned( FMainMenu ) then
     CreateTabsMenuItem;

  CheckInterface;
end ;

procedure TTDINoteBook.RemovePage(Index : Integer) ;
Var
  CanRemovePage : Boolean ;
  APage         : TTabSheet;
begin
  CanRemovePage    := True;
  FIsRemovingAPage := True;
  APage            := Pages[Index] ;
  try
    if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
    begin
      if APage is TTDIPage then
      begin
        with TTDIPage(APage) do
        begin
          if Assigned( FormInPage ) then
          begin
            CanRemovePage := False;
            FormInPage.Close ;
          end ;
        end ;
      end ;
    end ;

    if CanRemovePage then
    begin
      {$if (lcl_major > 0) or (lcl_release > 30)}
        inherited RemovePage(APage.PageIndex) ;
      {$else}
        APage.Free;
      {$endif}

      if PageCount < 1 then  // On this case, DoChange is not fired //
        CheckInterface;
    end ;
  finally
    FIsRemovingAPage := False;
  end ;
end ;

procedure TTDINoteBook.msg_ClosePage(var Msg : TLMessage) ;
begin
  RemovePage( Msg.lParam );
end ;

procedure TTDINoteBook.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  APageIndex : Integer ;
begin
  if (tdiMiddleButtomClosePage in FTDIOptions) and (Button = mbMiddle) then
  begin
     APageIndex := TabIndexAtClientPos( Point(X,Y) );
     if (APageIndex >= 0) and (APageIndex >= FixedPages) then
     begin
       RemovePage( APageIndex );
       exit;
     end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TTDINoteBook.KeyDown(var Key : Word; Shift : TShiftState) ;
begin
  if (FTabsMenuItem = nil) then  // Is already Handled by TabsMenu itens?
  begin
    if (PageIndex >= FFixedPages) and
       (ShortCut(Key, Shift) = FShortCutClosePage) then
    begin
      Key := 0;
      RemovePage( PageIndex );
      exit;
    end;
  end
  else if (Key = VK_TAB) and (ssCtrl in Shift) then   // TabsMenu will do it...
    exit ;

  if ActivePage is TTDIPage then
  begin
    with TTDIPage( ActivePage ) do
    begin
      RestoreLastFocusedControl;

      // TODO: Propagate Key Pressed to FormInPage //
      //FormInPage.OnKeyDown(Self,Key,Shift);
    end ;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TTDINoteBook.Notification(AComponent : TComponent ;
  Operation : TOperation) ;
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) then
  begin
     if (AComponent = FBackgroundImage) then
       FBackgroundImage := nil

     else if (AComponent = FMainMenu) then
       FMainMenu := nil

     else if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then

     else if (AComponent is TForm) then
       RemoveInvalidPages ;
  end ;
end ;

procedure TTDINoteBook.DrawBackgroundImage ;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

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

procedure TTDINoteBook.RemoveInvalidPages ;
var
  I : Integer ;
begin
  // Remove all TTDIPage with FormInPage not assigned //;
  I := 0 ;
  while I < PageCount do
  begin
    if Page[I] is TTDIPage then
    begin
      with TTDIPage( Page[I] ) do
      begin
        if FormInPage = nil then
        begin
          RemovePage( I );
          Dec( I ) ;
        end ;
      end ;
    end ;

    Inc( I ) ;
  end ;
end ;

end.

