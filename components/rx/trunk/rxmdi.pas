{ Free DB Manager

  Copyright (C) 2005-2012 Lagunov Aleksey  alexs75 at hotbox.ru

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit RxMDI;

{$I rx.inc}

interface

uses
  Classes, SysUtils, Forms, Buttons, Menus, ExtCtrls, Graphics, Controls;

type
  TRxMDIPanel = class;
  TRxMDITasks = class;

  { TRxMDIButton }

  TRxMDIButton = class(TSpeedButton)
  private
    FNavForm: TForm;
    FActiveControl:TWinControl;
    FNavPanel:TRxMDITasks;
    FSaveClose:TCloseEvent;
    procedure SetRxMDIForm(AValue: TForm);
    procedure DoCreateMenuItems;

    procedure DoCloseMenu(Sender: TObject);
    procedure DoCloseAllMenu(Sender: TObject);
    procedure DoCloseAllExcepThisMenu(Sender: TObject);
    procedure DoActivateMenu(Sender: TObject);
    procedure DoCreateButtonImage;
  private
    FMenu:TPopupMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor CreateButton(AOwner:TRxMDITasks; AForm:TForm);
    procedure Click; override; // make Click public
    procedure UpdateCaption;
    property NavForm:TForm read FNavForm write SeTRxMDIForm;
  end;

  { TRxMDITasks }

  TRxMDITasks = class(TCustomPanel)
  private
    FBtnScrollLeft:TSpeedButton;
    FBtnScrollRigth:TSpeedButton;
    FMainPanel: TRxMDIPanel;
    procedure UpdateScrollBtnStatus;
    procedure ScrollLeftExecute(Sender: TObject);
    procedure ScrollRigthExecute(Sender: TObject);
    procedure ShowHiddenBtnOnResize;
    procedure ChildWindowsShowLast;
    procedure DoCloseAll(AIgnoreBtn:TRxMDIButton);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(Btn:TRxMDIButton);
    procedure ShowWindow(F:TForm);
    property MainPanel:TRxMDIPanel read FMainPanel write FMainPanel;
  published
    property Align;
  end;

  { TRxMDICloseButton }

  TRxMDICloseButton = class(TCustomSpeedButton)
  private
    FInfoLabel:TBoundLabel;
    FLabelSpacing:integer;
    FMDIPanel:TRxMDIPanel;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure DoPositionLabel;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateInternalLabel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Glyph;
    property Flat;
  end;

  { TRxMDIPanel }

  TRxMDIPanel = class(TCustomPanel)
  private
    FCurrentChildWindow: TForm;
    FCloseButton: TRxMDICloseButton;
    FTaskPanel: TRxMDITasks;
    procedure SetCurrentChildWindow(AValue: TForm);
    procedure navCloseButtonClick(Sender: TObject);
    procedure SetRxMDICloseButton(AValue: TRxMDICloseButton);
    procedure SetTaskPanel(AValue: TRxMDITasks);
    function MDIButtonByForm(AForm:TForm):TRxMDIButton;
    procedure HideCurrentWindow;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowWindow(F:TForm);

    procedure ChildWindowsAdd(F:TForm);
    procedure ChildWindowsCreate(var AForm; FC:TFormClass);
    procedure ChildWindowsUpdateCaption(F:TForm);
    procedure CloseAll;

    property CurrentChildWindow:TForm read FCurrentChildWindow write SetCurrentChildWindow;
  published
    property CloseButton:TRxMDICloseButton read FCloseButton write SetRxMDICloseButton;
    property TaskPanel:TRxMDITasks read FTaskPanel write SetTaskPanel;

    property Align;
    property BevelInner;
    property BevelOuter;
  end;

implementation
uses LResources, vclutils, rxconst;
//  LCLProc;


{ TRxMDICloseButton }

procedure TRxMDICloseButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  DoPositionLabel;
end;

procedure TRxMDICloseButton.Loaded;
begin
  inherited Loaded;
  DoPositionLabel;
end;

procedure TRxMDICloseButton.DoPositionLabel;
begin
  if FInfoLabel = nil then exit;
  if Parent<>nil then
    Parent.DisableAlign;
  //DebugLn(['TCustomLabeledEdit.DoPositionLabel ']);
  FInfoLabel.Parent := Parent;
  FInfoLabel.Visible := Visible;
{  case FLabelPosition of
    lpAbove:
      begin
        FInfoLabel.AnchorParallel(akLeft,0,Self);
        FInfoLabel.AnchorToCompanion(akBottom,FLabelSpacing,Self);
      end;
    lpBelow:
      begin
        FInfoLabel.AnchorParallel(akLeft,0,Self);
        FInfoLabel.AnchorToCompanion(akTop,FLabelSpacing,Self);
      end;
    lpLeft :
      begin}
        FInfoLabel.AnchorToCompanion(akRight,FLabelSpacing,Self);
        FInfoLabel.AnchorVerticalCenterTo(Self);
{      end;
    lpRight:
      begin
        FInfoLabel.AnchorToCompanion(akLeft,FLabelSpacing,Self);
        FInfoLabel.AnchorVerticalCenterTo(Self);
      end;
  end;}
  if Parent<>nil then
    Parent.EnableAlign;
end;

procedure TRxMDICloseButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FInfoLabel) and (Operation = opRemove) then
    FInfoLabel := nil
  else
  if (AComponent = FMDIPanel) and (Operation = opRemove) then
  begin
    FMDIPanel:=nil;
    OnClick:=nil;
  end;
end;

procedure TRxMDICloseButton.CreateInternalLabel;
begin
  if FInfoLabel<>nil then exit;
  FInfoLabel := TBoundLabel.Create(Self);
  FInfoLabel.ControlStyle := FInfoLabel.ControlStyle + [csNoDesignSelectable];
  //FInfoLabel.FocusControl := Self;
end;

constructor TRxMDICloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  CreateInternalLabel;
  Glyph:=LoadLazResBitmapImage('RxMDICloseIcon');
end;

{ TRxMDIPanel }

procedure TRxMDIPanel.SetCurrentChildWindow(AValue: TForm);
begin
  FCurrentChildWindow:=AValue;
  if Assigned(FCloseButton) then
  begin
    FCloseButton.Enabled:=Assigned(FCurrentChildWindow);
    if FCloseButton.Enabled then
      FCloseButton.FInfoLabel.Caption:=FCurrentChildWindow.Caption
    else
      FCloseButton.FInfoLabel.Caption:='';
  end;

  if Assigned(TaskPanel) then
    TaskPanel.Visible:=Assigned(FCurrentChildWindow);
end;

procedure TRxMDIPanel.navCloseButtonClick(Sender: TObject);
begin
  if Assigned(FCurrentChildWindow) then
    FCurrentChildWindow.Close;
end;

procedure TRxMDIPanel.SetRxMDICloseButton(AValue: TRxMDICloseButton);
begin
  if FCloseButton=AValue then Exit;
  if Assigned(FCloseButton) then
  begin
    FCloseButton.OnClick:=nil;
    FCloseButton.FMDIPanel:=nil;
  end;

  FCloseButton:=AValue;

  if Assigned(FCloseButton) then
  begin
    FCloseButton.OnClick:=@navCloseButtonClick;
    FCloseButton.FMDIPanel:=Self;
  end;
end;

procedure TRxMDIPanel.SetTaskPanel(AValue: TRxMDITasks);
begin
  if FTaskPanel=AValue then Exit;
  FTaskPanel:=AValue;
  if Assigned(FTaskPanel) then
    FTaskPanel.FMainPanel:=Self;
end;

function TRxMDIPanel.MDIButtonByForm(AForm: TForm): TRxMDIButton;
var
  i:integer;
begin
  Result:=nil;
  if not Assigned(FTaskPanel) then
    exit;
  for i:=0 to FTaskPanel.ComponentCount -1 do
  begin
    if (FTaskPanel.Components[i] is TRxMDIButton) and (TRxMDIButton(FTaskPanel.Components[i]).NavForm = AForm) then
    begin
      Result:=TRxMDIButton(FTaskPanel.Components[i]);
      exit;
    end;
  end;
end;

procedure TRxMDIPanel.HideCurrentWindow;
var
  MB:TRxMDIButton;
begin
  if Assigned(FCurrentChildWindow) and (FCurrentChildWindow.Visible) then
  begin
    MB:=MDIButtonByForm(FCurrentChildWindow);
    if Assigned(MB) then
      MB.FActiveControl:=Application.MainForm.ActiveControl;
    FCurrentChildWindow.Hide;
  end;
end;

procedure TRxMDIPanel.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FCloseButton) and (Operation = opRemove) then
    FCloseButton := nil
  else
  if (AComponent = FTaskPanel) and (Operation = opRemove) then
    FTaskPanel:=nil;
end;

procedure TRxMDIPanel.Loaded;
begin
  inherited Loaded;
  CurrentChildWindow:=nil;
end;

constructor TRxMDIPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:='';
  Align:=alClient;
  BevelOuter:=bvLowered;
end;

destructor TRxMDIPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TRxMDIPanel.ShowWindow(F: TForm);
begin
  TaskPanel.ShowWindow(F);
end;

procedure TRxMDIPanel.ChildWindowsAdd(F: TForm);
var
  B:TRxMDIButton;
begin
  HideCurrentWindow;
  F.BorderStyle:=bsNone;
  F.Align:=alClient;
  F.Parent:=Self;
  F.Visible:=true;
  F.BringToFront;
  Application.MainForm.ActiveControl:=F;

  B:=TRxMDIButton.CreateButton(TaskPanel, F);

end;

procedure TRxMDIPanel.ChildWindowsCreate(var AForm; FC: TFormClass);
var
  FForm:TForm absolute AForm;
begin
  if not Assigned(FForm) then
  begin
    HideCurrentWindow;
    FForm:=FC.Create(Self);
    ChildWindowsAdd(FForm);
  end
  else
    ShowWindow(FForm)
end;

procedure TRxMDIPanel.ChildWindowsUpdateCaption(F: TForm);
var
  i:integer;
  B:TRxMDIButton;
begin
  if (FCurrentChildWindow = F) and Assigned(FCloseButton) and FCloseButton.Enabled then
    FCloseButton.FInfoLabel.Caption:=F.Caption;

  for i:=0 to TaskPanel.ComponentCount -1 do
  begin
    if TRxMDIButton(TaskPanel.Components[i]).NavForm = F then
    begin
      TRxMDIButton(TaskPanel.Components[i]).UpdateCaption;
      exit;
    end;
  end;
end;

procedure TRxMDIPanel.CloseAll;
begin
  if Assigned(FTaskPanel) then
    FTaskPanel.DoCloseAll(nil);
end;


{ TRxMDITasks }

procedure TRxMDITasks.UpdateScrollBtnStatus;
var
  i, W:Integer;
  B:TRxMDIButton;
begin
  W:=FBtnScrollLeft.Width + FBtnScrollRigth.Width;
  FBtnScrollLeft.Enabled:=false;
  for i:=0 to ComponentCount-1 do
  begin
    B:=TRxMDIButton(Components[i]);
    if not B.Visible then
      FBtnScrollLeft.Enabled:=true
    else
      W:=W+B.Width + 2;
  end;

  FBtnScrollRigth.Enabled:=W > Width;
end;

procedure TRxMDITasks.ScrollLeftExecute(Sender: TObject);
var
  i:Integer;
  B:TRxMDIButton;
begin
  for i:=0 to ComponentCount-1 do
  begin
    if (Components[i] is TRxMDIButton) then
    begin
      B:=TRxMDIButton(Components[i]);
      if not B.Visible then
      begin
        B.Visible:=true;
        B.Left:=FBtnScrollLeft.Width;
        break;
      end;
    end;
  end;

  UpdateScrollBtnStatus;
  Invalidate;
end;

procedure TRxMDITasks.ScrollRigthExecute(Sender: TObject);
var
  i:Integer;
  B:TRxMDIButton;
begin
  for i:=0 to ComponentCount - 1 do
  begin
    if (Components[i] is TRxMDIButton) then
    begin
      B:=TRxMDIButton(Components[i]);
      if B.Visible then
      begin
        B.Visible:=false;
        break;
      end;
    end;
  end;

  UpdateScrollBtnStatus;
  Invalidate;
end;

procedure TRxMDITasks.ShowHiddenBtnOnResize;
begin

end;

procedure TRxMDITasks.ChildWindowsShowLast;
var
  CC:TControl;
  i:integer;
begin
{
  DebugLn(['FMainPanel.ControlCount =  ',  FMainPanel.ControlCount]);
  for i:=0 to  FMainPanel.ControlCount-1 do
    DebugLn(['FMainPanel.Controls[',i,'].Name =  ',  FMainPanel.Controls[i].Name]);
}



  if (FMainPanel.ControlCount>1) then
  begin
    CC:=FMainPanel.Controls[FMainPanel.ControlCount-2];
    if Assigned(CC) then
      ShowWindow(CC as TForm)
  end
  else
    FMainPanel.CurrentChildWindow:=nil;
  Invalidate;
end;

procedure TRxMDITasks.DoCloseAll(AIgnoreBtn: TRxMDIButton);
var
  i:integer;
begin
//  DebugLn('DoCloseAll');

  for i:=ComponentCount-1 downto 0 do
  begin
    if (Components[i] is TRxMDIButton) and (TRxMDIButton(Components[i]) <> AIgnoreBtn) then
      TRxMDIButton(Components[i]).DoCloseMenu(nil);
  end;
end;

procedure TRxMDITasks.Paint;
var
  i:integer;
  H:integer;
  B:TRxMDIButton;
begin
  inherited Paint;
  Canvas.Pen.Color:=clBlack;
  H:=Height - 2;
  for i:=0 to ComponentCount - 1 do
  begin
    if (Components[i] is TRxMDIButton) then
    begin
      B:=TRxMDIButton(Components[i]);
      if (B.Visible) and (B.Left > B.Width) then
      begin
        Canvas.Pen.Color:=clBtnShadow;
        Canvas.Line(B.Left - 2, 2, B.Left - 2, H);
        Canvas.Pen.Color:=clWindow;
        Canvas.Line(B.Left - 1, 2, B.Left - 1, H);
      end;
    end;
  end;
end;

procedure TRxMDITasks.Resize;
begin
  inherited Resize;
  if Assigned(FBtnScrollLeft) and Assigned(FBtnScrollRigth) then
    UpdateScrollBtnStatus;
end;

procedure TRxMDITasks.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FMainPanel) and (Operation = opRemove) then
    FMainPanel := nil
end;

constructor TRxMDITasks.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:='';

  FBtnScrollLeft:=TSpeedButton.Create(Self);
  FBtnScrollLeft.Parent:=Self;
  FBtnScrollLeft.Align:=alLeft;
  FBtnScrollLeft.AnchorSide[akLeft].Control:=Self;
  FBtnScrollLeft.Anchors:=[akLeft, akTop, akBottom];
  FBtnScrollLeft.OnClick:=@ScrollLeftExecute;
  FBtnScrollLeft.Caption:='<';
  FBtnScrollLeft.ShowCaption:=true;
  FBtnScrollLeft.AutoSize:=true;
  FBtnScrollLeft.Flat:=true;
  FBtnScrollLeft.Transparent:=false;

  FBtnScrollRigth:=TSpeedButton.Create(Self);
  FBtnScrollRigth.Parent:=Self;
  FBtnScrollRigth.Align:=alRight;
  FBtnScrollRigth.Anchors:=[akRight, akTop, akBottom];
  FBtnScrollRigth.AnchorSide[akRight].Control:=Self;
  FBtnScrollRigth.OnClick:=@ScrollRigthExecute;
  FBtnScrollRigth.Caption:='>';
  FBtnScrollRigth.ShowCaption:=true;
  FBtnScrollRigth.AutoSize:=true;
  FBtnScrollRigth.Flat:=true;
  FBtnScrollRigth.Transparent:=false;

  Align:=alBottom;
  Height:=25;
end;

destructor TRxMDITasks.Destroy;
begin
  FBtnScrollRigth:=nil;
  FBtnScrollLeft:=nil;
  inherited Destroy;
end;

procedure TRxMDITasks.AddButton(Btn: TRxMDIButton);
begin
  Btn.Parent:=Self;
  Btn.Left:=Width-1;
  Btn.Down:=true;
  Btn.BorderSpacing.Left:=3;
  Btn.BorderSpacing.Right:=3;

  FBtnScrollRigth.BringToFront;
  FBtnScrollLeft.BringToFront;

  UpdateScrollBtnStatus;
end;

procedure TRxMDITasks.ShowWindow(F: TForm);
var
  i:integer;
begin
  for i:=0 to ComponentCount -1 do
  begin
    if (Components[i] is TRxMDIButton) and (TRxMDIButton(Components[i]).NavForm = F) then
    begin
      TRxMDIButton(Components[i]).Click;
      TRxMDIButton(Components[i]).Visible:=true;
      exit;
    end;
  end;
end;


{ TRxMDIButton }

procedure TRxMDIButton.SetRxMDIForm(AValue: TForm);
var
  FImageIndex:integer;
  B:TBitmap;
begin
  if FNavForm=AValue then Exit;
  FNavForm:=AValue;
  if Assigned(FNavForm) then
  begin
    FSaveClose:=FNavForm.OnClose;
    //FSaveDeactivate:=FNavForm.OnDeactivate;
    FNavForm.OnClose:=@FormClose;
    //FNavForm.OnDeactivate:=@FormDeactivate;

    Caption:=' '+FNavForm.Caption+' ';
    DoCreateButtonImage;

    if Assigned(FNavPanel) then
      FNavPanel.FMainPanel.CurrentChildWindow:=NavForm;
  end;
end;

procedure TRxMDIButton.DoCreateMenuItems;
var
  Item: TMenuItem;
begin
  Item:=TMenuItem.Create(Self);
  Item.Caption:=Caption;
  Item.OnClick:=@DoActivateMenu;
  FMenu.Items.Add(Item);

  Item:=TMenuItem.Create(Self);
  Item.Caption:='-';
  FMenu.Items.Add(Item);

  Item:=TMenuItem.Create(Self);
  Item.Caption:=sCloseWindows;
  Item.OnClick:=@DoCloseMenu;
  FMenu.Items.Add(Item);

  Item:=TMenuItem.Create(Self);
  Item.Caption:='-';
  FMenu.Items.Add(Item);


  Item:=TMenuItem.Create(Self);
  Item.Caption:=sCloseAllExceptThis;
  Item.OnClick:=@DoCloseAllExcepThisMenu;
  FMenu.Items.Add(Item);

  Item:=TMenuItem.Create(Self);
  Item.Caption:=sCloseAllWindows;
  Item.OnClick:=@DoCloseAllMenu;
  FMenu.Items.Add(Item);
end;

procedure TRxMDIButton.DoCloseMenu(Sender: TObject);
begin
  if Assigned(FNavForm) then
    FNavForm.Close;
  Application.ProcessMessages;
end;

procedure TRxMDIButton.DoCloseAllMenu(Sender: TObject);
begin
  FNavPanel.DoCloseAll(nil);
end;

procedure TRxMDIButton.DoCloseAllExcepThisMenu(Sender: TObject);
begin
  FNavPanel.DoCloseAll(Self);
end;

procedure TRxMDIButton.DoActivateMenu(Sender: TObject);
begin
  Click;
end;

procedure TRxMDIButton.DoCreateButtonImage;
var
  FImageIndex:integer;
  B:TBitmap;
begin
  if Assigned(NavForm.Icon) and (NavForm.Icon.Count>0) then
  begin
    B:=TBitmap.Create;
    try
      B.Width:=NavForm.Icon.Width;
      B.Height:=NavForm.Icon.Height;

      B.Canvas.Brush.Color:=Color;
      B.Canvas.FillRect(0,0, B.Width, B.Height);
      B.Canvas.Draw(0, 0, NavForm.Icon);

      Glyph.Assign(B);
    finally
      B.Free;
    end;
  end;
end;

procedure TRxMDIButton.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FSaveClose) then
    FSaveClose(Sender, CloseAction);
  FNavPanel.ChildWindowsShowLast;
  FNavPanel.ShowHiddenBtnOnResize;
  CloseAction:=caFree;
  Owner.RemoveComponent(Self);
  Free;
end;

constructor TRxMDIButton.CreateButton(AOwner: TRxMDITasks; AForm: TForm);
begin
  inherited Create(AOwner);
  FNavPanel:=AOwner;
  Align:=alLeft;
  NavForm:=AForm;
  AutoSize:=true;
  Flat:=true;
  GroupIndex:=1;

  FMenu:=TPopupMenu.Create(Self);
  FMenu.Parent:=Self;
  PopupMenu:=FMenu;
  DoCreateMenuItems;

  AOwner.AddButton(Self);
end;

procedure TRxMDIButton.Click;
begin
  inherited Click;
  if Assigned(FNavForm) then
  begin
    FNavPanel.FMainPanel.HideCurrentWindow;
    FNavForm.Show;
    //FNavForm.BringToFront;
    FNavPanel.FMainPanel.CurrentChildWindow:=NavForm;
    //Application.MainForm.ActiveControl:=NavForm.ActiveControl;
    if Assigned(FActiveControl) then
      FActiveControl.SetFocus;
  end;
  Down:=true;
end;

procedure TRxMDIButton.UpdateCaption;
begin
  if Assigned(FNavForm) then
    Caption:=' '+FNavForm.Caption+' '
  else
    Caption:='---';
  AdjustSize;
end;

initialization
  {$I RxMDICloseIcon.lrs}
end.

