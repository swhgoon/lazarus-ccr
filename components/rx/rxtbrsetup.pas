unit rxtbrsetup;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  rxtoolbar, StdCtrls, ComCtrls, ExtCtrls, ButtonPanel;

type

  { TToolPanelSetupForm }

  TToolPanelSetupForm = class(TForm)
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbShowHint: TCheckBox;
    cbTransp: TCheckBox;
    cbFlatBtn: TCheckBox;
    cbShowCaption: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBtnAvaliable: TListBox;
    ListBtnVisible: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBtnAvaliableClick(Sender: TObject);
    procedure cbShowCaptionChange(Sender: TObject);
  private
    procedure FillItems(List:TStrings; AVisible:boolean);
    procedure UpdateStates;
  public
    FToolPanel:TToolPanel;
    constructor CreateSetupForm(AToolPanel:TToolPanel);
  end; 

var
  ToolPanelSetupForm: TToolPanelSetupForm;

implementation
uses vclutils, ActnList, boxprocs;

type
  THackToolPanel = class(TToolPanel);
{ TToolPanelSetupForm }

procedure TToolPanelSetupForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FToolPanel) then
  begin
    THackToolPanel(FToolPanel).SetCustomizing(false);
    THackToolPanel(FToolPanel).FCustomizer:=nil;
  end;
end;

procedure TToolPanelSetupForm.FormResize(Sender: TObject);
begin
  ListBtnVisible.Width:=BitBtn6.Left - 4 - ListBtnVisible.Left;
  ListBtnAvaliable.Left:=BitBtn6.Left + BitBtn6.Width + 4;
  ListBtnAvaliable.Width:=Width - ListBtnAvaliable.Left - 4;
  Label1.Left:=ListBtnAvaliable.Left;
end;

procedure TToolPanelSetupForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Offset:integer;
  P:TToolbarItem;
  BtnRect:TRect;
  Cnv:TCanvas;
begin
  Cnv:=(Control as TListBox).Canvas;
  Cnv.FillRect(ARect);       { clear the rectangle }
  P:=TToolbarItem((Control as TListBox).Items.Objects[Index]);
  if Assigned(P) then
  begin
    if Assigned(FToolPanel.ImageList) and Assigned(P.Action) then
    begin
      if (P.Action is TCustomAction) and
         (TCustomAction(P.Action).ImageIndex>-1) and
         (TCustomAction(P.Action).ImageIndex < FToolPanel.ImageList.Count) then
      begin
        Offset := 2;
        BtnRect.Top:=ARect.Top + 2;
        BtnRect.Left:=ARect.Left + Offset;
        BtnRect.Right:=BtnRect.Left + FToolPanel.BtnWidth;
        BtnRect.Bottom:=BtnRect.Top + FToolPanel.BtnHeight;
        Cnv.Brush.Color := clBtnFace;
        Cnv.FillRect(BtnRect);
        DrawButtonFrame(Cnv, BtnRect, false, false);
        FToolPanel.ImageList.Draw(Cnv, BtnRect.Left + (FToolPanel.BtnWidth - FToolPanel.ImageList.Width) div 2,
                                       BtnRect.Top + (FToolPanel.BtnHeight - FToolPanel.ImageList.Height) div 2,
                                       TCustomAction(P.Action).ImageIndex, True);
        Offset:=BtnRect.Right;
      end;
      Offset := Offset + 6;
      Cnv.TextOut(ARect.Left + Offset, (ARect.Top + ARect.Bottom  - Cnv.TextHeight('W')) div 2, TCustomAction(P.Action).Caption);  { display the text }
    end;
  end;
end;

procedure TToolPanelSetupForm.ListBtnAvaliableClick(Sender: TObject);
begin
  with (Sender as TListBox) do
  begin
    if (ItemIndex>-1) and (ItemIndex<Items.Count) then
    begin
      Panel1.Caption:=TCustomAction(TToolbarItem(Items.Objects[ItemIndex]).Action).Hint;
      if Sender = ListBtnVisible then
        cbShowCaption.Checked:=TToolbarItem(Items.Objects[ItemIndex]).ShowCaption;
    end;
  end;
end;

procedure TToolPanelSetupForm.cbShowCaptionChange(Sender: TObject);
begin
  if (ListBtnVisible.ItemIndex>-1) and (ListBtnVisible.ItemIndex<ListBtnVisible.Items.Count) then
    TToolbarItem(ListBtnVisible.Items.Objects[ListBtnVisible.ItemIndex]).ShowCaption:=cbShowCaption.Checked;
end;

procedure TToolPanelSetupForm.FillItems(List: TStrings; AVisible: boolean);
var
  i, p:integer;
begin
  List.Clear;
  for i:=0 to FToolPanel.Items.Count - 1 do
  begin
    if (FToolPanel.Items[i].Visible = AVisible) and Assigned(FToolPanel.Items[i].Action) then
    begin
      P:=List.Add(FToolPanel.Items[i].Action.Name);
      List.Objects[P]:=FToolPanel.Items[i];
    end;
  end;
end;

procedure TToolPanelSetupForm.UpdateStates;
var
  i:integer;
begin
  for I:=0 to ListBtnVisible.Items.Count - 1 do
    TToolbarItem(ListBtnVisible.Items.Objects[i]).Visible:=true;

  for I:=0 to ListBtnAvaliable.Items.Count - 1 do
    TToolbarItem(ListBtnAvaliable.Items.Objects[i]).Visible:=false;
    
  BitBtn6.Enabled:=ListBtnVisible.Items.Count>0;
  BitBtn5.Enabled:=ListBtnVisible.Items.Count>0;
  cbShowCaption.Enabled:=ListBtnVisible.Items.Count>0;

  BitBtn4.Enabled:=ListBtnAvaliable.Items.Count>0;
  BitBtn3.Enabled:=ListBtnAvaliable.Items.Count>0;
  cbFlatBtn.Checked:=tpTransparentBtns in FToolPanel.Options;
end;

procedure TToolPanelSetupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;


procedure TToolPanelSetupForm.CheckBox1Change(Sender: TObject);
var
  tpo:TToolPanelOptions;
begin
  tpo:=FToolPanel.Options;
  if cbTransp.Checked then
    tpo:=tpo + [tpTransparentBtns]
  else
    tpo:=tpo - [tpTransparentBtns];

  FToolPanel.ToolBarStyle:=TToolBarStyle(RadioGroup2.ItemIndex);

  if cbFlatBtn.Checked then
    tpo:=tpo + [tpFlatBtns]
  else
    tpo:=tpo - [tpFlatBtns];

  FToolPanel.ShowHint:=cbShowHint.Checked;
  FToolPanel.Options:=tpo;
  
  FToolPanel.ButtonAllign:=TToolButtonAllign(RadioGroup1.ItemIndex);
  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
end;

procedure TToolPanelSetupForm.BitBtn4Click(Sender: TObject);
begin
  BoxMoveSelectedItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn3Click(Sender: TObject);
begin
  BoxMoveAllItems(ListBtnAvaliable, ListBtnVisible);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn5Click(Sender: TObject);
begin
  BoxMoveSelectedItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
end;

procedure TToolPanelSetupForm.BitBtn6Click(Sender: TObject);
begin
  BoxMoveAllItems(ListBtnVisible, ListBtnAvaliable);
  UpdateStates;
end;

constructor TToolPanelSetupForm.CreateSetupForm(AToolPanel: TToolPanel);
begin
  inherited Create(AToolPanel);
  FormResize(nil);
  FToolPanel:=AToolPanel;


  cbFlatBtn.Checked:=tpFlatBtns in FToolPanel.Options;
  cbTransp.Checked:=tpTransparentBtns in FToolPanel.Options;
  cbShowHint.Checked:=FToolPanel.ShowHint;
  ListBtnAvaliable.ItemHeight:=FToolPanel.BtnHeight + 4;
  ListBtnVisible.ItemHeight:=FToolPanel.BtnHeight + 4;
  FillItems(ListBtnVisible.Items, true);
  FillItems(ListBtnAvaliable.Items, false);
  RadioGroup1.ItemIndex:=Ord(FToolPanel.ButtonAllign);
  RadioGroup2.ItemIndex:=Ord(FToolPanel.ToolBarStyle);

  UpdateStates;

  cbFlatBtn.OnChange:=@CheckBox1Change;
  cbTransp.OnChange:=@CheckBox1Change;
  cbShowHint.OnChange:=@CheckBox1Change;
  RadioGroup1.OnClick:=@CheckBox1Change;
  RadioGroup2.OnClick:=@CheckBox1Change;

  ListBtnAvaliable.ItemHeight:=FToolPanel.BtnHeight + 4;
  ListBtnVisible.ItemHeight:=FToolPanel.BtnHeight + 4;
end;

initialization
  {$I rxtbrsetup.lrs}

end.

