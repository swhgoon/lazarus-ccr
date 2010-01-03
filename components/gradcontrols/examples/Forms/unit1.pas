unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ugradbtn, StdCtrls, ComCtrls, LCLType, LCLProc, LCLIntf, Buttons, ugradtabcontrol,
  Menus, Spin, EditBtn, types;

type

  TTryOutPage = class(TGradTabPage)
  
  end;

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    GradButton1: TGradButton;
    GradTabPage1: TGradTabPage;
    GradTabPage2: TGradTabPage;
    GradTabPage3: TGradTabPage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    NewPageBtn: TGradButton;
    DeleteBtn: TGradButton;
    GradTabControl1: TGradTabControl;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    SpinEdit2: TSpinEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure GradButton1Click(Sender: TObject);
    procedure GradTabControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GradTabControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GradTabControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GradTabControl1MouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure GradTabControl1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GradTabControl1PageChanged(Sender: TObject);
    procedure GradTabControl1TabButtonMouseDown(
      GradTabControl: TGradTabControl; Button: TMouseButton;
      Shift: TShiftState; X, Y, AIndex: Integer);
    procedure GradTabControl1TabButtonMouseMove(
      GradTabControl: TGradTabControl; Shift: TShiftState; X, Y, AIndex: Integer
      );
    procedure GradTabControl1TabButtonMouseUp(GradTabControl: TGradTabControl;
      Button: TMouseButton; Shift: TShiftState; X, Y, AIndex: Integer);
    procedure GradTabControl1TabCloseButtonClick(
      GradTabControl: TGradTabControl; AIndex: Integer);
    procedure NewPageBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure SpinEdit2EditingDone(Sender: TObject);
  private
    { private declarations }
  public
    FDragIndex : Integer;
    { public declarations }
  end;

var
  Form1: TForm1; 

implementation

uses
  gradtabstyle;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  GradTabControl1.Style :=TGradTabVistaBlueStyle.Create;
  GradTabControl1.Style.RoundedCorners:=false;
  GradTabControl1.Align:=alClient;
  GradTabControl1.TabPosition:=tpTop;
  SpinEdit2.Value:=GradTabControl1.TabHeight;

  GradTabControl1.Images := ImageList1;
  GradTabControl1.Page[0].ImageIndex:=0;
  GradTabControl1.Page[0].TabButton.ShowGlyph:=true;
  //GradTabControl1.PagesBar.MoveToNext;

  ComboBox1.Items.Assign(GradTabControl1.Tabs);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GradTabControl1.LongTabs:=CheckBox1.Checked;
  //SpinEdit1.Enabled:=CheckBox1.Checked;
  Edit1.Enabled:=CheckBox1.Checked;
  GradButton1.Enabled:=CheckBox1.Checked;

  Edit1.Text:=IntToStr(GradTabControl1.LongWidth);
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  GradTabControl1.Style.RoundedCorners:= CheckBox2.Checked;
  Invalidate;
end;

procedure TForm1.CheckGroup1Click(Sender: TObject);
begin

end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  with GradTabControl1 do begin
  case Index of
     0 : AutoShowScrollButtons := CheckGroup1.Checked[0];
     1 : ShowLeftTopScrollButton := CheckGroup1.Checked[1];
     2 : ShowRightBottomScrollButton := CheckGroup1.Checked[2];
  end;

      CheckGroup1.Checked[0] := AutoShowScrollButtons;
      CheckGroup1.Checked[1] := ShowLeftTopScrollButton;
      CheckGroup1.Checked[2] := ShowRightBottomScrollButton;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
   GradTabControl1.PageIndex:=GradTabControl1.Tabs.IndexOf(ComboBox1.Text);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0: GradTabControl1.Style := TGradTabStandardStyle.Create;
    1: GradTabControl1.Style := TGradTabVistaStyle.Create;
    2: GradTabControl1.Style := TGradTabVistaBlueStyle.Create;
  end;

  GradTabControl1.Style.RoundedCorners:= CheckBox2.Checked;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   GradTabControl1.Free;
end;

procedure TForm1.DeleteBtnClick(Sender: TObject);
var
   C : Integer;
   T : String;
begin
  T := ComboBox1.Text;
  C := GradTabControl1.Tabs.IndexOf(T);

  DebugLn('Text=%s Index=%d',[T,ComboBox1.Items.IndexOf(T)]);

  if (C < GradTabControl1.PageList.Count) AND (GradTabControl1.PageList.Count<>0) then
  begin
     ComboBox1.Items.Delete(ComboBox1.Items.IndexOf(T));
     GradTabControl1.Tabs.Delete(C);

  end;
end;

procedure TForm1.GradButton1Click(Sender: TObject);
begin
  GradTabControl1.LongWidth := StrToInt(Edit1.Text);

  Edit1.Text:=IntToStr(GradTabControl1.LongWidth);
end;

procedure TForm1.GradTabControl1DragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
   SenderBtn, SourceBtn : TGradTabPageButton;
   SenderPage, SourcePage : TGradTabPage;
begin
   DebugLn('Drop Name=%s',[Sender.ClassName]);

   if not (Sender is TGradTabPageButton) then Exit;

   SenderBtn := Sender as TGradTabPageButton;

   SenderPage:= SenderBtn.Owner as TGradTabPage;
   SourcePage:= GradTabControl1.Page[FDragIndex];

   if Sender = Source then Exit;
   if SourcePage.PageIndex=SenderPage.PageIndex then Exit;

   SourcePage.PageIndex:=SenderPage.PageIndex;

   //if (Sender is TGradTabControl) then Accept := True;
end;

procedure TForm1.NewPageBtnClick(Sender: TObject);
var
   C,R,G,B : Integer;
   newName : String;
begin
  C := -1;

  repeat
     Inc(C);
     newName := 'tab_'+IntToStr(C);
  until(GradTabControl1.Tabs.IndexOf(newName)=-1);
  GradTabControl1.Tabs.Add(newName);

  Randomize;

  R := Random(255)+1;
  G := Random(255)+1;
  B := Random(255)+1;

  //WriteLn(R, ' ', G, ' ', B, ColorToString(RGBToColor(R,G,B)));
  with GradTabControl1.ActivePage do
  begin
    Caption:='tab_'+IntToStr(C);
    TabPopupMenu := PopupMenu1;
    PopupMenu:= PopupMenu1;
    ImageIndex:=0;
    TabShowGlyph:=true;
    ShowCloseButton:=true;
    ShowCloseButtonOnMouseOver:=true;
  end;

  ComboBox1.ItemIndex:=ComboBox1.Items.Add('tab_'+IntToStr(C));
  RadioGroup2Click(nil);
end;

procedure TForm1.GradTabControl1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
   SenderBtn, SourceBtn : TGradTabPageButton;
   SenderPage, SourcePage : TGradTabPage;
begin
   //DebugLn('Name=%s',[Sender.ClassName]);
   Accept := false;
   if (Sender is TGradTabPageButton) OR (Sender is TGradTabPagesBar) then Accept := True;
end;

procedure TForm1.GradTabControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //DebugLn('MouseDown on %s',[Sender.ClassName]);
end;

procedure TForm1.GradTabControl1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
   NewNum : Integer;
begin
   NewNum:= GradTabControl1.PageIndex;
   Inc(NewNum);
   GradTabControl1.PageIndex:=NewNum;
end;

procedure TForm1.GradTabControl1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
   NewNum : Integer;
begin
   NewNum:= GradTabControl1.PageIndex;
   Dec(NewNum);
   GradTabControl1.PageIndex:=NewNum;
end;

procedure TForm1.GradTabControl1PageChanged(Sender: TObject);
begin
  with GradTabControl1 do
     ComboBox1.Text:=Tabs[PageIndex];
end;

procedure TForm1.GradTabControl1TabButtonMouseDown(
  GradTabControl: TGradTabControl; Button: TMouseButton; Shift: TShiftState; X,
  Y, AIndex: Integer);
begin
  DebugLn('MouseDown - BeginDrag');
  if ssCtrl in Shift then begin
     GradTabControl1.BeginDrag(False);
     FDragIndex:=AIndex;
  end;
end;

procedure TForm1.GradTabControl1TabButtonMouseMove(
  GradTabControl: TGradTabControl; Shift: TShiftState; X, Y, AIndex: Integer);
begin

end;

procedure TForm1.GradTabControl1TabButtonMouseUp(
  GradTabControl: TGradTabControl; Button: TMouseButton; Shift: TShiftState; X,
  Y, AIndex: Integer);
begin
  //ShowMessage('TabMouseUp: Index: '+IntToStr(AIndex)+' Caption: '+GradTabControl.Page[AIndex].Caption);

  if Button=mbMiddle then
  begin
      //GradTabControl.Tabs.Delete(AIndex);
      ComboBox1.Items.Delete(ComboBox1.Items.IndexOf(GradTabControl.Page[AIndex].Caption));
      GradTabControl.Page[AIndex].Free;
  end;
end;

procedure TForm1.GradTabControl1TabCloseButtonClick(
  GradTabControl: TGradTabControl; AIndex: Integer);
begin
  ComboBox1.Items.Delete(ComboBox1.Items.IndexOf(GradTabControl.Page[AIndex].Caption));
  GradTabControl.Page[AIndex].Free;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: GradTabControl1.TabPosition := tpTop;
    1: GradTabControl1.TabPosition := tpBottom;
    2: GradTabControl1.TabPosition := tpLeft;
    3: GradTabControl1.TabPosition := tpRight;
  end;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to GradTabControl1.PageCount - 1 do
  with GradTabControl1.Page[i] do
  case RadioGroup2.ItemIndex of
    0: TabButtonLayout := blGlyphLeft;
    1: TabButtonLayout := blGlyphBottom;
    2: TabButtonLayout := blGlyphRight;
    3: TabButtonLayout := blGlyphTop;
  end;
end;

procedure TForm1.SpinEdit2EditingDone(Sender: TObject);
begin
  GradTabControl1.TabHeight:=SpinEdit2.Value;
end;

initialization
  {$I unit1.lrs}

end.

