unit Unit1; 

{$mode objfpc}{$H+}

(*
  ich denke mal das beste wäre erstmal die TGradTabPages mit der TGradTabControl
  richtig zu "verlinken", ich habe auch noch ne TabListe hinzufügt diese müsste man dann auch noch
  bei der TGradTabPages einbauen damit die Tabs schon mal automatisch erstellt und
  gelöscht werden ^^

  ToDo:
  - CurrentPage-Button ohne Abstand
  - IDE Testen hoffe es geht *g*
  
  Danach:
  - Tabs disable-n
  - noch welche Wünsche? xD
  - Testen testen testen und voila wir haben eine neue komponente :D
  
  - Irgendwann mal eine TGradDrawer klasse um mehre
*)

interface

uses
  windows, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ugradbtn, StdCtrls, ComCtrls, LCLType, LCLProc, LCLIntf, Buttons, ugradtabcontrol,
  Menus, Spin, ButtonPanel,
  MaskEdit, DBGrids, DbCtrls, EditBtn, Arrow,
  SynHighlighterPHP, Grids, SynEdit, SynMemo;

type

  TTryOutPage = class(TGradTabPage)
  
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    GradTabPage1: TGradTabPage;
    GradTabPage2: TGradTabPage;
    NewPageBtn: TGradButton;
    DeleteBtn: TGradButton;
    GradTabControl1: TGradTabControl;
    Label1: TLabel;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToggleBox2: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GradButton1Click(Sender: TObject);
    procedure GradButton1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GradButton1MouseEnter(Sender: TObject);
    procedure GradButton1MouseLeave(Sender: TObject);
    procedure GradButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GradButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteBtnClick(Sender: TObject);
    procedure BewegeBtn(Sender: TObject);
    procedure GradButton3Click(Sender: TObject);
    procedure GradButton4Click(Sender: TObject);
    procedure GradTabControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GradTabControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GradTabControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GradTabControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GradTabControl1MouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure GradTabControl1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GradTabControl1PageChanged(Sender: TObject);
    procedure GradTabControl1PagesBarDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure GradTabControl1TabButtonClick(GradTabControl: TGradTabControl;
      AIndex: Integer);
    procedure GradTabControl1TabButtonMouseDown(
      GradTabControl: TGradTabControl; Button: TMouseButton;
      Shift: TShiftState; X, Y, AIndex: Integer);
    procedure GradTabControl1TabButtonMouseMove(
      GradTabControl: TGradTabControl; Shift: TShiftState; X, Y, AIndex: Integer
      );
    procedure GradTabControl1TabButtonMouseUp(GradTabControl: TGradTabControl;
      Button: TMouseButton; Shift: TShiftState; X, Y, AIndex: Integer);
    procedure GradTabControl2TabButtonClick(GradTabControl: TGradTabControl;
      AIndex: Integer);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEdit1EditingDone(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    //procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    //GradTabControl1 : TGradTabControl;
    FControlWidth : Integer;
    FLastIndex : Integer;
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
   TestPage : TGradTabPage;
begin
  { GradTabControl1 := TGradTabControl.Create(nil);
   with GradTabControl1 do
   begin
       ControlStyle:=ControlStyle+[csAcceptsControls];
       Top := 1;
       Left := 1;
       Width := 400;
       Height := 400;
       Parent := Panel1;
       Anchors:=[akLeft,akTop];
       Align:=alClient;
       //BorderStyle:=bsSingle;
   end; }

  GradTabControl1.Align:=alClient;
  GradTabControl1.TabPosition:=tpTop;
  GradTabControl1.MoveIncrement:=20;
  SpinEdit2.Value:=20;

  { with TGradButton.Create(GradTabControl1) do
   begin
       Top := 0;
       Height:=30;
       Left := 2;
       Caption := 'Tab1';
       Name := 'Tab1';
       Parent := GradTabControl1;
       BorderSides:=BorderSides-[bsBottomLine];
       OnClick:=@GradTabControl1.TabButtonClick;
   end;
   
   with TGradButton.Create(GradTabControl1) do
   begin
       Top := 3;
       Height:=27;
       Left := 82;
       Caption := 'Tab2';
       Name := 'Tab2';
       Parent := GradTabControl1;
       Color := clGreen;
       BorderSides:=BorderSides-[bsBottomLine];
       OnClick:=@GradTabControl1.TabButtonClick;
   end;
   
   with TGradTabPage.Create(GradTabControl1) do
   begin
       Name := 'page1';
       Left := 2;
       Top := 32;
       Width:=496;
       Height:=466;
       Color:=clBlue;
       Parent:=GradTabControl1;
       Align:=alClient;
   end;
   
   with TGradTabPage.Create(GradTabControl1) do
   begin
       Name := 'page2';
       Left := 2;
       Top := 32;
       Width:=496;
       Height:=466;
       Color:=clGreen;
       Visible:=false;
       Parent:=GradTabControl1;
       Align:=alClient;
   end;   }
   
   {WriteLn(GradTabControl1.Tabs.Add('tab_0'));
   ComboBox1.Items.Add('tab_0');
   
   WriteLn(GradTabControl1.Tabs.Add('tab_1'));
   ComboBox1.Items.Add('tab_1');
   }//WriteLn(GradTabControl1.Tabs.Add('BLA2'));
   
   {
   
   GradButton3.PopupMenu := PopupMenu1;
   
   WriteLn(BoolToStr(GradTabControl1.Pages[0] <> nil,true));
   
   NewPageBtn.Parent := GradTabControl1.Pages[0];
   BitBtn1.Parent := GradTabControl1.Pages[0];
   }
   //GradButton3Click(GradButton3);
   //GradButton3Click(GradButton3);

   {TestPage := TGradTabPage(GradTabControl1.PageList.Items[0]);
   Memo1.Lines.Add(TestPage.Name+' '+TestPage.Caption);
   //GradTabControl1.Tabs.Text:='a';

   Memo1.Parent := GradTabControl1.Pages[0];
   Memo1.Align:=alClient;
   Memo1.Lines.Add(TestPage.Name+' '+TestPage.Caption);
    }

   {TestPage.Name:='GradTabPage1';
   TestPage.Caption:='GradTabPage1';
   TestPage.Parent := GradTabControl1;
   }
   //ComboBox1.Items.AddStrings(GradTabControl1.Tabs);
   //ComboBox1.Items.Add('a');
   //ComboBox1.Items.Add('GradTabPage1');

   FControlWidth:=247;
   FLastIndex:=1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  (GradTabControl1.ControlByName('page1') as TGradTabPage).Align:=alClient;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GradTabControl1.Tabs.Add('No2');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GradTabControl1.LongWidth:=SpinEdit1.Value;
  GradTabControl1.LongTabs:=CheckBox1.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
   GradTabControl1.CurrentPageNum:=GradTabControl1.Tabs.IndexOf(ComboBox1.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   GradTabControl1.Free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TForm1.GradButton1Click(Sender: TObject);
begin

end;

procedure TForm1.GradButton1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.GradButton1MouseEnter(Sender: TObject);
begin

end;

procedure TForm1.GradButton1MouseLeave(Sender: TObject);
begin

end;

procedure TForm1.GradButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TForm1.GradButton1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

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

procedure TForm1.BewegeBtn(Sender: TObject);
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
  GradTabControl1.ActivePage.Caption:='tab_'+IntToStr(C);
  //GradTabControl1.CurrentPage.Color:=RGBToColor(R, G, B);
  GradTabControl1.ActivePage.TabPopupMenu := PopupMenu1;
  GradTabControl1.ActivePage.PopupMenu:= PopupMenu1;

  ComboBox1.ItemIndex:=ComboBox1.Items.Add('tab_'+IntToStr(C));
end;

procedure TForm1.GradButton3Click(Sender: TObject);
var
   C : Integer;
begin
  GradTabControl1.PagesBar.MoveTo(50);
  for C := 50 downto -20 do
   begin
       GradTabControl1.PagesBar.MoveTo(-1);
       Sleep(5);
       Application.ProcessMessages;
   end;
end;

procedure TForm1.GradButton4Click(Sender: TObject);
begin
  GradTabControl1.PagesBar.MoveToNorm;
end;

procedure TForm1.GradTabControl1DragDrop(Sender, Source: TObject; X, Y: Integer
  );
begin
  DebugLn('DragDrop X=%d Y=%d',[X,Y]);
end;

procedure TForm1.GradTabControl1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DebugLn('Name=%s',[Sender.ClassName]);
  if (Sender is TGradTabControl) then Accept := True;
end;

procedure TForm1.GradTabControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowMessage('Jep');
end;

procedure TForm1.GradTabControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  DebugLn('X=%d Y=%d',[X,Y]);
end;

procedure TForm1.GradTabControl1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
   NewNum : Integer;
begin
   NewNum:= GradTabControl1.CurrentPageNum;
   Inc(NewNum);
   GradTabControl1.CurrentPageNum:=NewNum;
end;

procedure TForm1.GradTabControl1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
   NewNum : Integer;
begin
   NewNum:= GradTabControl1.CurrentPageNum;
   Dec(NewNum);
   GradTabControl1.CurrentPageNum:=NewNum;
end;

procedure TForm1.GradTabControl1PageChanged(Sender: TObject);
begin
  with GradTabControl1 do
     ComboBox1.Text:=Tabs[CurrentPageNum];
end;

procedure TForm1.GradTabControl1PagesBarDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  DebugLn('Name=%s',[Sender.ClassName]);
  if (Sender is TGradTabPagesBar) then Accept := True;
end;

procedure TForm1.GradTabControl1TabButtonClick(GradTabControl: TGradTabControl;
  AIndex: Integer);
begin
  //ShowMessage('TabClicked: Index: '+IntToStr(AIndex)+' Caption: '+GradTabControl.Page[AIndex].Caption);
end;

procedure TForm1.GradTabControl1TabButtonMouseDown(
  GradTabControl: TGradTabControl; Button: TMouseButton; Shift: TShiftState; X,
  Y, AIndex: Integer);
begin
  if ssCtrl in Shift then
  GradTabControl1.BeginDrag(False);
end;

procedure TForm1.GradTabControl1TabButtonMouseMove(
  GradTabControl: TGradTabControl; Shift: TShiftState; X, Y, AIndex: Integer);
var
   TabRect : TRect;
begin
  //TabRect := GradTabControl.GetTabRect(Aindex);
  //DebugLn('X=%d Y=%d AIndex=%d',[X+TabRect.Left,Y+TabRect.Top,AIndex]);
end;

procedure TForm1.GradTabControl1TabButtonMouseUp(
  GradTabControl: TGradTabControl; Button: TMouseButton; Shift: TShiftState; X,
  Y, AIndex: Integer);
begin
  //ShowMessage('TabMouseUp: Index: '+IntToStr(AIndex)+' Caption: '+GradTabControl.Page[AIndex].Caption);

  if Button=mbMiddle then
  begin
      //GradTabControl.Tabs.Delete(AIndex);
      GradTabControl.Page[AIndex].Free;
  end;
end;

procedure TForm1.GradTabControl2TabButtonClick(GradTabControl: TGradTabControl;
  AIndex: Integer);
begin
  if AIndex = FLastIndex then
  begin
      if GradTabControl.Width=GradTabControl.TabHeight then
         GradTabControl.Width:=FControlWidth
      else
         GradTabControl.Width:=GradTabControl.TabHeight;

  end else begin
      GradTabControl.Width:=FControlWidth;
      FLastIndex:=AIndex;
  end;
end;

procedure TForm1.PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
const
   TCM_GETITEMRECT = $130A;
var
   TabRect: TRect;
   j: Integer;
begin
   if (Sender is TGradTabControl) then
   for j := 0 to GradTabControl1.PageCount - 1 do
   begin
     //GradTabControl1.Perform(TCM_GETITEMRECT, j, LParam(@TabRect)) ;
     TabRect := GradTabControl1.GetTabRect(j);
     DebugLn('X=%d Y=%d T.L=%d T.T=%d T.R=%d T.B=%d',[X,Y,TabRect.Left, TabRect.Top, TabRect.Right, TabRect.Bottom]);
     if PtInRect(TabRect, Point(X, Y)) then
     begin
       if GradTabControl1.ActivePage.PageIndex <> j then
         GradTabControl1.ActivePage.PageIndex := j;
       Exit;
     end;
   end;
end;

procedure TForm1.PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
   DebugLn('Name=%s',[Sender.ClassName]);
   if (Sender is TGradTabControl) then Accept := True;
end;

procedure TForm1.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PageControl1.BeginDrag(False) ;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin

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

procedure TForm1.SpinEdit1EditingDone(Sender: TObject);
begin
  GradTabControl1.LongWidth:=SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  GradTabControl1.MoveIncrement:=SpinEdit2.Value;
end;



initialization
  {$I unit1.lrs}

end.

