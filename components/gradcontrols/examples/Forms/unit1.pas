unit Unit1; 

{$mode objfpc}{$H+}

(*
  ich denke mal das beste wäre erstmal die TGradTabPages mit der TGradTabControl
  richtig zu "verlinken", ich habe auch noch ne TabListe hinzufügt diese müsste man dann auch noch
  bei der TGradTabPages einbauen damit die Tabs schon mal automatisch erstellt und
  gelöscht werden ^^

  ToDo:
  - TGradTabBar & TGradTabPagesBar mit sortieren von Tabs "ausstatten"
  - TabPosition einbauen, sowie das zeichnen und anordnen der Tabs/Pages ändern
  - CurrentPage-Button ohne Abstand
  - IDE Testen hoffe es geht *g*
  
  Danach:
  - Eigene Page Classen zu ordnen ( bräuchte ich zumindest ^^ ) - Feddich
  - Tabs disable-n
  - noch welche Wünsche? xD
  - Testen testen testen und voila wir haben eine neue komponente :D
  
  - Irgendwann mal eine TGradDrawer klasse um mehre
*)

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ugradbtn, StdCtrls, ComCtrls, LCLType, LCLProc, Buttons, ugradtabcontrol,
  Menus, Spin, ButtonPanel,
  MaskEdit, DBGrids, DbCtrls, EditBtn, Arrow,
  SynHighlighterPHP, Grids, SynEdit;

type

  TTryOutPage = class(TGradTabPage)
  
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Arrow1: TArrow;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    GradTabControl1: TGradTabControl;
    GradTabPage1: TGradTabPage;
    GradTabPage2: TGradTabPage;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    ScrollBox1: TScrollBox;
    SpinEdit1: TSpinEdit;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    SynEdit1: TSynEdit;
    SynPHPSyn1: TSynPHPSyn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GradButton1Click(Sender: TObject);
    procedure GradButton1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GradButton1MouseEnter(Sender: TObject);
    procedure GradButton1MouseLeave(Sender: TObject);
    procedure GradButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GradButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GradButton2Click(Sender: TObject);
    procedure GradButton3Click(Sender: TObject);
    procedure GradButton4Click(Sender: TObject);
    procedure GradTabControl1TabButtonClick(GradTabControl: TGradTabControl;
      AIndex: Integer);
    procedure GradTabControl1TabButtonMouseUp(GradTabControl: TGradTabControl;
      Button: TMouseButton; Shift: TShiftState; X, Y, AIndex: Integer);
    procedure GradTabControl2TabButtonClick(GradTabControl: TGradTabControl;
      AIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEdit1EditingDone(Sender: TObject);
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
   
   GradButton1.Parent := GradTabControl1.Pages[0];
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

procedure TForm1.GradButton2Click(Sender: TObject);
var
   C : Integer;
begin
  with TTryOutPage.Create(Self) do
  begin
      Parent := GradTabControl1;
      C := PageIndex;
  end;

  ComboBox1.ItemIndex:=ComboBox1.Items.Add('Custom_'+IntToStr(GradTabControl1.Tabs.Count-1));

  //WriteLn('CustomIndex: ', IntToStr(C));
  
  GradTabControl1.Pages[C].Color:=clRed;
  GradTabControl1.Pages[C].Caption:=ComboBox1.Text; //WICHTIG, eine Caption muss für
  //die Eigenschaft Tabs gesetzt werden!
  
  GradTabControl1.Pages[C].TabPopupMenu := PopupMenu1;
  GradTabControl1.Pages[C].PopupMenu:= PopupMenu1;
end;

procedure TForm1.GradButton3Click(Sender: TObject);
var
   C,R,G,B : Integer;
begin
  C := GradTabControl1.Tabs.Count;
  GradTabControl1.Tabs.Add('tab_'+IntToStr(C));
  
  Randomize;
  
  R := Random(255)+1;
  G := Random(255)+1;
  B := Random(255)+1;
  
  //WriteLn(R, ' ', G, ' ', B, ColorToString(RGBToColor(R,G,B)));
  GradTabControl1.CurrentPage.Caption:='tab_'+IntToStr(C);
  GradTabControl1.CurrentPage.Color:=RGBToColor(R, G, B);
  GradTabControl1.CurrentPage.TabPopupMenu := PopupMenu1;
  GradTabControl1.CurrentPage.PopupMenu:= PopupMenu1;
  
  ComboBox1.ItemIndex:=ComboBox1.Items.Add('tab_'+IntToStr(C));
end;

procedure TForm1.GradButton4Click(Sender: TObject);
begin
  if GradTabControl1.Tabs.IndexOf(ComboBox1.Text) <> -1 then
  begin
     GradTabControl1.Tabs.Delete(GradTabControl1.Tabs.IndexOf(ComboBox1.Text));
     ComboBox1.Items.Delete(ComboBox1.Items.IndexOf(ComboBox1.Text));
     ComboBox1.ItemIndex:=GradTabControl1.CurrentPageNum;
  end;
end;

procedure TForm1.GradTabControl1TabButtonClick(GradTabControl: TGradTabControl;
  AIndex: Integer);
begin
  ShowMessage('TabClicked: Index: '+IntToStr(AIndex)+' Caption: '+GradTabControl.Page[AIndex].Caption);
end;

procedure TForm1.GradTabControl1TabButtonMouseUp(
  GradTabControl: TGradTabControl; Button: TMouseButton; Shift: TShiftState; X,
  Y, AIndex: Integer);
begin
  ShowMessage('TabMouseUp: Index: '+IntToStr(AIndex)+' Caption: '+GradTabControl.Page[AIndex].Caption);

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



initialization
  {$I unit1.lrs}

end.

