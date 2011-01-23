unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ugradbtn, Buttons, ComCtrls,
  LCLType, ExtDlgs, Menus, urotatebitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox2: TCheckBox;
    DDColorButton: TGradButton;
    DDPressedColorButton: TGradButton;
    BoldCheckBox: TCheckBox;
    CheckBox1: TCheckBox;
    GlyphTransparentCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    ItalicCheckBox: TCheckBox;
    Label12: TLabel;
    ExamplePopupMenu: TPopupMenu;
    Label13: TLabel;
    Label14: TLabel;
    LabeledEdit2: TLabeledEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    TextColorButton: TGradButton;
    UnderlineCheckBox: TCheckBox;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    ColorDialog1: TColorDialog;
    b: TGradButton;
    BaseColorButton: TGradButton;
    NormalBlendColorButton: TGradButton;
    OverBlendButton: TGradButton;
    LoadGlyphButton: TGradButton;
    GlyphBackgroundColorButton: TGradButton;
    ClickColorButton: TGradButton;
    DisabledColorButton: TGradButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabeledEdit1: TLabeledEdit;
    OpenPictureDialog1: TOpenPictureDialog;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure bClick(Sender: TObject);
    procedure BoldCheckBoxChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CheckGroup2ItemClick(Sender: TObject; Index: integer);
    procedure BaseColorClick(Sender: TObject);
    procedure DDPressedColorButtonClick(Sender: TObject);
    procedure DDColorButtonClick(Sender: TObject);
    procedure GlyphTransparentCheckBoxChange(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure LoadGlyphButtonClick(Sender: TObject);
    procedure GlyphBackgroundColorButtonClick(Sender: TObject);
    procedure ClickColorButtonClick(Sender: TObject);
    procedure DisabledColorButtonClick(Sender: TObject);
    procedure ItalicCheckBoxChange(Sender: TObject);
    procedure NormalBlendClick(Sender: TObject);
    procedure OverBlendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure TextColorButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
    procedure UnderlineCheckBoxChange(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  GraphType;

procedure UpdateButtonColor(Button: TGradButton; Color: TColor);
begin
  Button.BaseColor := Color;
  Button.Caption := ColorToString(Color);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Position:= trunc(b.NormalBlend * TrackBar1.Max);
  TrackBar1Change(TrackBar1);
  
  TrackBar2.Position:= trunc(b.OverBlend * TrackBar2.Max);
  TrackBar2Change(TrackBar2);

  UpdateButtonColor(BaseColorButton, b.BaseColor);
  UpdateButtonColor(NormalBlendColorButton, b.NormalBlendColor);
  UpdateButtonColor(OverBlendButton, b.OverBlendColor);
  UpdateButtonColor(GlyphBackgroundColorButton, b.GlyphBackgroundColor);
  UpdateButtonColor(ClickColorButton, b.ClickColor);
  UpdateButtonColor(DisabledColorButton, b.DisabledColor);
  UpdateButtonColor(TextColorButton, b.Font.Color);
  UpdateButtonColor(DDColorButton, b.DropDownSettings.Color);
  UpdateButtonColor(DDPressedColorButton, b.DropDownSettings.PressedColor);

  CheckGroup1.Checked[0]:=true;
  CheckGroup1.Checked[1]:=true;
  CheckGroup1.Checked[2]:=true;
  CheckGroup1.Checked[3]:=true;
  
  CheckGroup2.Checked[0]:=b.ShowFocusBorder;
  CheckGroup2.Checked[2]:=true;

  b.Caption:=LabeledEdit1.Text;

  //ImageList1.Add(LoadBitmapFromLazarusResource('script_go'),nil);
  //b.Glyph := LoadBitmapFromLazarusResource('table_gear');

  b.ShowGlyph:=true;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = 0 then
    b.GradientType := gtHorizontal
  else
    b.GradientType := gtVertical;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  if RadioGroup2.ItemIndex = 0 then
    b.RotateDirection := rdNormal
  else if RadioGroup2.ItemIndex = 1 then
    b.RotateDirection := rdLeft
  else
    b.RotateDirection := rdRight;
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);
begin
  if RadioGroup3.ItemIndex = 0 then
    b.ButtonLayout := blGlyphRight
  else if RadioGroup3.ItemIndex = 1 then
    b.ButtonLayout := blGlyphLeft
  else if RadioGroup3.ItemIndex = 2 then
    b.ButtonLayout := blGlyphTop
  else b.ButtonLayout := blGlyphBottom;
end;

procedure TForm1.RadioGroup4Click(Sender: TObject);
begin
  case RadioGroup4.ItemIndex of
    0 : b.TextAlignment := taCenter;
    1 : b.TextAlignment := taLeftJustify;
    2 : b.TextAlignment := taRightJustify;
  end;
end;

procedure TForm1.RadioGroup5Click(Sender: TObject);
begin
  case RadioGroup5.ItemIndex of                 
    0: b.DropDownSettings.MarkDirection:=mdUp;  
    1: b.DropDownSettings.MarkDirection:=mdLeft;  
    2: b.DropDownSettings.MarkDirection:=mdDown;  
    3: b.DropDownSettings.MarkDirection:=mdRight;  
  end;
end;

procedure TForm1.RadioGroup6Click(Sender: TObject);
begin
  case RadioGroup6.ItemIndex of                 
    0: b.DropDownSettings.MarkPosition:=mpLeft;  
    1: b.DropDownSettings.MarkPosition:=mpRight;    
  end; 
end;

procedure TForm1.TextColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := b.Font.Color;
  if ColorDialog1.Execute then
  begin
    b.Font.Color := ColorDialog1.Color;
    UpdateButtonColor(TextColorButton, ColorDialog1.Color);
  end;
end;

procedure TForm1.BaseColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=BaseColorButton.BaseColor;
  if ColorDialog1.Execute then
  begin
    b.BaseColor:=ColorDialog1.Color;
    UpdateButtonColor(BaseColorButton, ColorDialog1.Color);
  end;
end;

procedure TForm1.DDPressedColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color:=DDPressedColorButton.BaseColor;
  if ColorDialog1.Execute then
  begin
    b.DropDownSettings.PressedColor:=ColorDialog1.Color;
    UpdateButtonColor(DDPressedColorButton, ColorDialog1.Color);
  end;
end;

procedure TForm1.DDColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color:=DDColorButton.BaseColor;
  if ColorDialog1.Execute then
  begin
    b.DropDownSettings.Color:=ColorDialog1.Color;
    UpdateButtonColor(DDColorButton, ColorDialog1.Color);
  end;
end;

procedure TForm1.GlyphTransparentCheckBoxChange(Sender: TObject);
begin
  b.Glyph.Transparent := GlyphTransparentCheckBox.Checked;
  LoadGlyphButton.Glyph.Transparent := GlyphTransparentCheckBox.Checked;
end;

procedure TForm1.LabeledEdit2Change(Sender: TObject);
var
  i : Integer;
begin
  if TryStrToInt(LabeledEdit2.Text, i) then
  begin
    b.DropDownSettings.Size := i;
  end;
end;

procedure TForm1.LoadGlyphButtonClick(Sender: TObject);
var
   tempPicture : TPicture;
begin
  if OpenPictureDialog1.Execute then
  begin
      tempPicture := TPicture.Create;
      try
          tempPicture.LoadFromFile(OpenPictureDialog1.FileName);
          tempPicture.Graphic.Transparent := GlyphTransparentCheckBox.Checked;

          b.Glyph.Assign(tempPicture.Graphic);

          LoadGlyphButton.Glyph.Assign(tempPicture.Graphic);
          LoadGlyphButton.ShowGlyph := true;
          LoadGlyphButton.Caption := '';
      finally
          tempPicture.Free;
      end;
  end;
end;

procedure TForm1.GlyphBackgroundColorButtonClick(Sender: TObject);
begin
   ColorDialog1.Color:=b.GlyphBackgroundColor;
   if ColorDialog1.Execute then
   begin
       b.GlyphBackgroundColor:=ColorDialog1.Color;
       UpdateButtonColor(GlyphBackgroundColorButton, ColorDialog1.Color);
   end;
end;

procedure TForm1.ClickColorButtonClick(Sender: TObject);
begin
    ColorDialog1.Color:=b.ClickColor;
    if ColorDialog1.Execute then
    begin
        b.ClickColor:=ColorDialog1.Color;
        UpdateButtonColor(ClickColorButton, ColorDialog1.Color);
    end;
end;

procedure TForm1.DisabledColorButtonClick(Sender: TObject);
begin
    ColorDialog1.Color:=b.DisabledColor;
    if ColorDialog1.Execute then
    begin
        b.DisabledColor:=ColorDialog1.Color;
        UpdateButtonColor(DisabledColorButton, ColorDialog1.Color);
    end;
end;

procedure TForm1.ItalicCheckBoxChange(Sender: TObject);
begin
  if ItalicCheckBox.Checked then
    b.Font.Style := b.Font.Style + [fsItalic]
  else
    b.Font.Style := b.Font.Style - [fsItalic];
end;

procedure TForm1.LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  b.Caption:=LabeledEdit1.Text;
end;

procedure TForm1.UnderlineCheckBoxChange(Sender: TObject);
begin
  if UnderlineCheckBox.Checked then
    b.Font.Style := b.Font.Style + [fsUnderline]
  else
    b.Font.Style := b.Font.Style - [fsUnderline];
end;

procedure TForm1.NormalBlendClick(Sender: TObject);
begin
    ColorDialog1.Color:=b.NormalBlendColor;
    if ColorDialog1.Execute then
    begin
        b.NormalBlendColor:=ColorDialog1.Color;
        UpdateButtonColor(NormalBlendColorButton, ColorDialog1.Color);
    end;
end;

procedure TForm1.OverBlendClick(Sender: TObject);
begin
   ColorDialog1.Color:=b.OverBlendColor;
    if ColorDialog1.Execute then
    begin
        b.OverBlendColor:=ColorDialog1.Color;
        UpdateButtonColor(OverBlendButton, ColorDialog1.Color);
    end;
end;

procedure TForm1.BoldCheckBoxChange(Sender: TObject);
begin
  if BoldCheckBox.Checked then
    b.Font.Style := b.Font.Style + [fsBold]
  else
    b.Font.Style := b.Font.Style - [fsBold];
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  b.DropDownSettings.Show := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  b.DropDownSettings.SplitButton:= CheckBox2.Checked;
end;

procedure TForm1.bClick(Sender: TObject);
begin

end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
var
   bs : TBorderSides;
begin

  bs := [];

  if CheckGroup1.Checked[0] then bs := bs + [bsBottomLine];
  if CheckGroup1.Checked[1] then bs := bs + [bsTopLine];
  if CheckGroup1.Checked[2] then bs := bs + [bsLeftLine];
  if CheckGroup1.Checked[3] then bs := bs + [bsRightLine];
  
  b.BorderSides:=bs;
end;

procedure TForm1.CheckGroup2ItemClick(Sender: TObject; Index: integer);
begin
  b.ShowFocusBorder:=CheckGroup2.Checked[0];
  b.ShowGlyph:=CheckGroup2.Checked[1];
  b.Enabled:=CheckGroup2.Checked[2];
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
   blend : Extended;
begin
   blend := TrackBar1.Position / TrackBar1.Max;
   b.NormalBlend:=blend;
   Label3.Caption:=FloatToStr(blend);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var
   blend : Extended;
begin
   blend := TrackBar2.Position / TrackBar2.Max;
   b.OverBlend:=blend;
   Label4.Caption:=FloatToStr(blend);
end;

initialization
  {$I main.lrs}
end.

