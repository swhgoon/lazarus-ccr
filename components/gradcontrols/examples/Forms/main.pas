unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ugradbtn, Buttons, ComCtrls,
  LCLType, ExtDlgs, urotatebitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    ColorDialog1: TColorDialog;
    b: TGradButton;
    GradButton1: TGradButton;
    GradButton2: TGradButton;
    GradButton3: TGradButton;
    GradButton4: TGradButton;
    GradButton5: TGradButton;
    GradButton6: TGradButton;
    GradButton7: TGradButton;
    ImageList1: TImageList;
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
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CheckGroup2ItemClick(Sender: TObject; Index: integer);
    procedure BaseColorClick(Sender: TObject);
    procedure GradButton4Click(Sender: TObject);
    procedure GradButton5Click(Sender: TObject);
    procedure GradButton6Click(Sender: TObject);
    procedure GradButton7Click(Sender: TObject);
    procedure NormalBlendClick(Sender: TObject);
    procedure OverBlendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  end;

var
  Form1: TForm1;

implementation

uses
    GraphType, LazPNG;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Position:= trunc(b.NormalBlend * TrackBar1.Max);
  TrackBar1Change(TrackBar1);
  
  TrackBar2.Position:= trunc(b.OverBlend * TrackBar2.Max);
  TrackBar2Change(TrackBar2);

  with GradButton1 do
  begin
      BaseColor:=b.BaseColor;
      Caption:=ColorToString(BaseColor);
  end;
  
  with GradButton2 do
  begin
      BaseColor:=b.NormalBlendColor;
      Caption:=ColorToString(BaseColor);
  end;
  
  with GradButton3 do
  begin
      BaseColor:=b.OverBlendColor;
      Caption:=ColorToString(BaseColor);
  end;

  CheckGroup1.Checked[0]:=true;
  CheckGroup1.Checked[1]:=true;
  CheckGroup1.Checked[2]:=true;
  CheckGroup1.Checked[3]:=true;
  
  CheckGroup2.Checked[0]:=b.ShowFocusBorder;
  CheckGroup2.Checked[2]:=true;

  //ImageList1.AddLazarusResource('house');
  
  //ImageList1.GetBitmap(0,b.Glyph,gdeHighlighted);

  //ImageList1.GetBitmap();

  b.GlyphBackgroundColor:=clWhite;
  GradButton5.BaseColor:=clWhite;
  GradButton5.Caption:=ColorToString(clWhite);
  GradButton6.Caption:=ColorToString(GradButton6.BaseColor);
  GradButton7.Caption:=ColorToString(b.NotEnabledColor);
  GradButton7.BaseColor:=b.NotEnabledColor;
  
  b.ClickColor:=GradButton6.BaseColor;
  b.Caption:=LabeledEdit1.Text;
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

procedure TForm1.BaseColorClick(Sender: TObject);
begin
    ColorDialog1.Color:=GradButton1.BaseColor;
    if ColorDialog1.Execute then
    begin
        b.BaseColor:=ColorDialog1.Color;
        GradButton1.BaseColor:=ColorDialog1.Color;
        GradButton1.Caption:=ColorToString(GradButton1.BaseColor);
    end;
end;

procedure TForm1.GradButton4Click(Sender: TObject);
var
   tempPicture : TPicture;
   png : TPNGImage;
begin
  if OpenPictureDialog1.Execute then
  begin
      ImageList1.Clear;
      try
          tempPicture := TPicture.Create;
          tempPicture.LoadFromFile(OpenPictureDialog1.FileName);
          //png :=  TPNGImage.Create;
          //png.LoadFromFile(OpenPictureDialog1.FileName);

          GradButton4.Glyph.Assign(tempPicture.Graphic);
          b.Glyph.Assign(tempPicture.Graphic);

          GradButton4.UpdateButton;
          b.UpdateButton;
          //ImageList1.;
          //Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          //tempPicture.Picture.Bitmap.Canvas.Pixels[1,2]:=clRed;
          //ImageList1.AddMasked(tempPicture.Picture.Bitmap,clNone);
          //ImageList1.GetBitmap(0,b.Glyph);
          //mageList1.GetBitmap(0,);
          //GradButton4.Glyph := Image1.Picture.Bitmap;
          //Image1.Picture.Bitmap := tempPicture.Picture.Bitmap;
          GradButton4.ShowGlyph:=true;
          GradButton4.Caption:='';
      finally
          tempPicture.Free;
          //png.Free;
      end;
  end;
end;

procedure TForm1.GradButton5Click(Sender: TObject);
begin
   ColorDialog1.Color:=b.GlyphBackgroundColor;
   if ColorDialog1.Execute then
   begin
       b.GlyphBackgroundColor:=ColorDialog1.Color;
       GradButton5.BaseColor:=ColorDialog1.Color;
       GradButton5.Caption:=ColorToString(GradButton5.BaseColor);
   end;
end;

procedure TForm1.GradButton6Click(Sender: TObject);
begin
    ColorDialog1.Color:=b.ClickColor;
    if ColorDialog1.Execute then
    begin
        b.ClickColor:=ColorDialog1.Color;
        GradButton6.BaseColor:=ColorDialog1.Color;
        GradButton6.Caption:=ColorToString(GradButton6.BaseColor);
    end;
end;

procedure TForm1.GradButton7Click(Sender: TObject);
begin
    ColorDialog1.Color:=b.NotEnabledColor;
    if ColorDialog1.Execute then
    begin
        b.NotEnabledColor:=ColorDialog1.Color;
        GradButton7.BaseColor:=ColorDialog1.Color;
        GradButton7.Caption:=ColorToString(GradButton7.BaseColor);
    end;
end;

procedure TForm1.LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  b.Caption:=LabeledEdit1.Text;
end;

procedure TForm1.NormalBlendClick(Sender: TObject);
begin
    ColorDialog1.Color:=b.NormalBlendColor;
    if ColorDialog1.Execute then
    begin
        b.NormalBlendColor:=ColorDialog1.Color;
        GradButton2.BaseColor:=ColorDialog1.Color;
        GradButton2.Caption:=ColorToString(GradButton2.BaseColor);
    end;
end;

procedure TForm1.OverBlendClick(Sender: TObject);
begin
   ColorDialog1.Color:=b.OverBlendColor;
    if ColorDialog1.Execute then
    begin
        b.OverBlendColor:=ColorDialog1.Color;
        GradButton3.BaseColor:=ColorDialog1.Color;
        GradButton3.Caption:=ColorToString(GradButton3.BaseColor);
    end;
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

