unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus, FileCache, MyDrawingControl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuHD50: TMenuItem;
    mnuHD: TMenuItem;
    MenuItem2: TMenuItem;
    mnuTPaintBox: TMenuItem;
    mnuTImage: TMenuItem;
    MenuItem5: TMenuItem;
    mnuTGraphicControl: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    ImageTabSheet: TTabSheet;
    PaintBoxTabSheet: TTabSheet;
    Splitter1: TSplitter;
    GraphicControlTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure mnuHD50Click(Sender: TObject);
    procedure mnuHDClick(Sender: TObject);
    procedure mnuTGraphicControlClick(Sender: TObject);
    procedure mnuTImageClick(Sender: TObject);
    procedure mnuTPaintBoxClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
    FileCache50: TFileCache;
    FileCache100: TFileCache;

    img: TMyDrawingControl;
    procedure AddTestToListBox(AName: string; FPS: double);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  png: TPortableNetworkGraphic;

implementation

{$R *.lfm}

uses
  GraphType;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FileCache50 := TFileCache.Create('./images50.cache');
  FileCache100 := TFileCache.Create('./images100.cache');

  png := TPortableNetworkGraphic.Create;

  //load all images to the caches
  for i := 1 to 23 do
  begin
    png.LoadFromFile(Format('./images/%.4d.png', [i]));
    FileCache100.Add(i, png);
    png.LoadFromFile(Format('./images_50/%.4d.png', [i]));
    FileCache50.Add(i, png);
  end;

  //PaintBoxTabSheet.DoubleBuffered := True;
  //ImageTabSheet.DoubleBuffered := True;
  //GraphicControlTabSheet.DoubleBuffered := True;

  img := TMyDrawingControl.Create(nil);
  img.Parent := GraphicControlTabSheet;
  img.Align := alClient;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FileCache50.Free;
  FileCache100.Free;

  png.Free;
  img.Free;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  mnuTPaintBoxClick(nil);
  mnuTImageClick(nil);
  mnuTGraphicControlClick(nil);
end;

procedure TForm1.mnuHD50Click(Sender: TObject);
begin
  mnuHD50.Checked := True;
end;

procedure TForm1.mnuHDClick(Sender: TObject);
begin
  mnuHD.Checked := True;
end;

procedure TForm1.mnuTGraphicControlClick(Sender: TObject);
var
  s: TDateTime;
  i: integer;
  j: integer;
  f: boolean;
begin
  PageControl1.ActivePage := GraphicControlTabSheet;

  s := Now;
  for j := 1 to 10 do
    for i := 1 to 23 do
    begin
      if mnuHD50.Checked then
        f := FileCache50.GetData(i, png)
      else
        f := FileCache100.GetData(i, png);

      if f then
      begin
        img.Bitmap := png;
        img.Invalidate;
        Application.ProcessMessages;
      end;
    end;

  AddTestToListBox('TGraphicControl', 230 / ((Now - s) * 24 * 3600));
end;

procedure TForm1.mnuTImageClick(Sender: TObject);
var
  s: TDateTime;
  i: integer;
  j: integer;
  f: boolean;
begin
  PageControl1.ActivePage := ImageTabSheet;

  s := Now;
  for j := 1 to 10 do
    for i := 1 to 23 do
    begin
      if mnuHD50.Checked then
        f := FileCache50.GetData(i, png)
      else
        f := FileCache100.GetData(i, png);

      if f then
      begin
        Image1.Picture.Bitmap.LoadFromRawImage(png.RawImage, False);
        Application.ProcessMessages;
      end;
    end;

  AddTestToListBox('TImage', 230 / ((Now - s) * 24 * 3600));
end;

procedure TForm1.mnuTPaintBoxClick(Sender: TObject);
var
  s: TDateTime;
  i: integer;
  j: integer;
  f: Boolean;
begin
  PageControl1.ActivePage := PaintBoxTabSheet;

  s := Now;
  for j := 1 to 10 do
    for i := 1 to 23 do
    begin
      if mnuHD50.Checked then
        f := FileCache50.GetData(i, png)
      else
        f := FileCache100.GetData(i, png);

      if f then
      begin
        PaintBox1.Invalidate;
        Application.ProcessMessages;
      end;
    end;

  AddTestToListBox('TPaintBox', 230 / ((Now - s) * 24 * 3600));
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, png);
end;

procedure TForm1.AddTestToListBox(AName: string; FPS: double);
var
  i: integer;
  found: boolean = False;
begin
  //first check if test is already added earlier
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items[i].Caption = AName then
    begin
      found := True;
      ListView1.Items[i].SubItems.Clear;
      ListView1.Items[i].SubItems.Add(FloatToStr(FPS));
    end;
  end;

  if not found then
    with ListView1.Items.Add do
    begin
      Caption := AName;
      SubItems.Add(FloatToStr(FPS));
    end;
end;

end.
