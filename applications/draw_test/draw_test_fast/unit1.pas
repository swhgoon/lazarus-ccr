unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus, FileCache, FastBitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuHD50: TMenuItem;
    mnuHD: TMenuItem;
    MenuItem2: TMenuItem;
    mnuTImage: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    ImageTabSheet: TTabSheet;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure mnuHD50Click(Sender: TObject);
    procedure mnuHDClick(Sender: TObject);
    procedure mnuTImageClick(Sender: TObject);
  private
    { private declarations }
    FileCache50: TFileCache;

    procedure AddTestToListBox(AName: string; FPS: double);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  png: TPortableNetworkGraphic;
  bmp: TFastBitmap;

implementation

{$R *.lfm}

uses
  GraphType;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  RawImage: TRawImage;
  RowPtr: PInteger;
  BytePerRow: PtrUInt;
begin
  FileCache50 := TFileCache.Create('./images50.cache');

  png := TPortableNetworkGraphic.Create;

  bmp := TFastBitmap.Create;
  bmp.Size := Point(1920, 1080);

  //load all images to the caches
  for i := 1 to 23 do
  begin
    png.LoadFromFile(Format('../images/%.4d.png', [i]));
    RawImage := png.RawImage;
    RowPtr := PInteger(RawImage.Data);
    BytePerRow := RawImage.Description.BytesPerLine;
    Move(RowPtr^, bmp.PixelsData^, bmp.Size.Y * BytePerRow);
    FileCache50.Add(i, bmp);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FileCache50.Free;

  png.Free;
  bmp.Free;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  mnuTImageClick(nil);
end;

procedure TForm1.mnuHD50Click(Sender: TObject);
begin
  mnuHD50.Checked := True;
end;

procedure TForm1.mnuHDClick(Sender: TObject);
begin
  mnuHD.Checked := True;
end;

procedure TForm1.mnuTImageClick(Sender: TObject);
var
  RowPtr: PInteger;
  RawImage: TRawImage;
  BytePerRow: integer;
  s: TDateTime;
  i: integer;
  j: integer;
begin
  PageControl1.ActivePage := ImageTabSheet;

  s := Now;
  for j := 1 to 10 do
    for i := 1 to 23 do
    begin
      if FileCache50.GetData(i, bmp) then
      begin
        with bmp do
          try
            Image1.Picture.Bitmap.Width := 1920;
            Image1.Picture.Bitmap.Height := 1080;
            Image1.Picture.Bitmap.PixelFormat := pf32bit;
            Image1.Picture.Bitmap.BeginUpdate(False);
            RawImage := Image1.Picture.Bitmap.RawImage;
            RowPtr := PInteger(RawImage.Data);
            BytePerRow := RawImage.Description.BytesPerLine;
            Move(bmp.PixelsData^, RowPtr^, Size.Y * BytePerRow);
          finally
            Image1.Picture.Bitmap.EndUpdate(False);
          end;
        //Image1.Picture.Bitmap.LoadFromRawImage(png.RawImage, False);
        Application.ProcessMessages;
      end;
    end;

  AddTestToListBox('TImage', 230 / ((Now - s) * 24 * 3600));
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
