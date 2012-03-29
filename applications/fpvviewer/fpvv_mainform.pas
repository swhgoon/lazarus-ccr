unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Grids,
  fpvv_drawer, fpimage, fpcanvas, coreconrec;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnVisualize: TButton;
    btnViewDXFTokens: TButton;
    Button1: TButton;
    btnContourLines: TButton;
    buttonRenderingTest: TButton;
    editFileName: TFileNameEdit;
    notebook: TNotebook;
    pageViewer: TPage;
    Page2: TPage;
    spinScale: TFloatSpinEdit;
    Label1: TLabel;
    DXFTreeView: TTreeView;
    procedure btnContourLinesClick(Sender: TObject);
    procedure btnVisualizeClick(Sender: TObject);
    procedure btnViewDXFTokensClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure buttonRenderingTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spinScaleChange(Sender: TObject);
  private
    procedure MyContourLineDrawingProc(z,x1,y1,x2,y2: Double);
  public
    { public declarations }
    Drawer: TFPVVDrawer;
  end;

var
  frmFPVViewer: TfrmFPVViewer;

implementation

uses
  fpvectorial, lasvectorialreader, svgvectorialwriter,
  dxfvectorialreader, epsvectorialreader,
  fpvtocanvas,
  dxftokentotree;

{$R *.lfm}

{ TfrmFPVViewer }

procedure TfrmFPVViewer.btnVisualizeClick(Sender: TObject);
const
  FPVVIEWER_MAX_IMAGE_SIZE = 1000;
  FPVVIEWER_MIN_IMAGE_SIZE = 100;
  FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS = 100;
var
  Vec: TvVectorialDocument;
  CanvasSize: TPoint;
begin
  // First check the in input
  //if not CheckInput() then Exit;

  notebook.PageIndex := 0;

  Drawer.Clear;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editFileName.FileName);

    // We need to be robust, because sometimes the document size won't be given
    // also give up drawing everything if we need more then 4MB of RAM for the image
    // and also give some space in the image to allow for negative coordinates
    if Vec.Width * spinScale.Value > FPVVIEWER_MAX_IMAGE_SIZE then CanvasSize.X := FPVVIEWER_MAX_IMAGE_SIZE
    else if Vec.Width < FPVVIEWER_MIN_IMAGE_SIZE then CanvasSize.X := Drawer.Width
    else CanvasSize.X := Round(Vec.Width * spinScale.Value);
    if CanvasSize.X < Drawer.Width then CanvasSize.X := Drawer.Width;

    if Vec.Height * spinScale.Value > FPVVIEWER_MAX_IMAGE_SIZE then CanvasSize.Y := FPVVIEWER_MAX_IMAGE_SIZE
    else  if Vec.Height < FPVVIEWER_MIN_IMAGE_SIZE then CanvasSize.Y := Drawer.Height
    else CanvasSize.Y := Round(Vec.Height * spinScale.Value);
    if CanvasSize.Y < Drawer.Height then CanvasSize.Y := Drawer.Height;

    Drawer.Drawing.Width := CanvasSize.X;
    Drawer.Drawing.Height := CanvasSize.Y;
    Drawer.Drawing.Canvas.Brush.Color := clWhite;
    Drawer.Drawing.Canvas.Brush.Style := bsSolid;
    Drawer.Drawing.Canvas.FillRect(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);
    DrawFPVectorialToCanvas(
      Vec.GetPage(0),
      Drawer.Drawing.Canvas,
      FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS,
      Drawer.Drawing.Height - FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS,
      spinScale.Value,
      -1 * spinScale.Value);
    Drawer.Invalidate;
  finally
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.btnContourLinesClick(Sender: TObject);
const
  dimx = 1024;  // dimension west - east
  dimy = 1024;  // dimenstion north west
  dimh = 10;   // dimension for contour levels
var
  Mat:TMatrix;  // 2D - Datafield
  scx:TVector;  // scaling vector west - east
  scy:TVector;  // scaling vector north - west
  hgt:TVector;  // vector for the countur levels
  i,j:Integer;  // adress indexes
  x,y:Double;   // coord. values
  mi,ma:Double; // for minimum & maximum
  Vec: TvVectorialDocument;
  lPage: TvVectorialPage;
  lRasterImage: TvRasterImage;
begin
  // Drawing size setting and initialization
  Drawer.Drawing.Width := Drawer.Width;
  Drawer.Drawing.Height := Drawer.Height;
  Drawer.Drawing.Canvas.Brush.Color := clWhite;
  Drawer.Drawing.Canvas.Brush.Style := bsSolid;
  Drawer.Drawing.Canvas.FillRect(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);

  Vec := TvVectorialDocument.Create;
  Vec.ReadFromFile(editFileName.FileName);
  lPage := Vec.GetPage(0);
  lRasterImage := TvRasterImage(lPage.GetEntity(0));

  // create dynamicaly the vectors and datafield
  setlength(scx,dimx);
  setlength(scy,dimy);
  setlength(hgt,dimh);
  setlength(mat,dimx);
  for i:=0 to dimx-1 do Setlength(mat[i],dimy);
  try
    for i:=0 to dimx-1 do scx[i]:= i * 10; // set scaling vector west - east
    for i:=0 to dimy-1 do scy[i]:= i * 10; // set scaling vector north - south

    for i:=0 to dimx-1 do  // ----------------------------------- set 2d data field
      for j:=0 to dimy-1 do
      begin
        x:=i-dimx/2;
        y:=j-dimy/2;
        mat[i,j]:= Round(lRasterImage.RasterImage.Colors[i, j].red * 10 / $FFFF);
                 { (sin(x/dimx*4*pi)    * cos(y/dimy*4*pi)) +
                  (sin(x/dimx*2*pi)    * cos(y/dimy*2*pi)) +
                  (sin(x/dimx*1*pi)    * cos(y/dimy*1*pi)) +
                  (sin(x/dimx*0.5*pi)  * cos(y/dimy*0.5*pi))+
                  (sin(x/dimx*0.25*pi) * cos(y/dimy*0.25*pi));}
      end; // -----------------------------------------------------------------------

    mi:=1e16;    // ------------    Set the minimunm and maximum fof the data field
    ma:=-1e16;
    for i:=0 to dimx-1 Do
      for j:=0 to dimy-1 do
      begin
        if mat[i,j]<mi then mi:=mat[i,j];
        if mat[i,j]>ma then ma:=mat[i,j];
      end;        //----------------------------------------------------------------

    For i:=0 to dimh-1 Do hgt[i]:=mi+i*(ma-mi)/(dimh-1); // ----- create cut levels

    ContourLineDrawingProc := @MyContourLineDrawingProc;
    // call the contour algorithm
    conrec(mat,0,dimx-1,0,dimy-1,scx,scy,dimh,hgt);
  finally
    // Finalization of allocated memory
    setlength(scx, 0);
    setlength(scy, 0);
    setlength(hgt, 0);
    For i:=0 to dimx-1 Do Setlength(mat[i], 0);
    setlength(mat, 0);
    Vec.Free;
  end;

  Drawer.Invalidate;
end;

procedure TfrmFPVViewer.btnViewDXFTokensClick(Sender: TObject);
var
  Reader: TvDXFVectorialReader;
  Vec: TvVectorialDocument;
begin
  // First check the in input
  //if not CheckInput() then Exit;

  notebook.PageIndex := 1;

  Reader := TvDXFVectorialReader.Create;
  Vec := TvVectorialDocument.Create;
  try
    Reader.ReadFromFile(editFileName.FileName, Vec);
    ConvertDXFTokensToTreeView(Reader.Tokenizer.Tokens, DXFTreeView);
  finally
    Reader.Free;
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.Button1Click(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editFileName.FileName);
    Vec.WriteToFile(ChangeFileExt(editFileName.FileName, '.svg'));
  finally
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.buttonRenderingTestClick(Sender: TObject);
var
  VecDoc: TvVectorialDocument;
  Vec: TvVectorialPage;
begin
  notebook.PageIndex := 0;

  Drawer.Clear;

  VecDoc := TvVectorialDocument.Create;
  Vec := VecDoc.AddPage();
  try
    Vec.AddAlignedDimension(Make2DPoint(100, 50), Make2DPoint(200, 100), Make2DPoint(100, 150), Make2DPoint(200, 150));
    Vec.AddAlignedDimension(Make2DPoint(50, 250), Make2DPoint(100, 200), Make2DPoint(150, 250), Make2DPoint(150, 200));

    Vec.StartPath(0, 0);
    Vec.SetPenColor(colYellow);
    Vec.SetPenWidth(1);
    Vec.AddLineToPath(100, 100);
    Vec.EndPath();

    Drawer.Drawing.Width := 400;
    Drawer.Drawing.Height := 400;
    Drawer.Drawing.Canvas.Brush.Color := clWhite;
    Drawer.Drawing.Canvas.Brush.Style := bsSolid;
    Drawer.Drawing.Canvas.FillRect(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);
    DrawFPVectorialToCanvas(
      Vec,
      Drawer.Drawing.Canvas,
      0,
      Drawer.Drawing.Height,
      spinScale.Value,
      -1 * spinScale.Value);
    Drawer.Invalidate;
  finally
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.FormCreate(Sender: TObject);
begin
  Drawer := TFPVVDrawer.Create(Self);
  Drawer.Parent := pageViewer;
  Drawer.Top := 5;
  Drawer.Left := 5;
  Drawer.AnchorClient(5);
  Drawer.TabStop := True;
end;

procedure TfrmFPVViewer.FormDestroy(Sender: TObject);
begin
  Drawer.Free;
end;

procedure TfrmFPVViewer.spinScaleChange(Sender: TObject);
begin
  if spinScale.Value <= 0.2 then spinScale.Increment := 0.01
  else if spinScale.Value <= 2 then spinScale.Increment := 0.1
  else spinScale.Increment := 1;
end;

procedure TfrmFPVViewer.MyContourLineDrawingProc(z, x1, y1, x2, y2: Double);
begin
  Drawer.Drawing.Canvas.Pen.Style := psSolid;
  Drawer.Drawing.Canvas.Pen.Color := clBlack;
  Drawer.Drawing.Canvas.Line(Round(x1 / 20), Round(y1 / 20), Round(x2 / 20), Round(y2 / 20));
end;

end.

