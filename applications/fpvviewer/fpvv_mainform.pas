unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls, ComCtrls,
  fpvv_drawer, fpimage, fpcanvas;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnVisualize: TButton;
    btnViewDXFTokens: TButton;
    buttonRenderingTest: TButton;
    editFileName: TFileNameEdit;
    notebook: TNotebook;
    pageViewer: TPage;
    Page2: TPage;
    spinScale: TFloatSpinEdit;
    Label1: TLabel;
    DXFTreeView: TTreeView;
    procedure btnVisualizeClick(Sender: TObject);
    procedure btnViewDXFTokensClick(Sender: TObject);
    procedure buttonRenderingTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Drawer: TFPVVDrawer;
  end;

var
  frmFPVViewer: TfrmFPVViewer;

implementation

uses
  fpvectorial, cdrvectorialreader, svgvectorialwriter, pdfvectorialreader,
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
      Vec,
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

procedure TfrmFPVViewer.buttonRenderingTestClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  notebook.PageIndex := 0;

  Drawer.Clear;

  Vec := TvVectorialDocument.Create;
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

end.

