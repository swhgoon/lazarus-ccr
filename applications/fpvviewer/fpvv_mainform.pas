unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls, ComCtrls,
  fpvv_drawer;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnVisualize: TButton;
    btnViewDXFTokens: TButton;
    editFileName: TFileNameEdit;
    notebook: TNotebook;
    pageViewer: TPage;
    Page2: TPage;
    spinScale: TFloatSpinEdit;
    Label1: TLabel;
    DXFTreeView: TTreeView;
    procedure btnVisualizeClick(Sender: TObject);
    procedure btnViewDXFTokensClick(Sender: TObject);
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
  dxfvectorialreader,
  fpvtocanvas,
  dxftokentotree;

{$R *.lfm}

{ TfrmFPVViewer }

procedure TfrmFPVViewer.btnVisualizeClick(Sender: TObject);
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
    Vec.ReadFromFile(editFileName.FileName, vfDXF);

    // We need to be robust, because sometimes the document size won't be given
    // also give up drawing everything if we need more then 4MB of RAM for the image
    if Vec.Width * spinScale.Value > 1000 then CanvasSize.X := 1000
    else if Vec.Width < 100 then CanvasSize.X := Drawer.Width
    else CanvasSize.X := Round(Vec.Width * spinScale.Value);

    if Vec.Height * spinScale.Value > 1000 then CanvasSize.Y := 1000
    else  if Vec.Height < 100 then CanvasSize.Y := Drawer.Height
    else CanvasSize.Y := Round(Vec.Height * spinScale.Value);

    Drawer.Drawing.Width := CanvasSize.X;
    Drawer.Drawing.Height := CanvasSize.Y;
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

