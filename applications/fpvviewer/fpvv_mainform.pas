unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls, ComCtrls;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnVisualize: TButton;
    Button1: TButton;
    editFileName: TFileNameEdit;
    imageView: TImage;
    Label2: TLabel;
    Label3: TLabel;
    notebook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    spinStartX: TSpinEdit;
    spinStartY: TSpinEdit;
    spinScale: TFloatSpinEdit;
    Label1: TLabel;
    DXFTreeView: TTreeView;
    procedure btnVisualizeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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
begin
  // First check the in input
  //if not CheckInput() then Exit;

  notebook.PageIndex := 0;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editFileName.FileName, vfDXF);
    imageView.Canvas.Brush.Color := clWhite;
    imageView.Canvas.FillRect(0, 0, imageView.Width, imageView.Height);
    DrawFPVectorialToCanvas(
      Vec,
      imageView.Canvas,
      spinStartX.Value,
      spinStartY.Value + imageView.Height,
      spinScale.Value,
      -1 * spinScale.Value);
  finally
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.Button1Click(Sender: TObject);
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

end.

