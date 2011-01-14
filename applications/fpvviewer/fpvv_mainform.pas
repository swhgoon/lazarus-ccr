unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnVisualize: TButton;
    editFileName: TFileNameEdit;
    imageView: TImage;
    Label2: TLabel;
    Label3: TLabel;
    spinStartX: TSpinEdit;
    spinStartY: TSpinEdit;
    spinScale: TFloatSpinEdit;
    Label1: TLabel;
    procedure btnVisualizeClick(Sender: TObject);
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
  fpvtocanvas;

{$R *.lfm}

{ TfrmFPVViewer }

procedure TfrmFPVViewer.btnVisualizeClick(Sender: TObject);
var
  Vec: TvVectorialDocument;
begin
  // First check the in input
  //if not CheckInput() then Exit;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editFileName.FileName, vfDXF);
    imageView.Canvas.Brush.Color := clWhite;
    imageView.Canvas.FillRect(0, 0, imageView.Width, imageView.Height);
    DrawFPVectorialToCanvas(Vec, imageView.Canvas, spinStartX.Value, spinStartY.Value, spinScale.Value, spinScale.Value);
  finally
    Vec.Free;
  end;
end;

end.

