unit fpvv_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Grids, ColorBox, Math,
  Printers, PrintersDlgs, LCLIntf, LCLType,
  fpvv_drawer, fpimage, fpcanvas, coreconrec, fpvutils,
  fpvectorial, lasvectorialreader, svgvectorialwriter,
  dxfvectorialreader, epsvectorialreader, fpvtocanvas, dxftokentotree;

type

  { TfrmFPVViewer }

  TfrmFPVViewer = class(TForm)
    btnContourLines: TButton;
    btnSearchInTokens: TButton;
    btnViewDXFTokens: TButton;
    btnVisualize: TButton;
    Button1: TButton;
    Button2: TButton;
    buttonPrint: TButton;
    buttonAdjust: TButton;
    buttonViewDebugInfo: TButton;
    buttonRenderingTest: TButton;
    checkShowPage: TCheckBox;
    checkForceWhiteBackground: TCheckBox;
    comboEncoding: TComboBox;
    editFileName: TFileNameEdit;
    editSearchInTokens: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    labelFileEncoding: TLabel;
    memoDebug: TMemo;
    notebook: TNotebook;
    pageDebug: TPage;
    pageViewer: TPage;
    pageTreeData: TPage;
    Panel1: TPanel;
    spinAdjustX: TSpinEdit;
    spinAdjustY: TSpinEdit;
    spinScale: TFloatSpinEdit;
    TokensTreeView: TTreeView;
    procedure btnContourLinesClick(Sender: TObject);
    procedure btnSearchInTokensClick(Sender: TObject);
    procedure btnVisualizeClick(Sender: TObject);
    procedure btnViewDXFTokensClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure buttonAdjustClick(Sender: TObject);
    procedure buttonPrintClick(Sender: TObject);
    procedure buttonRenderingTestClick(Sender: TObject);
    procedure buttonViewDebugInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spinAdjustXChange(Sender: TObject);
    procedure spinAdjustYChange(Sender: TObject);
    procedure spinScaleChange(Sender: TObject);
  private
    FVec: TvVectorialDocument;
    procedure MyContourLineDrawingProc(z,x1,y1,x2,y2: Double);
    function FPVDebugAddItemProc(AStr: string; AParent: Pointer): Pointer;
    procedure HandleDrawerMouseWheel(Sender: TObject; Shift: TShiftState;
       WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleDrawerPosChanged(Sender: TObject);
    procedure HandleDrawerRedraw(Sender: TObject);
    procedure ViewerDebugOutCallback(AStr: string);
    //
    procedure Render_PrepareFile();
    procedure Render_DoRender(ACanvasSizeX, ACanvasSizeY,
      ADrawerPosX, ADrawerPosY: Integer; AScale: Double);
    procedure Render_FreeFile();
  public
    { public declarations }
    Drawer: TFPVVDrawer;
  end;

var
  frmFPVViewer: TfrmFPVViewer;

implementation

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
  lCurPage: TvVectorialPage;
  lPage: TvPage;
begin
  // First check the in input
  if editFileName.FileName = '' then Exit; // silent exit in this simple case
  //if not CheckInput() then Exit;

  notebook.PageIndex := 0;

  Vec := TvVectorialDocument.Create;
  try
    // If we desire, force a encoding for the read operation
    if comboEncoding.ItemIndex > 0 then
      Vec.ForcedEncodingOnRead := comboEncoding.Text
    else Vec.ForcedEncodingOnRead := '';

    Vec.ReadFromFile(editFileName.FileName);

    // Show document properties
    labelFileEncoding.Caption := 'File encoding: ' + Vec.Encoding;

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
    lPage := Vec.GetPage(0);
    if lPage = nil then
      Exception.Create('The document has no pages');
    if checkForceWhiteBackground.Checked then lPage.BackgroundColor := colWhite;
    if not checkForceWhiteBackground.Checked then
      lPage.DrawBackground(Drawer.Drawing.Canvas);
    lPage.Render(
      Drawer.Drawing.Canvas,
      FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS + Drawer.PosX,
      Drawer.Drawing.Height - FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS + Drawer.PosY,
      spinScale.Value,
      -1 * spinScale.Value);
    if checkShowPage.Checked then
      lPage.RenderPageBorder(
        Drawer.Drawing.Canvas,
        FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS + Drawer.PosX,
        Drawer.Drawing.Height - FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS + Drawer.PosY,
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
  dimh = 20;   // dimension for contour levels
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
  lPage := Vec.GetPageAsVectorial(0);
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
        mat[i,j]:= Round(lRasterImage.RasterImage.Colors[i, j].red * dimh / $FFFF);
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

procedure TfrmFPVViewer.btnSearchInTokensClick(Sender: TObject);
var
  lTokenWithText, lCurNode: TTreeNode;
  // minimal positions, based on the currently selected item to do "search next"
  lSearchStart: Word = 0;
  i, j: Integer;
  lStr, lText: TCaption;
  lNodesCount: Integer;
begin
  lText := editSearchInTokens.Text;

  // if something is selected, set this position as a minimum
  if TokensTreeView.Selected <> nil then
    lSearchStart := TokensTreeView.Selected.AbsoluteIndex;

  // Now do the actual search
  lTokenWithText := nil;
  lNodesCount := TokensTreeView.Items.Count-1;
  for i := 0 to lNodesCount do
  begin
    lCurNode := TokensTreeView.Items.Item[i];
    if lCurNode = nil then Continue;

    // Check the minimum level first
    if lSearchStart >= lCurNode.AbsoluteIndex then Continue;

    // Check the text
    lStr := lCurNode.Text;
    if System.Pos(lText, lStr) > 0 then
    begin
      lTokenWithText := lCurNode;
      Break;
    end;
  end;
  if lTokenWithText <> nil then
  begin
    TokensTreeView.Selected := lTokenWithText;
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
    ConvertDXFTokensToTreeView(Reader.Tokenizer.Tokens, TokensTreeView);
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

procedure TfrmFPVViewer.Button2Click(Sender: TObject);
var
  Vec: TvVectorialDocument;
  i: Integer;
  lCurPage: TvVectorialPage;
begin
  // First check the in input
  //if not CheckInput() then Exit;

  notebook.PageIndex := 1;

  Vec := TvVectorialDocument.Create;
  try
    Vec.ReadFromFile(editFileName.FileName);

    // Generate the positioning info
    for i := 0 to Vec.GetPageCount()-1 do
    begin
      if Vec.GetPageAsVectorial(i) <> nil then
        Vec.GetPageAsVectorial(i).PositionEntitySubparts(Canvas, 0, 0);
    end;

    TokensTreeView.Items.Clear;
    Vec.GenerateDebugTree(@FPVDebugAddItemProc);
  finally
    Vec.Free;
  end;
end;

procedure TfrmFPVViewer.buttonAdjustClick(Sender: TObject);
const
  FPVVIEWER_MAX_IMAGE_SIZE = 1000;
  FPVVIEWER_MIN_IMAGE_SIZE = 100;
  FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS = 100;
var
  lSpaceForNegCoords: TPoint;
  lCurPage: TvPage;
  lZoom: Double;
  lCanvasWidth: Integer;
begin
  Render_PrepareFile();
  try
    lCurPage := FVec.GetPage(0);
    lCurPage.CalculateDocumentSize();

    lCanvasWidth := Drawer.Width;
    lZoom := (Drawer.Width / lCurPage.Width) * 0.8;
    spinScale.Value := lZoom;
    // Centralizes the image in the canvas
    lSpaceForNegCoords.X := Round(lCurPage.MinX * -1 * lZoom + Drawer.Width * 0.1);
    lSpaceForNegCoords.Y := Round(lCurPage.MinY * lZoom - Drawer.Height * 0.1);
    spinAdjustX.Value := lSpaceForNegCoords.X - FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS;
    spinAdjustY.Value := lSpaceForNegCoords.Y + FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS;
    Render_DoRender(Drawer.Width, Drawer.Height, lSpaceForNegCoords.X, lSpaceForNegCoords.Y, lZoom);
  finally
    Render_FreeFile();
  end;
end;

procedure TfrmFPVViewer.buttonPrintClick(Sender: TObject);
var
  printDialog: TPrintDialog;
  ScaleX, ScaleY, lScale: Double;
  lRectSrc, lRectDest: TRect;
begin
  // Create a printer selection dialog
  printDialog := TPrintDialog.Create(Self);

  // Set up print dialog options
  printDialog.Options := [];

  // if the user has selected a printer (or default), then print!
  if printDialog.Execute then
  begin
    // Generate the image
    btnVisualizeClick(Sender);

    // Start printing
    Printer.BeginDoc;

    // Draw the image
    {$IFDEF OLD_PRINT_CODE}
    ScaleX := LCLIntf.GetDeviceCaps(Handle, logPixelsX) / PixelsPerInch; // Don't know why, but GetDeviceCaps is returning zero...
    ScaleY := LCLIntf.GetDeviceCaps(Handle, logPixelsY) / PixelsPerInch;
    lRectSrc := Bounds(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);
    lRectDest := Bounds(0, 0, Printer.PageWidth, Printer.PageHeight);
    {$ELSE}
    ScaleX := Printer.PageWidth / Drawer.Drawing.Width;
    ScaleY := Printer.PageHeight / Drawer.Drawing.Height;
    lScale := Min(ScaleX, ScaleY);
    lRectSrc := Bounds(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);
    lRectDest := Bounds(0, 0,
      Round(Drawer.Drawing.Width * lScale),
      Round(Drawer.Drawing.Height * lScale));
    {$ENDIF}
    Printer.Canvas.StretchDraw(
      lRectDest,
      Drawer.Drawing);

    // Finish printing
    Printer.EndDoc;
  end;

  printDialog.Free;
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

procedure TfrmFPVViewer.buttonViewDebugInfoClick(Sender: TObject);
begin
  notebook.PageIndex := 2;
end;

procedure TfrmFPVViewer.FormCreate(Sender: TObject);
begin
  Drawer := TFPVVDrawer.Create(Self);
  Drawer.Parent := pageViewer;
  Drawer.Top := 5;
  Drawer.Left := 5;
  Drawer.AnchorClient(5);
  Drawer.TabStop := True;
  Drawer.OnMouseWheel := @HandleDrawerMouseWheel;
  Drawer.PosChangedCallback := @HandleDrawerPosChanged;
  Drawer.RedrawCallback := @HandleDrawerRedraw;

  FPVUDebugOutCallback := @ViewerDebugOutCallback;
end;

procedure TfrmFPVViewer.FormDestroy(Sender: TObject);
begin
  Drawer.Free;
end;

procedure TfrmFPVViewer.spinAdjustXChange(Sender: TObject);
begin
  Drawer.PosX := spinAdjustX.Value;
end;

procedure TfrmFPVViewer.spinAdjustYChange(Sender: TObject);
begin
  Drawer.PosY := spinAdjustY.Value;
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
  Drawer.Drawing.Canvas.Line(
    Round(x1 * spinScale.Value / 20),
    Round(y1 * spinScale.Value / 20),
    Round(x2 * spinScale.Value / 20),
    Round(y2 * spinScale.Value / 20));
end;

function TfrmFPVViewer.FPVDebugAddItemProc(AStr: string; AParent: Pointer): Pointer;
var
  lTreeItem: TTreeNode;
begin
  lTreeItem := TokensTreeView.Items.AddChild(TTreeNode(AParent), AStr);
  Result := lTreeItem;
end;

procedure TfrmFPVViewer.HandleDrawerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if WheelDelta > 0 then
    spinScale.Value := spinScale.Value + spinScale.Increment
  else
    spinScale.Value := spinScale.Value - spinScale.Increment;

  btnVisualize.Click();
end;

procedure TfrmFPVViewer.HandleDrawerPosChanged(Sender: TObject);
begin
  spinAdjustX.Value := Drawer.PosX;
  spinAdjustY.Value := Drawer.PosY;
end;

procedure TfrmFPVViewer.HandleDrawerRedraw(Sender: TObject);
begin
  btnVisualizeClick(Sender);
end;

procedure TfrmFPVViewer.ViewerDebugOutCallback(AStr: string);
begin
  memoDebug.Lines.Add(AStr);
end;

procedure TfrmFPVViewer.Render_PrepareFile;
begin
  // First check the in input
  if editFileName.FileName = '' then Exit; // silent exit in this simple case
  //if not CheckInput() then Exit;

  notebook.PageIndex := 0;

  FVec := TvVectorialDocument.Create;

  // If we desire, force a encoding for the read operation
  if comboEncoding.ItemIndex > 0 then
    FVec.ForcedEncodingOnRead := comboEncoding.Text
  else FVec.ForcedEncodingOnRead := '';

  FVec.ReadFromFile(editFileName.FileName);
end;

procedure TfrmFPVViewer.Render_DoRender(ACanvasSizeX, ACanvasSizeY,
  ADrawerPosX, ADrawerPosY: Integer; AScale: Double);
const
  FPVVIEWER_MAX_IMAGE_SIZE = 1000;
  FPVVIEWER_MIN_IMAGE_SIZE = 100;
  FPVVIEWER_SPACE_FOR_NEGATIVE_COORDS = 100;
begin
  // Show document properties
  labelFileEncoding.Caption := 'File encoding: ' + FVec.Encoding;

  Drawer.Drawing.Width := ACanvasSizeX;
  Drawer.Drawing.Height := ACanvasSizeY;
  Drawer.Drawing.Canvas.Brush.Color := clWhite;
  Drawer.Drawing.Canvas.Brush.Style := bsSolid;
  Drawer.Drawing.Canvas.FillRect(0, 0, Drawer.Drawing.Width, Drawer.Drawing.Height);
  if checkForceWhiteBackground.Checked then FVec.GetPageAsVectorial(0).BackgroundColor := colWhite;
  if not checkForceWhiteBackground.Checked then
    FVec.GetPageAsVectorial(0).DrawBackground(Drawer.Drawing.Canvas);
  FVec.GetPageAsVectorial(0).Render(
    Drawer.Drawing.Canvas,
    ADrawerPosX,
    Drawer.Drawing.Height + ADrawerPosY,
    AScale,
    -1 * AScale);
  Drawer.Invalidate;
end;

procedure TfrmFPVViewer.Render_FreeFile;
begin
  FVec.Free;
end;

end.

