unit chessdrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType,
  //
  chessgame, chessconfig;

type

  { TChessDrawer }

  TChessDrawer = class(TCustomControl)
  private
    imgBoard,
     imgWPawn, imgWKnight, imgWBishop, imgWRook, imgWQueen, imgWKing,
     imgBPawn, imgBKnight, imgBBishop, imgBRook, imgBQueen, imgBKing:
     TPortableNetworkGraphic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    procedure DrawChessTile(ACanvas: TCanvas; ACol, ARow: Integer;
      ATile: TChessTile);
    procedure LoadImages();
  end;

var
  vChessDrawer: TChessDrawer;

implementation

constructor TChessDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  imgBoard := TPortableNetworkGraphic.Create;
  imgWPawn := TPortableNetworkGraphic.Create;
  imgWKnight := TPortableNetworkGraphic.Create;
  imgWBishop := TPortableNetworkGraphic.Create;
  imgWRook := TPortableNetworkGraphic.Create;
  imgWQueen := TPortableNetworkGraphic.Create;
  imgWKing := TPortableNetworkGraphic.Create;
  imgBPawn := TPortableNetworkGraphic.Create;
  imgBKnight := TPortableNetworkGraphic.Create;
  imgBBishop := TPortableNetworkGraphic.Create;
  imgBRook := TPortableNetworkGraphic.Create;
  imgBQueen := TPortableNetworkGraphic.Create;
  imgBKing := TPortableNetworkGraphic.Create;
end;

procedure TChessDrawer.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TChessDrawer.Paint;
var
  x, y: Integer;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    // Initializes the Bitmap Size
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    DrawToCanvas(Bitmap.Canvas);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;

//  inherited Paint;
end;

procedure TChessDrawer.DrawToCanvas(ACanvas: TCanvas);
var
  col, row: Integer;
begin
  // First draw the board
  ACanvas.Draw(0, 0, imgBoard);

  // Now all pieces
  for col := 1 to 8 do
    for row := 1 to 8 do
      DrawChessTile(ACanvas, col, row, vChessGame.Board[col][row]);
end;

procedure TChessDrawer.DrawChessTile(ACanvas: TCanvas; ACol, ARow: Integer;
  ATile: TChessTile);
var
  X, Y: Integer;
begin
  if ATile = ctEmpty then Exit;

  X := (ACol - 1) * INT_CHESSTILE_SIZE;
  Y := (8 - ARow) * INT_CHESSTILE_SIZE;

  case ATile of
  ctWPawn: ACanvas.Draw(X, Y, imgWPawn);
  ctWKnight: ACanvas.Draw(X, Y, imgWKnight);
  ctWBishop: ACanvas.Draw(X, Y, imgWBishop);
  ctWRook: ACanvas.Draw(X, Y, imgWRook);
  ctWQueen: ACanvas.Draw(X, Y, imgWQueen);
  ctWKing: ACanvas.Draw(X, Y, imgWKing);
  ctBPawn: ACanvas.Draw(X, Y, imgBPawn);
  ctBKnight: ACanvas.Draw(X, Y, imgBKnight);
  ctBBishop: ACanvas.Draw(X, Y, imgBBishop);
  ctBRook: ACanvas.Draw(X, Y, imgBRook);
  ctBQueen: ACanvas.Draw(X, Y, imgBQueen);
  ctBKing: ACanvas.Draw(X, Y, imgBKing);
  end;
end;

procedure TChessDrawer.LoadImages();
var
  lDir: string;
begin
  lDir := vChessConfig.GetCurrentSkinDir();

  imgBoard.LoadFromFile(lDir + 'board.png');
  imgWPawn.LoadFromFile(lDir + 'wpawn.png');
  imgWKnight.LoadFromFile(lDir + 'wknight.png');
  imgWBishop.LoadFromFile(lDir + 'wbishop.png');
  imgWRook.LoadFromFile(lDir + 'wrook.png');
  imgWQueen.LoadFromFile(lDir + 'wqueen.png');
  imgWKing.LoadFromFile(lDir + 'wking.png');
  imgBPawn.LoadFromFile(lDir + 'bpawn.png');
  imgBKnight.LoadFromFile(lDir + 'bknight.png');
  imgBBishop.LoadFromFile(lDir + 'bbishop.png');
  imgBRook.LoadFromFile(lDir + 'brook.png');
  imgBQueen.LoadFromFile(lDir + 'bqueen.png');
  imgBKing.LoadFromFile(lDir + 'bking.png');
end;

end.

