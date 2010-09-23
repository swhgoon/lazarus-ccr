unit chessdrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, IntfGraphics, fpimage,
  Math,
  chessgame, chessconfig;

type

  TChessDrawerDelegate = class
  public
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer); virtual; abstract;
  end;

  { TChessDrawer }

  TChessDrawer = class(TCustomControl)
  private
    imgBoard, imgWPawn, imgWKnight, imgWBishop, imgWRook, imgWQueen,
    imgWKing, imgBPawn, imgBKnight, imgBBishop, imgBRook, imgBQueen,
    imgBKing: TPortableNetworkGraphic;
{    bmpBoard, bmpWPawn, bmpWKnight, bmpWBishop, bmpWRook, bmpWQueen,
    bmpWKing, bmpBPawn, bmpBKnight, bmpBBishop, bmpBRook, bmpBQueen,
    bmpBKing: TBitmap;}
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    procedure DrawImageWithTransparentColor(
      ADest: TLazIntfImage; const ADestX, ADestY: Integer; AColor: TFPColor;
      AImage: TFPImageBitmap);
    function GetChessTileImage(ATile: TChessTile): TPortableNetworkGraphic;
    procedure LoadImages();
    procedure SetDelegate(ADelegate: TChessDrawerDelegate);
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

{  bmpBoard := TBitmap.Create;
  bmpWPawn := TBitmap.Create;
  bmpWKnight := TBitmap.Create;
  bmpWBishop := TBitmap.Create;
  bmpWRook := TBitmap.Create;
  bmpWQueen := TBitmap.Create;
  bmpWKing := TBitmap.Create;
  bmpBPawn := TBitmap.Create;
  bmpBKnight := TBitmap.Create;
  bmpBBishop := TBitmap.Create;
  bmpBRook := TBitmap.Create;
  bmpBQueen := TBitmap.Create;
  bmpBKing := TBitmap.Create;       }
end;

procedure TChessDrawer.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TChessDrawer.Paint;
var
  x, y: integer;
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
  col, row: integer;
  lIntfImage: TLazIntfImage;
  lTmpBmp: TBitmap;
  lTileBmp: TPortableNetworkGraphic;
  X, Y: integer;
begin
  lIntfImage := TLazIntfImage.Create(0, 0);
  lTmpBmp := TBitmap.Create;
  try
    // First draw the board
    lIntfImage.LoadFromBitmap(imgBoard.Handle, 0{bmpBoard.MaskHandle});

    // Now all pieces
    for col := 1 to 8 do
      for row := 1 to 8 do
      begin
        lTileBmp := GetChessTileImage(vChessGame.Board[col][row]);
        if lTileBmp = nil then Continue;

        X := (col - 1) * INT_CHESSTILE_SIZE;
        Y := (8 - row) * INT_CHESSTILE_SIZE;

        DrawImageWithTransparentColor(lIntfImage, X, Y, FPCOLOR_TRANSPARENT_TILE, lTileBmp);
      end;

    lTmpBmp.LoadFromIntfImage(lIntfImage);
    ACanvas.Draw(0, 0, lTmpBmp);
  finally
    lTmpBmp.Free;
    lIntfImage.Free;
  end;
end;

procedure TChessDrawer.DrawImageWithTransparentColor(ADest: TLazIntfImage;
  const ADestX, ADestY: Integer; AColor: TFPColor; AImage: TFPImageBitmap);
var
  x, y, CurX, CurY: Integer;
  IntfImage: TLazIntfImage;
  lDrawWidth, lDrawHeight: Integer;
  CurColor: TFPColor;
  lCurColorDiv, lTranspColorDiv: Byte;
begin
  IntfImage := TLazIntfImage.Create(0,0);
  try
    IntfImage.LoadFromBitmap(AImage.Handle, AImage.MaskHandle);

    // Take care not to draw outside the destination area
    lDrawWidth := Min(ADest.Width - ADestX, AImage.Width);
    lDrawHeight := Min(ADest.Height - ADestY, AImage.Height);
    for y := 0 to lDrawHeight - 1 do
    begin
      for x := 0 to lDrawWidth - 1 do
      begin
        CurX := ADestX + x;
        CurY := ADestY + y;

        // Never draw outside the destination
        if (CurX < 0) or (CurY < 0) then Continue;

        CurColor := IntfImage.Colors[x, y]; // Good for debugging
        lCurColorDiv := CurColor.Green div $FF;
        lTranspColorDiv := AColor.Green div $FF;
        if lCurColorDiv <> lTranspColorDiv then
          ADest.Colors[CurX, CurY] := IntfImage.Colors[x, y];
      end;
    end;
  finally
    IntfImage.Free;
  end;
end;

function TChessDrawer.GetChessTileImage(ATile: TChessTile): TPortableNetworkGraphic;
begin
  case ATile of
    ctWPawn:   Result := imgWPawn;
    ctWKnight: Result := imgWKnight;
    ctWBishop: Result := imgWBishop;
    ctWRook:   Result := imgWRook;
    ctWQueen:  Result := imgWQueen;
    ctWKing:   Result := imgWKing;
    ctBPawn:   Result := imgBPawn;
    ctBKnight: Result := imgBKnight;
    ctBBishop: Result := imgBBishop;
    ctBRook:   Result := imgBRook;
    ctBQueen:  Result := imgBQueen;
    ctBKing:   Result := imgBKing;
  else
    Result := nil;
  end;
end;

procedure TChessDrawer.LoadImages();
var
  lDir: string;
begin
  lDir := vChessConfig.GetCurrentSkinDir();

  imgBoard.LoadFromFile(lDir + 'base.png');
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

{  bmpWKnight.Assign(imgWKnight);
  bmpWKnight.Assign(imgWBishop);
  bmpWKnight.Assign(imgWRook);
  bmpWKnight.Assign(imgWQueen);
  bmpWKnight.Assign(imgWKing);
  bmpWKnight.Assign(imgBPawn);
  bmpWKnight.Assign(imgBKnight);
  bmpWKnight.Assign(imgBBishop);
  bmpWKnight.Assign(imgBRook);
  bmpWKnight.Assign(imgBQueen);
  bmpWKnight.Assign(imgBKing);    }
end;

procedure TChessDrawer.SetDelegate(ADelegate: TChessDrawerDelegate);
begin
  // Events
  OnMouseMove := @ADelegate.HandleMouseMove;
  OnMouseUp := @ADelegate.HandleMouseUp;
  OnMouseDown := @ADelegate.HandleMouseDown;
end;

end.

