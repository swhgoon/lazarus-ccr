unit tappydrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, IntfGraphics, fpimage,
  Math,
  tappygamedata, tappyconfig, tappymodules;

type

  TTappyTuxAnimation = class
    CurrentStep: Integer;
    FinalStep: Integer;
    constructor Create;
    procedure DrawToIntfImg(AIntfImg: TLazIntfImage); virtual; abstract;
    procedure ExecuteFinal; virtual; abstract;
  end;

  { TFireAnimation }

  TFireAnimation = class(TTappyTuxAnimation)
  public
    procedure DrawToIntfImg(AIntfImg: TLazIntfImage); override;
    procedure ExecuteFinal; override;
  end;

  { TTappyTuxDrawer }

  TTappyTuxDrawer = class(TCustomControl)
  private
    imgSomething: TPortableNetworkGraphic;
    FAnimationList: TFPList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    procedure DrawImageWithTransparentColor(
      ADest: TLazIntfImage; const ADestX, ADestY: Integer; AColor: TFPColor;
      AImage: TFPImageBitmap);
    //function GetImage(ATile: TChessTile): TPortableNetworkGraphic;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure HandleOnTimer(Sender: TObject);
    procedure AddAnimation(AAnimation: TTappyTuxAnimation);
  end;

var
  vTappyTuxDrawer: TTappyTuxDrawer;

implementation

{ TFireAnimation }

procedure TFireAnimation.DrawToIntfImg(AIntfImg: TLazIntfImage);
var
  lTileBmp: TPortableNetworkGraphic;
  X, Y, SourceX, SourceY, DestX, DestY: integer;
  dx, dy: Integer;
  t: Double;
  //lTile: TChessTile;
begin
{  // Draw the moving tile
  //WriteLn(Format('[TChessMoveAnimation.DrawToIntfImg] Afrom=%d,%d', [AFrom.X, AFrom.Y]));
  lTile := vChessGame.Board[AFrom.X][AFrom.Y];
  lTileBmp := vChessDrawer.GetChessTileImage(lTile);
  if lTileBmp = nil then Exit;

  SourceX := (AFrom.X - 1) * INT_CHESSTILE_SIZE;
  SourceY := (8 - AFrom.Y) * INT_CHESSTILE_SIZE;
  DestX := (ATo.X - 1) * INT_CHESSTILE_SIZE;
  DestY := (8 - ATo.Y) * INT_CHESSTILE_SIZE;
  t := CurrentStep / FinalStep;
  X := Round(t * DestX + (1-t) * SourceX);
  Y := Round(t * DestY + (1-t) * SourceY);

  vChessDrawer.DrawImageWithTransparentColor(AIntfImg, X, Y, FPCOLOR_TRANSPARENT_TILE, lTileBmp);}
end;

procedure TFireAnimation.ExecuteFinal;
begin
  //vChessGame.MovePiece(AFrom, ATo);
end;

{ TTappyTuxAnimation }

constructor TTappyTuxAnimation.Create;
begin
  inherited Create;

  CurrentStep := 0;
  FinalStep := 20;
end;

constructor TTappyTuxDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

{  imgBoard := TPortableNetworkGraphic.Create;
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
  imgBKing := TPortableNetworkGraphic.Create;}

  // Events
  OnMouseMove := @HandleMouseMove;
  OnMouseUp := @HandleMouseUp;
  OnMouseDown := @HandleMouseDown;
end;

procedure TTappyTuxDrawer.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TTappyTuxDrawer.Paint;
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

procedure TTappyTuxDrawer.DrawToCanvas(ACanvas: TCanvas);
var
  col, row: integer;
  lIntfImage: TLazIntfImage;
  lTmpBmp: TBitmap;
  X, Y: integer;
begin
  lIntfImage := TLazIntfImage.Create(0, 0);
  lTmpBmp := TBitmap.Create;
  try
    // First draw the background
    lIntfImage.LoadFromBitmap(GetCurrentModule().GetBackgroundImage(2).Handle, 0{bmpBoard.MaskHandle});

    // Now the module should draw itself

    // Draw all animations

{    // Now all pieces
    for col := 1 to 8 do
      for row := 1 to 8 do
      begin
        // Check if the animation wants us to skip drawing this piece
        if Assigned(FAnimation) and FAnimation.SkipDrawingPiece(col, row) then Continue;

        lTileBmp := GetChessTileImage(vChessGame.Board[col][row]);
        if lTileBmp = nil then Continue;

        X := (col - 1) * INT_CHESSTILE_SIZE;
        Y := (8 - row) * INT_CHESSTILE_SIZE;

        DrawImageWithTransparentColor(lIntfImage, X, Y, FPCOLOR_TRANSPARENT_TILE, lTileBmp);
      end;

    // Now animations
    if Assigned(FAnimation) then FAnimation.DrawToIntfImg(lIntfImage);}

    lTmpBmp.LoadFromIntfImage(lIntfImage);
    ACanvas.Draw(0, 0, lTmpBmp);
  finally
    lTmpBmp.Free;
    lIntfImage.Free;
  end;
end;

procedure TTappyTuxDrawer.DrawImageWithTransparentColor(ADest: TLazIntfImage;
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

{function TTappyTuxDrawer.GetChessTileImage(ATile: TChessTile): TPortableNetworkGraphic;
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
end;}

procedure TTappyTuxDrawer.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
{  if Assigned(FDelegate) and (FDrawerState in [dsIdle, dsDragging]) then
    FDelegate.HandleMouseMove(Sender, Shift, X, Y);}
end;

procedure TTappyTuxDrawer.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{  if Assigned(FDelegate) and (FDrawerState in [dsIdle, dsDragging]) then
    FDelegate.HandleMouseUp(Sender, Button, Shift, X, Y);}
end;

procedure TTappyTuxDrawer.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{  if Assigned(FDelegate) and (FDrawerState in [dsIdle, dsDragging]) then
    FDelegate.HandleMouseDown(Sender, Button, Shift, X, Y);}
end;

procedure TTappyTuxDrawer.HandleOnTimer(Sender: TObject);
begin
{  if FDrawerState = dsRunningAnimation then
  begin
    Inc(FAnimation.CurrentStep);
    if FAnimation.CurrentStep >= FAnimation.FinalStep then
    begin
      FAnimation.ExecuteFinal;
      FAnimation.Free;
      FAnimation := nil;
      FDrawerState := dsIdle;
    end;
    Invalidate();
  end;}
end;

procedure TTappyTuxDrawer.AddAnimation(AAnimation: TTappyTuxAnimation);
begin
{  FDrawerState := dsRunningAnimation;
  FAnimation := AAnimation;}
end;

end.
