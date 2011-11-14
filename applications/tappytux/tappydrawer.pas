unit tappydrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, IntfGraphics, fpimage,
  Math,
  {tappygamedata,} tappyconfig, tappymodules;

type

  { TTappyTuxAnimation }

  TTappyTuxAnimation = class
    CurrentStep: Integer;
    StepCount: Integer;
    IsInfinite: Boolean; // if True the animation will never end
    constructor Create; virtual;
    procedure DrawToCanvas(ACanvas: TCanvas); virtual;
    procedure DrawToIntfImg(AIntfImage: TLazIntfImage); virtual;
    procedure ExecuteFinal; virtual;
  end;

  { TTappySpriteAnimation }

  TTappySpriteAnimation = class(TTappyTuxAnimation)
  public
    StartPoint, EndPoint: TPoint;
    Bitmaps: array of TFPImageBitmap;
    procedure DrawToIntfImg(AIntfImage: TLazIntfImage); override;
    procedure ExecuteFinal; override;
  end;

  { TBallonAnimation }

  TBallonAnimation = class(TTappyTuxAnimation)
  public
    constructor Create; override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    procedure ExecuteFinal; override;
  end;

  { TTappyTuxDrawer }

  TTappyTuxDrawer = class(TCustomControl)
  private
    imgSomething: TPortableNetworkGraphic;
    FAnimationList: TFPList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    class procedure DrawImageWithTransparentColor(
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
    function GetAnimation(AIndex: Integer): TTappyTuxAnimation;
    function GetAnimationCount: Integer;
    procedure HandleAnimationOnTimer();
  end;

var
  vTappyTuxDrawer: TTappyTuxDrawer;

implementation

{ TBallonAnimation }

constructor TBallonAnimation.Create;
begin
  inherited Create;

  CurrentStep := 0;
  StepCount := 200;
end;

procedure TBallonAnimation.DrawToCanvas(ACanvas: TCanvas);
begin
  ACanvas.Pixels[CurrentStep, CurrentStep] := clRed;
end;

procedure TBallonAnimation.ExecuteFinal;
begin
  // Lost the game if the ballon reached its end
end;

{ TTappySpriteAnimation }

procedure TTappySpriteAnimation.DrawToIntfImg(AIntfImage: TLazIntfImage);
var
  lNumBitmaps, lCurBmpIndex: Integer;
  t: Double;
  lPos: TPoint;
begin
  lNumBitmaps := Length(Bitmaps);
  if lNumBitmaps = 0 then Exit;

  lCurBmpIndex := CurrentStep mod lNumBitmaps;

  t := CurrentStep / StepCount;
  lPos.X := Round(StartPoint.X + t * (EndPoint.X - StartPoint.X));
  lPos.Y := Round(StartPoint.Y + t * (EndPoint.Y - StartPoint.Y));

  TTappyTuxDrawer.DrawImageWithTransparentColor(AIntfImage,
   lPos.X, lPos.Y, colFuchsia, Bitmaps[lCurBmpIndex]);
end;

procedure TTappySpriteAnimation.ExecuteFinal;
begin
  inherited ExecuteFinal;
end;

{ TTappyTuxAnimation }

constructor TTappyTuxAnimation.Create;
begin
  inherited Create;

  CurrentStep := 0;
  StepCount := 20;
end;

procedure TTappyTuxAnimation.DrawToCanvas(ACanvas: TCanvas);
begin

end;

procedure TTappyTuxAnimation.DrawToIntfImg(AIntfImage: TLazIntfImage);
begin

end;

procedure TTappyTuxAnimation.ExecuteFinal;
begin
  // inherit from this class and add something to ExecuteFinal
end;

constructor TTappyTuxDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAnimationList := TFPList.Create;

  // Events
  OnMouseMove := @HandleMouseMove;
  OnMouseUp := @HandleMouseUp;
  OnMouseDown := @HandleMouseDown;
end;

destructor TTappyTuxDrawer.Destroy;
begin
  FAnimationList.Free;

  inherited Destroy;
end;

procedure TTappyTuxDrawer.EraseBackground(DC: HDC);
begin
  // Don't erase the background
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
end;

procedure TTappyTuxDrawer.DrawToCanvas(ACanvas: TCanvas);
var
  col, row: integer;
  lIntfImage: TLazIntfImage;
  lTmpBmp: TBitmap;
  X, Y: integer;
  i: Integer;
  lAnimation: TTappyTuxAnimation;
begin
  lIntfImage := TLazIntfImage.Create(0, 0);
  lTmpBmp := TBitmap.Create;
  try
    // First draw the background
    lIntfImage.LoadFromBitmap(GetCurrentModule().GetBackgroundImage(2).Handle, 0{bmpBoard.MaskHandle});

    // Draw all animations via TLazIntfImage
    for i := 0 to FAnimationList.Count - 1 do
    begin
      lAnimation := TTappyTuxAnimation(FAnimationList.Items[i]);
      lAnimation.DrawToIntfImg(lIntfImage);
    end;

    lTmpBmp.LoadFromIntfImage(lIntfImage);
    ACanvas.Draw(0, 0, lTmpBmp);

    // -------------------------
    // Now TCanvas drawings
    // -------------------------

    // Now the module should draw itself

    // Draw all animations via TLazIntfImage
    for i := 0 to FAnimationList.Count - 1 do
    begin
      lAnimation := TTappyTuxAnimation(FAnimationList.Items[i]);
      lAnimation.DrawToCanvas(ACanvas);
    end;

  finally
    lTmpBmp.Free;
    lIntfImage.Free;
  end;
end;

class procedure TTappyTuxDrawer.DrawImageWithTransparentColor(ADest: TLazIntfImage;
  const ADestX, ADestY: Integer; AColor: TFPColor; AImage: TFPImageBitmap);
var
  x, y, CurX, CurY: Integer;
  IntfImage: TLazIntfImage;
  lDrawWidth, lDrawHeight: Integer;
  CurColor: TFPColor;
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

        CurColor := AImage.Canvas.Colors[x, y]; // Good for debugging
        if ((CurColor.Green div $FF) <> (AColor.Green div $FF)) or
          ((CurColor.Red div $FF) <> (AColor.Red div $FF)) or
          ((CurColor.Blue div $FF) <> (AColor.Blue div $FF)) then
          ADest.Colors[CurX, CurY] := IntfImage.Colors[x, y];
      end;
    end;
  finally
    IntfImage.Free;
  end;
end;

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
  FAnimationList.Add(AAnimation);
end;

function TTappyTuxDrawer.GetAnimation(AIndex: Integer): TTappyTuxAnimation;
begin
  Result := TTappyTuxAnimation(FAnimationList.Items[AIndex]);
end;

function TTappyTuxDrawer.GetAnimationCount: Integer;
begin
  Result := FAnimationList.Count;
end;

procedure TTappyTuxDrawer.HandleAnimationOnTimer;
var
  i: Integer;
  lAnimation: TTappyTuxAnimation;
begin

  for i := 0 to FAnimationList.Count - 1 do
  begin
    lAnimation := TTappyTuxAnimation(FAnimationList.Items[i]);
    Inc(lAnimation.CurrentStep);
  end;

  Self.Invalidate;
end;

end.
