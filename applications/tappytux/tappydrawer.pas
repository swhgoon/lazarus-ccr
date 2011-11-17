unit tappydrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, IntfGraphics, fpimage,
  Math, LCLIntf,
  tappyconfig, tappymodules;

type

  { TTappyTuxAnimation }

  TTappyTuxAnimation = class
  public
    StartPoint, EndPoint: TPoint;
    Position: TPoint;
    CurrentStep: Integer;
    StepCount: Integer; // In miliseconds
    IsInfinite: Boolean; // if True the animation will never end
    Stopped: Boolean;
    // User data
    UserData: TObject;
    UserPosition: TPoint;
    constructor Create; virtual;
    procedure DrawToIntfImg(AIntfImage: TLazIntfImage); virtual;
    procedure DrawToCanvas(ACanvas: TCanvas); virtual;
    procedure CalculatePosition;
    procedure ExecuteFinal; virtual;
  end;

  { TTappySpriteAnimation }

  TTappySpriteAnimation = class(TTappyTuxAnimation)
  public
    Images: array of TLazIntfImage;
    SpriteChangeInterval: Integer;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawToIntfImg(AIntfImage: TLazIntfImage); override;
    procedure ExecuteFinal; override;
    procedure LoadImageFromPng(AIndex: Integer; APath: string);
  end;

  { TFallingText }
  TFallingText = class(TTappyTuxAnimation)
  public
    Caption: String;
    Value: Integer;
    ProcessOnEnd: Boolean;
    Image: TLazIntfImage;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawToIntfImg(AIntfImage: TLazIntfImage); override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    procedure ExecuteFinal; override;
    procedure LoadImageFromPng(APath: string);
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
      AImage: TLazIntfImage);
    class function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
    //function GetImage(ATile: TChessTile): TPortableNetworkGraphic;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
    procedure HandleOnTimer(Sender: TObject);
    function AddAnimation(AAnimation: TTappyTuxAnimation): Integer;
    function GetAnimation(AIndex: Integer): TTappyTuxAnimation;
    function GetAnimationIndex(AAnimation: TTappyTuxAnimation): Integer;
    function GetAnimationCount: Integer;
    procedure RemoveAnimation(AIndex: Integer);
    procedure HandleAnimationOnTimer(AInterval: Integer);
  end;

var
  vTappyTuxDrawer: TTappyTuxDrawer;

implementation

{ TTappySpriteAnimation }

constructor TTappySpriteAnimation.Create;
begin
  inherited Create;
  SpriteChangeInterval := 1000;
end;

destructor TTappySpriteAnimation.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(Images)-1 do
    if Assigned(Images[i]) then Images[i].Free;
  inherited Destroy;
end;

procedure TTappySpriteAnimation.DrawToIntfImg(AIntfImage: TLazIntfImage);
var
  lNumBitmaps, lCurBmpIndex: Integer;
begin
  lNumBitmaps := Length(Images);
  if lNumBitmaps = 0 then Exit;

  lCurBmpIndex := (CurrentStep div SpriteChangeInterval) mod lNumBitmaps;

  TTappyTuxDrawer.DrawImageWithTransparentColor(AIntfImage,
   Position.X, Position.Y, colFuchsia, Images[lCurBmpIndex]);
end;

procedure TTappySpriteAnimation.ExecuteFinal;
begin
  GetCurrentModule().ProcessSpriteEnd(UserData, UserPosition);
end;

procedure TTappySpriteAnimation.LoadImageFromPng(AIndex: Integer; APath: string);
var
  lBitmap: TPortableNetworkGraphic;
begin
  lBitmap := TPortableNetworkGraphic.Create;
  try
    lBitmap.LoadFromFile(APath);
    Images[AIndex] := TLazIntfImage.Create(0, 0);
    Images[AIndex].LoadFromBitmap(lBitmap.Handle, 0);
  finally
    lBitmap.Free;
  end;
end;

{TFallingText}

constructor TFallingText.Create;
begin
  inherited Create;
  ProcessOnEnd := True;
end;

destructor TFallingText.Destroy;
begin
  if Assigned(Image) then Image.Free;
  inherited Destroy;
end;

procedure TFallingText.DrawToIntfImg(AIntfImage: TLazIntfImage);
begin
  TTappyTuxDrawer.DrawImageWithTransparentColor(AIntfImage,
   Position.X, Position.Y, colFuchsia, Image);
end;

procedure TFallingText.DrawToCanvas(ACanvas: TCanvas);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Rectangle(Position.X + 25, Position.Y + 45,
    Position.X + 30 + ACanvas.TextWidth(Caption)+5,
    Position.Y + 50 + ACanvas.TextHeight(Caption)+5);
  ACanvas.TextOut(Position.X + 30, Position.Y + 50, caption);
end;

procedure TFallingText.ExecuteFinal;
var
  snowmanWrong: TFallingText;
begin
  if ProcessOnEnd then
  begin
    snowmanWrong := TFallingText.Create;
    snowmanWrong.IsInfinite := False;
    snowmanWrong.StartPoint := Position;
    snowmanWrong.EndPoint := Position;
    snowmanWrong.Position := Position;
    snowmanWrong.caption:= 'Oh-oh!';
    snowmanWrong.ProcessOnEnd := False;
    snowmanWrong.StepCount := 2000;
    snowmanWrong.LoadImageFromPng(vTappyTuxConfig.GetResourcesDir() + 'images' + PathDelim + 'sprites' + PathDelim + 'snowmanwrong.png');
    vTappyTuxDrawer.AddAnimation(snowmanWrong);

    GetCurrentModule().ProcessFallingTextEnd();
  end;
end;

procedure TFallingText.LoadImageFromPng(APath: string);
var
  lBitmap: TPortableNetworkGraphic;
begin
  lBitmap := TPortableNetworkGraphic.Create;
  try
    lBitmap.LoadFromFile(APath);
    Image := TLazIntfImage.Create(0, 0);
    Image.LoadFromBitmap(lBitmap.Handle, 0);
  finally
    lBitmap.Free;
  end;
end;


{ TTappyTuxAnimation }

constructor TTappyTuxAnimation.Create;
begin
  inherited Create;

  CurrentStep := 0;
  StepCount := 20000;
end;

procedure TTappyTuxAnimation.DrawToIntfImg(AIntfImage: TLazIntfImage);
begin

end;

procedure TTappyTuxAnimation.DrawToCanvas(ACanvas: TCanvas);
begin

end;

procedure TTappyTuxAnimation.CalculatePosition;
var
  t: Double;
begin
  t := CurrentStep / StepCount;
  Position.X := Round(StartPoint.X + t * (EndPoint.X - StartPoint.X));
  Position.Y := Round(StartPoint.Y + t * (EndPoint.Y - StartPoint.Y));
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
  lStartTime, lAnimTime, lTimeDiff: TDateTime;
begin
  {$IFDEF TAPPY_PROFILER}
  lStartTime := Now;
  {$ENDIF}
  lIntfImage := TLazIntfImage.Create(0, 0);
  lTmpBmp := TBitmap.Create;
  try
    // First draw the background
    lIntfImage.LoadFromBitmap(GetCurrentModule().GetBackgroundImage(2).Handle, 0{bmpBoard.MaskHandle});

    // Draw all animations via TLazIntfImage
    for i := 0 to FAnimationList.Count - 1 do
    begin
      lAnimation := TTappyTuxAnimation(FAnimationList.Items[i]);
      lAnimation.CalculatePosition();
      {$IFDEF TAPPY_PROFILER}
      lAnimTime := Now;
      {$ENDIF}
      lAnimation.DrawToIntfImg(lIntfImage);
      {$IFDEF TAPPY_PROFILER}
      lTimeDiff := Now - lAnimTime;
      WriteLn(Format('[TTappyTuxDrawer.DrawToCanvas] %s %d DrawToIntfImage Performance: %7d ms', [lAnimation.ClassName, i, DateTimeToMilliseconds(lTimeDiff)]));
      {$ENDIF}
    end;

    lTmpBmp.LoadFromIntfImage(lIntfImage);
    ACanvas.Draw(0, 0, lTmpBmp);

    // -------------------------
    // Now TCanvas drawings
    // -------------------------

    // Second pass of animation drawings, now draw via TCanvas for using fonts
    for i := 0 to FAnimationList.Count - 1 do
    begin
      lAnimation := TTappyTuxAnimation(FAnimationList.Items[i]);
      lAnimation.DrawToCanvas(ACanvas);
    end;

  finally
    lTmpBmp.Free;
    lIntfImage.Free;
  end;
  {$IFDEF TAPPY_PROFILER}
  lTimeDiff := Now - lStartTime;
  WriteLn(Format('[TTappyTuxDrawer.DrawToCanvas] Performance: %7d ms', [DateTimeToMilliseconds(lTimeDiff)]));
  {$ENDIF}
end;

class procedure TTappyTuxDrawer.DrawImageWithTransparentColor(ADest: TLazIntfImage;
  const ADestX, ADestY: Integer; AColor: TFPColor; AImage: TLazIntfImage);
var
  x, y, CurX, CurY: Integer;
  lDrawWidth, lDrawHeight: Integer;
  CurColor: TFPColor;
begin
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

      CurColor := AImage.Colors[x, y];
      if (AColor.Green <> CurColor.Green) or (AColor.Red <> CurColor.Red)
       or (AColor.Blue <> CurColor.Blue) then
        ADest.Colors[CurX, CurY] := CurColor;
    end;
  end;
end;

class function TTappyTuxDrawer.DateTimeToMilliseconds(aDateTime: TDateTime
  ): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
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

function TTappyTuxDrawer.AddAnimation(AAnimation: TTappyTuxAnimation): Integer;
begin
  Result := FAnimationList.Add(AAnimation);
end;

function TTappyTuxDrawer.GetAnimation(AIndex: Integer): TTappyTuxAnimation;
begin
  Result := TTappyTuxAnimation(FAnimationList.Items[AIndex]);
end;

function TTappyTuxDrawer.GetAnimationIndex(AAnimation: TTappyTuxAnimation
  ): Integer;
begin
  Result := FAnimationList.IndexOf(AAnimation);
end;

function TTappyTuxDrawer.GetAnimationCount: Integer;
begin
  Result := FAnimationList.Count;
end;

procedure TTappyTuxDrawer.RemoveAnimation(AIndex: Integer);
begin
  FAnimationList.Delete(AIndex);
end;

procedure TTappyTuxDrawer.HandleAnimationOnTimer(AInterval: Integer);
var
  i: Integer;
  lAnimation: TTappyTuxAnimation;
begin
  i := 0;
  while i < FAnimationList.Count do
  begin
    lAnimation := GetAnimation(i);
    if (not lAnimation.Stopped) then Inc(lAnimation.CurrentStep, AInterval);
    if (not lAnimation.IsInfinite) and (lAnimation.CurrentStep >= lAnimation.StepCount)
      and (not lAnimation.Stopped) then
    begin
      lAnimation.ExecuteFinal();
      RemoveAnimation(i);
      Continue;
    end;

    Inc(i);
  end;

  Self.Invalidate;
end;

end.
