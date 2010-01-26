// iPhone Arkanoid test game
//
// Author: Dmitry 'skalogryz' Boyarintsev
//
// You're free to use this unit in anyway you want

program iArk;

{$mode objfpc}{$h+}
{$modeswitch objectivec1}

uses
  Types, SysUtils, CGContext, CGGeometry, iPhoneAll, gameengine;

type
  // should conform to UIApplicationDelegateProtocol

  { MyAppDelegate }

  MyAppDelegate = objcclass(NSObject)
    procedure applicationDidFinishLaunching_(app: UIApplication); message 'applicationDidFinishLaunching:';
    procedure applicationWillTerminate_(application: UIApplication); message 'applicationWillTerminate:';
  end; 
  
  { MyMainWindow }

  MyMainWindow  = objcclass(UIWindow)
  private
    timer   : NSTimer;
    clr     : Integer;
    game    : TArkanoid;
    canmove : Boolean;
    tx, ty  : Integer;
    px      : Integer;
  protected
    procedure timerEvent_(Sender: NSTimer); message 'timerEvent:';

  public
    procedure touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesEnded_withEvent(touches: NSSetPointer; event: UIEvent); override;
    procedure touchesCancelled_withEvent(touches: NSSetPointer; event: UIEvent); override;

    procedure initObjects; message 'initObjects';
    procedure releaseObjects; message 'releaseObjects';
    procedure drawRect(c: CGRect); override;
  end;

var
  mainWin     : MyMainWindow = nil;
  
const
  PaddleHeight = 10;
  GameYOffset  = 50;

procedure MakeCGRect(const r: TRect; var cg: CGRect); inline;
begin
  cg.origin.x := r.Left;
  cg.origin.y := r.Top;
  cg.size.width := r.Right - r.Left;
  cg.size.height := r.Bottom - r.Top;
end;

procedure DrawGame(game: TArkanoid; ctx: CGContextRef);
var
  i : Integer;
  r : CGRect;
const
  ballsz  = 10;
  ballsz2 = ballsz div 2;
begin
  {drawing balls}
  r.size.width := ballsz;
  r.size.height := ballsz;
  CGContextSetRGBFillColor(ctx, 1, 1, 1, 1);
  CGContextSetRGBStrokeColor(ctx, 0.0, 0.8, 0.8, 1);
  for i := 0 to game.ballcount - 1 do begin
    with game.Balls[i] do begin
      r.origin.x := x - ballsz2;
      r.origin.y := y - ballsz2;
    end;
    CGContextFillEllipseInRect(ctx, r);
  end;

  {drawing players paddle}
  r.origin.x := game.PaddleX;
  r.origin.y := game.PaddleY;
  r.size.width := game.PaddleW;
  r.size.height := PaddleHeight;
  CGContextSetRGBFillColor(ctx, 0.0, 1, 0.2, 1);
  CGContextFillRect(ctx, r);

  for i := 0 to game.brickCount - 1 do begin
    MakeCGRect(game.bricks[i].bounds, r);
    if game.bricks[i].health > 0 then begin
      MakeCGRect(game.bricks[i].bounds, r);
      CGContextSetRGBFillColor(ctx, 1, 0, 0, 1);
      CGContextFillRect(ctx, r);
      r.origin.x := r.origin.x+2;
      r.origin.y := r.origin.y+2;
      r.size.width := r.size.width-2;
      r.size.Height := r.size.height-2;
      CGContextSetRGBFillColor(ctx, 0.8, 0, 0, 1);
      CGContextFillRect(ctx, r);
    end;
    if game.bricks[i].flash > 0 then begin
      CGContextSetRGBFillColor(ctx, 1, 1, 1, 1/(game.bricks[i].flash+1));
      CGContextFillRect(ctx, r);
      dec(game.bricks[i].flash);
    end;
  end;
end;

{ timer event, called every 0.1 second }
procedure MyMainWindow.timerEvent_(Sender: NSTimer);
begin
  clr := Random($FFFFFF+1);
  game.Move;

  setNeedsDisplay;
end;

{ initilize game object and timers }
procedure MyMainWindow.initObjects;
begin
  { create timer }
  timer := NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    double(0.075), Self, objcselector(timerEvent_), nil, true);
  game := TArkanoid.Create(300, 400);
  game.Init;
end;

procedure MyMainWindow.releaseObjects;
begin
  if not Assigned(game) then Exit;
  timer.invalidate;
  game.Free;
  game:=nil;
end;


function GetTouchCoord(touches: NSSetPointer; Window: UIWindow; var x, y: single): Boolean;
var
  st    : NSSet;
  touch : UITouch;
  p     : CGPoint;
begin
  Result := Assigned(touches);
  if not Result then Exit;
  st := NSSet(touches);
  Result := st.count = 1;
  if not Result then Exit;

  touch := UITouch(st.anyObject);
  p := touch.locationInView(Window);
  x := p.x;
  y := p.y;
end;


function isCoordNearPabble(game: TArkanoid; x, y: single; Border: Integer): Boolean;
var
  p     : TPoint;
  r     : TRect;
  px,py : Integer;
  pw    : Integer;
begin
  p.x := Round(x);
  p.y := Round(y);
  px:=Round(game.PaddleX)-Border;
  pw:=Round(game.PaddleW)+Border*2;
  py:=Round(game.PaddleY)-Border;
  r:=Rect(px, py, px+pw, Round(UIScreen.mainScreen.bounds.size.height));
  Result:=PtInRect(r, p);
end;

procedure MyMainWindow.touchesBegan_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  canmove := isCoordNearPabble(game, x, y-GameYOffset, 20);
  if canmove then px := Round(x);
  tx:=round(x);
  ty:=round(y);
end;

procedure MyMainWindow.touchesMoved_withEvent(touches: NSSetPointer; event: UIEvent);
var
  x, y : single;
begin
  if not GetTouchCoord(touches, self, x, y) then Exit;
  tx:=round(x);
  ty:=round(y);
  if canmove then begin
    game.PaddleX := game.PaddleX + Round(x) - px;
    px := Round(x);
  end;
  inherited touchesMoved_withEvent(touches, event);
end;

procedure MyMainWindow.touchesEnded_withEvent(touches: NSSetPointer;
  event: UIEvent);
begin
  canmove := false;

  game.LaunchSticked;
  inherited touchesEnded_withEvent(touches, event);
end;

procedure MyMainWindow.touchesCancelled_withEvent(touches: NSSetPointer;
  event: UIEvent);
begin
  canmove := false;
  inherited touchesCancelled_withEvent(touches, event);
end;

procedure MyMainWindow.drawRect(c: CGRect);
var
  ctx : CGContextRef;
  txt : string;
begin
  if not Assigned(game) then Exit;

  ctx := UIGraphicsGetCurrentContext;
  if not canmove then
    CGContextSetRGBFillColor(ctx, 0, 0.0, 0.25, 1)
  else
    CGContextSetRGBFillColor(ctx, 0, 0.25, 0.0, 1);
  UIRectFill(c);


  CGContextScaleCTM(ctx, 1, -1);
  CGContextTranslateCTM(ctx, 0, -c.size.height);

  CGContextSetRGBFillColor(ctx, 1, 1, 0, 1);
  CGContextSetRGBStrokeColor(ctx, 1, 1, 0, 1);

  CGContextSelectFont(ctx, 'Helvetica', 20, kCGEncodingMacRoman);
  if Assigned(game) then begin
    txt := Format('touch: %d %d; paddle: %d %d', [tx,ty, Round(game.PaddleX), Round(game.PaddleY)]);
    CGContextShowTextAtPoint(ctx, 10, c.size.height - 40, PChar(txt), length(txt));
  end;

  CGContextTranslateCTM(ctx, 0, c.size.height);
  CGContextScaleCTM(ctx, 1, -1);

  CGContextTranslateCTM(ctx, 0, GameYOffset);
  DrawGame(game, ctx);
  CGContextTranslateCTM(ctx, 0, -GameYOffset);
end;

procedure MyAppDelegate.applicationDidFinishLaunching_(app: UIApplication); 
var
  screen  : UIScreen;
  r       : CGRect;
begin
  screen := UIScreen.mainScreen;
  if Assigned(screen) then begin
    r := screen.bounds;
    if not Assigned(mainWin) then begin
      mainWin := MyMainWindow(MyMainWindow.alloc).initWithFrame(r);
      {initilize game object and timers}
      mainWin.initObjects;
    end;
    mainWin.makeKeyAndVisible;
  end;
end;

procedure MyAppDelegate.applicationWillTerminate_(application: UIApplication);
begin
  mainWin.releaseObjects;
end;

var
  pool    : NSAutoreleasePool;
  retVal  : Integer;

begin
  randomize;
  pool := NSAutoreleasePool(NSAutoreleasePool.alloc).init;

  retVal := UIApplicationMain(argc, argv, nil, NSStr('MyAppDelegate'));

  pool.release;
  ExitCode := retVal;
end.

