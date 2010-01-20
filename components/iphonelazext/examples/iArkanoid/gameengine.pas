// Arkanoid test game engine.
// developed for iPhone test samples
//
// Author: Dmitry 'skalogryz' Boyarintsev 2009
//
// Arkanoid is an arcade game developed by Taito in 1986
// http://en.wikipedia.org/wiki/Arkanoid
//
// You're free to use this unit in anyway you want

unit gameengine;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils;

const
  ballVelocity = 18;

type
  TGameItem = record
    x,  y  : single; // coodinates
    dx, dy : single; // velocity vector
    sticked: Boolean;
  end;

  TBrick = record
    bounds  : TRect;
    hitRect : TRect;
    health  : Integer;
    flash   : Integer;
  end;

  { TArkanoid }

  TArkanoid = class(TObject)
  private
    fPaddleX  : single;
    procedure SetPaddleX(const AValue: single);
  protected
    procedure BoundsBack(const HitRect: TRect; var dx, dy: single);
    procedure BallMove(var move: TGameItem; atime: single);
    procedure SetPaddleX(AValue: Integer);
    procedure ResetPaddle;
  public
    bricks      : array [0..99] of TBrick;
    brickCount  : Integer;
    balls     : array [0..9] of TGameItem;
    ballcount : Integer;
    width     : Integer;
    height    : Integer;
    PaddleY   : single;
    PaddleW   : single;

    constructor Create(awidth, aheight: Integer);
    procedure InitBricks;
    procedure Init;
    procedure Move;
    function AnySticked: Boolean;
    procedure LaunchSticked;
    property PaddleX: single read fPaddleX write SetPaddleX;
  end;

implementation

{ TArkanoid }

procedure TArkanoid.SetPaddleX(const AValue: single);
var
  dx  : single;
  i   : integer;
begin
  dx := fPaddleX - AValue;
  for i := 0 to ballcount - 1 do begin
    if balls[i].sticked then
      balls[i].x:=balls[i].x-dx;
  end;
  fPaddleX := AValue;
end;

// hacky test
procedure TArkanoid.BoundsBack(const HitRect: TRect; var dx, dy: single);
var
  my  : Integer;
begin
  my := HitRect.Top + (HitRect.Bottom - HitRect.Top) div 2;
  if abs(my - dy)>5 then dy := -dy
  else dx := -dx;
end;

procedure TArkanoid.BallMove(var move: TGameItem; atime: single);
var
  cx, cy  : single;
  p       : TPoint;
  i       : Integer;

  t       : single;
begin
  with move do begin
    if sticked then Exit;

    cx := x + dx * atime;
    cy := y + dy * atime;
    if cy < 0 then begin
      cy := 0;
      dy := -dy;
    end;

    if cx < 0 then begin
      cx := 0;
      dx := -dx;
    end else if cx > width then begin
      cx := width;
      dx := -dx;
    end;

    p.x := Round(cx);
    p.y := Round(cy);

    for i := 0 to brickCount - 1 do begin
      if (bricks[i].health > 0) and PtInRect(bricks[i].hitRect, p) then begin
        BoundsBack(bricks[i].bounds, dx, dy);
        bricks[i].flash:=2;
        dec(bricks[i].health);
      end;
    end;

    if (cy >= PaddleY-5) and (cx >= PaddleX) and (cx <= PaddleX + PaddleW) and (dy>0) then begin
      t := (cx - PaddleX - PaddleW/2) / (PaddleW/2);

      if t > 1 then t := 1
      else if t < -1 then t:=-1;


      if t < 0 then
        t := pi/2 + pi/16 - t / 2
      else
        t := pi/2 - pi/16 - t / 2;
      dx := cos(t)*ballVelocity;
      dy := -sin(t)*ballVelocity;
      cy := PaddleY-5;
    end else if cy > height then begin
      ResetPaddle;
      Exit;
    end;

    x:=cx;
    y:=cy;
  end;
end;

procedure TArkanoid.SetPaddleX(AValue: Integer);
begin

end;

procedure TArkanoid.ResetPaddle;
var
  i : integer;
  a : single;
begin
  PaddleW := width / 5;
  PaddleY := height - 10;
  fPaddleX := width / 2 - PaddleW / 2;
  ballcount := 1;
  for i := 0 to ballcount - 1 do
    with Balls[i] do begin
      {x := random(height-40)+20;
      y := random(width-40)+20;}
      x := PaddleX+PaddleW/2;
      y := PaddleY-5;
      a := random*pi/4 + (pi/2 - pi/8);
      if cos(a)*5 < 0.1 then a := a + pi/5;
      dx := ballVelocity*cos(a);
      dy := ballVelocity*sin(a);
      sticked:=true;
    end;

end;

constructor TArkanoid.Create(awidth, aheight: Integer);
begin
  width := awidth;
  height := aheight;
end;

procedure MakeHitRect(const boundsRect: TRect; var hitRect: TRect);
begin
  hitRect.left := boundsRect.left - 3;
  hitRect.Top  := boundsRect.top - 3;
  hitRect.right := boundsRect.right + 3;
  hitRect.bottom:= boundsRect.bottom+ 3;
end;

procedure TArkanoid.InitBricks;
var
  i    : Integer;
  x, y : integer;
begin
  brickCount := 8*8;
  for i := 0 to brickCount - 1 do begin
    x := (i mod 8) * 40;
    y := (i div 8) * 20 + 40;
    bricks[i].bounds:=Bounds(x,y,40,20);
  end;

  for i:=0 to brickCount - 1 do begin
    bricks[i].health := random(4)+1;
    MakeHitRect(bricks[i].bounds, bricks[i].hitRect);
    bricks[i].flash:=0;
  end;
end;

procedure TArkanoid.Init;
begin
  InitBricks;
  ResetPaddle;
end;

procedure TArkanoid.Move;
var
  i : Integer;
const
  time : single = 1;
begin
  for i := 0 to ballcount - 1 do
    BallMove(balls[i], time);
end;

function TArkanoid.AnySticked: Boolean;
var
  i : Integer;
begin
  Result := false;
  for i := 0 to ballcount - 1 do
    if balls[i].sticked then begin
      Result:=true;
      exit;
    end;
end;

procedure TArkanoid.LaunchSticked;
var
  i : integer;
begin
  for i := 0 to ballcount - 1 do
    if balls[i].sticked then begin
      balls[i].sticked:=false;
      if balls[i].dy > 0 then balls[i].dy := - balls[i].dy;
    end;
end;

end.

