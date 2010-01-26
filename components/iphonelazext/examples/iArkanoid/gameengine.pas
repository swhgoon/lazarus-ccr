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
  Classes, SysUtils;

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
    health  : Integer;
    flash   : Integer;
  end;

  { TArkanoid }

  TArkanoid = class(TObject)
  private
    fPaddleX  : single;
  protected
    procedure BallMove(var move: TGameItem; atime: single);
    procedure ResetPaddle;
    procedure SetPaddleX(const AValue: single);
  public
    bricks      : array [0..99] of TBrick;
    brickCount  : Integer;
    balls       : array [0..9] of TGameItem;
    ballcount   : Integer;
    width       : Integer;
    height      : Integer;
    PaddleY     : single;
    PaddleW     : single;

    constructor Create(awidth, aheight: Integer);
    procedure InitBricks;
    procedure Init;
    procedure Move;
    function AnySticked: Boolean;
    procedure LaunchSticked;
    property PaddleX: single read fPaddleX write SetPaddleX;
  end;

type
  TMoveRecord = packed record
    sx, sy  : single;
    dx, dy  : single;
    k    : single;   // y = kx + b
    divk : single;
    b    : single;
  end;

  THitSide = (hs_LeftRight, hs_TopBottom);
  THitSides = set of THitSide;

procedure InitMoveRecord(sx, sy, dx, dy: single; var r: TMoveRecord);
function HitRect(const r: TRect; const mr: TMoveRecord; var hitpoint: TPoint; var HitSides: THitSides): Boolean;

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

procedure InitMoveRecord(sx, sy, dx, dy: single; var r: TMoveRecord);
var
  diff : single;
begin
  r.sx:=sx; 
  r.sy:=sy;
  r.dx:=dx; 
  r.dy:=dy;
  diff:=dx-sx;
  
  if diff<>0 then begin
    r.k:=(dy-sy) / diff;
    if r.k<>0 then r.divk:=1/r.k
    else r.divk:=0;
  end else begin
    r.k:=0;
    r.divk:=0;
  end;
  r.b:=sy-sx*r.k;
end;

function InRange(p, r1,r2: Integer): Boolean; overload; inline;
begin
  if r1<r2 then Result:=(p>=r1) and (p<r2)
  else Result:=(p>=r2) and (p<r1)
end;

function InRange(p: Integer; r1,r2: single): Boolean; overload; inline;
begin
  if r1<r2 then Result:=(p>=r1) and (p<r2)
  else Result:=(p>=r2) and (p<r1)
end;

function HitRect(const r: TRect; const mr: TMoveRecord; var hitpoint: TPoint; var HitSides: THitSides): Boolean;
var
  c, ch     : Integer;
  hitside : Boolean;
  hittop  : Boolean;
begin
  HitSides:=[];

  with r, mr do begin
    if sx<=dx then begin
      c:=Left;
    end else begin
      c:=Right;
    end;

    ch:=Round(k*c+b);
    hitside:=inRange(c, sx, dx) and (ch >= Top) and (ch < Bottom);
    if hitside then begin
      hitpoint:=Point(c, ch);
      Include(HitSides, hs_LeftRight)
    end;

    if sy<=dy then begin
      c:=Top 
    end else begin
      c:=Bottom;
    end;
    
    if k <> 0 then 
      ch:=Round((c-b)/k) 
    else 
      ch:=Round(sx);

    hittop:=inRange(c, sy, dy) and (ch>=Left) and (ch<Right);
    if hittop then begin
      hitpoint:=Point(ch, c);
      Include(HitSides, hs_TopBottom)
    end;
  end;
  
  Result:=hittop or hitside;
end;

function SqrLen(const p1, p2: TPoint): single;
var
  x,y:single;
begin
  x:=p1.x-p2.x;
  y:=p1.y-p2.y;
  Result:=x*x+y*y;
end;

procedure TArkanoid.BallMove(var move: TGameItem; atime: single);
var
  cx, cy  : single;
  p       : TPoint;
  i       : Integer;

  t       : single;
  hitidx  : Integer;
  hsides    : THitSides;
  hitsides  : THitSides;
  hlen    : single;
  hitlen  : single;
  hpnt    : TPoint;
  hitpnt  : TPoint;
  mr      : TMoveRecord;
  br      : TRect;
begin
  with move do begin
    if sticked then Exit;

    cx := x + dx * atime;
    cy := y + dy * atime;
    if cy < 0 then begin
      cy := 0;
      dy := -dy;
    end;
    InitMoveRecord(x, y, cx, cy, mr);
    

    if cx < 0 then begin
      cx := 0;
      dx := -dx;
    end else if cx > width then begin
      cx := width;
      dx := -dx;
    end;

    p.x := Round(x);
    p.y := Round(y);

    hitidx:=-1;
    hitlen:=0;
    for i := 0 to brickCount - 1 do  begin
      if (bricks[i].health > 0) and HitRect(bricks[i].bounds, mr, hpnt, hsides) then begin
        hlen:=SqrLen(hpnt, p);
        if (hitidx<0) or (hlen<hitlen) then begin
          hitidx:=i;
          hitlen:=hlen;
          hitsides:=hsides;
          hitpnt:=hpnt;
        end;
      end;
    end;
    
    if hitidx>=0 then begin
      if hs_TopBottom in hitsides then move.dy:=-move.dy;
      if hs_LeftRight in hitsides then move.dx:=-move.dx;
      bricks[hitidx].flash:=2;
      cx:=hitpnt.x;
      cy:=hitpnt.y;
      dec(bricks[hitidx].health);
    end;

    br:=Bounds( Round(PaddleX), Round(PaddleY), Round(PaddleW), 2);
    if HitRect(br, mr, hpnt, hsides) then begin
      t := (hpnt.x - PaddleX) / PaddleW;
      t:=pi*(1-(t*0.8+0.1));
      dx := cos(t)*ballVelocity;
      dy := -sin(t)*ballVelocity;
      cy := hpnt.y;
      cx := hpnt.x;
    end else if cy > height then begin
      ResetPaddle;
      Exit;
    end;
    x:=cx;
    y:=cy;
  end;
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

