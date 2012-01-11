unit nvTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { Point }

  Point = object
    x, y: integer;

    constructor Point;
    constructor Point(ix, iy: integer);
    constructor Point(const p: Point);
  end;

function SetPoint(x, y: integer): Point;

type
  { Rect }

  Rect = object
    x, y, w, h: integer;

    constructor Rect;
    constructor Rect(const p: TPoint);
    constructor Rect(ix, iy: integer; iw: integer = 0; ih: integer = 0);
    constructor Rect(const r: Rect);
  end;

function SetRect(x, y: integer): Rect;
function SetRect(x, y, w, h: integer): Rect;

const
  ButtonFlags_Off = $0;
  ButtonFlags_On = $1;
  ButtonFlags_Begin = $2;
  ButtonFlags_End = $4;
  ButtonFlags_Shift = $8;
  ButtonFlags_Alt = $10;
  ButtonFlags_Ctrl = $20;

type
  ButtonState = record
    state: integer;
    time: TDateTime;
    cursor: Point;
  end;

const
  // An enum to identify the mouse buttons
  MouseButton_Left = 1;
  MouseButton_Middle = 2;
  MouseButton_Right = 3;

  // An enum to identify the special key buttons not translated with ASCII codes
  Key_F1 = 128;
  Key_F2 = 129;
  Key_F3 = 130;
  Key_F4 = 131;
  Key_F5 = 132;
  Key_F6 = 133;
  Key_F7 = 134;
  Key_F8 = 135;
  Key_F9 = 136;
  Key_F10 = 137;
  Key_F11 = 138;
  Key_F12 = 139;

  Key_Left = 140;
  Key_Up = 141;
  Key_Right = 142;
  Key_Down = 143;
  Key_PageUp = 144;
  Key_PageDown = 145;
  Key_Home = 146;
  Key_End = 147;
  Key_Insert = 148;

  // The various flags to modify the behavior of the groups

  // Layout behavior flags
  GroupFlags_LayoutNone = $01;
  GroupFlags_LayoutVertical = $02;
  GroupFlags_LayoutHorizontal = $04;
  GroupFlags_LayoutMask = $07;
  GroupFlags_LayoutXMask = $ffff xor GroupFlags_LayoutMask;

  // Alignment flags for the widgets inserted in the group
  GroupFlags_AlignLeft = $10;
  GroupFlags_AlignRight = $20;
  GroupFlags_AlignTop = $40;
  GroupFlags_AlignBottom = $80;
  GroupFlags_AlignMask = $f0;
  GroupFlags_AlignXMask = $ffff xor GroupFlags_AlignMask;

  // Start flags defining the starting origin of the group
  GroupFlags_StartLeft = $100;
  GroupFlags_StartRight = $200;
  GroupFlags_StartTop = $400;
  GroupFlags_StartBottom = $800;
  GroupFlags_StartMask = $f00;
  GroupFlags_StartXMask = $ffff xor GroupFlags_StartMask;

  // Optional flags
  GroupFlags_LayoutForce = $8000;
  GroupFlags_LayoutDefault = $4000;
  GroupFlags_LayoutNoMargin = $2000;
  GroupFlags_LayoutNoSpace = $1000;
  GroupFlags_GrowRightFromBottom = GroupFlags_LayoutHorizontal or GroupFlags_StartLeft or GroupFlags_AlignLeft or GroupFlags_StartBottom or GroupFlags_AlignBottom;

  // Predefined configurations
  GroupFlags_GrowRightFromTop = GroupFlags_LayoutHorizontal or GroupFlags_StartLeft or GroupFlags_AlignLeft or GroupFlags_StartTop or GroupFlags_AlignTop;
  GroupFlags_GrowLeftFromBottom = GroupFlags_LayoutHorizontal or GroupFlags_StartRight or GroupFlags_AlignRight or GroupFlags_StartBottom or GroupFlags_AlignBottom;
  GroupFlags_GrowLeftFromTop = GroupFlags_LayoutHorizontal or GroupFlags_StartRight or GroupFlags_AlignRight or GroupFlags_StartTop or GroupFlags_AlignTop;
  GroupFlags_GrowUpFromLeft = GroupFlags_LayoutVertical or GroupFlags_StartBottom or GroupFlags_AlignBottom or GroupFlags_StartLeft or GroupFlags_AlignLeft;
  GroupFlags_GrowUpFromRight = GroupFlags_LayoutVertical or GroupFlags_StartBottom or GroupFlags_AlignBottom or GroupFlags_StartRight or GroupFlags_AlignRight;
  GroupFlags_GrowDownFromLeft = GroupFlags_LayoutVertical or GroupFlags_StartTop or GroupFlags_AlignTop or GroupFlags_StartLeft or GroupFlags_AlignLeft;
  GroupFlags_GrowDownFromRight = GroupFlags_LayoutVertical or GroupFlags_StartTop or GroupFlags_AlignTop or GroupFlags_StartRight or GroupFlags_AlignRight;
  GroupFlags_LayoutDefaultFallback = GroupFlags_GrowDownFromLeft;

type
  Group = record
    bounds: Rect;    //anchor point + width and height of the region
    flags: integer;  //group behavior
    margin: integer; //border
    space: integer;  //interior
  end;
  PGroup = ^Group;

implementation

{ Point }

constructor Point.Point;
begin
  x := 0;
  y := 0;
end;

constructor Point.Point(ix, iy: integer);
begin
  x := ix;
  y := iy;
end;

constructor Point.Point(const p: Point);
begin
  x := p.x;
  y := p.y;
end;

{ Rect }

constructor Rect.Rect;
begin
  x := 0;
  y := 0;
  w := 0;
  h := 0;
end;

constructor Rect.Rect(const p: TPoint);
begin
  x := p.x;
  y := p.y;
  w := 0;
  h := 0;
end;

constructor Rect.Rect(ix, iy: integer; iw: integer; ih: integer);
begin
  x := ix;
  y := iy;
  w := iw;
  h := ih;
end;

constructor Rect.Rect(const r: Rect);
begin
  x := r.x;
  y := r.y;
  w := r.w;
  h := r.h;
end;

function SetPoint(x, y: integer): Point;
begin
  Result.Point(x, y);
end;

function SetRect(x, y: integer): Rect;
begin
  Result.Rect(x, y);
end;

function SetRect(x, y, w, h: integer): Rect;
begin
  Result.Rect(x, y, w, h);
end;

end.

