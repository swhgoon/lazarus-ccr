unit AutoPanel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LCLType, ExtCtrls;
type

  TPlacement = packed record
    Left, Top, Width, Height: Integer;
  end;
  
  PIntArray = ^TRectArray;
  TRectArray = array[0..4096] of TPlacement;

  TAutoPanel = class(TPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
    pWidth :Integer;
    pHeight:Integer;
    FAutoChildPosLeft : Boolean;
    FAutoChildPosTop : Boolean;
    FAutoChildWidth : Boolean;
    FAutoChildHeight : Boolean;
    PCtrlsCoordArr:PIntArray;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Resize; override;
  published
    { Published declarations }
    property AutoChildPosLeft : Boolean read FAutoChildPosLeft write FAutoChildPosLeft default False;
    property AutoChildPosTop : Boolean read FAutoChildPosTop write FAutoChildPosTop default False;
    property AutoChildWidth : Boolean  read FAutoChildWidth write FAutoChildWidth default False;
    property AutoChildHeight : Boolean read FAutoChildHeight write FAutoChildHeight default False;

    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Caption;
    property Color;
    property Font;
    //property Locked;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;

    property Anchors;
    property AutoSize;
    //property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property FullRepaint;
    //property ParentBiDiMode;

    //property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
  end;

//procedure Register;

implementation

//--------------------------------------
constructor TAutoPanel.Create(AOwner: TComponent);
begin
 inherited;
 FAutoChildPosLeft := False;
 FAutoChildPosTop := False;
 FAutoChildWidth := False;
 FAutoChildHeight := False;
 pWidth := -1;
 pHeight := -1;
 PCtrlsCoordArr := nil;
end;


destructor TAutoPanel.Destroy;
begin
 inherited;
 FreeMem(PCtrlsCoordArr);
end;

procedure TAutoPanel.Loaded;
var i:Integer;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then Exit;

  if (pWidth = -1) and (pHeight = -1) then begin
   GetMem(PCtrlsCoordArr, ControlCount * sizeof(TRect));
   for  i := 0 to ControlCount - 1 do begin
      PCtrlsCoordArr^[i].Left := Controls[i].Left;
      PCtrlsCoordArr^[i].Top := Controls[i].Top;
      PCtrlsCoordArr^[i].Width := Controls[i].Width;
      PCtrlsCoordArr^[i].Height := Controls[i].Height;
   end;
   pWidth := Width;
   pHeight := Height;
  end;
end;

procedure TAutoPanel.Resize;
var I:Integer;
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  if not (AutoChildPosLeft or AutoChildWidth or AutoChildPosTop or AutoChildHeight) then Exit;
  try
    for  i := 0 to ControlCount - 1 do
    begin
      if(AutoChildPosLeft = true) then
        if (AutoChildWidth = true) then
        begin
          Controls[i].Left := MulDiv (PCtrlsCoordArr^[i].Left,Width,pWidth);
          Controls[i].Width :=  MulDiv (PCtrlsCoordArr^[i].Width,Width,pWidth);
        end
        else
          Controls[i].Left := Round(
             PCtrlsCoordArr^[i].Left * Width / pWidth  +
             ((PCtrlsCoordArr^[i].Width) * Width / pWidth -
             (PCtrlsCoordArr^[i].Width))/2
            );

      if(AutoChildPosTop = true) then
        if (AutoChildHeight = true) then
        begin
          Controls[i].Top := MulDiv (PCtrlsCoordArr^[i].Top,Height,pHeight);
          Controls[i].Height := MulDiv (PCtrlsCoordArr^[i].Height,Height,pHeight);
        end
        else
          Controls[i].Top := Round(
             PCtrlsCoordArr^[i].Top * Height / pHeight +
             ((PCtrlsCoordArr^[i].Height)  * Height / pHeight -
             (PCtrlsCoordArr^[i].Height))/2
            );
    end;
  finally
  end;
end;
//--------------------------------------

end.
