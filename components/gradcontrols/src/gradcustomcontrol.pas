unit gradcustomcontrol; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics; 

type
  
  { TGradCustomControl }

  TGradCustomControl = class(TCustomControl)
  protected
    FBuffer: TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; 
       KeepBase: boolean); override;
    procedure Paint; override;
    procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); overload;
    procedure _Paint(ACanvas: TCanvas); virtual; abstract;
  end;

implementation

{ TGradCustomControl }

constructor TGradCustomControl.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);  
  
  FBuffer := TBitmap.Create;
end;

destructor TGradCustomControl.Destroy;  
begin
  FBuffer.Free;
  
  inherited Destroy;  
end;

procedure TGradCustomControl.ChangeBounds(ALeft, ATop, AWidth,  
  AHeight: integer; KeepBase: boolean);  
begin
  FBuffer.SetSize(AWidth, AHeight);
  
  _Paint(FBuffer.Canvas);
  
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);  
end;

procedure TGradCustomControl.Paint;  
begin
  if not HasParent then
    Exit; 
  
  Canvas.Draw(0,0, FBuffer);
  
  inherited Paint;  
end;

procedure TGradCustomControl.PaintTo(ACanvas: TCanvas; X, Y: Integer);  
begin
  ACanvas.Draw(0,0, FBuffer);
end;

end.

