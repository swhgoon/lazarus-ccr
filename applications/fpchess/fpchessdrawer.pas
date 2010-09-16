unit fpchessdrawer;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Controls, Graphics, LCLType;

type

  { TFPChessDrawer }

  TFPChessDrawer = class(TCustomControl)
  public
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
  end;

var
  vFPChessDrawer: TFPChessDrawer;

implementation

procedure TFPChessDrawer.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TFPChessDrawer.Paint;
var
  x, y: Integer;
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

  inherited Paint;
end;

procedure TFPChessDrawer.DrawToCanvas(ACanvas: TCanvas);
begin

end;

end.

