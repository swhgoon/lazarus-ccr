unit MyDrawingControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

type

  { TMyDrawingControl }

  TMyDrawingControl = class(TCustomControl)
  private
    FBitmap: TPortableNetworkGraphic;
  public
    property Bitmap: TPortableNetworkGraphic read FBitmap write FBitmap;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

implementation

procedure TMyDrawingControl.EraseBackground(DC: HDC);
begin
  //Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

procedure TMyDrawingControl.Paint;
begin
  if Assigned(Bitmap) then
    Canvas.Draw(0, 0, Bitmap);

  inherited Paint;
end;

end.
