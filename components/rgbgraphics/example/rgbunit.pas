{
 /***************************************************************************
                                  RGBUnit.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
}
unit RGBUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  RGBGraphics;

type

  { TFormExample }

  TFormExample = class(TForm)
    ButtonReplace: TButton;
    ButtonInvert: TButton;
    ButtonRotate90: TButton;
    ButtonRedLine: TButton;
    procedure ButtonInvertClick(Sender: TObject);
    procedure ButtonRedLineClick(Sender: TObject);
    procedure ButtonReplaceClick(Sender: TObject);
    procedure ButtonRotate90Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public
  end; 

var
  FormExample: TFormExample;
  RGBBitmap: TRGB32Bitmap;

implementation

{ TFormExample }

procedure TFormExample.FormCreate(Sender: TObject);
begin
  RGBBitmap := TRGB32Bitmap.CreateFromFile('splash_logo.xpm');
end;

procedure TFormExample.ButtonRedLineClick(Sender: TObject);
begin
  RGBBitmap.Canvas.OutlineColor := clRed;
  RGBBitmap.Canvas.Line(10, 10, 100, 100);
  
  Invalidate;
end;

procedure TFormExample.ButtonReplaceClick(Sender: TObject);
begin
  RGBBitmap.Canvas.EraseMode := emReplace;
  RGBBitmap.Canvas.FillColor := clWhite;
  RGBBitmap.Canvas.PaperColor := clBlue;

  RGBBitmap.Canvas.FillRect(0, 0, Pred(RGBBitmap.Width), Pred(RGBBitmap.Height));
  
  RGBBitmap.Canvas.EraseMode := emNone;
  
  Invalidate;
end;

procedure TFormExample.ButtonInvertClick(Sender: TObject);
begin
  RGBBitmap.Invert;

  Invalidate;
end;

procedure TFormExample.ButtonRotate90Click(Sender: TObject);
begin
  RGBBitmap.Rotate90;

  Invalidate;
end;

procedure TFormExample.FormDestroy(Sender: TObject);
begin
  RGBBitmap.Free;
end;

procedure TFormExample.FormPaint(Sender: TObject);
begin
  if RGBBitmap = nil then Exit;
  // draw bitmap 2x smaller
  RGBBitmap.Canvas.StretchDrawTo(Canvas, 140, 10, RGBBitmap.Width div 2,
    RGBBitmap.Height div 2);
end;

initialization
  {$I rgbunit.lrs}

end.

