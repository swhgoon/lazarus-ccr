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

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  RGBGraphics, ExtDlgs;

type

  { TFormExample }

  TFormExample = class(TForm)
    ButtonReplace: TButton;
    ButtonInvert: TButton;
    ButtonRotate90: TButton;
    ButtonRedLine: TButton;
    OpenPictureDialog: TOpenPictureDialog;
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
  RGBMask: TRGBMask;

implementation

{ TFormExample }

procedure TFormExample.FormCreate(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    RGBBitmap := TRGB32Bitmap.CreateFromFile(OpenPictureDialog.FileName);
  end
  else
  begin
    RGBBitmap := TRGB32Bitmap.Create(400, 300);
    RGBBitmap.Canvas.DrawMode := dmFill;
    RGBBitmap.Canvas.Fill(clBlack);
    RGBBitmap.Canvas.FillColor := clRed;
    RGBBitmap.Canvas.Ellipse(100, 0, 300, 200);
    RGBBitmap.Canvas.FillColor := clGreen;
    RGBBitmap.Canvas.Ellipse(50, 100, 250, 300);
    RGBBitmap.Canvas.FillColor := clBlue;
    RGBBitmap.Canvas.Ellipse(150, 100, 350, 300);
    RGBBitmap.Canvas.FillColor := clWhite;
    RGBBitmap.Canvas.Ellipse(150, 100, 250, 200);
    RGBBitmap.Canvas.DrawMode := dmFillAndOutline;
  end;


  RGBMask := TRGBMask.Create(160, 100);

  RGBMask.Clear;
  RGBMask.Ellipse(10, 10, 150, 90);
end;

procedure TFormExample.ButtonRedLineClick(Sender: TObject);
begin
  RGBBitmap.Canvas.OutlineColor := clRed;
  RGBBitmap.Canvas.Line(10, 10, 100, 100);
  
  Invalidate;
end;

procedure TFormExample.ButtonReplaceClick(Sender: TObject);
begin
  RGBBitmap.Canvas.EraseMode := ermReplace;
  RGBBitmap.Canvas.FillColor := clRed;
  RGBBitmap.Canvas.PaperColor := clBlue;

  RGBBitmap.Canvas.FillRect(0, 0, Pred(RGBBitmap.Width), Pred(RGBBitmap.Height));
  
  RGBBitmap.Canvas.EraseMode := ermNone;
  
  Invalidate;
end;

procedure TFormExample.ButtonInvertClick(Sender: TObject);
begin
  RGBBitmap.Invert;
  RGBMask.Invert;

  Invalidate;
end;

procedure TFormExample.ButtonRotate90Click(Sender: TObject);
begin
  RGBBitmap.Rotate90;
  RGBMask.Rotate90;

  Invalidate;
end;

procedure TFormExample.FormDestroy(Sender: TObject);
begin
  RGBBitmap.Free;
  RGBMask.Free;
end;

procedure TFormExample.FormPaint(Sender: TObject);
begin
  if RGBBitmap = nil then Exit;
  // draw bitmap
  RGBBitmap.Canvas.DrawTo(Canvas, 180, 10);
    
  RGBMask.DrawTo(Canvas, 10, 160);
  RGBMask.DrawShapeTo(Canvas, 10, 340);
end;


initialization
  {$I rgbunit.lrs}

end.

