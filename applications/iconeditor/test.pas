{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Tom Gregorovic

  Abstract:
    Picture function tests.
}
unit Test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LCLIntf, ComCtrls;

type

  { TTestForm }

  TTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Output: TMemo;
    UpDown1: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end; 

var
  TestForm: TTestForm;

implementation

uses Main;

{ TTestForm }


procedure TTestForm.Button1Click(Sender: TObject);
var
  I, X, R: Integer;
begin
  R := UpDown1.Position;
  X := 1;
  with MainForm.ActivePictureEdit do
  begin
    for I := 1 to 20 do
    begin
      FillColor := clBlue;
      OutlineColor := clBlue;
      Rectangle(X, 1, X + R - 1, I);
      FillColor := clWhite;
      OutlineColor := clBlack;
      Ellipse(X, 1, X + R - 1, I);
      Inc(X, R + 2);
    end;
  end;
end;

procedure TTestForm.Button2Click(Sender: TObject);
begin
  MainForm.ActivePicture.Mask.DrawTo(Canvas, 0, 100);
end;

initialization
  {$I test.lrs}

end.

