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
    Preview picture window.
}
unit Preview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  PictureCtrls;

type

  { TPreviewForm }

  TPreviewForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure Preview(Bitmap: TPictureBitmap);
  end; 

var
  PreviewForm: TPreviewForm;
  View: TPictureView;

implementation

{ TPreviewForm }

procedure TPreviewForm.FormCreate(Sender: TObject);
begin
  View := TPictureView.Create(Self);
  View.Options := View.Options - [poShowGrid, poShowMask];
  View.Align := alClient;
  View.Parent := Self;
  
  Left := Screen.Width - Width - 10;
end;

procedure TPreviewForm.Preview(Bitmap: TPictureBitmap);
begin
  View.Picture := Bitmap;
end;

initialization
  {$I preview.lrs}

end.

