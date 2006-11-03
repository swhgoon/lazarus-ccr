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
    Common picture modify dialog.
}
unit PictureDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  PictureCtrls, PictureManager;

type

  { TPictureDialogForm }

  TPictureDialogForm = class(TForm)
  private
    FPicturePage: TPicturePage;
    function GetAspectRatio: Single;
    function GetPicture: TPictureBitmap;
    function GetPictureEdit: TPictureEdit;
  protected
    procedure Initialize; dynamic;
    procedure Apply; dynamic;
  public
    procedure ShowDialog(APicturePage: TPicturePage);

    property PicturePage: TPicturePage read FPicturePage;
    property PictureEdit: TPictureEdit read GetPictureEdit;
    property Picture: TPictureBitmap read GetPicture;
    property AspectRatio: Single read GetAspectRatio;
  end; 

var
  PictureDialogForm: TPictureDialogForm;

implementation

{ TPictureDialogForm }

function TPictureDialogForm.GetPicture: TPictureBitmap;
begin
  Result := FPicturePage.PictureEdit.Picture;
end;

function TPictureDialogForm.GetPictureEdit: TPictureEdit;
begin
  Result := FPicturePage.PictureEdit;
end;

function TPictureDialogForm.GetAspectRatio: Single;
begin
  Result := Picture.Height / Picture.Width;
end;

procedure TPictureDialogForm.Initialize;
begin
  //
end;

procedure TPictureDialogForm.ShowDialog(APicturePage: TPicturePage);
begin
  FPicturePage := APicturePage;

  Initialize;
  
  if inherited ShowModal = mrOK then Apply;
end;

procedure TPictureDialogForm.Apply;
begin
  //
end;

initialization
  {$I picturedialog.lrs}

end.

