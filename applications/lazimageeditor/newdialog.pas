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
    New picture dialog.
}
unit NewDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls;

type

  { TNewDialogForm }

  TNewDialogForm = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ColorButtonPaper: TColorButton;
    EditWidth: TEdit;
    EditHeight: TEdit;
    GroupBoxProperties: TGroupBox;
    LabelPaperColor: TLabel;
    LabelWidth: TLabel;
    LabelHeight: TLabel;
    UpDownWidth: TUpDown;
    UpDownHeight: TUpDown;
    procedure FormCreate(Sender: TObject);
  private
  public
  end; 

var
  NewDialogForm: TNewDialogForm;

implementation

uses IconStrConsts;
{ TNewDialogForm }

procedure TNewDialogForm.FormCreate(Sender: TObject);
begin
  Caption:=lieNewDialog;
  LabelWidth.Caption:=lieLabelWidth;
  LabelHeight.Caption:=lieLabelHeight;
  LabelPaperColor.Caption:=lieLabelPaperColor;
  ColorButtonPaper.Caption:=lieColorButtonPaper;
  ButtonOK.Caption:=lieButtonOK;
  ButtonCancel.Caption:=lieButtonCancel;
end;

initialization
  {$I newdialog.lrs}

end.


