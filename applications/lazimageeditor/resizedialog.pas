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
    Resize picture dialog.
}
unit ResizeDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, PictureDialog;

type

  { TResizeDialogForm }

  TResizeDialogForm = class(TPictureDialogForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxAspectRatio: TCheckBox;
    ComboBoxSmooth: TComboBox;
    EditHeight: TEdit;
    EditWidth: TEdit;
    GroupBoxStretchMethod: TGroupBox;
    GroupBoxProperties: TGroupBox;
    LabelHeight: TLabel;
    LabelWidth: TLabel;
    RadioButtonTruncate: TRadioButton;
    RadioButtonSmooth: TRadioButton;
    UpDownHeight: TUpDown;
    UpDownWidth: TUpDown;
    procedure EditHeightChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure CheckBoxAspectRatioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end; 

var
  ResizeDialogForm: TResizeDialogForm;

implementation
uses IconStrConsts;
{ TResizeDialogForm }

procedure TResizeDialogForm.EditHeightChange(Sender: TObject);
var
  TempEvent: TNotifyEvent;
begin
  if CheckBoxAspectRatio.Checked then
  begin
    TempEvent := EditWidth.OnChange;
    EditWidth.OnChange := nil;
    UpDownWidth.Position := Round(UpDownHeight.Position / AspectRatio);
    EditWidth.OnChange := TempEvent;
  end;
end;

procedure TResizeDialogForm.EditWidthChange(Sender: TObject);
var
  TempEvent: TNotifyEvent;
begin
  if CheckBoxAspectRatio.Checked then
  begin
    TempEvent := EditHeight.OnChange;
    EditHeight.OnChange := nil;
    UpDownHeight.Position := Round(UpDownWidth.Position * AspectRatio);
    EditHeight.OnChange := TempEvent;
  end;
end;

procedure TResizeDialogForm.CheckBoxAspectRatioClick(Sender: TObject);
begin
  if CheckBoxAspectRatio.Checked then
  begin
    EditWidthChange(nil);
    EditHeightChange(nil);
  end;
end;

procedure TResizeDialogForm.FormCreate(Sender: TObject);
begin
  Caption:=lieResizeDialog;
  GroupBoxProperties.Caption:=lieGroupBoxProperties;
  LabelWidth.Caption:=lieLabelNewWidth;
  LabelHeight.Caption:=lieLabelNewHeight;
  CheckBoxAspectRatio.Caption:=lieCheckBoxAspectRatio;
  GroupBoxStretchMethod.Caption:=lieGroupBoxStretchMethod;
  RadioButtonTruncate.Caption:=lieRadioButtonTruncate;
  RadioButtonSmooth.Caption:=lieRadioButtonSmooth;
  ComboBoxSmooth.Items.Clear;
  ComboBoxSmooth.Items.Add(lieAreapixel);
  ComboBoxSmooth.Items.Add(lieBilinear);
  ComboBoxSmooth.Items.Add(lieBicubic);
  ButtonOK.Caption:=lieButtonOK;
  ButtonCancel.Caption:=lieButtonCancel;
end;

procedure TResizeDialogForm.Initialize;
begin
  UpDownWidth.Position := Picture.Width;
  UpDownHeight.Position := Picture.Height;
end;

procedure TResizeDialogForm.Apply;
begin
  if RadioButtonTruncate.Checked then
  begin
    PictureEdit.StretchTruncate(UpDownWidth.Position, UpDownHeight.Position);
  end
  else
  begin
  end;
end;

initialization
  {$I resizedialog.lrs}

end.


