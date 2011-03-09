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
    Resize picture paper dialog.
}
unit ResizePaperDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, PictureCtrls, Math, BmpRGBGraph, PictureDialog;

type

  { TResizePaperDialogForm }

  TResizePaperDialogForm = class(TPictureDialogForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxAspectRatio: TCheckBox;
    ColorButtonPaper: TColorButton;
    ComboBoxPicturePosition: TComboBox;
    EditHeight: TEdit;
    EditWidth: TEdit;
    GroupBoxProperties: TGroupBox;
    LabelPicturePosition: TLabel;
    LabelHeight: TLabel;
    LabelPaperColor: TLabel;
    LabelWidth: TLabel;
    PanelPreview: TPanel;
    UpDownHeight: TUpDown;
    UpDownWidth: TUpDown;
    procedure ColorButtonPaperColorChanged(Sender: TObject);
    procedure ComboBoxPicturePositionChange(Sender: TObject);
    procedure EditHeightChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxAspectRatioChange(Sender: TObject);
  private
    FPreviewFactor: Single;
  protected
    procedure Initialize; override;
  public
    procedure UpdatePreview;
    
    procedure Apply; override;
  end; 

var
  ResizePaperDialogForm: TResizePaperDialogForm;
  Preview: TPictureView;

implementation
uses IconStrConsts;
{ TResizePaperDialogForm }

procedure TResizePaperDialogForm.FormCreate(Sender: TObject);
begin
  Preview := TPictureView.Create(PanelPreview);
  Preview.Parent := PanelPreview;
  Preview.Align := alClient;

  Caption:=lielieResizePaperDialog;
  LabelPicturePosition.Caption:=lieLabelPicturePosition;
  LabelWidth.Caption:=lieLabelPaperWidth;
  LabelHeight.Caption:=lieLabelPaperHeight;
  CheckBoxAspectRatio.Caption:=lieCheckBoxAspectRatio;
  LabelPaperColor.Caption:=lieLabelPaperColor;
  ColorButtonPaper.Caption:=lieColorButtonPaper;
  ButtonOK.Caption:=lieButtonOK;
  ButtonCancel.Caption:=lieButtonCancel;
  ComboBoxPicturePosition.Items.Clear;
  ComboBoxPicturePosition.Items.Add(lieTopLeft);
  ComboBoxPicturePosition.Items.Add(lieTopCenter);
  ComboBoxPicturePosition.Items.Add(lieTopRight);
  ComboBoxPicturePosition.Items.Add(lieCenterLeft);
  ComboBoxPicturePosition.Items.Add(lieCentered);
  ComboBoxPicturePosition.Items.Add(lieCenterRight);
  ComboBoxPicturePosition.Items.Add(lieBottomLeft);
  ComboBoxPicturePosition.Items.Add(lieBottomCenter);
  ComboBoxPicturePosition.Items.Add(lieBottomRight);
end;

procedure TResizePaperDialogForm.EditHeightChange(Sender: TObject);
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
  
  UpdatePreview;
end;

procedure TResizePaperDialogForm.EditWidthChange(Sender: TObject);
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
  
  UpdatePreview;
end;

procedure TResizePaperDialogForm.ComboBoxPicturePositionChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TResizePaperDialogForm.ColorButtonPaperColorChanged(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TResizePaperDialogForm.FormDestroy(Sender: TObject);
begin
  Preview.Free;
end;

procedure TResizePaperDialogForm.CheckBoxAspectRatioChange(Sender: TObject);
begin
  if CheckBoxAspectRatio.Checked then
  begin
    EditWidthChange(nil);
    EditHeightChange(nil);
  end;
end;

procedure TResizePaperDialogForm.Initialize;
begin
  inherited;
  
  UpDownWidth.Position := Picture.Width;
  UpDownHeight.Position := Picture.Height;
  ColorButtonPaper.ButtonColor := PictureEdit.PaperColor;
  
  UpdatePreview;
end;

procedure TResizePaperDialogForm.UpdatePreview;
var
  W, H, SW, SH: Integer;
begin
  W := UpDownWidth.Position;
  H := UpDownHeight.Position;

  if W > 256 then
  begin
    if H > 256 then
    begin
      if W > H then
        FPreviewFactor := 256 / W
      else
        FPreviewFactor := 256 / H;
    end
    else
      FPreviewFactor := 256 / W;
  end
  else
    if H > 256 then FPreviewFactor := 256 / H
    else FPreviewFactor := 1.0;

  W := Max(1, Round(W * FPreviewFactor));
  H := Max(1, Round(H * FPreviewFactor));
  SW := Max(1, Round(Picture.Width * FPreviewFactor));
  SH := Max(1, Round(Picture.Height * FPreviewFactor));

  Preview.Color := ColorButtonPaper.ButtonColor;
  Preview.Picture.Free;
  Preview.Picture := TPictureBitmap.CreateAsCopy(Picture);
  Preview.Picture.StretchTrunc(SW, SH);
  //Preview.ResizePaper(W, H, TPicturePos(ComboBoxPicturePosition.ItemIndex));
end;

procedure TResizePaperDialogForm.Apply;
begin
  PictureEdit.PaperColor := ColorButtonPaper.ButtonColor;
  PictureEdit.ResizePaper(UpDownWidth.Position, UpDownHeight.Position,
    TPicturePos(ComboBoxPicturePosition.ItemIndex));
end;

initialization
  {$I resizepaperdialog.lrs}

end.



