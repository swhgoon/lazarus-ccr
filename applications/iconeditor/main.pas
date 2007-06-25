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
    Icon Editor main unit.
}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, ActnList, StdActns, ExtDlgs, Buttons, StdCtrls, LazJPEG,
  NewDialog, ResizeDialog, ResizePaperDialog, AboutDialog, Preview,
  
  PictureManager, RGBGraphics, PictureCtrls, ColorPalette;

type

  { TMainForm }

  TMainForm = class(TForm)
    Palette: TColorPalette;
    ImageListActionsDisabled: TImageList;
    ImageListToolsDisabled: TImageList;
    MenuItemShowGrid: TMenuItem;
    MenuItemShowPreview: TMenuItem;
    ViewShowPreview: TAction;
    ViewShowMask: TAction;
    ViewShowGrid: TAction;
    MenuItemShowMask: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemClipPaperToMask: TMenuItem;
    PictureClipPaperToMask: TAction;
    Bevel2: TBevel;
    EditDensity: TEdit;
    EditRoundness: TEdit;
    EditSize: TEdit;
    EditTolerance: TEdit;
    LabelDensity: TLabel;
    LabelRoundness: TLabel;
    LabelShape: TLabel;
    LabelMaskTool: TLabel;
    LabelSize: TLabel;
    LabelTolerance: TLabel;
    MaskInvert: TAction;
    MaskRemove: TAction;
    Bevel1: TBevel;
    EditSize1: TEdit;
    FileExportAsLRS: TAction;
    ColorsDisable: TAction;
    ColorDialog: TColorDialog;
    ColorsGrayscale: TAction;
    ColorsInvert: TAction;
    ImageListActions: TImageList;
    LabelFillOutline: TLabel;
    LabelZoom: TLabel;
    MenuItem4: TMenuItem;
    MenuItemMaskRemove: TMenuItem;
    MenuItemMaskInvert: TMenuItem;
    MenuItemMask: TMenuItem;
    MenuItemExportAsLRS: TMenuItem;
    MenuItemDisable: TMenuItem;
    MenuItemInvert: TMenuItem;
    MenuItemGrayscale: TMenuItem;
    MenuItemColors: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    ImageListTools: TImageList;
    LabelFill: TLabel;
    LabelOutline: TLabel;
    LabelPaper: TLabel;
    MenuItemHorizontally: TMenuItem;
    MenuItemVertically: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem180: TMenuItem;
    MenuItem270: TMenuItem;
    MenuItemCustom: TMenuItem;
    MenuItemRotate: TMenuItem;
    MenuItemResize: TMenuItem;
    MenuItemResizePaper: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItemFlip: TMenuItem;
    PanelDensity: TPanel;
    PanelRoundness: TPanel;
    PanelShape: TPanel;
    PanelMaskTool: TPanel;
    PanelSize: TPanel;
    PanelTolerance: TPanel;
    PanelToolOptions: TPanel;
    PanelOutline: TPanel;
    PanelFill: TPanel;
    PanelPaper: TPanel;
    PanelColors: TPanel;
    PanelFillOutline: TPanel;
    RotateCustom: TAction;
    Rotate180: TAction;
    Rotate90: TAction;
    Rotate270: TAction;
    FlipVertically: TAction;
    FlipHorizontally: TAction;
    PictureResizePaper: TAction;
    PictureResize: TAction;
    ComboBoxZoom: TComboBox;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditDelete: TEditDelete;
    EditPaste: TEditPaste;
    EditRedo: TAction;
    EditSelectAll: TEditSelectAll;
    EditUndo: TEditUndo;
    FileClose: TAction;
    FileSave: TAction;
    FileNew: TAction;
    ActionList: TActionList;
    FileOpen: TAction;
    FileSaveAs: TAction;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelpTopics: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemPicture: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFile: TMenuItem;
    PanelZoom: TPanel;
    PanelPictures: TPanel;
    PanelToolBar: TPanel;
    PanelPallete: TPanel;
    PanelTools: TPanel;
    ExportResourceDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    ToolCircleShape: TSpeedButton;
    ToolMaskEllipse: TSpeedButton;
    ToolMaskFloodFill: TSpeedButton;
    ToolFillOutline: TSpeedButton;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    PanelOptions: TPanel;
    ToolBarTools: TToolBar;
    ToolRectShape: TSpeedButton;
    ToolOutline: TSpeedButton;
    ToolFill: TSpeedButton;
    ToolMaskRectangle: TSpeedButton;
    ToolRectShape2: TSpeedButton;
    ToolUndo: TToolButton;
    ToolButton2: TToolButton;
    ToolRedo: TToolButton;
    ToolClose: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolSave: TToolButton;
    ToolOpen: TToolButton;
    ToolNew: TToolButton;
    ToolButton6: TToolButton;
    ToolCut: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolEllipse: TToolButton;
    ToolRectangle: TToolButton;
    ToolLine: TToolButton;
    ToolPolygon: TToolButton;
    ToolSpray: TToolButton;
    ToolFloodFill: TToolButton;
    ToolEraser: TToolButton;
    ToolPen: TToolButton;
    ToolColorPick: TToolButton;
    ToolMask: TToolButton;
    UpDownDensity: TUpDown;
    UpDownRoundness: TUpDown;
    UpDownSize: TUpDown;
    UpDownSize1: TUpDown;
    UpDownTolerance: TUpDown;
    procedure ColorsDisableExecute(Sender: TObject);
    procedure ColorsGrayscaleExecute(Sender: TObject);
    procedure ColorsInvertExecute(Sender: TObject);
    procedure ComboBoxZoomEditingDone(Sender: TObject);
    procedure ComboBoxZoomChange(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure EditDensityChange(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditRoundnessChange(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure EditSizeChange(Sender: TObject);
    procedure EditToleranceChange(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileExportAsLRSExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FlipHorizontallyExecute(Sender: TObject);
    procedure FlipVerticallyExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaskInvertExecute(Sender: TObject);
    procedure MaskRemoveExecute(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClipPaperToMaskClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemResizeClick(Sender: TObject);
    procedure MenuItemResizePaperClick(Sender: TObject);
    procedure PaletteColorMouseMove(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure PaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure PanelFillDblClick(Sender: TObject);
    procedure PanelOutlineDblClick(Sender: TObject);
    procedure PanelPaperDblClick(Sender: TObject);
    procedure PicturePageChange(Sender: TObject);
    procedure PictureChange(Sender: TObject);
    procedure PicturePageClose(Sender: TObject);
    procedure PicturePageCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Rotate180Execute(Sender: TObject);
    procedure Rotate270Execute(Sender: TObject);
    procedure Rotate90Execute(Sender: TObject);
    procedure ToolCircleShapeClick(Sender: TObject);
    procedure ToolColorPickClick(Sender: TObject);
    procedure ToolEllipseClick(Sender: TObject);
    procedure ToolFillClick(Sender: TObject);
    procedure ToolFillOutlineClick(Sender: TObject);
    procedure ToolFloodFillClick(Sender: TObject);
    procedure ToolLineClick(Sender: TObject);
    procedure ToolMaskClick(Sender: TObject);
    procedure ToolMaskEllipseClick(Sender: TObject);
    procedure ToolMaskFloodFillClick(Sender: TObject);
    procedure ToolMaskRectangleClick(Sender: TObject);
    procedure ToolOutlineClick(Sender: TObject);
    procedure ToolPenClick(Sender: TObject);
    procedure ToolPolygonClick(Sender: TObject);
    procedure ToolRectangleClick(Sender: TObject);
    procedure ToolRectShapeClick(Sender: TObject);
    procedure ToolEraserClick(Sender: TObject);
    procedure ToolSprayClick(Sender: TObject);
    procedure PictureMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PictureColorChange(Sender: TObject);
    procedure PictureFileNameChange(Sender: TObject);
    procedure PictureSizeChange(Sender: TObject);
    procedure ViewShowGridExecute(Sender: TObject);
    procedure ViewShowMaskExecute(Sender: TObject);
    procedure ViewShowPreviewExecute(Sender: TObject);
  private
    Pictures: TPictureManager;
    function GetActivePicture: TPictureBitmap;
    function GetActivePictureEdit: TPictureEdit;
    function GetActivePicturePage: TPicturePage;
    
    procedure UpdateToolSettings;
    procedure SelectTool(Tool: TPictureEditTool);
    procedure UpdatePictureToolsEnabled;
    procedure UpdatePreview;
  public
    property ActivePicture: TPictureBitmap read GetActivePicture;
    property ActivePicturePage: TPicturePage read GetActivePicturePage;
    property ActivePictureEdit: TPictureEdit read GetActivePictureEdit;
  end; 
  
  TToolSetting = (tsFillAndOutline, tsShape, tsMaskTools, tsSize, tsDensity, tsRoundness,
    tsTolerance);
    
  TToolSettings = set of TToolSetting;

var
  MainForm: TMainForm;

implementation

uses Test, IconStrConsts;

procedure SetControlsEnabled(AControl: TControl; AEnabled: Boolean);
var
  AWinControl: TWinControl;
  I: Integer;
begin
  AControl.Enabled := AEnabled;
  if AControl is TWinControl then
  begin
    AWinControl := AControl as TWinControl;
    for I := 0 to Pred(AWinControl.ControlCount) do
     SetControlsEnabled(AWinControl.Controls[I], AEnabled);
  end;
end;

{ TMainForm }

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemResizeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ResizeDialogForm.ShowDialog(ActivePicturePage);
end;

procedure TMainForm.MenuItemResizePaperClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ResizePaperDialogForm.ShowDialog(ActivePicturePage);
end;


procedure TMainForm.PaletteColorMouseMove(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  StatusBar.Panels[4].Text := ColorToString(AColor);
end;

procedure TMainForm.PicturePageChange(Sender: TObject);
begin
  UpdatePictureToolsEnabled;
  UpdatePreview;
  
  if not Pictures.CanEdit then Exit;
  with ActivePictureEdit do
  begin
    PanelOutline.Color := OutlineColor;
    PanelFill.Color := FillColor;
    PanelPaper.Color := PaperColor;
    
    UpDownDensity.Position := Round(RandomDensity * 100);
    UpDownSize.Position := Size;

    case Shape of
    psRect: ToolRectShape.Down := True;
    psCircle: ToolCircleShape.Down := True;
    end;
    
    case FillAndOutline of
    dmFillAndOutline: ToolFillOutline.Down := True;
    dmOutline: ToolOutline.Down := True;
    dmFill: ToolFill.Down := True;
    end;
    
    case MaskTool of
    mtRectangle: ToolMaskRectangle.Down := True;
    mtEllipse: ToolmaskEllipse.Down := True;
    mtFloodFill: ToolMaskFloodFill.Down := True;
    end;
    
    UpDownRoundness.Position := RectangleRoundness;
    UpDownTolerance.Position := Round(FloodFillTolerance * 100);
    
    ComboBoxZoom.Text := Format('%d %%', [Round(Zoom * 100)]);
    
    ViewShowGrid.Checked := poShowGrid in ActivePictureEdit.Options;
    ViewShowMask.Checked := poShowMask in ActivePictureEdit.Options;
    ViewShowPreview.Checked := ActivePicturePage.ShowPreview;
    
    SelectTool(Tool);
    UpdateToolSettings;
    
    PictureSizeChange(nil);
    PictureFileNameChange(nil);
  end;
end;

procedure TMainForm.PictureChange(Sender: TObject);
begin
  UpdatePreview;
  UpdatePictureToolsEnabled;
  UpdateToolSettings;
end;

procedure TMainForm.PicturePageClose(Sender: TObject);
begin
  PictureChange(Sender);
end;

procedure TMainForm.PicturePageCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not Pictures.CanEdit then Exit;
  CanClose := True;
  
  if ActivePictureEdit.Modified then
  begin
    case MessageDlg(Application.Title,
      Format(lieSaveChanges, [ActivePicturePage.Caption]), mtWarning,
      mbYesNoCancel, 0) of
    mrYes:
    begin
      ActivePicturePage.Save;
      CanClose := not ActivePictureEdit.Modified;
    end;
    mrCancel: CanClose := False;
    end;
  end;
end;

procedure TMainForm.Rotate180Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Rotate180Clockwise;
end;

procedure TMainForm.Rotate270Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Rotate270Clockwise;
end;

procedure TMainForm.Rotate90Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Rotate90Clockwise;
end;

procedure TMainForm.ToolCircleShapeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Shape := psCircle;
end;

procedure TMainForm.ToolColorPickClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptColorPick;
  UpdateToolSettings;
end;

procedure TMainForm.ToolEllipseClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptEllipse;
  UpdateToolSettings;
end;

procedure TMainForm.ToolFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FillAndOutline := dmFill;
end;

procedure TMainForm.ToolFillOutlineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FillAndOutline := dmFillAndOutline;
end;

procedure TMainForm.ToolFloodFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptFloodFill;
  UpdateToolSettings;
end;

procedure TMainForm.ToolLineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptLine;
  UpdateToolSettings;
end;

procedure TMainForm.ToolMaskClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptMask;
  UpdateToolSettings;
end;

procedure TMainForm.ToolMaskEllipseClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.MaskTool := mtEllipse;
end;

procedure TMainForm.ToolMaskFloodFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.MaskTool := mtFloodFill;
end;

procedure TMainForm.ToolMaskRectangleClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.MaskTool := mtRectangle;
end;

procedure TMainForm.ToolOutlineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FillAndOutline := dmOutline;
end;

procedure TMainForm.ToolPenClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptPen;
  UpdateToolSettings;
end;

procedure TMainForm.ToolPolygonClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptPolygon;
  UpdateToolSettings;
end;

procedure TMainForm.ToolRectangleClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptRectangle;
  UpdateToolSettings;
end;

procedure TMainForm.ToolRectShapeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Shape := psRect;
end;

procedure TMainForm.ToolEraserClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptEraser;
  UpdateToolSettings;
end;

procedure TMainForm.ToolSprayClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Tool := ptSpray;
  UpdateToolSettings;
end;

procedure TMainForm.PictureMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  DX, DY: Integer;
begin
  if not Pictures.CanEdit then Exit;
  StatusBar.Panels[2].Text := Format('%3.d : %3.d', [X, Y]);
  
  DX := ActivePictureEdit.EndPos.X - ActivePictureEdit.StartPos.X;
  DY := ActivePictureEdit.EndPos.Y - ActivePictureEdit.StartPos.Y;
  if DX >= 0 then Inc(DX)
  else Dec(DX);
  if DY >= 0 then Inc(DY)
  else Dec(DY);
  
  StatusBar.Panels[3].Text := Format('%3.d x %3.d', [DX, DY]);
  StatusBar.Panels[4].Text :=
    ColorToString(ActivePicture.Canvas.GetColor(X, Y));
end;

procedure TMainForm.PictureColorChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  with ActivePictureEdit do
  begin
    PanelOutline.Color := OutlineColor;
    PanelFill.Color := FillColor;
    PanelPaper.Color := PaperColor;
  end;
end;

procedure TMainForm.PictureFileNameChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  StatusBar.Panels[0].Text := ActivePicturePage.Filename;
end;

procedure TMainForm.PictureSizeChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  with ActivePictureEdit do
    StatusBar.Panels[1].Text := Format('%d x %d',
      [Picture.Width, Picture.Height]);
end;

procedure TMainForm.ViewShowGridExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  if ViewShowGrid.Checked then
    ActivePictureEdit.Options := ActivePictureEdit.Options + [poShowGrid]
  else
    ActivePictureEdit.Options := ActivePictureEdit.Options - [poShowGrid];

  ActivePictureEdit.UpdatePicture;
end;

procedure TMainForm.ViewShowMaskExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  if ViewShowMask.Checked then
    ActivePictureEdit.Options := ActivePictureEdit.Options + [poShowMask]
  else
    ActivePictureEdit.Options := ActivePictureEdit.Options - [poShowMask];

  ActivePictureEdit.UpdatePicture;
end;

procedure TMainForm.ViewShowPreviewExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePicturePage.ShowPreview := ViewShowPreview.Checked;

  UpdatePreview;
end;

procedure TMainForm.PaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  if not Pictures.CanEdit then Exit;
  with ActivePictureEdit do
  begin
    if ssLeft in Shift then OutlineColor := AColor;
    if ssRight in Shift then FillColor := AColor;
    if ssMiddle in Shift then PaperColor := AColor;
  end;
end;

procedure TMainForm.PanelFillDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ColorDialog.Color := ActivePictureEdit.FillColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.FillColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.PanelOutlineDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ColorDialog.Color := ActivePictureEdit.OutlineColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.OutlineColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.PanelPaperDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ColorDialog.Color := ActivePictureEdit.PaperColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.PaperColor := ColorDialog.Color;
  end;
end;

function TMainForm.GetActivePicture: TPictureBitmap;
begin
  Result := Pictures.ActivePicturePage.PictureEdit.Picture;
end;

procedure TMainForm.SelectTool(Tool: TPictureEditTool);
begin
  case Tool of
    ptMask: ToolMask.Down := True;
    ptColorPick: ToolColorPick.Down := True;
    ptPen: ToolPen.Down := True;
    ptEraser: ToolEraser.Down := True;
    ptFloodFill: ToolFloodFill.Down := True;
    ptSpray: ToolSpray.Down := True;
    ptLine: ToolLine.Down := True;
    ptPolygon: ToolPolygon.Down := True;
    ptRectangle: ToolRectangle.Down := True;
    ptEllipse: ToolEllipse.Down := True;
  end;
end;

procedure TMainForm.UpdatePictureToolsEnabled;
var
  AValue: Boolean;
begin
  AValue := Pictures.CanEdit;
  
  SetControlsEnabled(PanelZoom, AValue);
  SetControlsEnabled(PanelOptions, AValue);
  SetControlsEnabled(PanelToolOptions, AValue);
  SetControlsEnabled(ToolBarTools, AValue);
  SetControlsEnabled(Palette, AValue);
  
  FileNew.Enabled := True;
  FileOpen.Enabled := True;
  FileSave.Enabled := AValue and ActivePictureEdit.Modified;
  FileSaveAs.Enabled := AValue;
  FileExportAsLRS.Enabled := AValue;
  FileClose.Enabled := AValue;
  
  EditUndo.Enabled := False;//AValue;
  EditRedo.Enabled := False;//AValue;
  EditCopy.Enabled := AValue;
  EditCut.Enabled := AValue;
  EditPaste.Enabled := False;//AValue;
  EditDelete.Enabled := AValue;
  EditSelectAll.Enabled := AValue;
  
  PictureResize.Enabled := AValue;
  PictureResizePaper.Enabled := AValue;
  PictureClipPaperToMask.Enabled := AValue;
  
  FlipHorizontally.Enabled := AValue;
  FlipVertically.Enabled := AValue;
  
  Rotate90.Enabled := AValue;
  Rotate180.Enabled := AValue;
  Rotate270.Enabled := AValue;
  RotateCustom.Enabled := AValue;
  
  ColorsInvert.Enabled := AValue;
  ColorsGrayscale.Enabled := AValue;
  ColorsDisable.Enabled := AVAlue;
  
  ViewShowGrid.Enabled := AValue;
  ViewShowMask.Enabled := AValue;
  ViewShowPreview.Enabled := AValue;
  
  ToolBar.Invalidate;
  ToolBarTools.Invalidate;
end;

procedure TMainForm.UpdatePreview;
begin
  if Pictures.CanEdit and ActivePicturePage.ShowPreview then
  begin
    PreviewForm.Preview(ActivePicture);
    PreviewForm.Show;
  end
  else PreviewForm.Hide;
end;

function TMainForm.GetActivePictureEdit: TPictureEdit;
begin
  Result := Pictures.ActivePicturePage.PictureEdit;
end;

function TMainForm.GetActivePicturePage: TPicturePage;
begin
  Result := Pictures.ActivePicturePage;
end;

procedure TMainForm.UpdateToolSettings;
var
  Settings: TToolSettings;
begin
  if not Pictures.CanEdit then Exit;
  case ActivePictureEdit.Tool of
  ptColorPick: Settings := [];
  ptMask: Settings := [tsMaskTools, tsTolerance];
  ptLine, ptPen: Settings := [];
  ptRectangle: Settings := [tsFillAndOutline, tsRoundness];
  ptEllipse: Settings := [tsFillAndOutline];
  ptPolygon: Settings := [tsFillAndOutline];
  ptFloodFill: Settings := [tsTolerance];
  ptEraser: Settings := [tsShape, tsSize];
  ptSpray: Settings := [tsShape, tsSize, tsDensity];
  end;
  
  PanelShape.Enabled := tsShape in Settings;
  LabelShape.Enabled := tsShape in Settings;
  
  PanelSize.Enabled := tsSize in Settings;
  LabelSize.Enabled := tsSize in Settings;
  
  PanelDensity.Enabled := tsDensity in Settings;
  LabelDensity.Enabled := tsDensity in Settings;
  
  PanelRoundness.Enabled := tsRoundness in Settings;
  LabelRoundness.Enabled := tsRoundness in Settings;
  
  PanelTolerance.Enabled := tsTolerance in Settings;
  LabelTolerance.Enabled := tsTolerance in Settings;
  
  PanelFillOutline.Enabled := tsFillAndOutline in Settings;
  LabelFillOutline.Enabled := tsFillAndOutline in Settings;
  
  PanelMaskTool.Enabled := tsMaskTools in Settings;
  LabelMaskTool.Enabled := tsMaskTools in Settings;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Pictures := TPictureManager.Create(Self);
  Pictures.Parent := PanelPictures;
  Pictures.Align := alClient;
  Pictures.OnChange := @PicturePageChange;
  Pictures.OnPictureMouseMove := @PictureMouseMove;
  Pictures.OnColorChange := @PictureColorChange;
  Pictures.OnPictureSizeChange := @PictureSizeChange;
  Pictures.OnSaveAs := @FileSaveAsExecute;
  Pictures.OnFileNameChange := @PictureFileNameChange;
  Pictures.OnPictureChange := @PictureChange;
  Pictures.OnPageClose := @PicturePageClose;
  Pictures.OnPageCloseQuery := @PicturePageCloseQuery;

  UpdatePictureToolsEnabled;
  UpdateToolSettings;
  
  Palette.LoadPalette('../../default.pal');
  
  MenuItemFile.Caption := lieMenuFile;
  MenuItemEdit.Caption := lieMenuEdit;
  MenuItemPicture.Caption := lieMenuPicture;
  MenuItemMask.Caption := lieMenuMask;
  MenuItemView.Caption := lieMenuView;
  MenuItemHelp.Caption := lieMenuHelp;
  FileNew.Caption := lieFileNew;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF EnableTestForm}
  TestForm.Show;
  {$ENDIF}
end;

procedure TMainForm.MaskInvertExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.InvertMask;
end;

procedure TMainForm.MaskRemoveExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.RemoveMask;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  AboutDialogForm := TAboutDialogForm.Create(Self);
  try
    AboutDialogForm.ShowModal;
  finally
    AboutDialogForm.Free;
  end;
end;

procedure TMainForm.MenuItemClipPaperToMaskClick(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.ClipPaperToMask;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  if NewDialogForm.ShowModal = mrOK then
  begin
    with NewDialogForm do
      Pictures.New(UpDownWidth.Position, UpDownHeight.Position,
        ColorButtonPaper.ButtonColor);
  end;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  I: Integer;
begin
  if OpenPictureDialog.Execute then
  begin
    for I := 0 to Pred(OpenPictureDialog.Files.Count) do
      Pictures.Load(OpenPictureDialog.Files[I]);
  end;
end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
begin
  if SavePictureDialog.Execute then
  begin
    Pictures.Save(SavePictureDialog.FileName);
  end;
end;

procedure TMainForm.FileExportAsLRSExecute(Sender: TObject);
var
  Value: String;
begin
  if ExportResourceDialog.Execute then
  begin
    Value := ExtractFileName(ExportResourceDialog.FileName);
    Value := Copy(Value, 1, Length(Value) - Length(ExtractFileExt(Value)));
    if InputQuery('Set resource name', 'Resource name: ', Value) then
    begin
      Pictures.ExportAsLazarusResource(ExportResourceDialog.FileName, Value);
    end;
  end;
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  Pictures.Save;
end;

procedure TMainForm.FlipHorizontallyExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FlipHorizontally;
end;

procedure TMainForm.FlipVerticallyExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FlipVertically;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Pictures.CloseAll;
  CanClose := Pictures.PageCount = 0;
end;

procedure TMainForm.FileCloseExecute(Sender: TObject);
begin
  Pictures.Close;
end;

procedure TMainForm.ComboBoxZoomChange(Sender: TObject);
var
  V, E: Integer;
  S: String;
begin
  if not Pictures.CanEdit then Exit;
  if Pos('%', ComboBoxZoom.Text) > 0 then
    S := Trim(Copy(ComboBoxZoom.Text, 1, Pos('%', ComboBoxZoom.Text) - 1))
  else
    S := Trim(ComboBoxZoom.Text);
    
  Val(S, V, E);
  if V <= 0 then V := 100;
  ActivePictureEdit.Zoom := V / 100;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Copy;
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Cut;
end;

procedure TMainForm.EditDeleteExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Delete;
end;

procedure TMainForm.EditDensityChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.RandomDensity := UpDownDensity.Position / 100;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  Pictures.Paste;
end;

procedure TMainForm.EditRoundnessChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.RectangleRoundness := UpDownRoundness.Position;
end;

procedure TMainForm.EditSelectAllExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.SelectAll;
end;

procedure TMainForm.ColorsInvertExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Invert;
end;

procedure TMainForm.ComboBoxZoomEditingDone(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ComboBoxZoomChange(nil);
  ComboBoxZoom.Text := Format('%d %%', [Round(ActivePictureEdit.Zoom * 100)]);
end;

procedure TMainForm.ColorsGrayscaleExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Grayscale;
end;

procedure TMainForm.ColorsDisableExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Disable;
end;

procedure TMainForm.EditSizeChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.Size := UpDownSize.Position;
end;

procedure TMainForm.EditToleranceChange(Sender: TObject);
begin
  if not Pictures.CanEdit then Exit;
  ActivePictureEdit.FloodFillTolerance := UpDownTolerance.Position / 100;
end;


initialization
  {$I main.lrs}

end.


