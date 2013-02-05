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
    Image Editor main unit.
}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, ActnList, StdActns, ExtDlgs, Buttons, StdCtrls, Spin,
  ColorBox, NewDialog, ResizeDialog, ResizePaperDialog, AboutDialog, DLBitmap,
  PictureManager, PictureCtrls, ColorPalette,
  appsettings;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList: TActionList;
    checkFuzzy: TCheckBox;
    BtnFromColor: TColorButton;
    BtnToColor: TColorButton;
    ColorsDisable: TAction;
    ColorsGrayscale: TAction;
    ColorsInvert: TAction;
    FontListBox: TComboBox;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditDelete: TEditDelete;
    EditPaste: TEditPaste;
    EditRedo: TAction;
    EditSelectAll: TEditSelectAll;
    EditUndo: TEditUndo;
    FileClose: TAction;
    FileExportAsLRS: TAction;
    FileNew: TAction;
    FileOpen: TAction;
    FileSave: TAction;
    FileSaveAs: TAction;
    FlipHorizontally: TAction;
    FlipVertically: TAction;
    EditSize: TSpinEdit;
    EditRoundness: TSpinEdit;
    EditDensity: TSpinEdit;
    EditTolerance: TSpinEdit;
    MenuItemExportAsICO: TMenuItem;
    ToolsImageList: TImageList;
    PolyNum: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelTolerance1: TLabel;
    LabelTolerance2: TLabel;
    MaskInvert: TAction;
    MaskRemove: TAction;
    MenuItem5: TMenuItem;
    Palette: TColorPalette;
    MenuItemShowGrid: TMenuItem;
    Panel1: TPanel;
    PanelTolerance1: TPanel;
    PanelTolerance2: TPanel;
    PictureClipPaperToMask: TAction;
    PictureResize: TAction;
    PictureResizePaper: TAction;
    Rotate180: TAction;
    Rotate270: TAction;
    Rotate90: TAction;
    RotateCustom: TAction;
    FontSize: TSpinEdit;
    spinFillAlpha: TSpinEdit;
    MenuItemShowMask: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemClipPaperToMask: TMenuItem;
    Bevel2: TBevel;
    LabelDensity: TLabel;
    LabelRoundness: TLabel;
    LabelShape: TLabel;
    LabelMaskTool: TLabel;
    LabelSize: TLabel;
    LabelTolerance: TLabel;
    Bevel1: TBevel;
    EditSize1: TEdit;
    ColorDialog: TColorDialog;
    ImageListActions: TImageList;
    LabelFillOutline: TLabel;
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
    ComboBoxZoom: TComboBox;
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
    ToolBrush: TToolButton;
    ToolButton1: TToolButton;
    RegularPolygon: TToolButton;
    ToolText: TToolButton;
    ToolcolorReplacer: TToolButton;
    ZoomInBtn: TToolButton;
    ZoomOutBtn: TToolButton;
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
    ToolPolygon: TToolButton;
    ToolEllipse: TToolButton;
    ToolLine: TToolButton;
    ToolRectangle: TToolButton;
    ToolSpray: TToolButton;
    ToolFloodFill: TToolButton;
    ToolEraser: TToolButton;
    ToolPen: TToolButton;
    ToolColorPick: TToolButton;
    ToolMask: TToolButton;
    UpDownSize1: TUpDown;
    ViewShowGrid: TAction;
    ViewShowMask: TAction;
    procedure checkFuzzyChange(Sender: TObject);
    procedure BtnToColorClick(Sender: TObject);
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
    procedure FontListBoxChange(Sender: TObject);
    procedure FontListBoxClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MaskInvertExecute(Sender: TObject);
    procedure MaskRemoveExecute(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClipPaperToMaskClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemExportAsICOClick(Sender: TObject);
    procedure MenuItemResizeClick(Sender: TObject);
    procedure MenuItemResizePaperClick(Sender: TObject);
    procedure PaletteColorMouseMove(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure PaletteColorPick(Sender: TObject; AColor: TColor; Shift: TShiftState);
    procedure PanelFillDblClick(Sender: TObject);
    procedure PanelOutlineDblClick(Sender: TObject);
    procedure PanelPaperDblClick(Sender: TObject);
    procedure PanelPaperDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure PanelZoomClick(Sender: TObject);
    procedure PicturePageChange(Sender: TObject);
    procedure PictureChange(Sender: TObject);
    procedure PicturePageClose(Sender: TObject);
    procedure PicturePageCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PolyNumChange(Sender: TObject);
    procedure RegularPolygonClick(Sender: TObject);
    procedure Rotate180Execute(Sender: TObject);
    procedure Rotate270Execute(Sender: TObject);
    procedure Rotate90Execute(Sender: TObject);
    procedure spinFillAlphaChange(Sender: TObject);
    procedure ToolBarToolsClick(Sender: TObject);
    procedure ToolBrushClick(Sender: TObject);
    procedure ToolcolorReplacerClick(Sender: TObject);
    procedure ToolTextClick(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure ToolCircleShapeClick(Sender: TObject);
    procedure ToolColorPickClick(Sender: TObject);
    procedure ToolEllClick(Sender: TObject);
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
    procedure PictureMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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
    procedure ChangeTool(Tool: TPictureEditTool);
    procedure UpdatePictureToolsEnabled;
    procedure UpdateAll;
  public
    TextEditor: TTextEditor;
    procedure FileNewOnStart;
    procedure OpenImageFile(FileName: string);
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

uses Test, IconStrConsts, iconsizeselection;

procedure SetControlsEnabled(AControl: TControl; AEnabled: boolean);
var
  AWinControl: TWinControl;
  I: integer;
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

procedure TMainForm.MenuItemExportAsICOClick(Sender: TObject);
var
  lExt, Value: string;
begin
  ExportResourceDialog.Filter := 'Windows Icon (*.ico)|*.ico|All files (*.*)|*.*';
  if ExportResourceDialog.Execute then
  begin
    Value := ExtractFileName(ExportResourceDialog.FileName);
    lExt := ExtractFileExt(Value);
    Value := Copy(Value, 1, Length(Value) - Length(lExt));
    Pictures.ExportAsWindowsIcon(ExportResourceDialog.FileName);
  end;
end;

procedure TMainForm.MenuItemResizeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ResizeDialogForm.ShowDialog(ActivePicturePage);
end;

procedure TMainForm.MenuItemResizePaperClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
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

  if not Pictures.CanEdit then
    Exit;
  with ActivePictureEdit do
  begin
    PanelOutline.Color := OutlineColor;
    PanelFill.Color := FillColor;
    PanelPaper.Color := PaperColor;

//    UpDownDensity.Position := Round(RandomDensity * 100);
//    UpDownSize.Position := Size;

    case Shape of
      psRect: ToolRectShape.Down := True;
      psCircle: ToolCircleShape.Down := True;
    end;

{    case FillAndOutline of
    dmFillAndOutline: ToolFillOutline.Down := True;
    dmOutline: ToolOutline.Down := True;
    dmFill: ToolFill.Down := True;
    end;     }

    case MaskTool of
      mtRectangle: ToolMaskRectangle.Down := True;
      mtEllipse: ToolmaskEllipse.Down := True;
      mtFloodFill: ToolMaskFloodFill.Down := True;
    end;

//    UpDownRoundness.Position := RectangleRoundness;
//    UpDownTolerance.Position := Round(FloodFillTolerance * 100);

    ComboBoxZoom.Text := Format('%d %%', [Round(Zoom * 100)]);

    ViewShowGrid.Checked := poShowGrid in ActivePictureEdit.Options;
    ViewShowMask.Checked := poShowMask in ActivePictureEdit.Options;

    SelectTool(Tool);

    PictureSizeChange(nil);
    PictureFileNameChange(nil);
  end;
end;

procedure TMainForm.PictureChange(Sender: TObject);
begin
  FileSave.Enabled := Pictures.CanEdit and ActivePictureEdit.Modified;
end;

procedure TMainForm.PicturePageClose(Sender: TObject);
begin
  UpdateAll;
end;

procedure TMainForm.PicturePageCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not Pictures.CanEdit then
    Exit;
  CanClose := True;

  if ActivePictureEdit.Modified then
  begin
    case MessageDlg(Application.Title, Format(lieSaveChanges,
        [ActivePicturePage.Caption]), mtWarning, mbYesNoCancel, 0) of
      mrYes:
      begin
        ActivePicturePage.Save;
        CanClose := not ActivePictureEdit.Modified;
      end;
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TMainForm.PolyNumChange(Sender: TObject);
begin
  ActivePictureEdit.RegularPolyNum := PolyNum.Value;
end;

procedure TMainForm.RegularPolygonClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptRegularPolygon);
  ActivePictureEdit.ChangeColor(BtnFromColor.ButtonColor, BtnToColor.ButtonColor);
end;

procedure TMainForm.Rotate180Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.RegularPolyNum := PolyNum.Value;
  ActivePictureEdit.Rotate180Clockwise;
end;

procedure TMainForm.Rotate270Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Rotate270Clockwise;
end;

procedure TMainForm.Rotate90Execute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Rotate90Clockwise;
end;

procedure TMainForm.spinFillAlphaChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.FillAlpha := spinFillAlpha.Value;
end;

procedure TMainForm.ToolBarToolsClick(Sender: TObject);
begin
  if ActivePictureEdit.Tool <> ptText then
    TextEditor.StopEdit;
end;

procedure TMainForm.ToolBrushClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptBrush);
end;

procedure TMainForm.ToolcolorReplacerClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptColorReplacer);
  ActivePictureEdit.ChangeColor(BtnFromColor.ButtonColor, BtnToColor.ButtonColor);
end;

procedure TMainForm.ToolTextClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptText);
  ActivePictureEdit.TextEditor := TextEditor;
end;

procedure TMainForm.ZoomInBtnClick(Sender: TObject);
var
  V, E: integer;
  S: string;
begin
  if not Pictures.CanEdit then
    Exit;
  if Pos('%', ComboBoxZoom.Text) > 0 then
    S := Trim(Copy(ComboBoxZoom.Text, 1, Pos('%', ComboBoxZoom.Text) - 1))
  else
    S := Trim(ComboBoxZoom.Text);
  E := StrToInt(S);
  if E < 100 then
    V := E + 10
  else
    V := E + 100;
  if V <= 0 then
    V := 100;
  ActivePictureEdit.Zoom := V / 100;
  ComboBoxZoom.Text := IntToStr(V) + '%';
end;

procedure TMainForm.ZoomOutBtnClick(Sender: TObject);
var
  V, E: integer;
  S: string;
begin
  if not Pictures.CanEdit then
    Exit;
  if Pos('%', ComboBoxZoom.Text) > 0 then
    S := Trim(Copy(ComboBoxZoom.Text, 1, Pos('%', ComboBoxZoom.Text) - 1))
  else
    S := Trim(ComboBoxZoom.Text);
  E := StrToInt(S);
  if E <= 100 then
    V := E - 10
  else
    V := E - 100;
  if V <= 0 then
    V := 100;
  ActivePictureEdit.Zoom := V / 100;
  ComboBoxZoom.Text := IntToStr(V) + '%';
end;

procedure TMainForm.ToolCircleShapeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Shape := psCircle;
end;

procedure TMainForm.ToolColorPickClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptColorPick);
end;

procedure TMainForm.ToolEllClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptEllipse);
end;

procedure TMainForm.ToolFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  //  ActivePictureEdit.FillAndOutline := dmFill;
end;

procedure TMainForm.ToolFillOutlineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  //  ActivePictureEdit.FillAndOutline := dmFillAndOutline;
end;

procedure TMainForm.ToolFloodFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptFloodFill);
end;

procedure TMainForm.ToolLineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptLine);
end;

procedure TMainForm.ToolMaskClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptMask);
end;

procedure TMainForm.ToolMaskEllipseClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.MaskTool := mtEllipse;
end;

procedure TMainForm.ToolMaskFloodFillClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.MaskTool := mtFloodFill;
end;

procedure TMainForm.ToolMaskRectangleClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.MaskTool := mtRectangle;
end;

procedure TMainForm.ToolOutlineClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  //  ActivePictureEdit.FillAndOutline := dmOutline;
end;

procedure TMainForm.ToolPenClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptPen);
end;

procedure TMainForm.ToolPolygonClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptPolygon);
end;

procedure TMainForm.ToolRectangleClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptRectangle);
end;

procedure TMainForm.ToolRectShapeClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Shape := psRect;
end;

procedure TMainForm.ToolEraserClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptEraser);
end;

procedure TMainForm.ToolSprayClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ChangeTool(ptSpray);
end;

procedure TMainForm.PictureMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  DX, DY: integer;
begin
  if not Pictures.CanEdit then
    Exit;
  StatusBar.Panels[2].Text := Format('%3.d : %3.d', [X, Y]);

  DX := ActivePictureEdit.EndPos.X - ActivePictureEdit.StartPos.X;
  DY := ActivePictureEdit.EndPos.Y - ActivePictureEdit.StartPos.Y;
  if DX >= 0 then
    Inc(DX)
  else
    Dec(DX);
  if DY >= 0 then
    Inc(DY)
  else
    Dec(DY);

  StatusBar.Panels[3].Text := Format('%3.d x %3.d', [DX, DY]);
  StatusBar.Panels[4].Text :=
    ColorToString(ActivePicture.GetColor(X, Y));
end;

procedure TMainForm.PictureColorChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  with ActivePictureEdit do
  begin
    PanelOutline.Color := OutlineColor;
    PanelFill.Color := FillColor;
    PanelPaper.Color := PaperColor;
  end;
end;

procedure TMainForm.PictureFileNameChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  StatusBar.Panels[0].Text := ActivePicturePage.Filename;
end;

procedure TMainForm.PictureSizeChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  with ActivePictureEdit do
    StatusBar.Panels[1].Text := Format('%d x %d', [Picture.Width, Picture.Height]);
end;

procedure TMainForm.ViewShowGridExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  if ViewShowGrid.Checked then
    ActivePictureEdit.Options := ActivePictureEdit.Options + [poShowGrid]
  else
    ActivePictureEdit.Options := ActivePictureEdit.Options - [poShowGrid];

  ActivePictureEdit.UpdatePicture;
end;

procedure TMainForm.ViewShowMaskExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  if ViewShowMask.Checked then
    ActivePictureEdit.Options := ActivePictureEdit.Options + [poShowMask]
  else
    ActivePictureEdit.Options := ActivePictureEdit.Options - [poShowMask];

  ActivePictureEdit.UpdatePicture;
end;

procedure TMainForm.ViewShowPreviewExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
end;

procedure TMainForm.PaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  if not Pictures.CanEdit then
    Exit;
  with ActivePictureEdit do
  begin
    if ssLeft in Shift then
      OutlineColor := AColor;
    if ssRight in Shift then
      FillColor := AColor;
    if ssMiddle in Shift then
      PaperColor := AColor;
  end;
end;

procedure TMainForm.PanelFillDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ColorDialog.Color := ActivePictureEdit.FillColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.FillColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.PanelOutlineDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ColorDialog.Color := ActivePictureEdit.OutlineColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.OutlineColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.PanelPaperDblClick(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ColorDialog.Color := ActivePictureEdit.PaperColor;
  if ColorDialog.Execute then
  begin
    ActivePictureEdit.PaperColor := ColorDialog.Color;
  end;
end;

procedure TMainForm.PanelPaperDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin
  if not Pictures.CanEdit then
    Exit;
  if Source is TColorPalette then
  begin
    TPanel(Sender).Color := Palette.PickedColor;
    if Sender = PanelPaper then
      ActivePictureEdit.PaperColor := Palette.PickedColor;
    if Sender = PanelFill then
      ActivePictureEdit.FillColor := Palette.PickedColor;
    if Sender = PanelOutline then
      ActivePictureEdit.OutlineColor := Palette.PickedColor;
  end;
end;

procedure TMainForm.PanelZoomClick(Sender: TObject);
begin

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
    ptPolygon: ToolRectangle.Down := True;
    ptRectangle: ToolEllipse.Down := True;
    ptEllipse: ToolPolygon.Down := True;
  end;
  ChangeTool(Tool);
end;

procedure TMainForm.ChangeTool(Tool: TPictureEditTool);
begin
  if ActivePictureEdit.IsSelection then
    with ActivePictureEdit do
    begin
      Mask(XX1, YY1, XX2, YY2, [ssLeft]);
      IsSelection := False;
      XX1 := 0;
      XX2 := 0;
      YY1 := 0;
      YY2 := 0;
    end;
  ActivePictureEdit.pcount := 0;
  if ActivePictureEdit.Tool = ptPolygon then
    ActivePictureEdit.FinishPolygon;
  ActivePictureEdit.Tool := Tool;
  UpdateToolSettings;
  ToolBarToolsClick(nil);
end;

procedure TMainForm.UpdatePictureToolsEnabled;
var
  AValue: boolean;
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

  ToolBar.Invalidate;
  ToolBarTools.Invalidate;
end;

procedure TMainForm.UpdateAll;
begin
  UpdatePictureToolsEnabled;
  UpdateToolSettings;
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
  if not Pictures.CanEdit then
    Exit;

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

  // PanelToolBar/PanelToolOptions
  PanelSize.Enabled := tsSize in Settings;
  LabelSize.Enabled := tsSize in Settings;

  PanelDensity.Enabled := tsDensity in Settings;
  LabelDensity.Enabled := tsDensity in Settings;

  PanelRoundness.Enabled := tsRoundness in Settings;
  LabelRoundness.Enabled := tsRoundness in Settings;

  PanelTolerance.Enabled := tsTolerance in Settings;
  LabelTolerance.Enabled := tsTolerance in Settings;

  // PanelToolbar/PanelOptions
  PanelShape.Enabled := tsShape in Settings;
  LabelShape.Enabled := tsShape in Settings;

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

  Palette.LoadPalette(vConfigurations.MyDirectory + 'default.pal');

  // Main Form
  Caption := lieMain;

  // Menus
  MenuItemFile.Caption := lieMenuFile;
  MenuItemEdit.Caption := lieMenuEdit;
  MenuItemPicture.Caption := lieMenuPicture;
  MenuItemMask.Caption := lieMenuMask;
  MenuItemView.Caption := lieMenuView;
  MenuItemHelp.Caption := lieMenuHelp;

  // Actions
  FileNew.Caption := lieFileNew;
  FileOpen.Caption := lieFileOpen;
  FileSave.Caption := lieFileSave;
  FileSaveAs.Caption := lieFileSaveAs;
  FileExportAsLRS.Caption := lieFileExportAsLRS;
  FileClose.Caption := lieFileClose;
  Rotate90.Caption := lieRotate90;
  Rotate180.Caption := lieRotate180;
  Rotate270.Caption := lieRotate270;
  RotateCustom.Caption := lieRotateCustom;
  FlipVertically.Caption := lieFlipVertically;
  FlipHorizontally.Caption := lieFlipHorizontally;
  PictureResizePaper.Caption := liePictureResizePaper;
  PictureResize.Caption := liePictureResize;
  ColorsGrayscale.Caption := lieColorsGrayscale;
  ColorsInvert.Caption := lieColorsInvert;
  ColorsDisable.Caption := lieColorsDisable;
  ViewShowMask.Caption := lieViewShowMask;
  ViewShowGrid.Caption := lieViewShowGrid;
  MaskInvert.Caption := lieMaskInvert;
  MaskRemove.Caption := lieMaskRemove;
  EditCopy.Caption := lieEditCopy;
  EditCut.Caption := lieEditCut;
  EditDelete.Caption := lieEditDelete;
  EditPaste.Caption := lieEditPaste;
  EditRedo.Caption := lieEditRedo;
  EditSelectAll.Caption := lieEditSelectAll;
  EditUndo.Caption := lieEditUndo;

  // Hints for Actions
  FileNew.Hint := lieHintFileNew;
  FileOpen.Hint := lieHintFileOpen;
  FileSave.Hint := lieHintFileSave;
  FileSaveAs.Hint := lieHintFileSaveAs;
  FileExportAsLRS.Hint := lieHintFileExportAsLRS;
  FileClose.Hint := lieHintFileClose;
  Rotate90.Hint := lieHintRotate90;
  Rotate180.Hint := lieHintRotate180;
  Rotate270.Hint := lieHintRotate270;
  RotateCustom.Hint := lieHintRotateCustom;
  FlipVertically.Hint := lieHintFlipVertically;
  FlipHorizontally.Hint := lieHintFlipHorizontally;
  PictureResizePaper.Hint := lieHintPictureResizePaper;
  PictureResize.Hint := lieHintPictureResize;
  ColorsGrayscale.Hint := lieHintColorsGrayscale;
  ColorsInvert.Hint := lieHintColorsInvert;
  ColorsDisable.Hint := lieHintColorsDisable;
  ViewShowMask.Hint := lieHintViewShowMask;
  ViewShowGrid.Hint := lieHintViewShowGrid;
  MaskInvert.Hint := lieHintMaskInvert;
  MaskRemove.Hint := lieHintMaskRemove;
  EditCopy.Hint := lieHintEditCopy;
  EditCut.Hint := lieHintEditCut;
  EditDelete.Hint := lieHintEditDelete;
  EditPaste.Hint := lieHintEditPaste;
  EditRedo.Hint := lieHintEditRedo;
  EditSelectAll.Hint := lieHintEditSelectAll;
  EditUndo.Hint := lieHintEditUndo;

  //Labels
  //  LabelZoom.Caption := lieLabelZoom;
  LabelShape.Caption := lieLabelShape;
  LabelFillOutline.Caption := lieLabelFillOutline;
  LabelMaskTool.Caption := lieLabelMaskTool;
  LabelOutline.Caption := lieLabelOutline;
  LabelFill.Caption := lieLabelFill;
  LabelPaper.Caption := lieLabelPaper;
  LabelSize.Caption := lieLabelSize;
  LabelRoundness.Caption := lieLabelRoundness;
  LabelDensity.Caption := lieLabelDensity;
  LabelTolerance.Caption := lieLabelTolerance;

  //Hints for Tools
  ToolSpray.Hint := lieHintToolSpray;
  ToolFloodFill.Hint := lieHintToolFloodFill;
  ToolEraser.Hint := lieHintToolEraser;
  ToolPen.Hint := lieHintToolPen;
  ToolColorPick.Hint := lieHintToolColorPick;
  ToolMask.Hint := lieHintToolMask;
  ToolLine.Hint := lieHintToolLine;
  ToolRectangle.Hint := lieHintToolPolygon;
  ToolPolygon.Hint := lieHintToolEllipse;
  ToolEllipse.Hint := lieHintToolRectangle;

  //File Dialogs
  ColorDialog.Title := lieColorDialog;
  OpenPictureDialog.Title := lieOpenPictureDialog;
  SavePictureDialog.Title := lieSavePictureDialog;
  ExportResourceDialog.Title := lieExportResourceDialog;
  TextEditor := TTextEditor.Create(Self);
  TextEditor.Parent := Self;
  FontListBox.Items := Screen.Fonts;
  FontListBox.ItemIndex := FontListBox.Items.IndexOf(UTF8Encode(Screen.MenuFont.Name));
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF EnableTestForm}
  TestForm.Show;
  {$ENDIF}
end;

procedure TMainForm.MaskInvertExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.InvertMask;
end;

procedure TMainForm.MaskRemoveExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.RemoveMask;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.ColorReplace(BtnFromColor.ButtonColor, BtnToColor.ButtonColor);
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
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.ClipPaperToMask;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  if NewDialogForm.ShowModal = mrOk then
  begin
    with NewDialogForm do
      Pictures.New(UpDownWidth.Position, UpDownHeight.Position,
        ColorButtonPaper.ButtonColor);
  end;
end;

procedure TMainForm.FileNewOnStart;
var
  i: integer;
begin
  // With OS X app, ParamStr not meaningful unless launched with --args switch.
  if (ParamCount > 0) {$IFDEF DARWIN} and (Copy(ParamStr(1), 1, 4) <> '-psn') {$ENDIF} then
  begin
    for i := 1 to ParamCount - 1 do
      Pictures.Load(ParamStr(i));
  end
  else
    Pictures.New(520, 390, $F1EFDA);
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  I: integer;
begin
  Pictures.CloseAll;
  if OpenPictureDialog.Execute then
  begin
    for I := 0 to Pred(OpenPictureDialog.Files.Count) do
      //Pictures.Load(OpenPictureDialog.Files[I]);
      OpenImageFile(OpenPictureDialog.Files[I]);
  end;
end;

procedure TMainForm.OpenImageFile(FileName: string);
begin
  if UpperCase(ExtractFileExt(FileName)) = '.ICO' then
    SelectIconSizeForm.ShowModal;
  Pictures.Load(FileName);
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
  lExt, Value: string;
begin
  ExportResourceDialog.Filter := 'Lazarus resource (*.lrs)|*.lrs|All files (*.*)|*.*';
  if ExportResourceDialog.Execute then
  begin
    Value := ExtractFileName(ExportResourceDialog.FileName);
    lExt := ExtractFileExt(Value);
    Value := Copy(Value, 1, Length(Value) - Length(lExt));
    if InputQuery(lieSetResource, lieResourceName, Value) then
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
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.FlipHorizontally;
end;

procedure TMainForm.FlipVerticallyExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.FlipVertically;
end;

procedure TMainForm.FontListBoxChange(Sender: TObject);
begin
  //ActivePictureEdit.Canvas.Font.Name := FontListBox.Text;
  ActivePictureEdit.Picture.Canvas.Font.Name := FontListBox.Text;
  ActivePictureEdit.Picture.Canvas.Font.Color := PanelOutline.Color;
  if ActivePictureEdit.Tool <> ptText then
    TextEditor.StopEdit;
end;

procedure TMainForm.FontListBoxClick(Sender: TObject);
begin

end;

procedure TMainForm.FontSizeChange(Sender: TObject);
begin
  ActivePictureEdit.Picture.Canvas.Font.Size := FontSize.Value;
  ActivePictureEdit.Picture.Canvas.Font.Color := PanelOutline.Color;
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
  V, E: integer;
  S: string;
begin
  if not Pictures.CanEdit then
    Exit;
  if Pos('%', ComboBoxZoom.Text) > 0 then
    S := Trim(Copy(ComboBoxZoom.Text, 1, Pos('%', ComboBoxZoom.Text) - 1))
  else
    S := Trim(ComboBoxZoom.Text);

  Val(S, V, E);
  if V <= 0 then
    V := 100;
  ActivePictureEdit.Zoom := V / 100;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  //if not Pictures.CanEdit then
  //  Exit;
  ActivePictureEdit.Copy;
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Cut;
end;

procedure TMainForm.EditDeleteExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Delete;
end;

procedure TMainForm.EditDensityChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.RandomDensity := EditDensity.Value / 100;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  Pictures.Paste;
end;

procedure TMainForm.EditRoundnessChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.RectangleRoundness := EditRoundness.Value;
end;

procedure TMainForm.EditSelectAllExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.SelectAll;
  MenuItemCopy.Enabled := True;
  MenuItemPaste.Enabled := True;
  MenuItemDelete.Enabled := True;
  MenuItemCopy.OnClick := @EditCopyExecute;
  MenuItemPaste.OnClick := @EditPasteExecute;
  MenuItemDelete.OnClick := @EditDeleteExecute;
end;

procedure TMainForm.ColorsInvertExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Invert;
end;

procedure TMainForm.ComboBoxZoomEditingDone(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ComboBoxZoomChange(nil);
  ComboBoxZoom.Text := Format('%d %%', [Round(ActivePictureEdit.Zoom * 100)]);
end;

procedure TMainForm.ColorsGrayscaleExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Grayscale;
end;

procedure TMainForm.ColorsDisableExecute(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Disable;
end;

procedure TMainForm.checkFuzzyChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Fuzzy := checkFuzzy.Checked;
end;

procedure TMainForm.BtnToColorClick(Sender: TObject);
begin
  ActivePictureEdit.ChangeColor(BtnFromColor.ButtonColor, BtnToColor.ButtonColor);
end;

procedure TMainForm.EditSizeChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.Size := EditSize.Value;
end;

procedure TMainForm.EditToleranceChange(Sender: TObject);
begin
  if not Pictures.CanEdit then
    Exit;
  ActivePictureEdit.FloodFillTolerance := EditTolerance.Value / 100;
end;


initialization

  {$I main.lrs}

end.

