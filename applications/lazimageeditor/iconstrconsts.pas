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

  Author: Laurent Jacques

  Abstract:
    This unit contains all resource strings.

Note: All resource strings should be prefixed with 'lie' (Lazarus Image Editor)

}
unit IconStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

resourcestring

  // standard Buttons
  lieButtonClose = 'Close';
  lieButtonOK = 'OK';
  lieButtonCancel = 'Cancel';

  //Messages
  lieSaveChanges = 'Save changes to %s?';
  lieNew = 'New';
  lieSetResource = 'Set resource name';
  lieResourceName = 'Resource name: ';

  // Main Form
  lieMain = 'Lazarus Image Editor';

  // main bar menu
  lieMenuFile = '&File';
  lieMenuEdit = '&Edit';
  lieMenuPicture = '&Picture';
  lieMenuMask = 'Mask';
  lieMenuView = 'View';
  lieMenuHelp = '&Help';

  // Action Lists
  lieFileNew = '&New...';
  lieFileOpen = '&Open...';
  lieFileSave = '&Save';
  lieFileSaveAs = 'Save &As...';
  lieFileExportAsLRS = '&Export As *.lrs...';
  lieFileClose = '&Close';
  lieRotate90 = '90?Clockwise';
  lieRotate180 = '180?Clockwise';
  lieRotate270 = '270?Clockwise';
  lieRotateCustom = 'Custom...';
  lieFlipVertically = 'Vertically';
  lieFlipHorizontally = 'Horizontally';
  liePictureResizePaper = 'Resize Paper...';
  liePictureResize = 'Resize...';
  lieColorsGrayscale = 'Grayscale';
  lieColorsInvert = 'Invert';
  lieColorsDisable = 'Disable';
  lieViewShowPreview = 'Show Preview';
  lieViewShowMask = 'Show Mask';
  lieViewShowGrid = 'Show Grid';
  lieMaskInvert = 'Invert';
  lieMaskRemove = 'Remove';
  lieEditCopy = '&Copy';
  lieEditCut = 'Cu&t';
  lieEditDelete = '&Delete';
  lieEditPaste = '&Paste';
  lieEditRedo = '&Redo';
  lieEditSelectAll = 'Select &All';
  lieEditUndo = '&Undo';

  // hints For Actions list
  lieHintFileNew = 'New';
  lieHintFileOpen = 'Open';
  lieHintFileSave = 'Save';
  lieHintFileSaveAs = 'Save As';
  lieHintFileExportAsLRS = 'Export As *.lrs';
  lieHintFileClose = 'Close';
  lieHintRotate90 = '90?Clockwise';
  lieHintRotate180 = '180?Clockwise';
  lieHintRotate270 = '270?Clockwise';
  lieHintRotateCustom = 'Custom';
  lieHintFlipVertically = 'Vertically';
  lieHintFlipHorizontally = 'Horizontally';
  lieHintPictureResizePaper = 'Resize Paper';
  lieHintPictureResize = 'Resize';
  lieHintColorsGrayscale = 'Grayscale';
  lieHintColorsInvert = 'Invert';
  lieHintColorsDisable = 'Disable';
  lieHintViewShowPreview = 'Show Preview';
  lieHintViewShowMask = 'Show Mask';
  lieHintViewShowGrid = 'Show Grid';
  lieHintMaskInvert = 'Invert';
  lieHintMaskRemove = 'Remove';
  lieHintEditCopy = 'Copy';
  lieHintEditCut = 'Cut';
  lieHintEditDelete = 'Delete';
  lieHintEditPaste = 'Paste';
  lieHintEditRedo = 'Redo';
  lieHintEditSelectAll = 'Select All';
  lieHintEditUndo = 'Undo';

  // Hints for Tools
  lieHintToolSpray = 'Spray';
  lieHintToolFloodFill = 'Flood Fill';
  lieHintToolEraser = 'Eraser/Replacer';
  lieHintToolPen = 'Pen';
  lieHintToolColorPick = 'Color Pick';
  lieHintToolMask = 'Mask';
  lieHintToolLine = 'Line';
  lieHintToolPolygon = 'Polygon';
  lieHintToolEllipse = 'Ellipse';
  lieHintToolRectangle = 'Rectangle/Round rectangle';

  //Labels
  lieLabelZoom = 'Zoom:';
  lieLabelShape = 'Shape:';
  lieLabelFillOutline = 'Fill, Outline:';
  lieLabelMaskTool = 'Mask Tool:';
  lieLabelOutline = 'Outline:';
  lieLabelFill = 'Fill:';
  lieLabelPaper = 'Paper:';
  lieLabelSize = 'Size:';
  lieLabelRoundness = 'Roundness:';
  lieLabelDensity = 'Density:';
  lieLabelTolerance = 'Tolerance:';

  // Dialog About
  lieAbouDialog = 'About Lazarus Image Editor';
  lieLabelVersion = 'Version: 0.9';
  lieLabelAuthor = 'Authors: Tom Gregorovic, Felipe Monteiro de Carvalho, Yang JiXian';

  // Dialog New
  lieNewDialog = 'New picture';
  lieLabelWidth = 'Width:';
  lieLabelHeight = 'Height:';
  lieLabelPaperColor = 'Paper color:';
  lieColorButtonPaper = 'Change...';

  // Dialog Preview
  liePreviewDialog = 'Preview';

  // Dialog Resize
  lieResizeDialog = 'Resize picture';
  lieGroupBoxProperties = 'Properties';
  lieLabelNewWidth = 'New width:';
  lieLabelNewHeight = 'New height:';
  lieCheckBoxAspectRatio = 'Preserve aspect ratio';
  lieGroupBoxStretchMethod = 'Stretch method';
  lieRadioButtonTruncate = 'Truncate';
  lieRadioButtonSmooth = 'Smooth:';
  lieAreapixel = 'Area pixel';
  lieBilinear = 'Bilinear';
  lieBicubic = 'Bicubic';

  // Dialog Resize Paper
  lielieResizePaperDialog = 'Resize paper';
  lieLabelPicturePosition = 'Picture position:';
  lieLabelPaperWidth = 'Paper width:';
  lieLabelPaperHeight = 'Paper height:';
//  lieCheckBoxAspectRatio = 'Preserve aspect ratio';  in dialog Resize
//  lieLabelPaperColor = 'Paper color:';  in dialog New
//  lieColorButtonPaper = 'Change...';  in dialog New
  lieTopLeft = 'Top Left';
  lieTopCenter = 'Top Center';
  lieTopRight = 'Top Right';
  lieCenterLeft = 'Center Left';
  lieCentered = 'Centered';
  lieCenterRight = 'Center Right';
  lieBottomLeft = 'Bottom Left';
  lieBottomCenter = 'Bottom Center';
  lieBottomRight = 'Bottom Right';

  //File Dialogs
  lieColorDialog = 'Select color';
  lieOpenPictureDialog = 'Open existing file';
  lieSavePictureDialog = 'Save file as';
  lieExportResourceDialog = 'Export as lazarus resource';

implementation

end.






