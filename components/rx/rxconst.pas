{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit rxconst;

interface

{$I RX.INC}

uses LMessages, Controls;

const
  RX_VERSION = $0002004B;  { 2.75 }

const
{ Command message for Speedbar editor }
  CM_SPEEDBARCHANGED = CM_BASE + 80;
{ Command message for TRxSpeedButton }
  CM_RXBUTTONPRESSED = CM_BASE + 81;
{ Command messages for TRxWindowHook }
  CM_RECREATEWINDOW  = CM_BASE + 82;
  CM_DESTROYHOOK     = CM_BASE + 83;
{ Notify message for TRxTrayIcon }
  CM_TRAYICON        = CM_BASE + 84;

const
  crHand     = TCursor(14000);
  crDragHand = TCursor(14001);

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS uses this value }
  PaletteMask = $02000000;

resourcestring
  sBrowse          = 'Browse';
  sDefaultFilter   = 'All files (*.*)|*.*';
  sDateDlgTitle    = 'Select a Date';
  sNextYear        = 'Next Year|';
  sNextMonth       = 'Next Month|';
  sPrevYear        = 'Previous Year|';
  sPrevMonth       = 'Previous Month|';
  sNotImplemented  = 'Function not yet implemented';
  sFileNotExec     = 'File specified is not an executable file, dynamic-link library, or icon file';
  sLoadLibError    = 'Could not load ''%s'' library';
  sDetails         = 'Details';
  sWindowsIcoFiles = 'Windows Ico files (*.ico)|*.ico|All files (*.*)|*.*';
  sToCurDate       = 'Set current date';

  //TDualListDialog
  SDualListSrcCaption   = 'Source';
  SDualListDestCaption  = 'Destination';
  SDualListCaption      = 'Dual list dialog';

  //TToolPanelSetupForm
  sToolPanelSetup       = 'Tool panel setup';
  sVisibleButtons       = 'Visible buttons';
  sOptions              = 'Options';
  sAvaliableButtons     = 'Avaliable buttons';
  sShowCaption          = 'Show caption';
  sToolBarStyle         = 'Tool bar style';
  sToolBarStyle1        = 'Standart';
  sToolBarStyle2        = 'Windows XP';
  sToolBarStyle3        = 'Native';
  sFlatButtons          = 'Flat buttons';
  sTransparent          = 'Transparent';
  sShowHint             = 'Show hint';
  sButtonAlign          = 'Button align';
  sButtonAlign1         = 'None';
  sButtonAlign2         = 'Left';
  sButtonAlign3         = 'Rignt';
  sGTKWidgetSet         = 'GTK widget set';
  sGTK2WidgetSet        = 'GTK 2 widget set';
  sWin32_64WidgetSet    = 'Win32/Win64 widget set';
  sWinCEWidgetSet       = 'WinCE widget set';
  sCarbonWidgetSet      = 'Carbon widget set';
  sQTWidgetSet          = 'QT widget set';
  sFpGUIWidgetSet       = 'FpGUI widget set';
  sOtherGUIWidgetSet    = 'Other gui';
  sAppVersion           = 'Version : ';
  sLCLVersion           = 'LCL Version: ';
  sFpcVersion           = 'FPC version : ';
  sTargetCPU            = 'Target CPU : ';
  sTargetOS             = 'Target OS : ';
  sAbout                = 'About';
  sGeneral              = 'General';
  sLicense              = 'License';


implementation

uses Forms;

{initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'RX_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'RX_DRAGCUR'); }
end.
