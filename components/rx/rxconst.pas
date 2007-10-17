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
//const
  {$I rxstrconsts.inc}
  
implementation

uses Forms;

initialization
{  Screen.Cursors[crHand] := LoadCursor(hInstance, 'RX_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'RX_DRAGCUR'); }
end.
