{
lclexports.dpr

Export library to use LCL functions in other languages

This file is part of the LCL Exports library.

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library
         
Copyright (C) 2008 Felipe Monteiro de Carvalho
}
library lclexports;

{$ifndef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
  Interfaces,    // Necessary to create the widgetset object
  Classes,
  formsexports,
  controlsexports,
  stdctrlsexports;

exports
  { ControlsExports }
  TControl_GetOnClick,
  TControl_SetOnClick,
  TControl_GetCaption,
  TControl_SetCaption,
  TControl_GetLeft,
  TControl_SetLeft,
  TControl_GetHeight,
  TControl_SetHeight,
  TControl_GetHint,
  TControl_SetHint,
  TControl_GetTop,
  TControl_SetTop,
  TControl_GetWidth,
  TControl_SetWidth,
  TWinControl_GetParent,
  TWinControl_SetParent,
  { FormsExports }
  Application_CreateForm,
  Application_Initialize,
  Application_Run,
  TCustomForm_Close,
  TCustomForm_Show,
  TForm_Create,
  { StdCtrlsExports }
  TButton_Create,
  TLabel_Create;

begin
end.

