/*
forms.h

C/C++ header for the LCL Exports library

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library
         
Copyright (C) 2008 Felipe Monteiro de Carvalho
*/

#ifndef LAZARUS_FORMS_H
#define LAZARUS_FORMS_H

#include "system.h"
#include "classes.h"
#include "controls.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Application */

__cdecl void Application_CreateForm(void** Reference);
__cdecl void Application_Initialize();
__cdecl void Application_Run();

/* TCustomForm */

typedef void* TCustomFormH;

__cdecl void TCustomForm_Close(TCustomFormH Self);
__cdecl void TCustomForm_Show(TCustomFormH Self);

/* TForm */

typedef void* TFormH;

__cdecl TFormH TForm_Create(TComponentH Owner);

#ifdef __cplusplus
}
#endif

#endif	/* !LAZARUS_FORMS_H */
