/*
stdctrls.h

C/C++ header for the LCL Exports library

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library
         
Copyright (C) 2008 Felipe Monteiro de Carvalho
*/

#ifndef LAZARUS_STDCTRLS_H
#define LAZARUS_STDCTRLS_H

#include "system.h"
#include "classes.h"
#include "controls.h"

#ifdef __cplusplus
extern "C" {
#endif

/* TButton */

typedef void* TButtonH;

__cdecl TButtonH TButton_Create(TComponentH TheOwner);

/* TLabel */

typedef void* TLabelH;

__cdecl TLabelH TLabel_Create(TComponentH TheOwner);

#ifdef __cplusplus
}
#endif

#endif	/* !LAZARUS_STDCTRLS_H */
