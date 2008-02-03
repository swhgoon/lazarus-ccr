/*
controls.h

C/C++ header for the LCL Exports library

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library
         
Copyright (C) 2008 Felipe Monteiro de Carvalho
*/

#ifndef LAZARUS_CONTROLS_H
#define LAZARUS_CONTROLS_H

#include "system.h"

#ifdef __cplusplus
extern "C" {
#endif

/* TControl */

typedef void* TControlH;

/* Helper macros to access the parameters of an event */
#ifdef __BORLANDC__

typedef __fastcall void (*TNotifyEvent) (void* Self, TObjectH Sender);

#define FASTCALL_TNOTIFYEVENT_START(EventName) \
 __fastcall void EventName(void* Self, TObjectH Sender){

#elif __GNUC__

#ifdef __i386__

typedef void (*TNotifyEvent) (void);

#define FASTCALL_TNOTIFYEVENT_START(EventName) \
 void EventName(){ \
 void* Self;  \
 TObjectH Sender; \
 __asm__ __volatile__ ("nop"  \
          : "=a" (Self), "=d" (Sender) \
          : "a" (Self), "d" (Sender) \
          );


#else

typedef void (*TNotifyEvent) (void* Self, TObjectH Sender);

#define FASTCALL_TNOTIFYEVENT_START(EventName) \
 void EventName(void* Self, TObjectH Sender){

#endif

#endif

__cdecl TNotifyEvent TControl_GetOnClick(TControlH Self);
__cdecl TControl_SetOnClick(TControlH Self, TNotifyEvent AValue);

__cdecl char* TControl_GetCaption(TControlH Self);
__cdecl TControl_SetCaption(TControlH Self, char* AValue);

__cdecl Integer TControl_GetLeft(TControlH Self);
__cdecl TControl_SetLeft(TControlH Self, Integer AValue);
__cdecl Integer TControl_GetHeight(TControlH Self);
__cdecl TControl_SetHeight(TControlH Self, Integer AValue);
__cdecl char* TControl_GetHint(TControlH Self);
__cdecl TControl_SetHint(TControlH Self, char* AValue);
__cdecl Integer TControl_GetTop(TControlH Self);
__cdecl TControl_SetTop(TControlH Self, Integer AValue);
__cdecl Integer TControl_GetWidth(TControlH Self);
__cdecl TControl_SetWidth(TControlH Self, Integer AValue);

/* TWinControl */

typedef void* TWinControlH;

__cdecl TWinControlH TWinControl_GetParent(TWinControlH Self);
__cdecl TWinControl_SetParent(TWinControlH Self, TWinControlH AValue);

#ifdef __cplusplus
}
#endif

#endif	/* !LAZARUS_CONTROLS_H */
