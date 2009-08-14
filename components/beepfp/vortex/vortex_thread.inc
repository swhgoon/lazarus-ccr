
(*
 * LibVortex: C Header file to Free Pascal translation.
 * Copyright (C) 2009, Wimpie Nortje <wimpienortje@gmail.com>
 *)

(*
 *  LibVortex:  A BEEP (RFC3080/RFC3081) implementation.
 *  Copyright (C) 2008 Advanced Software Production Line, S.L.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *  
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to develop
 *  proprietary applications using this library without any royalty or
 *  fee but returning back any change, improvement or addition in the
 *  form of source code, project image, documentation patches, etc.
 *
 *  For commercial support on build BEEP enabled solutions contact us:
 *          
 *      Postal address:
 *         Advanced Software Production Line, S.L.
 *         C/ Antonio Suarez Nº 10, 
 *         Edificio Alius A, Despacho 102
 *         Alcalá de Henares 28802 (Madrid)
 *         Spain
 *
 *      Email address:
 *         info@aspl.es - http://www.aspl.es/vortex
 *)

(**
 * \addtogroup vortex_thread
 * @{
 *)

//axl_bool vortex_thread_create  (VortexThread      * thread_def,
//                                VortexThreadFunc    func,
//                                axlPointer          user_data,
//                                ...);
function   vortex_thread_create  (thread_def : PVortexThread;
                                  func       : TVortexThreadFunc;
                                  user_data  : TaxlPointer;
                                  args       : array of const):Taxl_bool;
                                  cdecl;external External_library name 'vortex_thread_create';
function   vortex_thread_create  (thread_def : PVortexThread;
                                  func       : TVortexThreadFunc;
                                  user_data  : TaxlPointer):Taxl_bool;
                                  cdecl;external External_library name 'vortex_thread_create';

//axl_bool vortex_thread_destroy (VortexThread      * thread_def,
//                                axl_bool            free_data);
function   vortex_thread_destroy (thread_def : PVortexThread;
                                  free_data  : Taxl_bool):Taxl_bool;
                                  cdecl;external External_library name 'vortex_thread_destroy';

//void     vortex_thread_set_create (VortexThreadCreateFunc  create_fn);
procedure  vortex_thread_set_create (create_fn: TVortexThreadCreateFunc);
                                     cdecl;external External_library name 'vortex_thread_set_create';

//void     vortex_thread_set_destroy(VortexThreadDestroyFunc destroy_fn);
procedure  vortex_thread_set_destroy(destroy_fn:TVortexThreadDestroyFunc);
                                     cdecl;external External_library name 'vortex_thread_set_destroy';

//axl_bool vortex_mutex_create   (VortexMutex       * mutex_def);
function   vortex_mutex_create   (mutex_def:PVortexMutex):Taxl_bool;
                                  cdecl;external External_library name 'vortex_mutex_create';

//axl_bool vortex_mutex_destroy  (VortexMutex       * mutex_def);
function   vortex_mutex_destroy  (mutex_def : PVortexMutex):Taxl_bool;
                                  cdecl;external External_library name 'vortex_mutex_destroy';

//void     vortex_mutex_lock     (VortexMutex       * mutex_def);
procedure  vortex_mutex_lock     (mutex_def : PVortexMutex);
                                  cdecl;external External_library name 'vortex_mutex_lock';

//void     vortex_mutex_unlock   (VortexMutex       * mutex_def);
procedure  vortex_mutex_unlock   (mutex_def:PVortexMutex);
                                  cdecl;external External_library name 'vortex_mutex_unlock';

//axl_bool vortex_cond_create    (VortexCond        * cond);
function   vortex_cond_create    (cond : PVortexCond):Taxl_bool;
                                  cdecl;external External_library name 'vortex_cond_create';

//void     vortex_cond_signal    (VortexCond        * cond);
procedure  vortex_cond_signal    (cond : PVortexCond);
                                  cdecl;external External_library name 'vortex_cond_signal';

//void     vortex_cond_broadcast (VortexCond        * cond);
procedure  vortex_cond_broadcast (cond : PVortexCond);
                                  cdecl;external External_library name 'vortex_cond_broadcast';


(**
 * @brief Useful macro that allows to perform a call to
 * vortex_cond_wait registering the place where the call was started
 * and ended.
 * 
 * @param c The cond variable to use.
 * @param mutex The mutex variable to use.
 *)
{$if 0}
#define VORTEX_COND_WAIT(c, mutex) do{\
vortex_cond_wait (c, mutex);\
}while(0);
{$endif}

//axl_bool           vortex_cond_wait       (VortexCond        * cond,
//                                           VortexMutex       * mutex);
function             vortex_cond_wait       (cond         : PVortexCond;
                                             mutex        : PVortexMutex):Taxl_bool;
                                             cdecl;external External_library name 'vortex_cond_wait';

(**
 * @brief Useful macro that allows to perform a call to
 * vortex_cond_timewait registering the place where the call was
 * started and ended. 
 * 
 * @param r Wait result
 * @param c The cond variable to use.
 * @param mutex The mutex variable to use.
 * @param m The amount of microseconds to wait.
 *)
{$if 0}
#define VORTEX_COND_TIMEDWAIT(r, c, mutex, m) do{\
r = vortex_cond_timedwait (c, mutex, m);\
}while(0)
{$endif}

//axl_bool           vortex_cond_timedwait  (VortexCond        * cond,
//                                           VortexMutex       * mutex,
//                                           long int            microseconds);
function             vortex_cond_timedwait  (cond         : PVortexCond;
                                             mutex        : PVortexMutex;
                                             microseconds : int64):Taxl_bool;
                                             cdecl;external External_library name 'vortex_cond_timedwait';

//void               vortex_cond_destroy    (VortexCond        * cond);
procedure            vortex_cond_destroy    (cond : PVortexCond);
                                             cdecl;external External_library name 'vortex_cond_destroy';


//VortexAsyncQueue * vortex_async_queue_new       (void);
function             vortex_async_queue_new       :PVortexAsyncQueue;
                                                   cdecl;external External_library name 'vortex_async_queue_new';

//void               vortex_async_queue_push    (VortexAsyncQueue * queue,
//                                               axlPointer         data);
procedure            vortex_async_queue_push    (queue : PVortexAsyncQueue;
                                                 data  : TaxlPointer);
                                                 cdecl;external External_library name 'vortex_async_queue_push';

//void               vortex_async_queue_priority_push(VortexAsyncQueue * queue,
//                                                    axlPointer         data);
procedure            vortex_async_queue_priority_push(queue : PVortexAsyncQueue;
                                                      data  : TaxlPointer);
                                                      cdecl;external External_library name 'vortex_async_queue_priority_push';

//axlPointer         vortex_async_queue_pop       (VortexAsyncQueue * queue);
function             vortex_async_queue_pop       (queue : PVortexAsyncQueue):TaxlPointer;
                                                   cdecl;external External_library name 'vortex_async_queue_pop';

//axlPointer         vortex_async_queue_timedpop  (VortexAsyncQueue * queue,
//                                                 long int           microseconds);
function             vortex_async_queue_timedpop  (queue        : PVortexAsyncQueue;
                                                   microseconds : int64):TaxlPointer;
                                                   cdecl;external External_library name 'vortex_async_queue_timedpop';

//int                vortex_async_queue_length    (VortexAsyncQueue * queue);
function             vortex_async_queue_length    (queue : PVortexAsyncQueue):longint;
                                                   cdecl;external External_library name 'vortex_async_queue_length';

//int                vortex_async_queue_waiters   (VortexAsyncQueue * queue);
function             vortex_async_queue_waiters   (queue : PVortexAsyncQueue):longint;
                                                   cdecl;external External_library name 'vortex_async_queue_waiters';

//int                vortex_async_queue_items     (VortexAsyncQueue * queue);
function             vortex_async_queue_items     (queue : PVortexAsyncQueue):longint;
                                                   cdecl;external External_library name 'vortex_async_queue_items';

//void               vortex_async_queue_ref       (VortexAsyncQueue * queue);
procedure            vortex_async_queue_ref       (queue : PVortexAsyncQueue)
                                                   cdecl;external External_library name 'vortex_async_queue_ref';

//void               vortex_async_queue_unref     (VortexAsyncQueue * queue);
procedure            vortex_async_queue_unref     (queue : PVortexAsyncQueue)
                                                   cdecl;external External_library name 'vortex_async_queue_unref';

//void               vortex_async_queue_foreach   (VortexAsyncQueue         * queue,
//                                                 VortexAsyncQueueForeach    foreach_func,
//                                                 axlPointer                 user_data);
procedure            vortex_async_queue_foreach   (queue        : PVortexAsyncQueue;
                                                   foreach_func : TVortexAsyncQueueForeach;
                                                   user_data    : TaxlPointer);
                                                   cdecl;external External_library name 'vortex_async_queue_foreach';

(**
 * @}
 *)

