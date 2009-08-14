
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


//typedef struct _VortexThreadPool VortexThreadPool;
type
  VortexThreadPool = Pointer;


//void    vortex_thread_pool_init    (VortexCtx * ctx,
//                                    int  max_threads);
procedure vortex_thread_pool_init    (ctx        : PVortexCtx;
                                      max_threads: longint);
                                      cdecl;external External_library name 'vortex_thread_pool_init';

//void    vortex_thread_pool_exit    (VortexCtx * ctx);
procedure vortex_thread_pool_exit    (ctx : PVortexCtx);
                                      cdecl;external External_library name 'vortex_thread_pool_exit';

//void    vortex_thread_pool_being_closed        (VortexCtx * ctx);
procedure vortex_thread_pool_being_closed        (ctx:PVortexCtx);
                                                  cdecl;external External_library name 'vortex_thread_pool_being_closed';

//void    vortex_thread_pool_new_task            (VortexCtx        * ctx,
//                                 					      VortexThreadFunc   func,
//                                					      axlPointer         data);
procedure vortex_thread_pool_new_task            (ctx  : PVortexCtx;
                                                  func : TVortexThreadFunc;
                                                  data : TaxlPointer);
                                                  cdecl;external External_library name 'vortex_thread_pool_new_task';

//int     vortex_thread_pool_get_running_threads (VortexCtx        * ctx);
function  vortex_thread_pool_get_running_threads (ctx : PVortexCtx):longint;
                                                  cdecl;external External_library name 'vortex_thread_pool_get_running_threads';

//void    vortex_thread_pool_set_num             (int  number);
procedure vortex_thread_pool_set_num             (number : longint);
                                                  cdecl;external External_library name 'vortex_thread_pool_set_num';

//int     vortex_thread_pool_get_num             (void);
function  vortex_thread_pool_get_num:longint;     cdecl;external External_library name 'vortex_thread_pool_get_num';

//void    vortex_thread_pool_set_exclusive_pool  (VortexCtx        * ctx,
//                                                axl_bool           value);
procedure vortex_thread_pool_set_exclusive_pool  (ctx : PVortexCtx;
                                                  value:Taxl_bool);
                                                  cdecl;external External_library name 'vortex_thread_pool_set_exclusive_pool';

