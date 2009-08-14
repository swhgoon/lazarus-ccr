
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

//VortexQueue * vortex_queue_new         (void);
function        vortex_queue_new:PVortexQueue;
                                          cdecl;external External_library name 'vortex_queue_new';

//axl_bool      vortex_queue_is_empty    (VortexQueue * queue);
function        vortex_queue_is_empty    (queue : PVortexQueue):Taxl_bool;
                                          cdecl;external External_library name 'vortex_queue_is_empty';

//unsigned int  vortex_queue_get_length  (VortexQueue * queue);
function        vortex_queue_get_length  (queue : PVortexQueue):longword;
                                          cdecl;external External_library name 'vortex_queue_get_length';

//axl_bool      vortex_queue_push        (VortexQueue * queue,
//                                        axlPointer data);
function        vortex_queue_push        (queue : PVortexQueue;
                                          data  : TaxlPointer):Taxl_bool;
                                          cdecl;external External_library name 'vortex_queue_push';

//axl_bool      vortex_queue_head_push   (VortexQueue * queue,
//                                        axlPointer data);
function        vortex_queue_head_push   (queue : PVortexQueue;
                                          data  : TaxlPointer):Taxl_bool;
                                          cdecl;external External_library name 'vortex_queue_head_push';

//axlPointer    vortex_queue_pop         (VortexQueue * queue);
function        vortex_queue_pop         (queue : PVortexQueue):TaxlPointer;
                                          cdecl;external External_library name 'vortex_queue_pop';

//axlPointer    vortex_queue_peek        (VortexQueue * queue);
function        vortex_queue_peek        (queue : PVortexQueue):TaxlPointer;
                                          cdecl;external External_library name 'vortex_queue_peek';

//void          vortex_queue_free        (VortexQueue * queue);
procedure       vortex_queue_free        (queue:PVortexQueue);
                                          cdecl;external External_library name 'vortex_queue_free';

