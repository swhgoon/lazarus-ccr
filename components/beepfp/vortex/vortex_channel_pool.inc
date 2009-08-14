
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

//VortexChannelPool * vortex_channel_pool_new            (VortexConnection           * connection,
//                                                        const char                 * profile,
//                                                        int                          init_num,
//                                                        VortexOnCloseChannel         close,
//                                                        axlPointer                   close_user_data,
//                                                        VortexOnFrameReceived        received,
//                                                        axlPointer                   received_user_data,
//                                                        VortexOnChannelPoolCreated   on_channel_pool_created,
//                                                        axlPointer                   user_data);
function              vortex_channel_pool_new            (connection              : PVortexConnection;
                                                          profile                 : Pchar;
                                                          init_num                : longint;
                                                          close                   : TVortexOnCloseChannel;
                                                          close_user_data         : TaxlPointer;
                                                          received                : TVortexOnFrameReceived;
                                                          received_user_data      : TaxlPointer;
                                                          on_channel_pool_created : TVortexOnChannelPoolCreated;
                                                          user_data               : TaxlPointer):PVortexChannelPool;
                                                          cdecl;external External_library name 'vortex_channel_pool_new';


//VortexChannelPool * vortex_channel_pool_new_full       (VortexConnection          * connection,
//                                                        const char                * profile,
//                                                        int                         init_num,
//                                                        VortexChannelPoolCreate     create_channel,
//                                                        axlPointer                  create_channel_user_data,
//                                                        VortexOnCloseChannel        close,
//                                                        axlPointer                  close_user_data,
//                                                        VortexOnFrameReceived       received,
//                                                        axlPointer                  received_user_data,
//                                                        VortexOnChannelPoolCreated  on_channel_pool_created,
//                                                        axlPointer                  user_data);
function              vortex_channel_pool_new_full       (connection              : PVortexConnection;
                                                          profile                 : Pchar;
                                                          init_num                : longint;
                                                          create_channel          : TVortexChannelPoolCreate;
                                                          create_channel_user_data: TaxlPointer;
                                                          close                   : TVortexOnCloseChannel;
                                                          close_user_data         : TaxlPointer;
                                                          received                : TVortexOnFrameReceived;
                                                          received_user_data      : TaxlPointer;
                                                          on_channel_pool_created : TVortexOnChannelPoolCreated;
                                                          user_data               : TaxlPointer):PVortexChannelPool;
                                                          cdecl;external External_library name 'vortex_channel_pool_new_full';

//int                 vortex_channel_pool_get_num        (VortexChannelPool * pool);
function              vortex_channel_pool_get_num        (pool : PVortexChannelPool):longint;
                                                          cdecl;external External_library name 'vortex_channel_pool_get_num';

//void                vortex_channel_pool_add            (VortexChannelPool * pool,
//                                                        int  num);
procedure             vortex_channel_pool_add            (pool : PVortexChannelPool;
                                                          num  : longint);
                                                          cdecl;external External_library name 'vortex_channel_pool_add';

//void                vortex_channel_pool_add_full       (VortexChannelPool * pool,
//                                                        int  num,
//                                                        axlPointer user_data);
procedure             vortex_channel_pool_add_full       (pool      : PVortexChannelPool;
                                                          num       : longint;
                                                          user_data : TaxlPointer);
                                                          cdecl;external External_library name 'vortex_channel_pool_add_full';

//void                vortex_channel_pool_remove         (VortexChannelPool * pool,
//                                                        int  num);
procedure             vortex_channel_pool_remove         (pool : PVortexChannelPool;
                                                          num  : longint);
                                                          cdecl;external External_library name 'vortex_channel_pool_remove';

//void                vortex_channel_pool_close          (VortexChannelPool * pool);
procedure             vortex_channel_pool_close          (pool : PVortexChannelPool);
                                                          cdecl;external External_library name 'vortex_channel_pool_close';

//void                vortex_channel_pool_attach         (VortexChannelPool * pool,
//                                                        VortexChannel     * channel);
procedure             vortex_channel_pool_attach         (pool    : PVortexChannelPool;
                                                          channel : PVortexChannel);
                                                          cdecl;external External_library name 'vortex_channel_pool_attach';

//void                vortex_channel_pool_deattach       (VortexChannelPool * pool,
//                                                        VortexChannel     * channel);
procedure             vortex_channel_pool_deattach       (pool    : PVortexChannelPool;
                                                          channel : PVortexChannel);
                                                          cdecl;external External_library name 'vortex_channel_pool_deattach';

//VortexChannel     * vortex_channel_pool_get_next_ready (VortexChannelPool * pool,
//                                                        axl_bool            auto_inc);
function              vortex_channel_pool_get_next_ready (pool     : PVortexChannelPool;
                                                          auto_inc : Taxl_bool):PVortexChannel;
                                                          cdecl;external External_library name 'vortex_channel_pool_get_next_ready';

//VortexChannel     * vortex_channel_pool_get_next_ready_full (VortexChannelPool * pool,
//                                                             axl_bool            auto_inc,
//                                                             axlPointer          user_data);
function              vortex_channel_pool_get_next_ready_full (pool      : PVortexChannelPool;
                                                               auto_inc  : Taxl_bool;
                                                               user_data : TaxlPointer):PVortexChannel;
                                                               cdecl;external External_library name 'vortex_channel_pool_get_next_ready_full';

//void                vortex_channel_pool_release_channel(VortexChannelPool * pool,
//                                                        VortexChannel     * channel);
procedure             vortex_channel_pool_release_channel(pool    : PVortexChannelPool;
                                                          channel : PVortexChannel);
                                                          cdecl;external External_library name 'vortex_channel_pool_release_channel';

//int                 vortex_channel_pool_get_id         (VortexChannelPool * pool);
function              vortex_channel_pool_get_id         (pool : PVortexChannelPool):longint;
                                                          cdecl;external External_library name 'vortex_channel_pool_get_id';

//VortexConnection  * vortex_channel_pool_get_connection (VortexChannelPool * pool);
function              vortex_channel_pool_get_connection (pool:PVortexChannelPool):PVortexConnection;
                                                          cdecl;external External_library name 'vortex_channel_pool_get_connection';

