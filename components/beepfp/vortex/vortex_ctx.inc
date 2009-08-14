
(*
 * LibVortex: C Header file to Free Pascal translation.
 * Copyright (C) 2009, Wimpie Nortje <wimpienortje@gmail.com>
 *)

(*
 *  LibVortex:  A BEEP (RFC3080/RFC3081) implementation.
 *  Copyright (C) 2005 Advanced Software Production Line, S.L.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to
 *  develop proprietary applications using this library without any
 *  royalty or fee but returning back any change, improvement or
 *  addition in the form of source code, project image, documentation
 *  patches, etc.
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


//VortexCtx * vortex_ctx_new                     (void);
function    vortex_ctx_new:PVortexCtx;            cdecl;external External_library name 'vortex_ctx_new';
//function    vortex_ctx_new:PVortexCtx;            stdcall;external External_library name 'vortex_ctx_new';

//void      vortex_ctx_set_data                  (VortexCtx       * ctx,
//                                  						  axlPointer        key,
//                                  						  axlPointer        value);
procedure   vortex_ctx_set_data                  (ctx   : PVortexCtx;
                                                  key   : TaxlPointer;
                                                  value : TaxlPointer);
                                                  cdecl;external External_library name 'vortex_ctx_set_data';

//void      vortex_ctx_set_data_full             (VortexCtx       * ctx,
//                                                axlPointer        key,
//                                    						axlPointer        value,
//                                    						axlDestroyFunc    key_destroy,
//                                    						axlDestroyFunc    value_destroy);
procedure   vortex_ctx_set_data_full             (ctx           : PVortexCtx;
                                                  key           : TaxlPointer;
                                                  value         : TaxlPointer;
                                                  key_destroy   : TaxlDestroyFunc;
                                                  value_destroy : TaxlDestroyFunc);
                                                  cdecl;external External_library name 'vortex_ctx_set_data_full';

//axlPointer vortex_ctx_get_data                 (VortexCtx       * ctx,
//                                                axlPointer        key);
function    vortex_ctx_get_data                  (ctx : PVortexCtx;
                                                  key : TaxlPointer):TaxlPointer;
                                                  cdecl;external External_library name 'vortex_ctx_get_data';

{** global event notificaitons ** }
//void      vortex_ctx_set_frame_received        (VortexCtx             * ctx,
//                                                VortexOnFrameReceived   received,
//                                                axlPointer              received_user_data);
procedure   vortex_ctx_set_frame_received        (ctx               : PVortexCtx;
                                                  received          : TVortexOnFrameReceived;
                                                  received_user_data: TaxlPointer);
                                                  cdecl;external External_library name 'vortex_ctx_set_frame_received';

//void      vortex_ctx_set_close_notify_handler  (VortexCtx                  * ctx,
//                                 						    VortexOnNotifyCloseChannel   close_notify,
//                                 						    axlPointer                   user_data);
procedure   vortex_ctx_set_close_notify_handler  (ctx         : PVortexCtx;
                                                  close_notify: TVortexOnNotifyCloseChannel;
                                                  user_data   : TaxlPointer);
                                                  cdecl;external External_library name 'vortex_ctx_set_close_notify_handler';

//void      vortex_ctx_set_channel_added_handler  (VortexCtx                       * ctx,
//                                   						   VortexConnectionOnChannelUpdate   added_handler,
//                                   						   axlPointer                        user_data);
procedure   vortex_ctx_set_channel_added_handler  (ctx           : PVortexCtx;
                                                   added_handler : TVortexConnectionOnChannelUpdate;
                                                   user_data     : TaxlPointer);
                                                   cdecl;external External_library name 'vortex_ctx_set_channel_added_handler';

//void      vortex_ctx_set_channel_removed_handler(VortexCtx                       * ctx,
//                                  						   VortexConnectionOnChannelUpdate   removed_handler,
//                                   						   axlPointer                        user_data);
procedure   vortex_ctx_set_channel_removed_handler(ctx            : PVortexCtx;
                                                   removed_handler: TVortexConnectionOnChannelUpdate;
                                                   user_data      : TaxlPointer);
                                                   cdecl;external External_library name 'vortex_ctx_set_channel_removed_handler';

//void      vortex_ctx_set_channel_start_handler  (VortexCtx                       * ctx,
//                                  						   VortexOnStartChannelExtended      start_handler,
//                                  						   axlPointer                        start_handler_data);
procedure   vortex_ctx_set_channel_start_handler  (ctx               : PVortexCtx;
                                                   start_handler     : TVortexOnStartChannelExtended;
                                                   start_handler_data: TaxlPointer);
                                                   cdecl;external External_library name 'vortex_ctx_set_channel_start_handler';

//void      vortex_ctx_install_cleanup (VortexCtx * ctx,
//				                              axlDestroyFunc cleanup);
procedure   vortex_ctx_install_cleanup (ctx     : PVortexCtx;
                                        cleanup : TaxlDestroyFunc);
                                        cdecl;external External_library name 'vortex_ctx_install_cleanup';

//void      vortex_ctx_free                      (VortexCtx * ctx);

procedure   vortex_ctx_free            (ctx     : PVortexCtx);
                                        cdecl;external External_library name 'vortex_ctx_free';

