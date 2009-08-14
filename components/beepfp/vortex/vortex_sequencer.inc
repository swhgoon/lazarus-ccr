
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

//void     vortex_sequencer_queue_data             (VortexCtx           * ctx,
//                                                  VortexSequencerData * data);
procedure  vortex_sequencer_queue_data             (ctx  : PVortexCtx;
                                                    data : PVortexSequencerData);
                                                    cdecl;external External_library name 'vortex_sequencer_queue_data';

//axl_bool vortex_sequencer_run                    (VortexCtx * ctx);
function   vortex_sequencer_run                    (ctx : PVortexCtx):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_sequencer_run';

//void     vortex_sequencer_stop                   (VortexCtx * ctx);
procedure  vortex_sequencer_stop                   (ctx : PVortexCtx);
                                                    cdecl;external External_library name 'vortex_sequencer_stop';

//axl_bool vortex_sequencer_direct_send            (VortexConnection * connection,
//                                                  VortexChannel    * channel,
//                                                  VortexWriterData * packet);
function   vortex_sequencer_direct_send            (connection : PVortexConnection;
                                                    channel    : PVortexChannel;
                                                    packet     : PVortexWriterData):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_sequencer_direct_send';

//void     vortex_sequencer_signal_update          (VortexChannel       * channel);
procedure  vortex_sequencer_signal_update          (channel : PVortexChannel);
                                                    cdecl;external External_library name 'vortex_sequencer_signal_update';

//void	  vortex_sequencer_drop_connection_messages(VortexConnection * conn);
procedure vortex_sequencer_drop_connection_messages(conn : PVortexConnection);
                                                    cdecl;external External_library name 'vortex_sequencer_drop_connection_messages';

