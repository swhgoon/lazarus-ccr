
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
 * \addtogroup vortex_listener
 * @{
 *)

//VortexConnection * vortex_listener_new             (VortexCtx            * ctx,
//                                                    const char           * host,
//                                                    const char           * port,
//                                                    VortexListenerReady    on_ready,
//                                                    axlPointer             user_data);
function             vortex_listener_new             (ctx       : PVortexCtx;
                                                      host      : Pchar;
                                                      port      : Pchar;
                                                      on_ready  : TVortexListenerReady;
                                                      user_data : TaxlPointer):PVortexConnection;
                                                      cdecl;external External_library name 'vortex_listener_new';

//VortexConnection * vortex_listener_new2            (VortexCtx           * ctx,
//                                                    const char          * host,
//                                                    int                   port,
//                                                    VortexListenerReady   on_ready,
//                                                    axlPointer            user_data);
function             vortex_listener_new2            (ctx       : PVortexCtx;
                                                      host      : Pchar;
                                                      port      : longint;
                                                      on_ready  : TVortexListenerReady;
                                                      user_data : TaxlPointer):PVortexConnection;
                                                      cdecl;external External_library name 'vortex_listener_new2';

//VortexConnection * vortex_listener_new_full        (VortexCtx                * ctx,
//                                                    const char               * host,
//                                                    const char               * port,
//                                                    VortexListenerReadyFull    on_ready_full,
//                                                    axlPointer                 user_data);
function             vortex_listener_new_full        (ctx           : PVortexCtx;
                                                      host          : Pchar;
                                                      port          : Pchar;
                                                      on_ready_full : TVortexListenerReadyFull;
                                                      user_data     : TaxlPointer):PVortexConnection;
                                                      cdecl;external External_library name 'vortex_listener_new_full';

//VORTEX_SOCKET     vortex_listener_sock_listen      (VortexCtx   * ctx,
//                                                    const char  * host,
//                                                    const char  * port,
//                                                    axlError   ** error);
function            vortex_listener_sock_listen      (ctx   : PVortexCtx;
                                                      host  : Pchar;
                                                      port  : Pchar;
                                                      error : PPaxlError):TVORTEX_SOCKET;
                                                      cdecl;external External_library name 'vortex_listener_sock_listen';

//void          vortex_listener_accept_connections   (VortexCtx        * ctx,
//                                                    int                server_socket,
//                                                    VortexConnection * listener);
procedure       vortex_listener_accept_connections   (ctx           : PVortexCtx;
                                                      server_socket : longint;
                                                      listener      : PVortexConnection);
                                                      cdecl;external External_library name 'vortex_listener_accept_connections';

//void          vortex_listener_accept_connection    (VortexConnection * connection,
//                                                    axl_bool           send_greetings);
procedure       vortex_listener_accept_connection    (connection     : PVortexConnection;
                                                      send_greetings : Taxl_bool);
                                                      cdecl;external External_library name 'vortex_listener_accept_connection';

//void          __vortex_listener_second_step_accept (VortexFrame * frame,
//                                                    VortexConnection * connection);
procedure       __vortex_listener_second_step_accept (frame      : PVortexFrame;
                                                      connection : PVortexConnection);
                                                      cdecl;external External_library name '__vortex_listener_second_step_accept';

//void          vortex_listener_wait                 (VortexCtx * ctx);
procedure       vortex_listener_wait                 (ctx : PVortexCtx);
                                                      cdecl;external External_library name 'vortex_listener_wait';

//void          vortex_listener_unlock               (VortexCtx * ctx);
procedure       vortex_listener_unlock               (ctx : PVortexCtx);
                                                      cdecl;external External_library name 'vortex_listener_unlock';

//void          vortex_listener_init                 (VortexCtx * ctx);
procedure       vortex_listener_init                 (ctx : PVortexCtx);
                                                      cdecl;external External_library name 'vortex_listener_init';

//void          vortex_listener_cleanup              (VortexCtx * ctx);
procedure       vortex_listener_cleanup              (ctx : PVortexCtx);
                                                      cdecl;external External_library name 'vortex_listener_cleanup';

//axl_bool      vortex_listener_parse_conf_and_start (VortexCtx * ctx);
function        vortex_listener_parse_conf_and_start (ctx : PVortexCtx):Taxl_bool;
                                                      cdecl;external External_library name 'vortex_listener_parse_conf_and_start';

//void          vortex_listener_set_default_realm    (VortexCtx   * ctx,
//                                                    const char  * realm);
procedure       vortex_listener_set_default_realm    (ctx   : PVortexCtx;
                                                      realm : Pchar);
                                                      cdecl;external External_library name 'vortex_listener_set_default_realm';

//const char  * vortex_listener_get_default_realm    (VortexCtx   * ctx);
function        vortex_listener_get_default_realm    (ctx : PVortexCtx):Pchar;
                                                      cdecl;external External_library name 'vortex_listener_get_default_realm';

//void          vortex_listener_set_on_connection_accepted (VortexCtx                  * ctx,
//                                                          VortexOnAcceptedConnection   on_accepted,
//                                                          axlPointer                   data);
procedure       vortex_listener_set_on_connection_accepted (ctx         : PVortexCtx;
                                                            on_accepted : TVortexOnAcceptedConnection;
                                                            data        : TaxlPointer);
                                                            cdecl;external External_library name 'vortex_listener_set_on_connection_accepted';

//void          vortex_listener_shutdown (VortexConnection * listener,
//                                        axl_bool           also_created_conns);
procedure       vortex_listener_shutdown (listener           : PVortexConnection;
                                          also_created_conns : Taxl_bool);
                                          cdecl;external External_library name 'vortex_listener_shutdown';

  { @  }

