
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

//int    vortex_profiles_register                (VortexCtx             * ctx,
//                                                const char            * uri,
//                                                VortexOnStartChannel    start,
//                                                axlPointer              start_user_data,
//                                                VortexOnCloseChannel    close,
//                                                axlPointer              close_user_data,
//                                                VortexOnFrameReceived   received,
//                                                axlPointer              received_user_data);
function vortex_profiles_register                (ctx                : PVortexCtx;
                                                  uri                : Pchar;
                                                  start              : TVortexOnStartChannel;
                                                  start_user_data    : TaxlPointer;
                                                  close              : TVortexOnCloseChannel;
                                                  close_user_data    : TaxlPointer;
                                                  received           : TVortexOnFrameReceived;
                                                  received_user_data : TaxlPointer):longint;
                                                  cdecl;external External_library name 'vortex_profiles_register';

//int    vortex_profiles_unregister              (VortexCtx             * ctx,
//                                                const char            * uri);
function vortex_profiles_unregister              (ctx : PVortexCtx;
                                                  uri : Pchar):longint;
                                                  cdecl;external External_library name 'vortex_profiles_unregister';

//int    vortex_profiles_set_mime_type           (VortexCtx             * ctx,
//                                                const char            * uri,
//                                                const char            * mime_type,
//                                                const char            * transfer_encoding);
function vortex_profiles_set_mime_type           (ctx               : PVortexCtx;
                                                  uri               : Pchar;
                                                  mime_type         : Pchar;
                                                  transfer_encoding : Pchar):longint;
                                                  cdecl;external External_library name 'vortex_profiles_set_mime_type';

//const char   * vortex_profiles_get_mime_type   (VortexCtx       * ctx,
//                                                const char      * uri);
function         vortex_profiles_get_mime_type   (ctx : PVortexCtx;
                                                  uri : Pchar):Pchar;
                                                  cdecl;external External_library name 'vortex_profiles_get_mime_type';

//const char   * vortex_profiles_get_transfer_encoding (VortexCtx        * ctx,
//                                                      const char       * uri);
function         vortex_profiles_get_transfer_encoding (ctx : PVortexCtx;
                                                        uri : Pchar):Pchar;
                                                        cdecl;external External_library name 'vortex_profiles_get_transfer_encoding';

//int     vortex_profiles_register_extended_start (VortexCtx                    * ctx,
//                                                 const char                   * uri,
//                                                 VortexOnStartChannelExtended   extended_start,
//                                                 axlPointer                     extended_start_user_data);
function  vortex_profiles_register_extended_start (ctx                      : PVortexCtx;
                                                   uri                      : Pchar;
                                                   extended_start           : TVortexOnStartChannelExtended;
                                                   extended_start_user_data : TaxlPointer):longint;
                                                   cdecl;external External_library name 'vortex_profiles_register_extended_start';

//axl_bool vortex_profiles_invoke_start           (char  *            uri,
//                                                 int                channel_num,
//                                                 VortexConnection * connection,
//                                                 char  *            serverName,
//                                                 char  *            profile_content,
//                                                 char  **           profile_content_reply,
//                                                 VortexEncoding     encoding);
function   vortex_profiles_invoke_start           (uri                   : Pchar;
                                                   channel_num           : longint;
                                                   connection            : PVortexConnection;
                                                   serverName            : Pchar;
                                                   profile_content       : Pchar;
                                                   profile_content_reply : PPchar;
                                                   encoding              : TVortexEncoding):Taxl_bool;
                                                   cdecl;external External_library name 'vortex_profiles_invoke_start';

//axl_bool  vortex_profiles_is_defined_start      (VortexCtx   * ctx,
//                                                 const char  * uri);
function    vortex_profiles_is_defined_start      (ctx : PVortexCtx;
                                                   uri : Pchar):Taxl_bool;
                                                   cdecl;external External_library name 'vortex_profiles_is_defined_start';

//axl_bool  vortex_profiles_invoke_close          (char  * uri,
//                                                 int  channel_nu,
//                                                 VortexConnection * connection);
function    vortex_profiles_invoke_close          (uri        : Pchar;
                                                   channel_nu : longint;
                                                   connection : PVortexConnection):Taxl_bool;
                                                   cdecl;external External_library name 'vortex_profiles_invoke_close';

//axl_bool  vortex_profiles_is_defined_close      (VortexCtx   * ctx,
//                                                 const char  * uri);
function    vortex_profiles_is_defined_close      (ctx : PVortexCtx;
                                                   uri : Pchar):Taxl_bool;
                                                   cdecl;external External_library name 'vortex_profiles_is_defined_close';

//int       vortex_profiles_invoke_frame_received (const char       * uri,
//                                                 int                channel_num,
//                                                 VortexConnection * connection,
//                                                 VortexFrame      * frame);
function    vortex_profiles_invoke_frame_received (uri         : Pchar;
                                                   channel_num : longint;
                                                   connection  : PVortexConnection;
                                                   frame       : PVortexFrame):longint;
                                                   cdecl;external External_library name 'vortex_profiles_invoke_frame_received';

//int       vortex_profiles_is_defined_received   (VortexCtx        * ctx,
//                                                 const char       * uri);
function    vortex_profiles_is_defined_received   (ctx : PVortexCtx;
                                                   uri : Pchar):longint;
                                                   cdecl;external External_library name 'vortex_profiles_is_defined_received';

//axlList * vortex_profiles_get_actual_list       (VortexCtx        * ctx);
function    vortex_profiles_get_actual_list       (ctx : PVortexCtx):PaxlList;
                                                   cdecl;external External_library name 'vortex_profiles_get_actual_list';

//axlList * vortex_profiles_get_actual_list_ref   (VortexCtx        * ctx);
function    vortex_profiles_get_actual_list_ref   (ctx : PVortexCtx):PaxlList;
                                                   cdecl;external External_library name 'vortex_profiles_get_actual_list_ref';

//int       vortex_profiles_registered            (VortexCtx        * ctx);
function    vortex_profiles_registered            (ctx : PVortexCtx):longint;
                                                   cdecl;external External_library name 'vortex_profiles_registered';

//axl_bool  vortex_profiles_is_registered         (VortexCtx        * ctx,
//                                                 const char       * uri);
function vortex_profiles_is_registered            (ctx : PVortexCtx;
                                                   uri : Pchar):Taxl_bool;
                                                   cdecl;external External_library name 'vortex_profiles_is_registered';

//void      vortex_profiles_set_automatic_mime    (VortexCtx        * ctx,
//                                                 const char       * uri,
//                                                 int                value);
procedure   vortex_profiles_set_automatic_mime    (ctx   : PVortexCtx;
                                                   uri   : Pchar;
                                                   value : longint);
                                                   cdecl;external External_library name 'vortex_profiles_set_automatic_mime';

//int       vortex_profiles_get_automatic_mime    (VortexCtx        * ctx,
//                                                 const char       * uri);
function    vortex_profiles_get_automatic_mime    (ctx : PVortexCtx;
                                                   uri : Pchar):longint;
                                                   cdecl;external External_library name 'vortex_profiles_get_automatic_mime';

//void      vortex_profiles_init                  (VortexCtx   * ctx);
procedure   vortex_profiles_init                  (ctx : PVortexCtx);
                                                   cdecl;external External_library name 'vortex_profiles_init';

//void      vortex_profiles_cleanup               (VortexCtx   * ctx);
procedure   vortex_profiles_cleanup               (ctx : PVortexCtx);
                                                   cdecl;external External_library name 'vortex_profiles_cleanup';


