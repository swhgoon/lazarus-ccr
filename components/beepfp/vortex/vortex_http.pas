unit vortex_http;

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

interface

uses
  vortex, axl;

const
{$IFDEF FPC}
  External_library='vortex-1.1';
{$ELSE}
  External_library='libvortex-1.1';
{$ENDIF}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

(**
 * \addtogroup vortex_http
 * @{
 *)

(**
 * @brief Connection setup object. Allows to configure additional
 * settings required to perform a connection by using \ref
 * vortex_http_connection_new.
 *)
type
  PVortexHttpSetup = Pointer;

(**
 * @brief Configurations allowed to be set on \ref VortexHttpSetup.
 *)
type
  TVortexHttpConfItem = (
  	(**
  	 * @brief Allows to configure where is located the host
  	 * running the HTTP proxy with CONNECT support.
  	 *)
  	VORTEX_HTTP_CONF_ITEM_PROXY_HOST := 1,
  	(**
  	 * @brief Allows to configure on which port is running the
  	 * HTTP proxy with CONNECT support.
  	 *)
  	VORTEX_HTTP_CONF_ITEM_PROXY_PORT := 2
  );

//VortexHttpSetup  * vortex_http_setup_new      (VortexCtx * ctx);
function             vortex_http_setup_new      (ctx:PVortexCtx):PVortexHttpSetup;
                                                 cdecl;external External_library name 'vortex_http_setup_new';

//axl_bool           vortex_http_setup_ref      (VortexHttpSetup * setup);
function             vortex_http_setup_ref      (setup:PVortexHttpSetup):Taxl_bool;
                                                 cdecl;external External_library name 'vortex_http_setup_ref';

//void               vortex_http_setup_unref    (VortexHttpSetup * setup);
procedure            vortex_http_setup_unref    (setup:PVortexHttpSetup);
                                                 cdecl;external External_library name 'vortex_http_setup_unref';

//void               vortex_http_setup_conf     (VortexHttpSetup      * setup,
//                                               VortexHttpConfItem     item,
//                                               const char           * str_value);
procedure            vortex_http_setup_conf     (setup:PVortexHttpSetup;
                                                 item:TVortexHttpConfItem;
                                                 const str_value:Pchar);
                                                 cdecl;external External_library name 'vortex_http_setup_conf';

//VortexConnection * vortex_http_connection_new (const char           * host,
//                                               const char           * port,
//                                               VortexHttpSetup      * setup,
//                                               VortexConnectionNew    on_connected,
//                                               axlPointer             user_data);
function             vortex_http_connection_new (const host:Pchar;
                                                 const port:Pchar;
                                                 setup:PVortexHttpSetup;
                                                 on_connected:TVortexConnectionNew;
                                                 user_data:TaxlPointer):PVortexConnection;
                                                 cdecl;external External_library name 'vortex_http_connection_new';

//axl_bool           vortex_http_connection_is_proxied (VortexConnection * conn);
function             vortex_http_connection_is_proxied (conn:PVortexConnection):Taxl_bool;
                                                        cdecl;external External_library name 'vortex_http_connection_is_proxied';

(* @} *)

implementation


end.

