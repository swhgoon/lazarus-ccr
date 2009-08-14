unit vortex_tls;

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
 * \addtogroup vortex_tls 
 * @{ 
 *)

(**
 * @brief Digest method provided.
 *)
type
  TVortexDigestMethod = (
  	(**
  	 * @brief Allows to especify the DIGEST method SHA-1.
  	 *)
  	VORTEX_SHA1 := 1,
  	(**
  	 * @brief Allows to especify the DIGEST method MD5.
  	 *)
  	VORTEX_MD5 := 2,
  	(**
  	 * @internal Internal value. Do not modify.
  	 *)
  	VORTEX_DIGEST_NUM
  );

(**
 * @brief TLS Profile unique URI identifier.
 *)
const
  VORTEX_TLS_PROFILE_URI = 'http://iana.org/beep/TLS';

//axl_bool           vortex_tls_init                       (VortexCtx            * ctx);
function             vortex_tls_init                       (ctx:PVortexCtx):Taxl_bool;
                                                            cdecl;external External_library name 'vortex_tls_init';

//void               vortex_tls_set_ctx_creation           (VortexConnection     * connection,
//                                                          VortexTlsCtxCreation   ctx_creation,
//                                                          axlPointer             user_data);
procedure            vortex_tls_set_ctx_creation           (connection:PVortexConnection;
                                                            ctx_creation:TVortexTlsCtxCreation;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_tls_set_ctx_creation';

//void               vortex_tls_set_default_ctx_creation   (VortexCtx            * ctx,
//                                                          VortexTlsCtxCreation   ctx_creation,
//                                                          axlPointer             user_data);
procedure            vortex_tls_set_default_ctx_creation   (ctx:PVortexCtx;
                                                            ctx_creation:TVortexTlsCtxCreation;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_tls_set_default_ctx_creation';

//void               vortex_tls_set_post_check             (VortexConnection     * connection,
//                                                          VortexTlsPostCheck     post_check,
//                                                          axlPointer             user_data);
procedure            vortex_tls_set_post_check             (connection:PVortexConnection;
                                                            post_check:TVortexTlsPostCheck;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_tls_set_post_check';

//void               vortex_tls_set_default_post_check     (VortexCtx            * ctx,
//                                                          VortexTlsPostCheck     post_check,
//                                                          axlPointer             user_data);
procedure            vortex_tls_set_default_post_check     (ctx:PVortexCtx;
                                                            post_check:TVortexTlsPostCheck;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_tls_set_default_post_check';

//void               vortex_tls_start_negotiation          (VortexConnection     * connection,
//                                                          const char           * serverName,
//                                                          VortexTlsActivation    process_status,
//                                                          axlPointer             user_data);
procedure            vortex_tls_start_negotiation          (connection:PVortexConnection;
                                                            const serverName:Pchar;
                                                            process_status:TVortexTlsActivation;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_tls_start_negotiation';

//VortexConnection * vortex_tls_start_negotiation_sync     (VortexConnection  * connection,
//                                                          const char        * serverName,
//                                                          VortexStatus      * status,
//                                                          char             ** status_message);
function             vortex_tls_start_negotiation_sync     (connection:PVortexConnection;
                                                            const serverName:Pchar;
                                                            status:PVortexStatus;
                                                            status_message:PPchar):PVortexConnection;
                                                            cdecl;external External_library name 'vortex_tls_start_negotiation_sync';

//void               vortex_tls_set_auto_tls               (VortexCtx         * ctx,
//                                                          int                 enabled,
//                                                          int                 allow_tls_failures,
//                                                          const char        * serverName);
procedure            vortex_tls_set_auto_tls               (ctx:PVortexCtx;
                                                            enabled:longint;
                                                            allow_tls_failures:longint;
                                                            const serverName:Pchar);
                                                            cdecl;external External_library name 'vortex_tls_set_auto_tls';

//axl_bool           vortex_tls_accept_negotiation         (VortexCtx         * ctx,
//                                                          VortexTlsAcceptQuery            accept_handler,
//                                                          VortexTlsCertificateFileLocator certificate_handler,
//                                                          VortexTlsPrivateKeyFileLocator  private_key_handler);
function             vortex_tls_accept_negotiation         (ctx:PVortexCtx;
                                                            accept_handler:TVortexTlsAcceptQuery;
                                                            certificate_handler:TVortexTlsCertificateFileLocator;
                                                            private_key_handler:TVortexTlsPrivateKeyFileLocator):Taxl_bool;
                                                            cdecl;external External_library name 'vortex_tls_accept_negotiation';

//axlPointer         vortex_tls_get_ssl_object             (VortexConnection * connection);
function             vortex_tls_get_ssl_object             (connection:PVortexConnection):TaxlPointer;
                                                            cdecl;external External_library name 'vortex_tls_get_ssl_object';

//char             * vortex_tls_get_peer_ssl_digest        (VortexConnection   * connection,
//                                                          VortexDigestMethod   method);
function             vortex_tls_get_peer_ssl_digest        (connection:PVortexConnection;
                                                            method:TVortexDigestMethod):Pchar;
                                                            cdecl;external External_library name 'vortex_tls_get_peer_ssl_digest';

//char             * vortex_tls_get_digest                 (VortexDigestMethod   method,
//                                                          const char         * string);
function             vortex_tls_get_digest                 (method:TVortexDigestMethod;
                                                            const astring:Pchar):Pchar;
                                                            cdecl;external External_library name 'vortex_tls_get_digest';

//char             * vortex_tls_get_digest_sized           (VortexDigestMethod   method,
//                                                          const char         * content,
//                                                          int                  content_size);
function             vortex_tls_get_digest_sized           (method:TVortexDigestMethod;
                                                            const content:Pchar;
                                                            content_size:longint):Pchar;
                                                            cdecl;external External_library name 'vortex_tls_get_digest_sized';

//void               vortex_tls_cleanup                    (VortexCtx * ctx);
procedure            vortex_tls_cleanup                    (ctx:PVortexCtx);
                                                            cdecl;external External_library name 'vortex_tls_cleanup';

(* @} *)

implementation
end.

