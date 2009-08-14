unit vortex_sasl;

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
 * \addtogroup vortex_sasl
 * @{
 *)

const
(**
 * @internal
 * @brief This is actually not a SASL profile. It is only used to
 * perform some internal checks.
 *)
  VORTEX_SASL_FAMILY =       'http://iana.org/beep/SASL';
(**
 * @brief ANONYMOUS profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 *)
  VORTEX_SASL_ANONYMOUS =    'http://iana.org/beep/SASL/ANONYMOUS';
(**
 * @brief EXTERNAL profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 *)
  VORTEX_SASL_EXTERNAL =     'http://iana.org/beep/SASL/EXTERNAL';
(**
 * @brief PLAIN profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 *)
  VORTEX_SASL_PLAIN =        'http://iana.org/beep/SASL/PLAIN';
(**
 * @brief CRAM-MD5 profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 *)
  VORTEX_SASL_CRAM_MD5 =     'http://iana.org/beep/SASL/CRAM-MD5';
(**
 * @brief DIGEST-MD5 profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 *)
  VORTEX_SASL_DIGEST_MD5 =   'http://iana.org/beep/SASL/DIGEST-MD5';
(**
 * @brief GSSAPI profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 * Currently GSSAPI is not supported by Vortex Library.
 *)
  VORTEX_SASL_GSSAPI =       'http://iana.org/beep/SASL/GASSAPI';
(**
 * @brief KERBEROS_V4 profile identification to be used at \ref
 * vortex_sasl_start_auth or \ref vortex_sasl_accept_negotiation.
 * Currently KERBEROS_V4 is not supported by Vortex Library.
 *)
  VORTEX_SASL_KERBEROS_V4 =  'http://iana.org/beep/SASL/KERBEROS_V4';

(**
 * @brief Key value to access the user-defined pointer associated with a connection
 * using SASL ANONYMOUS authentication method. Use vortex_connection_set_data
 * and vortex_connection_get_data to access the data. 
 * 
 * This data is configured at \ref
 * vortex_sasl_accept_negotiation_full, and then passed to the handler
 * configured. Because the SASL module doesn't remove this data once
 * the authentication process have finished, you can still get access
 * to the data configured using this key.
 *)
  VORTEX_SASL_ANONYMOUS_USER_DATA = '__VORTEX_SASL_ANONYMOUS_USER_DATA';

(**
 * @brief Key value to access the user-defined pointer associated with a connection
 * using SASL PLAIN authentication method. Use vortex_connection_set_data
 * and vortex_connection_get_data to access the data.
 *
 * This data is configured at \ref
 * vortex_sasl_accept_negotiation_full, and then passed to the handler
 * configured. Because the SASL module doesn't remove this data once
 * the authentication process have finished, you can still get access
 * to the data configured using this key.
 *)
  VORTEX_SASL_PLAIN_USER_DATA  ='__VORTEX_SASL_PLAIN_USER_DATA';

(**
 * @brief Key value to access the user-defined pointer associated with a connection
 * using SASL EXTERNAL authentication method. Use vortex_connection_set_data
 * and vortex_connection_get_data to access the data.
 *
 * This data is configured at \ref
 * vortex_sasl_accept_negotiation_full, and then passed to the handler
 * configured. Because the SASL module doesn't remove this data once
 * the authentication process have finished, you can still get access
 * to the data configured using this key.
 *)
  VORTEX_SASL_EXTERNAL_USER_DATA =      '__VORTEX_SASL_EXTERNAL_USER_DATA';

(**
 * @brief Key value to access the user-defined pointer associated with a connection
 * using SASL CRAM MD5 authentication method. Use vortex_connection_set_data
 * and vortex_connection_get_data to access the data.
 *
 * This data is configured at \ref
 * vortex_sasl_accept_negotiation_full, and then passed to the handler
 * configured. Because the SASL module doesn't remove this data once
 * the authentication process have finished, you can still get access
 * to the data configured using this key.
 *)
  VORTEX_SASL_CRAM_MD5_USER_DATA =      '__VORTEX_SASL_CRAM_MD5_USER_DATA';

(**
 * @brief Key value to access the user-defined pointer associated with a connection
 * using SASL DIGEST MD5 authentication method. Use vortex_connection_set_data
 * and vortex_connection_get_data to access the data.
 *
 * This data is configured at \ref
 * vortex_sasl_accept_negotiation_full, and then passed to the handler
 * configured. Because the SASL module doesn't remove this data once
 * the authentication process have finished, you can still get access
 * to the data configured using this key.
 *)
  VORTEX_SASL_DIGEST_MD5_USER_DATA =      '__VORTEX_SASL_DIGEST_MD5_USER_DATA';

(**
 * @brief Mark used by the sasl module to store the authorization id
 * used for a successful SASL negotation (that is the user login).
 *)
  SASL_AUTHID =                    'sasl:authid';

(**
 * @brief Mark used by the sasl module to store the proxy
 * authorization id. This value is used by some SASL profiles.
 *)
  SASL_AUTHZID =                   'sasl:authzid';

(**
 * @internal Value used by the sasl module to but not exported as a
 * mark because this value is deleted once the SASL auth mechanism was
 * completed.
 *)
  SASL_PASSWORD =                  'sasl:password';

(**
 * @brief Mark used to store the realm value used by some SASL
 * mechanism.
 *)
  SASL_REALM =                     'sasl:realm';

(**
 * @brief Mark used to store the anonymous token provided to comple
 * the SASL ANONYMOUS mechanism.
 *)
  SASL_ANONYMOUS_TOKEN  =          'sasl:anonymous:token';

(**
 * @brief Mark used to check if a connection was completely
 * authenticated. This mark is especially used by turbulence to check
 * if a particular SASL channel not only exists but also has completed
 * its auth process.
 *)
  SASL_IS_AUTHENTICATED =          'sasl:is:authenticated';

(**
 * @brief Mark used to store the SASL mechanism used.
 *)
  SASL_METHOD_USED =               'sasl:method:used';


(**
 * @brief Set of properties to be used to configure client authentication. 
 *
 * This properties allows to configure the user id, password,
 * authorization id, etc, that are to be used while performing SASL negotiations. 
 *
 * These properties are set to or get from a connection using \ref
 * vortex_sasl_set_propertie and \ref vortex_sasl_get_propertie.
 *)
type
  TVortexSaslProperties = (
  	(**
  	 * @brief User identifier to be used while SASL negotiation is running. An example of this value could be: "bob".
  	 *)
  	VORTEX_SASL_AUTH_ID           := 1,
  	(**
  	 * @brief Inside SASL definition, there is support to authenticate as "bob" and act as "alice".
  	 *)
  	VORTEX_SASL_AUTHORIZATION_ID  := 2,
  	(**
  	 * @brief User password to be used while SASL negotiation is running. An example of this value could be: "secret".
  	 *)
  	VORTEX_SASL_PASSWORD          := 3,
  	(**
  	 * @brief Some SASL mechanism requires realm value to be defined. This allows to have a set of users to be authenticated according to the realm provided.
  	 *)
  	VORTEX_SASL_REALM             := 4,
  	(**
  	 * @brief Allows to configure anonymous token while using ANONYMOUS profile. This propertie is especific for ANONYMOUS profile.
  	 *)
  	VORTEX_SASL_ANONYMOUS_TOKEN   := 5,
  	(**
  	 * @internal
  	 * The following value must be always be the last. While adding new properties, they must go before this one.
  	 *)
  	VORTEX_SASL_PROP_NUM          := 6
  );

{$inline on}
(**
 * @brief Convenience macro that allows to get the auth Id for the
 * connection that is running the channel provided.
 *
 * The macro also check if the connection holding the channel was
 * authenticated (by calling to \ref
 * vortex_sasl_is_authenticated). The macro doesn't check the sasl
 * method used or if it is appropiated to your context. You must use
 * other means to check/enforce SASL method allowed.
 *
 * See also: \ref AUTH_ID_FROM_CONN. 
 *
 * @param channel The channel that is supposed to be running inside a
 * connection autenticated.
 * 
 * @return The auth id or NULL if it fails.
 *)
//#define AUTH_ID_FROM_CHANNEL(channel) (vortex_sasl_is_authenticated (vortex_channel_get_connection (channel)) ? vortex_sasl_get_propertie (vortex_channel_get_connection (channel), VORTEX_SASL_AUTH_ID) : NULL)
function AUTH_ID_FROM_CHANNEL(channel : PVortexChannel): PChar; inline;

(**
 * @brief Convenience macro that allows to get the auth Id for the
 * connection provided.
 *
 * The macro also check if the connection was authenticated (by
 * calling to \ref vortex_sasl_is_authenticated). The macro doesn't
 * check the sasl method used or if it is appropiated to your
 * context. You must use other means to check/enforce SASL method
 * allowed.
 *
 * See also: \ref AUTH_ID_FROM_CONN. 
 *
 * @param conn The connection to be checked for its auth id
 * associated.
 * 
 * @return The auth id or NULL if it fails.
 *)
//#define AUTH_ID_FROM_CONN(conn) (vortex_sasl_is_authenticated (conn) ? vortex_sasl_get_propertie (conn, VORTEX_SASL_AUTH_ID) : NULL)
function AUTH_ID_FROM_CONN(connection:PVortexConnection): PChar; inline;
{$inline off}

(**
 * @brief Async notifications for SASL auth process. 
 *
 * This notification handler is used to report user space what is the
 * final status for the SASL profile negotiation selected. 
 *
 * The handler report an <b>status</b> variable and a textual
 * diagnostic error on <b>status_message</b>.
 *
 * This function is used by: 
 *   - \ref vortex_sasl_start_auth
 *
 *
 * @param connection     The connection where the SASL process have ended
 * @param status         Final status for the SASL process
 * @param status_message A textual diagnostic
 * @param user_data      User defined data provided at the function receiving this handler.
 *)
//typedef void     ( *VortexSaslAuthNotify)          (VortexConnection * connection,
//                                                    VortexStatus       status,
//                                                    char             * status_message,
//                                                    axlPointer         user_data);
type
  TVortexSaslAuthNotify = procedure (connection:PVortexConnection;
    status:TVortexStatus; status_message:Pchar; user_data:TaxlPointer);cdecl;

(**
 * @brief Asynchronous notification to enable user space to accept or
 * deny authentication for SASL EXTERNAL profile.
 *
 * Functions using this handler are:
 *   - \ref vortex_sasl_set_external_validation
 * 
 * @param connection The connection where the request was received.
 * @param authorization_id The authorization id to authenticate.
 * 
 * @return axl_true to authenticate and allow request received or axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthExternal)        (VortexConnection * connection,
//                                                             const char       * authorization_id);
type
  TVortexSaslAuthExternal = function (connection:PVortexConnection; const authorization_id:Pchar):Taxl_bool;cdecl;

(**
 * @brief Asynchronous notification to enable user space to accept or
 * deny authentication for SASL EXTERNAL profile, and passes a user defined pointer.
 *
 * Functions using this handler are:
 *   - \ref vortex_sasl_set_external_validation_full
 * 
 * @param connection The connection where the request was received.
 * @param authorization_id The authorization id to authenticate.
 * @param user_data The user defined pointer to be passed to future SASL callbacks
 * 
 * @return axl_true to authenticate and allow request received or axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthExternalFull)        (VortexConnection * connection,
//                                                                const char       * authorization_id,
//                                                                axlPointer         user_data);
type
  TVortexSaslAuthExternalFull = function (connection:PVortexConnection; const authorization_id:Pchar; user_data:TaxlPointer):Taxl_bool;cdecl;

(**
 * @brief Asynchronous notification to enable user space to accept or
 * deny anonymous authentication for SASL ANONYMOUS profile.
 * 
 * Function using this definition:
 * - \ref vortex_sasl_set_anonymous_validation
 * 
 * @param connection The connection where the anonymous request was received.
 *
 * @param anonymous_token The anonymous token provided by the remote
 * client side. You must not modify or deallocate this value.
 * 
 * @return axl_true to validate the incoming anonymous auth request. axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthAnonymous)       (VortexConnection * connection,
//                                                             const char       * anonymous_token);
type
  TVortexSaslAuthAnonymous = function (connection:PVortexConnection;
    const anonymous_token:Pchar):Taxl_bool;cdecl;

(**
 * @brief Asynchronous notification to enable user space to accept or
 * deny anonymous authentication for SASL ANONYMOUS profile.
 * 
 * Function using this definition:
 * - \ref vortex_sasl_set_anonymous_validation_full
 * 
 * @param connection The connection where the anonymous request was received.
 *
 * @param anonymous_token The anonymous token provided by the remote
 * client side. You must not modify or deallocate this value.
 * 
 * @param user_data The user defined pointer to be passed to future SASL callbacks
 *
 * @return axl_true to validate the incoming anonymous auth request. axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthAnonymousFull)       (VortexConnection * connection,
//                                                                 const char       * anonymous_token,
//                                                                 axlPointer         user_data);
type
  TVortexSaslAuthAnonymousFull = function (connection:PVortexConnection;
    const anonymous_token:Pchar; user_data:TaxlPointer):Taxl_bool;cdecl;

(**
 * @brief Asynchronous notification to enable user space code to
 * validate SASL PLAIN request received.
 * 
 * Function using this handler are:
 *  - \ref vortex_sasl_set_plain_validation
 * 
 * 
 * @param connection The connection where the SASL notification was received.
 * @param auth_id User identifier
 * @param authorization_id User identifier which is acting on behalf of.
 * @param password Current user password
 * 
 * @return axl_true to accept incoming SASL request or axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthPlain)           (VortexConnection * connection,
//                                                            const char       * auth_id,
//                                                            const char       * authorization_id,
//                                                            const char       * password);
type
  TVortexSaslAuthPlain = function (connection:PVortexConnection;
    const auth_id:Pchar; const authorization_id:Pchar; const password:Pchar):Taxl_bool;cdecl;

(**
 * @brief Asynchronous notification to enable user space code to
 * validate SASL PLAIN request received.
 * 
 * Function using this handler are:
 *  - \ref vortex_sasl_set_plain_validation_full
 * 
 * 
 * @param connection The connection where the SASL notification was received.
 * @param auth_id User identifier
 * @param authorization_id User identifier which is acting on behalf of.
 * @param password Current user password
 * @param user_data The user defined pointer to be passed to future SASL callbacks.
 * 
 * @return axl_true to accept incoming SASL request or axl_false to deny it.
 *)
//typedef axl_bool          ( *VortexSaslAuthPlainFull)           (VortexConnection * connection,
//                                                                const char       * auth_id,
//                                                                const char       * authorization_id,
//                                                                const char       * password,
//                                                                axlPointer         user_data);
type
  TVortexSaslAuthPlainFull = function (connection:PVortexConnection; const auth_id:Pchar;
    const authorization_id:Pchar; const password:Pchar; user_data:TaxlPointer):Taxl_bool;cdecl;
(**
 * @brief Asynchronous notification to enable user space code to
 * validate SASL CRAM MD5 request received.
 *
 * Inside this function, the programmer must implement the password
 * lookup for the given user (<b>auth_id</b>) and return a copy to it
 * in clear text.
 *
 * Functions using this handler are:
 *  - \ref vortex_sasl_set_cram_md5_validation
 * 
 * @param connection The connection where the SASL notification was received
 * @param auth_id User id to authenticate.
 * 
 * @return The password the given auth_id have or NULL if the request
 * must be denied. Password returned must be the in plain text,
 * without any modification, as it would be introduced by the remote
 * peer. Later, the SASL engine will perform the hash operation to be
 * compared with the hash value received from the client
 * side. <b>Returned value must be dynamically allocated. </b>
 *)
//typedef char  *      ( *VortexSaslAuthCramMd5)         (VortexConnection * connection,
//                                                        const char       * auth_id);
type
  TVortexSaslAuthCramMd5 = function (connection: PVortexConnection;
    const auth_id: Pchar): PChar; cdecl;
(**
 * @brief Asynchronous notification to enable user space code to
 * validate SASL CRAM MD5 request received.
 *
 * Inside this function, the programmer must implement the password
 * lookup for the given user (<b>auth_id</b>) and return a copy to it
 * in clear text.
 *
 * Functions using this handler are:
 *  - \ref vortex_sasl_set_cram_md5_validation_full
 * 
 * @param connection The connection where the SASL notification was received
 * @param auth_id User id to authenticate.
 * @param user_data The user defined pointer to be passed to future SASL callbacks.
 * 
 * @return The password the given auth_id have or NULL if the request
 * must be denied. Password returned must be the in plain text,
 * without any modification, as it would be introduced by the remote
 * peer. Later, the SASL engine will perform the hash operation to be
 * compared with the hash value received from the client
 * side. <b>Returned value must be dynamically allocated. </b>
 *)
//typedef char  *      ( *VortexSaslAuthCramMd5Full)         (VortexConnection * connection,
//                                                            const char       * auth_id,
//                                                            axlPointer         user_data);
type
  TVortexSaslAuthCramMd5Full = function (connection: PVortexConnection;
    const auth_id: Pchar; user_data: TaxlPointer): PChar; cdecl;

(**
 * @brief Asynchronous notification to enable user space to validate
 * SASL DIGEST MD5 received requests.
 *
 * Inside this function, the programmer must implement the password
 * lookup for the given user (<b>auth_id</b>) and return a copy to it
 * in clear text.
 *
 * Functions using this handler are:
 *   - \ref vortex_sasl_set_digest_md5_validation
 * 
 * @param connection The connection where the SASL notification was received.
 * @param auth_id User identifier to authenticate
 * @param authorization_id If set, requesting auth_id is asking to get authorized to act as this value.
 * @param realm Optional realm value where the auth_id and the authorization_id will be validated.
 * 
 * @return The password the given auth_id have or NULL if the request
 * must be denied. Password returned must be the in plain text,
 * without any modification, as it would be introduced by the remote
 * peer. Later, the SASL engine will perform the hash operation to be
 * compared with the hash value received from the client
 * side. <b>Returned value must be dynamically allocated. </b>
 *)
//typedef char  *      ( *VortexSaslAuthDigestMd5)       (VortexConnection * connection,
//                                                        const char       * auth_id,
//                                                        const char       * authorization_id,
//                                                        const char       * realm);
type
  TVortexSaslAuthDigestMd5 = function (connection:PVortexConnection; const auth_id:Pchar;
    const authorization_id:Pchar; const realm:Pchar):PChar;cdecl;

(**
 * @brief Asynchronous notification to enable user space to validate
 * SASL DIGEST MD5 received requests.
 *
 * Inside this function, the programmer must implement the password
 * lookup for the given user (<b>auth_id</b>) and return a copy to it
 * in clear text.
 *
 * Functions using this handler are:
 *   - \ref vortex_sasl_set_digest_md5_validation_full
 * 
 * @param connection The connection where the SASL notification was received.
 * @param auth_id User identifier to authenticate
 * @param authorization_id If set, requesting auth_id is asking to get authorized to act as this value.
 * @param realm Optional realm value where the auth_id and the authorization_id will be validated.
 * @param user_data The user defined pointer to be passed to future SASL callbacks.
 * 
 * @return The password the given auth_id have or NULL if the request
 * must be denied. Password returned must be the in plain text,
 * without any modification, as it would be introduced by the remote
 * peer. Later, the SASL engine will perform the hash operation to be
 * compared with the hash value received from the client
 * side. <b>Returned value must be dynamically allocated. </b>
 *)
//typedef char  *      ( *VortexSaslAuthDigestMd5Full)       (VortexConnection * connection,
//                                                            const char       * auth_id,
//                                                            const char       * authorization_id,
//                                                            const char       * realm,
//                                                            axlPointer         user_data);
type
  TVortexSaslAuthDigestMd5Full = function (connection:PVortexConnection; const auth_id:Pchar;
    const authorization_id:Pchar; const realm:Pchar; user_data: TaxlPointer):PChar;cdecl;

//int                vortex_sasl_init                      (VortexCtx            * ctx);
function             vortex_sasl_init                      (ctx:PVortexCtx):longint;
                                                            cdecl;external External_library name 'vortex_sasl_init';

//void               vortex_sasl_cleanup                   (VortexCtx            * ctx);
procedure            vortex_sasl_cleanup                   (ctx:PVortexCtx);
                                                            cdecl;external External_library name 'vortex_sasl_cleanup';

//axl_bool           vortex_sasl_set_propertie             (VortexConnection     * connection,
//                                                          VortexSaslProperties   prop,
//                                                          char                 * value,
//                                                          axlDestroyFunc         value_destroy);
function             vortex_sasl_set_propertie             (connection:PVortexConnection;
                                                            prop:TVortexSaslProperties;
                                                            value:Pchar;
                                                            value_destroy:TaxlDestroyFunc):Taxl_bool;
                                                            cdecl;external External_library name 'vortex_sasl_set_propertie';

//char             * vortex_sasl_get_propertie             (VortexConnection     * connection,
//                                                          VortexSaslProperties   prop);
function             vortex_sasl_get_propertie             (connection:PVortexConnection;
                                                            prop:TVortexSaslProperties):Pchar;
                                                            cdecl;external External_library name 'vortex_sasl_get_propertie';

//axl_bool           vortex_sasl_is_authenticated          (VortexConnection     * connection);
function             vortex_sasl_is_authenticated          (connection:PVortexConnection):Taxl_bool;
                                                            cdecl;external External_library name 'vortex_sasl_is_authenticated';

//char             * vortex_sasl_auth_method_used          (VortexConnection     * connection);
function             vortex_sasl_auth_method_used          (connection:PVortexConnection):Pchar;
                                                            cdecl;external External_library name 'vortex_sasl_auth_method_used';

//void               vortex_sasl_start_auth                (VortexConnection     * connection,
//                                                          const char           * profile,
//                                                          VortexSaslAuthNotify   process_status,
//                                                          axlPointer             user_data);
procedure            vortex_sasl_start_auth                (connection:PVortexConnection;
                                                            const profile:Pchar;
                                                            process_status:TVortexSaslAuthNotify;
                                                            user_data:TaxlPointer);
                                                            cdecl;external External_library name 'vortex_sasl_start_auth';

//void               vortex_sasl_start_auth_sync           (VortexConnection     * connection,
//                                                          const char           * profile,
//                                                          VortexStatus         * status,
//                                                          char                ** status_message);
procedure            vortex_sasl_start_auth_sync           (connection:PVortexConnection;
                                                            const profile:Pchar;
                                                            status:PVortexStatus;
                                                            status_message:PPchar);
                                                            cdecl;external External_library name 'vortex_sasl_start_auth_sync';

//void               vortex_sasl_set_external_validation        (VortexCtx * ctx, VortexSaslAuthExternal  auth_handler);
procedure            vortex_sasl_set_external_validation        (ctx:PVortexCtx; auth_handler:TVortexSaslAuthExternal);
                                                                 cdecl;external External_library name 'vortex_sasl_set_external_validation';

//void               vortex_sasl_set_external_validation_full   (VortexCtx * ctx, VortexSaslAuthExternalFull  auth_handler);
procedure            vortex_sasl_set_external_validation_full   (ctx:PVortexCtx; auth_handler:TVortexSaslAuthExternalFull);
                                                                 cdecl;external External_library name 'vortex_sasl_set_external_validation_full';

//void               vortex_sasl_set_anonymous_validation       (VortexCtx * ctx, VortexSaslAuthAnonymous auth_handler);
procedure            vortex_sasl_set_anonymous_validation       (ctx:PVortexCtx; auth_handler:TVortexSaslAuthAnonymous);
                                                                 cdecl;external External_library name 'vortex_sasl_set_anonymous_validation';

//void               vortex_sasl_set_anonymous_validation_full  (VortexCtx * ctx, VortexSaslAuthAnonymousFull auth_handler);
procedure            vortex_sasl_set_anonymous_validation_full  (ctx:PVortexCtx; auth_handler:TVortexSaslAuthAnonymousFull);
                                                                 cdecl;external External_library name 'vortex_sasl_set_anonymous_validation_full';

//void               vortex_sasl_set_plain_validation           (VortexCtx * ctx, VortexSaslAuthPlain     auth_handler);
procedure            vortex_sasl_set_plain_validation           (ctx:PVortexCtx; auth_handler:TVortexSaslAuthPlain);
                                                                 cdecl;external External_library name 'vortex_sasl_set_plain_validation';

//void               vortex_sasl_set_plain_validation_full      (VortexCtx * ctx, VortexSaslAuthPlainFull     auth_handler);
procedure            vortex_sasl_set_plain_validation_full      (ctx:PVortexCtx; auth_handler:TVortexSaslAuthPlainFull);
                                                                 cdecl;external External_library name 'vortex_sasl_set_plain_validation_full';

//void               vortex_sasl_set_cram_md5_validation        (VortexCtx * ctx, VortexSaslAuthCramMd5   auth_handler);
procedure            vortex_sasl_set_cram_md5_validation        (ctx:PVortexCtx; auth_handler:TVortexSaslAuthCramMd5);
                                                                 cdecl;external External_library name 'vortex_sasl_set_cram_md5_validation';

//void               vortex_sasl_set_cram_md5_validation_full   (VortexCtx * ctx, VortexSaslAuthCramMd5Full   auth_handler);
procedure            vortex_sasl_set_cram_md5_validation_full   (ctx:PVortexCtx; auth_handler:TVortexSaslAuthCramMd5Full);
                                                                 cdecl;external External_library name 'vortex_sasl_set_cram_md5_validation_full';

//void               vortex_sasl_set_digest_md5_validation      (VortexCtx * ctx, VortexSaslAuthDigestMd5 auth_handler);
procedure            vortex_sasl_set_digest_md5_validation      (ctx:PVortexCtx; auth_handler:TVortexSaslAuthDigestMd5);
                                                                 cdecl;external External_library name 'vortex_sasl_set_digest_md5_validation';

//void               vortex_sasl_set_digest_md5_validation_full (VortexCtx * ctx, VortexSaslAuthDigestMd5Full auth_handler);
procedure            vortex_sasl_set_digest_md5_validation_full (ctx:PVortexCtx; auth_handler:TVortexSaslAuthDigestMd5Full);
                                                                 cdecl;external External_library name 'vortex_sasl_set_digest_md5_validation_full';

//axl_bool           vortex_sasl_accept_negotiation             (VortexCtx  * ctx,
//                                                               const char * mech);
function             vortex_sasl_accept_negotiation             (ctx:PVortexCtx;
                                                                 const mech:Pchar):Taxl_bool;
                                                                 cdecl;external External_library name 'vortex_sasl_accept_negotiation';

//axl_bool           vortex_sasl_accept_negotiation_full        (VortexCtx  * ctx,
//                                                               const char * mech,
//                                                               axlPointer user_data);
function             vortex_sasl_accept_negotiation_full        (ctx:PVortexCtx;
                                                                 const mech:Pchar;
                                                                 user_data:TaxlPointer):Taxl_bool;
                                                                 cdecl;external External_library name 'vortex_sasl_accept_negotiation_full';

(* @} *)

implementation

{$inline on}
function AUTH_ID_FROM_CHANNEL(channel : PVortexChannel): PChar; inline;
begin
  if axl_true = vortex_sasl_is_authenticated (vortex_channel_get_connection (channel)) then
    Result := vortex_sasl_get_propertie (vortex_channel_get_connection (channel), VORTEX_SASL_AUTH_ID)
  else
    Result := '';
end;

function AUTH_ID_FROM_CONN(connection:PVortexConnection): PChar; inline;
begin
  if axl_true = vortex_sasl_is_authenticated (connection) then
    Result := vortex_sasl_get_propertie (connection, VORTEX_SASL_AUTH_ID)
  else
    Result := '';
end;

{$inline off}

end.

