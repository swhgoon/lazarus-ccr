
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
 * \addtogroup vortex_connection
 * @{
 *)

(**
 * @brief Allows to get the associated serverName value under which
 * the connection is working. The serverName value is a centric
 * concept inside BEEP and defines under which name is acting the
 * connection. In general, this value is used to implement resource
 * separation based on the serverName configured.
 *
 * A key concept is that the serverName value is negociated, accepted
 * and fixed for the rest of the session on the first successful
 * channel created on the connection. Once done, no diferent
 * serverName can be negociated.
 *
 * @param channel The channel that is requested to return the
 * serverName associated to the connection holding the channel.
 * 
 * @return The serverName associated or NULL if none is defined.
 *)
{$if 0}
#define SERVER_NAME_FROM_CHANNEL(channel) (vortex_connection_get_server_name (vortex_channel_get_connection (channel)))
{TODO: Define the function in the correct place. The .inc file can't have an
 implementation section}
{$endif}

(**
 * @brief Allows to get the context associated to the provided
 * connection, logging a more verbose message if the context is null
 * or the connection provided is null.
 *
 * For vortex, the context object is central for its
 * function. Providing a function that warns that a null context is
 * returned is a key to find bugs, since no proper result could be
 * expected if no context is provided (\ref VortexCtx). 
 *
 * @param c The connection that is required to return the context
 * associated.
 *)
{$if 0}
#define CONN_CTX(c) vortex_connection_get_ctx_aux(__AXL_FILE__, __AXL_LINE__, c)
{TODO: Define the function in the correct place. The .inc file can't have an
 implementation section}
{$endif}

//VortexConnection  * vortex_connection_new                    (VortexCtx            * ctx,
//                                                              const char           * host,
//                                                              const char           * port,
//                                                              VortexConnectionNew    on_connected,
//                                                              axlPointer             user_data);
function              vortex_connection_new                    (ctx          : PVortexCtx;
                                                                host         : Pchar;
                                                                port         : Pchar;
                                                                on_connected : TVortexConnectionNew;
                                                                user_data    : TaxlPointer):PVortexConnection;
                                                                cdecl;external External_library name 'vortex_connection_new';

//axl_bool            vortex_connection_reconnect              (VortexConnection * connection,
//                                                              VortexConnectionNew on_connected,
//                                                              axlPointer user_data);
function              vortex_connection_reconnect              (connection   : PVortexConnection;
                                                                on_connected : TVortexConnectionNew;
                                                                user_data    : TaxlPointer):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_reconnect';

//axl_bool            vortex_connection_close                  (VortexConnection  * connection);
function              vortex_connection_close                  (connection : PVortexConnection):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_close';

//VORTEX_SOCKET       vortex_connection_sock_connect           (VortexCtx   * ctx,
//                                                              const char  * host,
//                                                              const char  * port,
//                                                              int         * timeout,
//                                                              axlError   ** error);
function              vortex_connection_sock_connect           (ctx         : PVortexCtx;
                                                                host        : Pchar;
                                                                port        : Pchar;
                                                                var timeout : longint;
                                                                error       : PPaxlError):TVORTEX_SOCKET;
                                                                cdecl;external External_library name 'vortex_connection_sock_connect';

//axl_bool            vortex_connection_do_greetings_exchange  (VortexCtx        * ctx,
//                                                              VortexConnection * connection,
//                                                              int                timeout);
function              vortex_connection_do_greetings_exchange  (ctx        : PVortexCtx;
                                                                connection : PVortexConnection;
                                                                timeout    : longint):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_do_greetings_exchange';

//axl_bool            vortex_connection_close_all_channels     (VortexConnection * connection,
//                                                              axl_bool           also_channel_0);
function              vortex_connection_close_all_channels     (connection     : PVortexConnection;
                                                                also_channel_0 : Taxl_bool):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_close_all_channels';

//axl_bool            vortex_connection_ref                    (VortexConnection * connection,
//                                                              const char       * who);
function              vortex_connection_ref                    (connection : PVortexConnection;
                                                                who        : Pchar):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_ref';

//axl_bool            vortex_connection_uncheck_ref            (VortexConnection * connection);
function              vortex_connection_uncheck_ref            (connection : PVortexConnection):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_uncheck_ref';

//void                vortex_connection_unref                  (VortexConnection * connection,
//                                                              const char       * who);
procedure             vortex_connection_unref                  (connection : PVortexConnection;
                                                                who        : Pchar);
                                                                cdecl;external External_library name 'vortex_connection_unref';

//int                 vortex_connection_ref_count              (VortexConnection * connection);
function              vortex_connection_ref_count              (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_ref_count';

//VortexConnection  * vortex_connection_new_empty              (VortexCtx        * ctx,
//                                                              VORTEX_SOCKET      socket,
//                                                              VortexPeerRole     role);
function              vortex_connection_new_empty              (ctx    : PVortexCtx;
                                                                socket : TVORTEX_SOCKET;
                                                                role   : TVortexPeerRole):PVortexConnection;
                                                                cdecl;external External_library name 'vortex_connection_new_empty';

//VortexConnection  * vortex_connection_new_empty_from_connection (VortexCtx        * ctx,
//                                                                 VORTEX_SOCKET      socket,
//                                                                 VortexConnection * __connection,
//                                                                 VortexPeerRole     role);
function              vortex_connection_new_empty_from_connection (ctx          : PVortexCtx;
                                                                   socket       : TVORTEX_SOCKET;
                                                                   __connection : PVortexConnection;
                                                                   role         : TVortexPeerRole):PVortexConnection;
                                                                   cdecl;external External_library name 'vortex_connection_new_empty_from_connection';

//axl_bool            vortex_connection_set_socket                (VortexConnection * conn,
//                                                                 VORTEX_SOCKET      socket,
//                                                                 const char       * real_host,
//                                                                 const char       * real_port);
function              vortex_connection_set_socket                (conn      : PVortexConnection;
                                                                   socket    : TVORTEX_SOCKET;
                                                                   real_host : Pchar;
                                                                   real_port : Pchar):Taxl_bool;
                                                                   cdecl;external External_library name 'vortex_connection_set_socket';

//void                vortex_connection_timeout                (VortexCtx        * ctx,
//                                                              long int           microseconds_to_wait);
procedure             vortex_connection_timeout                (ctx                  : PVortexCtx;
                                                                microseconds_to_wait : int64);
                                                                cdecl;external External_library name 'vortex_connection_timeout';

//void                vortex_connection_connect_timeout        (VortexCtx        * ctx,
//                                                              long int           microseconds_to_wait);
procedure             vortex_connection_connect_timeout        (ctx                  : PVortexCtx;
                                                                microseconds_to_wait : int64);
                                                                cdecl;external External_library name 'vortex_connection_connect_timeout';

//long int            vortex_connection_get_timeout            (VortexCtx        * ctx);
function              vortex_connection_get_timeout            (ctx : PVortexCtx):int64;
                                                                cdecl;external External_library name 'vortex_connection_get_timeout';

//long int            vortex_connection_get_connect_timeout    (VortexCtx        * ctx);
function              vortex_connection_get_connect_timeout    (ctx : PVortexCtx):int64;
                                                                cdecl;external External_library name 'vortex_connection_get_connect_timeout';

//axl_bool            vortex_connection_is_ok                  (VortexConnection * connection,
//                                                              axl_bool           free_on_fail);
function              vortex_connection_is_ok                  (connection   : PVortexConnection;
                                                                free_on_fail : Taxl_bool):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_is_ok';

//char              * vortex_connection_get_message            (VortexConnection * connection);
function              vortex_connection_get_message            (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_message';

//VortexStatus        vortex_connection_get_status             (VortexConnection * connection);
function              vortex_connection_get_status             (connection : PVortexConnection):TVortexStatus;
                                                                cdecl;external External_library name 'vortex_connection_get_status';

//axl_bool            vortex_connection_pop_channel_error      (VortexConnection  * connection,
//                                                              int               * code,
//                                                              char             ** msg);
function              vortex_connection_pop_channel_error      (connection : PVortexConnection;
                                                                var code   : longint;
                                                                msg        : PPchar):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_pop_channel_error';

//void                vortex_connection_push_channel_error     (VortexConnection  * connection,
//                                                              int                 code,
//                                                              char              * msg);
procedure             vortex_connection_push_channel_error     (connection : PVortexConnection;
                                                                code       : longint;
                                                                msg        : Pchar);
                                                                cdecl;external External_library name 'vortex_connection_push_channel_error';

//void                vortex_connection_free                   (VortexConnection * connection);
procedure             vortex_connection_free                   (connection : PVortexConnection);
                                                                cdecl;external External_library name 'vortex_connection_free';

//axlList           * vortex_connection_get_remote_profiles    (VortexConnection * connection);
function              vortex_connection_get_remote_profiles    (connection : PVortexConnection):PaxlList;
                                                                cdecl;external External_library name 'vortex_connection_get_remote_profiles';

//int                 vortex_connection_set_profile_mask       (VortexConnection      * connection,
//                                                              VortexProfileMaskFunc   mask,
//                                                              axlPointer              user_data);
function              vortex_connection_set_profile_mask       (connection : PVortexConnection;
                                                                mask       : TVortexProfileMaskFunc;
                                                                user_data  : TaxlPointer):longint;
                                                                cdecl;external External_library name 'vortex_connection_set_profile_mask';

//axl_bool            vortex_connection_is_profile_filtered    (VortexConnection      * connection,
//                                                              int                     channel_num,
//                                                              const char            * uri,
//                                                              const char            * profile_content,
//                                                              const char            * serverName,
//                                                              char                 ** error_msg);
function              vortex_connection_is_profile_filtered    (connection      : PVortexConnection;
                                                                channel_num     : longint;
                                                                uri             : Pchar;
                                                                profile_content : Pchar;
                                                                serverName      : Pchar;
                                                                error_msg       : PPchar):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_is_profile_filtered';

//axl_bool            vortex_connection_is_profile_supported   (VortexConnection * connection,
//                                                              const char       * uri);
function              vortex_connection_is_profile_supported   (connection : PVortexConnection;
                                                                uri        : Pchar):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_is_profile_supported';

//axl_bool            vortex_connection_channel_exists         (VortexConnection * connection,
//                                                              int  channel_num);
function              vortex_connection_channel_exists         (connection  : PVortexConnection;
                                                                channel_num : longint):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_channel_exists';

//int                 vortex_connection_channels_count         (VortexConnection * connection);
function              vortex_connection_channels_count         (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_channels_count';

//int                 vortex_connection_foreach_channel        (VortexConnection   * connection,
//                                                              axlHashForeachFunc   func,
//                                                              axlPointer           user_data);
function              vortex_connection_foreach_channel        (connection : PVortexConnection;
                                                                func       : TaxlHashForeachFunc;
                                                                user_data  : TaxlPointer):longint;
                                                                cdecl;external External_library name 'vortex_connection_foreach_channel';

//int                 vortex_connection_get_next_channel       (VortexConnection * connection);
function              vortex_connection_get_next_channel       (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_get_next_channel';

//VortexChannel     * vortex_connection_get_channel            (VortexConnection * connection,
//                                                              int  channel_num);
function              vortex_connection_get_channel            (connection  : PVortexConnection;
                                                                channel_num : longint):PVortexChannel;
                                                                cdecl;external External_library name 'vortex_connection_get_channel';

//VortexChannel     * vortex_connection_get_channel_by_uri     (VortexConnection * connection,
//                                                              const char       * profile);
function              vortex_connection_get_channel_by_uri     (connection : PVortexConnection;
                                                                profile    : Pchar):PVortexChannel;
                                                                cdecl;external External_library name 'vortex_connection_get_channel_by_uri';

//VortexChannel     * vortex_connection_get_channel_by_func    (VortexConnection     * connection,
//                                                              VortexChannelSelector  selector,
//                                                              axlPointer             user_data);
function              vortex_connection_get_channel_by_func    (connection : PVortexConnection;
                                                                selector   : TVortexChannelSelector;
                                                                user_data  : TaxlPointer):PVortexChannel;
                                                                cdecl;external External_library name 'vortex_connection_get_channel_by_func';

//int                 vortex_connection_get_channel_count      (VortexConnection     * connection,
//                                                              const char           * profile);
function              vortex_connection_get_channel_count      (connection : PVortexConnection;
                                                                profile    : Pchar):longint;
                                                                cdecl;external External_library name 'vortex_connection_get_channel_count';

//VORTEX_SOCKET       vortex_connection_get_socket             (VortexConnection * connection);
function              vortex_connection_get_socket             (connection:PVortexConnection):TVORTEX_SOCKET;
                                                                cdecl;external External_library name 'vortex_connection_get_socket';

//void                vortex_connection_set_close_socket       (VortexConnection * connection,
//                                                              axl_bool           action);
procedure             vortex_connection_set_close_socket       (connection : PVortexConnection;
                                                                action     : Taxl_bool);
                                                                cdecl;external External_library name 'vortex_connection_set_close_socket';

//void                vortex_connection_add_channel            (VortexConnection * connection,
//                                                              VortexChannel * channel);
procedure             vortex_connection_add_channel            (connection : PVortexConnection;
                                                                channel    : PVortexChannel);
                                                                cdecl;external External_library name 'vortex_connection_add_channel';

//void                vortex_connection_remove_channel         (VortexConnection * connection,
//                                                              VortexChannel * channel);
procedure             vortex_connection_remove_channel         (connection : PVortexConnection;
                                                                channel    : PVortexChannel);
                                                                cdecl;external External_library name 'vortex_connection_remove_channel';

//const char        * vortex_connection_get_host               (VortexConnection * connection);
function              vortex_connection_get_host               (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_host';

//const char        * vortex_connection_get_port               (VortexConnection * connection);
function              vortex_connection_get_port               (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_port';

//int                 vortex_connection_get_id                 (VortexConnection * connection);
function              vortex_connection_get_id                 (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_get_id';

//const char        * vortex_connection_get_server_name        (VortexConnection * connection);
function              vortex_connection_get_server_name        (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_server_name';

//void                vortex_connection_set_server_name        (VortexConnection * connection,
//                                                              const       char * serverName);
procedure             vortex_connection_set_server_name        (connection : PVortexConnection;
                                                                serverName : Pchar);
                                                                cdecl;external External_library name 'vortex_connection_set_server_name';

//axl_bool            vortex_connection_set_blocking_socket    (VortexConnection * connection);
function              vortex_connection_set_blocking_socket    (connection : PVortexConnection):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_set_blocking_socket';

//axl_bool            vortex_connection_set_nonblocking_socket (VortexConnection * connection);
function              vortex_connection_set_nonblocking_socket (connection : PVortexConnection):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_set_nonblocking_socket';

//axl_bool            vortex_connection_set_sock_tcp_nodelay   (VORTEX_SOCKET socket,
//                                                              axl_bool      enable);
function              vortex_connection_set_sock_tcp_nodelay   (socket : TVORTEX_SOCKET;
                                                                enable : Taxl_bool):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_set_sock_tcp_nodelay';

//axl_bool            vortex_connection_set_sock_block         (VORTEX_SOCKET socket,
//                                                              axl_bool      enable);
function              vortex_connection_set_sock_block         (socket : TVORTEX_SOCKET;
                                                                enable : Taxl_bool):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_set_sock_block';

//void                vortex_connection_set_data               (VortexConnection * connection,
//                                                              const char       * key,
//                                                              axlPointer         value);
procedure             vortex_connection_set_data               (connection : PVortexConnection;
                                                                key        : Pchar;
                                                                value      : TaxlPointer);
                                                                cdecl;external External_library name 'vortex_connection_set_data';

//void                vortex_connection_set_data_full          (VortexConnection * connection,
//                                                              char             * key,
//                                                              axlPointer         value,
//                                                              axlDestroyFunc     key_destroy,
//                                                              axlDestroyFunc     value_destroy);
procedure             vortex_connection_set_data_full          (connection    : PVortexConnection;
                                                                key           : Pchar;
                                                                value         : TaxlPointer;
                                                                key_destroy   : TaxlDestroyFunc;
                                                                value_destroy : TaxlDestroyFunc);
                                                                cdecl;external External_library name 'vortex_connection_set_data_full';

//void                vortex_connection_set_connection_actions (VortexCtx              * ctx,
//                                                              VortexConnectionStage    stage,
//                                                              VortexConnectionAction   action_handler,
//                                                              axlPointer               handler_data);
procedure             vortex_connection_set_connection_actions (ctx            : PVortexCtx;
                                                                stage          : TVortexConnectionStage;
                                                                action_handler : TVortexConnectionAction;
                                                                handler_data   : TaxlPointer);
                                                                cdecl;external External_library name 'vortex_connection_set_connection_actions';

//int                 vortex_connection_actions_notify         (VortexConnection        ** caller_conn,
//                                                              VortexConnectionStage      stage);
function              vortex_connection_actions_notify         (caller_conn : PPVortexConnection;
                                                                stage       : TVortexConnectionStage):longint;
                                                                cdecl;external External_library name 'vortex_connection_actions_notify';

//axlPointer          vortex_connection_get_data               (VortexConnection * connection,
//                                                              const char       * key);
function              vortex_connection_get_data               (connection : PVortexConnection;
                                                                key        : Pchar):TaxlPointer;
                                                                cdecl;external External_library name 'vortex_connection_get_data';

//VortexHash        * vortex_connection_get_channels_hash      (VortexConnection * connection);
function              vortex_connection_get_channels_hash      (connection : PVortexConnection):PVortexHash;
                                                                cdecl;external External_library name 'vortex_connection_get_channels_hash';

//VortexChannelPool * vortex_connection_get_channel_pool       (VortexConnection * connection,
//                                                              int                pool_id);
function              vortex_connection_get_channel_pool       (connection : PVortexConnection;
                                                                pool_id    : longint):PVortexChannelPool;
                                                                cdecl;external External_library name 'vortex_connection_get_channel_pool';

//int                 vortex_connection_get_pending_msgs       (VortexConnection * connection);
function              vortex_connection_get_pending_msgs       (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_get_pending_msgs';

//VortexPeerRole      vortex_connection_get_role               (VortexConnection * connection);
function              vortex_connection_get_role               (connection : PVortexConnection):TVortexPeerRole;
                                                                cdecl;external External_library name 'vortex_connection_get_role';

//const char        * vortex_connection_get_features           (VortexConnection * connection);
function              vortex_connection_get_features           (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_features';

//const char        * vortex_connection_get_localize           (VortexConnection * connection);
function              vortex_connection_get_localize           (connection : PVortexConnection):Pchar;
                                                                cdecl;external External_library name 'vortex_connection_get_localize';

//int                 vortex_connection_get_opened_channels    (VortexConnection * connection);
function              vortex_connection_get_opened_channels    (connection : PVortexConnection):longint;
                                                                cdecl;external External_library name 'vortex_connection_get_opened_channels';

//VortexConnection  * vortex_connection_get_listener           (VortexConnection * connection);
function              vortex_connection_get_listener           (connection : PVortexConnection):PVortexConnection;
                                                                cdecl;external External_library name 'vortex_connection_get_listener';

//VortexCtx         * vortex_connection_get_ctx                (VortexConnection * connection);
function              vortex_connection_get_ctx                (connection : PVortexConnection):PVortexCtx;
                                                                cdecl;external External_library name 'vortex_connection_get_ctx';

//VortexCtx         * vortex_connection_get_ctx_aux            (const char * file,
//                                                              int  line,
//                                                              VortexConnection * connection);
function              vortex_connection_get_ctx_aux            (afile      : Pchar;
                                                                line       : longint;
                                                                connection : PVortexConnection):PVortexCtx;
                                                                cdecl;external External_library name 'vortex_connection_get_ctx_aux';

//VortexSendHandler      vortex_connection_set_send_handler    (VortexConnection * connection,
//                                                              VortexSendHandler  send_handler);
function                 vortex_connection_set_send_handler    (connection   : PVortexConnection;
                                                                send_handler : TVortexSendHandler):TVortexSendHandler;
                                                                cdecl;external External_library name 'vortex_connection_set_send_handler';

//VortexReceiveHandler   vortex_connection_set_receive_handler (VortexConnection * connection,
//                                                              VortexReceiveHandler receive_handler);
function                 vortex_connection_set_receive_handler (connection      : PVortexConnection;
                                                                receive_handler : TVortexReceiveHandler):TVortexReceiveHandler;
                                                                cdecl;external External_library name 'vortex_connection_set_receive_handler';

//void                   vortex_connection_set_default_io_handler (VortexConnection * connection);
procedure                vortex_connection_set_default_io_handler (connection : PVortexConnection);
                                                                   cdecl;external External_library name 'vortex_connection_set_default_io_handler';

//void                   vortex_connection_set_on_close       (VortexConnection * connection,
//                                                             VortexConnectionOnClose on_close_handler);
procedure                vortex_connection_set_on_close       (connection       : PVortexConnection;
                                                               on_close_handler : TVortexConnectionOnClose);
                                                               cdecl;external External_library name 'vortex_connection_set_on_close';

//void                   vortex_connection_set_on_close_full  (VortexConnection * connection,
//                                                             VortexConnectionOnCloseFull on_close_handler,
//                                                             axlPointer data);
procedure                vortex_connection_set_on_close_full  (connection       : PVortexConnection;
                                                               on_close_handler : TVortexConnectionOnCloseFull;
                                                               data             : TaxlPointer);
                                                               cdecl;external External_library name 'vortex_connection_set_on_close_full';

//axl_bool            vortex_connection_remove_on_close_full   (VortexConnection              * connection,
//                                                              VortexConnectionOnCloseFull     on_close_handler,
//                                                              axlPointer                      data);
function              vortex_connection_remove_on_close_full   (connection       : PVortexConnection;
                                                                on_close_handler : TVortexConnectionOnCloseFull;
                                                                data             : TaxlPointer):Taxl_bool;
                                                                cdecl;external External_library name 'vortex_connection_remove_on_close_full';

//int                 vortex_connection_invoke_receive         (VortexConnection * connection,
//                                                              char             * buffer,
//                                                              int                buffer_len);
function              vortex_connection_invoke_receive         (connection : PVortexConnection;
                                                                buffer     : Pchar;
                                                                buffer_len : longint):longint;
                                                                cdecl;external External_library name 'vortex_connection_invoke_receive';

//int                 vortex_connection_invoke_send            (VortexConnection * connection,
//                                                              const char       * buffer,
//                                                              int                buffer_len);
function              vortex_connection_invoke_send            (connection : PVortexConnection;
                                                                buffer     : Pchar;
                                                                buffer_len : longint):longint;
                                                                cdecl;external External_library name 'vortex_connection_invoke_send';

//void                vortex_connection_sanity_socket_check        (VortexCtx * ctx,
//                                                                  axl_bool      enable);
procedure             vortex_connection_sanity_socket_check        (ctx    : PVortexCtx;
                                                                    enable : Taxl_bool);
                                                                    cdecl;external External_library name 'vortex_connection_sanity_socket_check';

//axl_bool            vortex_connection_parse_greetings_and_enable (VortexConnection * connection,
//                                                                  VortexFrame      * frame);
function              vortex_connection_parse_greetings_and_enable (connection : PVortexConnection;
                                                                    frame      : PVortexFrame):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_connection_parse_greetings_and_enable';

//void                vortex_connection_set_preread_handler        (VortexConnection * connection,
//                                                                  VortexConnectionOnPreRead pre_accept_handler);
procedure             vortex_connection_set_preread_handler        (connection         : PVortexConnection;
                                                                    pre_accept_handler : TVortexConnectionOnPreRead);
                                                                    cdecl;external External_library name 'vortex_connection_set_preread_handler';

//axl_bool            vortex_connection_is_defined_preread_handler (VortexConnection * connection);
function              vortex_connection_is_defined_preread_handler (connection : PVortexConnection):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_connection_is_defined_preread_handler';

//void                vortex_connection_invoke_preread_handler     (VortexConnection * connection);
procedure             vortex_connection_invoke_preread_handler     (connection : PVortexConnection);
                                                                    cdecl;external External_library name 'vortex_connection_invoke_preread_handler';

//void                vortex_connection_set_tlsfication_status     (VortexConnection * connection,
//                                                                  axl_bool           status);
procedure             vortex_connection_set_tlsfication_status     (connection : PVortexConnection;
                                                                    status     : Taxl_bool);
                                                                    cdecl;external External_library name 'vortex_connection_set_tlsfication_status';

//axl_bool            vortex_connection_is_tlsficated              (VortexConnection * connection);
function              vortex_connection_is_tlsficated              (connection : PVortexConnection):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_connection_is_tlsficated';

//void                vortex_connection_shutdown                   (VortexConnection * connection);
procedure             vortex_connection_shutdown                   (connection : PVortexConnection);
                                                                    cdecl;external External_library name 'vortex_connection_shutdown';

//void                vortex_connection_set_channel_added_handler  (VortexConnection                * connection,
//                                                                  VortexConnectionOnChannelUpdate   added_handler,
//                                                                  axlPointer                        user_data);
procedure             vortex_connection_set_channel_added_handler  (connection    : PVortexConnection;
                                                                    added_handler : TVortexConnectionOnChannelUpdate;
                                                                    user_data     : TaxlPointer);
                                                                    cdecl;external External_library name 'vortex_connection_set_channel_added_handler';

//void                vortex_connection_set_channel_removed_handler  (VortexConnection                * connection,
//                                                                    VortexConnectionOnChannelUpdate   removed_handler,
//                                                                    axlPointer                        user_data);
procedure             vortex_connection_set_channel_removed_handler  (connection      : PVortexConnection;
                                                                      removed_handler : TVortexConnectionOnChannelUpdate;
                                                                      user_data       : TaxlPointer);
                                                                      cdecl;external External_library name 'vortex_connection_set_channel_removed_handler';

//void                vortex_connection_block                        (VortexConnection * conn,
//                                                                    axl_bool           enable);
procedure             vortex_connection_block                        (conn   : PVortexConnection;
                                                                      enable : Taxl_bool);
                                                                      cdecl;external External_library name 'vortex_connection_block';

//axl_bool            vortex_connection_is_blocked                   (VortexConnection  * conn);
function              vortex_connection_is_blocked                   (conn : PVortexConnection):Taxl_bool;
                                                                      cdecl;external External_library name 'vortex_connection_is_blocked';

//int                 vortex_connection_get_next_frame_size          (VortexConnection * connection,
//                                                                    VortexChannel    * channel,
//                                                                    int                next_seq_no,
//                                                                    int                message_size,
//                                                                    int                max_seq_no);
function              vortex_connection_get_next_frame_size          (connection   : PVortexConnection;
                                                                      channel      : PVortexChannel;
                                                                      next_seq_no  : longint;
                                                                      message_size : longint;
                                                                      max_seq_no   : longint):longint;
                                                                      cdecl;external External_library name 'vortex_connection_get_next_frame_size';

//void                vortex_connection_seq_frame_updates            (VortexConnection * connection,
//                                                                    axl_bool           is_disabled);
procedure             vortex_connection_seq_frame_updates            (connection  : PVortexConnection;
                                                                      is_disabled : Taxl_bool);
                                                                      cdecl;external External_library name 'vortex_connection_seq_frame_updates';

//axl_bool            vortex_connection_seq_frame_updates_status     (VortexConnection * connection);
function              vortex_connection_seq_frame_updates_status     (connection : PVortexConnection):Taxl_bool;
                                                                      cdecl;external External_library name 'vortex_connection_seq_frame_updates_status';

//VortexChannelFrameSize  vortex_connection_set_next_frame_size_handler (VortexConnection        * connection,
//                                                                       VortexChannelFrameSize    next_frame_size,
//                                                                       axlPointer                user_data);
function                  vortex_connection_set_next_frame_size_handler (connection      : PVortexConnection;
                                                                         next_frame_size : TVortexChannelFrameSize;
                                                                         user_data       : TaxlPointer):TVortexChannelFrameSize;
                                                                         cdecl;external External_library name 'vortex_connection_set_next_frame_size_handler';

//VortexChannelFrameSize  vortex_connection_set_default_next_frame_size_handler (VortexCtx               * ctx,
//                                                                               VortexChannelFrameSize    next_frame_size,
//                                                                               axlPointer                user_data);
function                  vortex_connection_set_default_next_frame_size_handler (ctx             : PVortexCtx;
                                                                                 next_frame_size : TVortexChannelFrameSize;
                                                                                 user_data       : TaxlPointer):TVortexChannelFrameSize;
                                                                                 cdecl;external External_library name 'vortex_connection_set_default_next_frame_size_handler';

(* @} *)

