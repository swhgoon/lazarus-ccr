unit vortex_xml_rpc;

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


(* include types *)
{$include vortex_xml_rpc_types.inc}

(**
 * \addtogroup vortex_xml_rpc  
 * @{ 
 *)

const
(**
 * @brief Unique uri profile identificador for the XML-RPC.
 *)
  VORTEX_XML_RPC_PROFILE = 'http://iana.org/beep/xmlrpc';

(**
 * @brief String used to flag xml-rpc channel status.
 *)
  XML_RPC_BOOT_STATE = 'vortex-xml-rpc:state';

(**
 * @brief String used to flag xml-rpc channel resource used to boot.
 *)
  XML_RPC_RESOURCE = 'vortex-xml-rpc:resource';

(**
 * @brief Async notification handler for the XML-RPC channel boot.
 *
 * This async handler is used by: 
 *  - \ref vortex_xml_rpc_boot_channel
 * 
 * @param booted_channel The channel already booted or NULL if
 * something have happened.
 *
 * @param status The status value for the XML-RPC boot process
 * request, \ref VortexOk if boot was ok, \ref VortexError if something
 * have happened.
 *
 * @param message A textual diagnostic message reporting current status.
 *
 * @param user_data User space data defined at the function requesting
 * this handler.
 *)
//typedef void ( * VortexXmlRpcBootNotify)               (VortexChannel    * booted_channel,
//                                                        VortexStatus       status,
//                                                        char             * message,
//                                                        axlPointer         user_data);
type
  TVortexXmlRpcBootNotify = procedure (booted_channel:PVortexChannel; status:TVortexStatus;
    amessage:Pchar; user_data:TaxlPointer);cdecl;

(**
 * @brief This async handler allows to control how is accepted XML-RPC
 * initial boot channel based on a particular resource.
 *
 * XML-RPC specificaiton states that the method invocation is
 * considered as a two-phase invocation. 
 *
 * The first phase, the channel boot, is defined as a channel
 * creation, where the XML-RPC profile is prepared and, under the same
 * step, it is asked for a particular resource value to be supported
 * by the server (or the listener which is actually the entity
 * receiving the invocation).
 *
 * The second step is actually the XML-RPC invocation. 
 *
 * This resource value, by default, is "/". This allows to group
 * services under resources like: "/sales", "/sales/dep-a", etc. This
 * also allows to ask to listerners if they support a particular
 * interface. Think about grouping services, which represents a
 * concrete interface, under the same resource. Then, listener that
 * supports the resource are reporting that they, indeed, support a
 * particular interface.
 *
 * However, resource validation is not required at all. You can live
 * without it safely.
 *
 * As a note, you can pass a NULL handler reference to the \ref
 * vortex_xml_rpc_accept_negotiation, making the Vortex XML-RPC engine
 * to accept all resources (resource validation always evaluated to
 * true).
 *
 * This function is used by:
 *
 *  - \ref vortex_xml_rpc_accept_negotiation
 *
 * Here is an example of a resource validation handler that accept all
 * resources:
 * \code
 * int      validate_all_resources (VortexConnection * connection,
 *                                  int                channel_number,
 *                                  char             * serverName,
 *                                  char             * resource_path,
 *                                  axlPointer         user_data)
 * {
 *     // This is a kind of useless validation handler because 
 *     // it is doing the same like passing a NULL reference.
 *     // 
 *     // In this handler could be programmed any policy to validate a resource,
 *     // maybe based on the connection source,  the number of channels that the 
 *     // connection have, etc.
 *     return true;
 * }
 * \endcode
 * 
 * @param connection     The connection where the resource bootstrapping request was received.
 * @param channel_number The channel number to create requested.
 * @param serverName     An optional serverName value to act as.
 * @param resource_path  The resource path requested.
 * @param user_data      User space data.
 * 
 * @return axl_true to accept resource requested. axl_false if not.
 *)
//typedef axl_bool      ( *VortexXmlRpcValidateResource) (VortexConnection * connection,
//                                                        int                channel_number,
//                                                        const char       * serverName,
//                                                        const char       * resource_path,
//                                                        axlPointer         user_data);
type
  TVortexXmlRpcValidateResource = function (connection:PVortexConnection; channel_number:longint;
    const serverName:Pchar; const resource_path:Pchar; user_data:TaxlPointer):Taxl_bool;cdecl;

(**
 * @brief Async handler to process all incoming service invocation
 * through XML-RPC.
 *
 * This handler is executed to notify user space that a method
 * invocation has been received and should be dispatched, returning a
 * \ref XmlRpcMethodResponse. This reponse (\ref XmlRpcMethodResponse)
 * will be used to generate a reply as fast as possible.
 *
 * The method invocation, represented by this async handler, could
 * also return a NULL value, making it possible to not reply at the
 * moment the reply was received, but once generated the reply, the
 * method call object (\ref XmlRpcMethodCall) received should be used
 * in conjuction with the method response associated (\ref
 * XmlRpcMethodResponse) calling to \ref vortex_xml_rpc_notify_reply.
 *
 * Previous function, \ref vortex_xml_rpc_notify_reply, could allow to
 * marshall the invocation into another language execution, making it
 * easy because it is only required to marshall the method invocator
 * received but not to have a handler inside the runtime for the other
 * language. However, this is mainly provided to make it possible to defer
 * the reply.
 *
 *
 * @param channel The channel where the method call was received.
 *
 * @param method_call The method call object received, representing
 * the method being invoked (\ref XmlRpcMethodCall).
 *
 * @param user_data User space data. 
 *
 * @return The dispatch function should return a \ref
 * XmlRpcMethodResponse containing the value reply to be returned or
 * NULL if the method invocation have been deferred. 
 *)
//typedef XmlRpcMethodResponse *  ( *VortexXmlRpcServiceDispatch)  (VortexChannel    * channel,
//                                                                  XmlRpcMethodCall * method_call,
//                                                                  axlPointer         user_data);
type
  TVortexXmlRpcServiceDispatch = function (channel:PVortexChannel; method_call:PXmlRpcMethodCall;
    user_data:TaxlPointer):PXmlRpcMethodResponse;cdecl;

(**
 * @brief Async method response notifier.
 *
 * This handler is provided to enable Vortex XML-RPC engine to notify
 * the user space that a method response has been received.
 * 
 * This handler is used by:
 *
 *   - \ref vortex_xml_rpc_invoke
 * 
 * @param channel  The channel where the invocation took place.
 * @param response The reply received from the remote invocation.
 * @param user_data User defined data.
 * 
 *)
//typedef void     ( * XmlRpcInvokeNotify)          (VortexChannel        * channel,
//                                                  XmlRpcMethodResponse * response,
//                                                  axlPointer             user_data);
type
  TXmlRpcInvokeNotify = procedure (channel:PVortexChannel; response:PXmlRpcMethodResponse; user_data:TaxlPointer);cdecl;

(**
 * @brief Enum value to represent XML-RPC channel initialization
 * status.
 *)
type
  TVortexXmlRpcState = (
  	(**
  	 * @brief Value representing an error while getting current
  	 * XML-RPC channel status, either because the channel checked
  	 * is not properly created or it is created under a different
  	 * profile than XML-RPC profile.
  	 *)
  	XmlRpcStateUnknown,
  	(**
  	 * @brief The XML-RPC channel is running under boot state,
  	 * that is, it is still not possible to perform XML-RPC,
  	 * resource initialization wasn't done yet.
  	 *)
  	XmlRpcStateBoot,
  	(**
  	 * @brief The XML-RPC channel is running under ready state,
  	 * that is, it is ready to perform XML-RPC requests.
  	 *)
  	XmlRpcStateReady
  );

//void                vortex_xml_rpc_boot_channel            (VortexConnection        * connection,
//                                                            const char              * serverName,
//                                                            const char              * resourceName,
//                                                            VortexXmlRpcBootNotify    process_status,
//                                                            axlPointer                  user_data);
procedure             vortex_xml_rpc_boot_channel            (connection:PVortexConnection;
                                                              const serverName:Pchar;
                                                              const resourceName:Pchar;
                                                              process_status:TVortexXmlRpcBootNotify;
                                                              user_data:TaxlPointer);
                                                              cdecl;external External_library name 'vortex_xml_rpc_boot_channel';
(**
 * @brief Allows to boot an XML-RPC channel, in a synchronous way,
 * calling to \ref vortex_xml_rpc_boot_channel_sync.
 *
 * The macro will create an XML-RPC channel under the provided
 * resource (res). The resource value could be null, so the default
 * will be used: "/".
 *
 * The macro will pass null values for the status and the status
 * message. If it is required to get a complete error reporting you
 * must use: \ref vortex_xml_rpc_boot_channel_sync. Here is an
 * example:
 *
 * \code
 * // create an XML-RPC channel under the resource "/"
 * VortexChannel * channel = BOOT_CHANNEL(connection, NULL);
 *
 * // check the result 
 * if (channel == NULL) {
 *      printf ("Something have failed..\n");
 * }
 * \endcode
 * 
 * @param connection The connection where the XML-RPC channel will be
 * created.
 *
 * @param res The resource value or NULL if it fails.
 * 
 * @return The channel created and already booted or NULL if it fails.
 *)
//#define BOOT_CHANNEL(connection, res) (vortex_xml_rpc_boot_channel_sync(connection, NULL, res, NULL, NULL))
procedure BOOT_CHANNEL(connection:PVortexConnection; const resourceName:Pchar); inline;

//VortexChannel     * vortex_xml_rpc_boot_channel_sync       (VortexConnection        * connection,
//                                                            const char              * serverName,
//                                                            const char              * resourceName,
//                                                            VortexStatus            * status,
//                                                            char                   ** status_message);
function              vortex_xml_rpc_boot_channel_sync       (connection:PVortexConnection;
                                                              const serverName:Pchar;
                                                              const resourceName:Pchar;
                                                              status:PVortexStatus;
                                                              status_message:PPchar):PVortexChannel;
                                                              cdecl;external External_library name 'vortex_xml_rpc_boot_channel_sync';

//VortexChannelPool * vortex_xml_rpc_create_channel_pool     (VortexConnection            * connection,
//                                                            const char                  * serverName,
//                                                            const char                  * resourceName,
//                                                            VortexOnChannelPoolCreated    on_pool_created,
//                                                            axlPointer                    user_data);
function              vortex_xml_rpc_create_channel_pool     (connection:PVortexConnection;
                                                              const serverName: Pchar;
                                                              const resourceName:Pchar;
                                                              on_pool_created:TVortexOnChannelPoolCreated;
                                                              user_data:TaxlPointer):PVortexChannelPool;
                                                              cdecl;external External_library name 'vortex_xml_rpc_create_channel_pool';

//VortexChannel     * vortex_xml_rpc_channel_pool_get_next   (VortexConnection * connection,
//                                                            axl_bool           auto_inc,
//                                                            int                pool_id);
function              vortex_xml_rpc_channel_pool_get_next   (connection:PVortexConnection;
                                                              auto_inc:Taxl_bool;
                                                              pool_id:longint):PVortexChannel;
                                                              cdecl;external External_library name 'vortex_xml_rpc_channel_pool_get_next';

//axl_bool               vortex_xml_rpc_invoke               (VortexChannel           * channel,
//                                                            XmlRpcMethodCall        * method_call,
//                                                            XmlRpcInvokeNotify        reply_notify,
//                                                            axlPointer                user_data);
function                 vortex_xml_rpc_invoke               (channel:PVortexChannel;
                                                              method_call:PXmlRpcMethodCall;
                                                              reply_notify:TXmlRpcInvokeNotify;
                                                              user_data:TaxlPointer):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_xml_rpc_invoke';

//axl_bool               vortex_xml_rpc_notify_reply         (XmlRpcMethodCall        * method_call,
//                                                            XmlRpcMethodResponse    * method_response);
function                 vortex_xml_rpc_notify_reply         (method_call:PXmlRpcMethodCall;
                                                              method_response:PXmlRpcMethodResponse):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_xml_rpc_notify_reply';

//XmlRpcMethodResponse * vortex_xml_rpc_invoke_sync          (VortexChannel           * channel,
//                                                            XmlRpcMethodCall        * method_call);
function                 vortex_xml_rpc_invoke_sync          (channel:PVortexChannel;
                                                              method_call:PXmlRpcMethodCall):PXmlRpcMethodResponse;
                                                              cdecl;external External_library name 'vortex_xml_rpc_invoke_sync';

//VortexXmlRpcState   vortex_xml_rpc_channel_status          (VortexChannel * channel);
function              vortex_xml_rpc_channel_status          (channel:PVortexChannel):TVortexXmlRpcState;
                                                              cdecl;external External_library name 'vortex_xml_rpc_channel_status';

//const char        * vortex_xml_rpc_channel_get_resource    (VortexChannel * channel);
function              vortex_xml_rpc_channel_get_resource    (channel:PVortexChannel):Pchar;
                                                              cdecl;external External_library name 'vortex_xml_rpc_channel_get_resource';

//axl_bool            vortex_xml_rpc_accept_negotiation      (VortexCtx                    * ctx,
//                                                            VortexXmlRpcValidateResource   validate_resource,
//                                                            axlPointer                     validate_user_data,
//                                                            VortexXmlRpcServiceDispatch    service_dispatch,
//                                                            axlPointer                     dispatch_user_data);
function              vortex_xml_rpc_accept_negotiation      (ctx:PVortexCtx;
                                                              validate_resource:TVortexXmlRpcValidateResource;
                                                              validate_user_data:TaxlPointer;
                                                              service_dispatch:TVortexXmlRpcServiceDispatch;
                                                              dispatch_user_data:TaxlPointer):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_xml_rpc_accept_negotiation';

//axl_bool            vortex_xml_rpc_listener_parse_conf_and_start_listeners (VortexCtx * ctx);
function              vortex_xml_rpc_listener_parse_conf_and_start_listeners (ctx:PVortexCtx):Taxl_bool;
                                                                              cdecl;external External_library name 'vortex_xml_rpc_listener_parse_conf_and_start_listeners';

(**
 * @internal
 * 
 * Internal prototype declaration to support a reference from
 * __vortex_xml_rpc_parse_struct_value to this function.
 *)
//XmlRpcMethodValue * __vortex_xml_rpc_parse_value (VortexCtx * ctx, axlNode * value);
function              __vortex_xml_rpc_parse_value (ctx:PVortexCtx; value:PaxlNode):PXmlRpcMethodValue;
                                                    cdecl;external External_library name '__vortex_xml_rpc_parse_value';

(* int unmarshallers *)
//void vortex_xml_rpc_unmarshall_int      (VortexChannel *channel,
//                                        XmlRpcMethodResponse *response,
//                                        axlPointer user_data);
procedure vortex_xml_rpc_unmarshall_int   (channel:PVortexChannel;
                                           response:PXmlRpcMethodResponse;
                                           user_data:TaxlPointer);
                                           cdecl;external External_library name 'vortex_xml_rpc_unmarshall_int';

//int   vortex_xml_rpc_unmarshall_int_sync (XmlRpcMethodResponse  * response,
//                                          XmlRpcResponseStatus  * status,
//                                          VortexChannel         * channel,
//                                          int                   * fault_code,
//                                          char                 ** fault_string);
function vortex_xml_rpc_unmarshall_int_sync(response:PXmlRpcMethodResponse;
                                            status:PXmlRpcResponseStatus;
                                            channel:PVortexChannel;
                                            var fault_code:longint;
                                            fault_string:PPchar):longint;
                                            cdecl;external External_library name 'vortex_xml_rpc_unmarshall_int_sync';

(* string unmarshallers *)
//void vortex_xml_rpc_unmarshall_string      (VortexChannel *channel,
//                                            XmlRpcMethodResponse *response,
//                                            axlPointer user_data);
procedure  vortex_xml_rpc_unmarshall_string  (channel:PVortexChannel;
                                              response:PXmlRpcMethodResponse;
                                              user_data:TaxlPointer);
                                              cdecl;external External_library name 'vortex_xml_rpc_unmarshall_string';

//char  * vortex_xml_rpc_unmarshall_string_sync (XmlRpcMethodResponse  * response,
//                                              XmlRpcResponseStatus  * status,
//                                              VortexChannel         * channel,
//                                              int                   * fault_code,
//                                              char                 ** fault_string);
function vortex_xml_rpc_unmarshall_string_sync  (response:PXmlRpcMethodResponse;
                                                 status:PXmlRpcResponseStatus;
                                                 channel:PVortexChannel;
                                                 var fault_code:longint;
                                                 fault_string:PPchar):PPchar;
                                                 cdecl;external External_library name 'vortex_xml_rpc_unmarshall_string_sync';

(* double unmarshallers *)
//void vortex_xml_rpc_unmarshall_double (VortexChannel *channel,
//                                        XmlRpcMethodResponse *response,
//                                        axlPointer user_data);
procedure vortex_xml_rpc_unmarshall_double(channel:PVortexChannel;
                                           response:PXmlRpcMethodResponse;
                                           user_data:TaxlPointer);
                                           cdecl;external External_library name 'vortex_xml_rpc_unmarshall_double';

//double  vortex_xml_rpc_unmarshall_double_sync (XmlRpcMethodResponse  * response,
//                                              XmlRpcResponseStatus  * status,
//                                              VortexChannel         * channel,
//                                              int                   * fault_code,
//                                              char                 ** fault_string);
function  vortex_xml_rpc_unmarshall_double_sync(response:PXmlRpcMethodResponse;
                                                status:PXmlRpcResponseStatus;
                                                channel:PVortexChannel;
                                                var fault_code:longint;
                                                fault_string:PPchar):double;
                                                cdecl;external External_library name 'vortex_xml_rpc_unmarshall_double_sync';

(* struct unmarshallers *)
//void vortex_xml_rpc_unmarshall_struct (VortexChannel *channel,
//                                      XmlRpcMethodResponse *response,
//                                      axlPointer user_data);
procedure vortex_xml_rpc_unmarshall_struct(channel:PVortexChannel;
                                           response:PXmlRpcMethodResponse;
                                           user_data:TaxlPointer);
                                           cdecl;external External_library name 'vortex_xml_rpc_unmarshall_struct';

//axlPointer vortex_xml_rpc_unmarshall_struct_sync (XmlRpcMethodResponse     * response,
//                                                  XmlRpcStructUnMarshaller   unmarshaller,
//                                                  XmlRpcResponseStatus     * status,
//                                                  VortexChannel            * channel,
//                                                  int                      * fault_code,
//                                                  char                    ** fault_string);
function     vortex_xml_rpc_unmarshall_struct_sync (response:PXmlRpcMethodResponse;
                                                    unmarshaller:TXmlRpcStructUnMarshaller;
                                                    status:PXmlRpcResponseStatus;
                                                    channel:PVortexChannel;
                                                    var fault_code:longint;
                                                    fault_string:PPchar):TaxlPointer;
                                                    cdecl;external External_library name 'vortex_xml_rpc_unmarshall_struct_sync';

(* array unmarshallers *)
//void vortex_xml_rpc_unmarshall_array  (VortexChannel *channel,
//                                        XmlRpcMethodResponse *response,
//                                        axlPointer user_data);
procedure vortex_xml_rpc_unmarshall_array(channel:PVortexChannel;
                                          response:PXmlRpcMethodResponse;
                                          user_data:TaxlPointer);
                                          cdecl;external External_library name 'vortex_xml_rpc_unmarshall_array';

//axlPointer vortex_xml_rpc_unmarshall_array_sync (XmlRpcMethodResponse    * response,
//                                                  XmlRpcArrayUnMarshaller   unmarshaller,
//                                                  XmlRpcResponseStatus    * status,
//                                                  VortexChannel           * channel,
//                                                  int                     * fault_code,
//                                                  char                   ** fault_string);
function     vortex_xml_rpc_unmarshall_array_sync (response:PXmlRpcMethodResponse;
                                                   unmarshaller:TXmlRpcArrayUnMarshaller;
                                                   status:PXmlRpcResponseStatus;
                                                   channel:PVortexChannel;
                                                   var fault_code:longint;
                                                   fault_string:PPchar):TaxlPointer;
                                                   cdecl;external External_library name 'vortex_xml_rpc_unmarshall_array_sync';

//axl_bool  vortex_xml_rpc_init    (VortexCtx * ctx);
function    vortex_xml_rpc_init    (ctx:PVortexCtx):Taxl_bool;
                                    cdecl;external External_library name 'vortex_xml_rpc_init';

//void      vortex_xml_rpc_cleanup (VortexCtx * ctx);
procedure   vortex_xml_rpc_cleanup (ctx:PVortexCtx);
                                    cdecl;external External_library name 'vortex_xml_rpc_cleanup';

(* @} *)

implementation

{$include vortex_xml_rpc_types_imp.inc}

{$inline on}
procedure BOOT_CHANNEL(connection:PVortexConnection; const resourceName:Pchar); inline;
begin
  vortex_xml_rpc_boot_channel_sync(connection, nil, resourceName, nil, nil);
end;
{$inline off}
end.
