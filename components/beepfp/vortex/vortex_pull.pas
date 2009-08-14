unit vortex_pull;

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
 * \addtogroup vortex_pull
 * @{
 *)

(**
 * @brief Type representation for an event notification from the PULL
 * API. 
 *
 * An event has a type that can be check by using \ref
 * vortex_event_get_type. According to the type, the event has some
 * data associated which is meaningful to the event. 
 *
 * See \ref VortexEventType for events available and data associated
 * to them.
 *)
type
  PVortexEvent = Pointer;

(**
 * @brief List of events that can happen/pulled from the vortex pull
 * API. This list not only describes the event but also provides a
 * reference of all references that are associated to a particular
 * event.
 *
 * For example, the \ref VORTEX_EVENT_FRAME_RECEIVED signals that a
 * frame has arrive and a call to \ref vortex_event_get_frame will
 * return a reference to the frame. Not all events has all references
 * defined.
 * 
 * There are a list of references that is always defined. They are assumed implicitly:
 * - \ref vortex_event_get_ctx : the context where the event was received.
 * - \ref vortex_event_get_conn : the connection where the event happend.
 *)
type
  TVortexEventType = (
  	(**
  	 * @brief Undefined event. This type is used to report errors
  	 * found while using pull API.
  	 *)
  	VORTEX_EVENT_UNKNOWN              := 0,
  	(**
  	 * @brief Even type that represents a frame received. You must
  	 * call to vortex_pull_get_frame to get the reference to the
  	 * frame received.
  	 *
  	 * This event has the following especific references defined:
  	 * - \ref vortex_event_get_frame : frame received due to this event.
  	 * - \ref vortex_event_get_channel : the channel where the frame was received.
  	 *)
  	VORTEX_EVENT_FRAME_RECEIVED       := 1 shl 0,
  	(**
  	 * @brief This event signals that a channel close request has
  	 * been received. The peer must respond to the close request
  	 * as soon as possible since the block channel 0 for futher
  	 * operations.
  	 *
  	 * This event has the following especific references defined:
  	 * - \ref vortex_event_get_channel : the channel that is requested to be closed.
  	 * - \ref vortex_event_get_msgno : the msgno value that identifies the close request received.
  	 *
  	 * With these values, the close request must be replied by
  	 * using the function \ref vortex_channel_notify_close.
  	 *)
  	VORTEX_EVENT_CHANNEL_CLOSE       := 1 shl 1,

  	(**
  	 * @brief Event used to signal that a channel has been added
  	 * to a connection. This happens when a channel is created.
  	 *
  	 * This event has the following especific references defined:
  	 * - \ref vortex_event_get_channel : the channel that was added.
  	 *)
  	VORTEX_EVENT_CHANNEL_ADDED       := 1 shl 2,

  	(**
  	 * @brief Event used to signal that a channel has been removed
  	 * to a connection. This happens when a channel is closed
  	 *
  	 * This event has the following especific references defined:
  	 * - \ref vortex_event_get_channel : the channel that was added.
  	 *)
  	VORTEX_EVENT_CHANNEL_REMOVED     := 1 shl 3,

  	(**
  	 * @brief Event notification that a particular connection has
  	 * been closed. This event is also useful to detect connection
  	 * broken.
  	 *
  	 * This event has no especific references. It only has \ref
  	 * vortex_event_get_conn defined.
  	 *)
  	VORTEX_EVENT_CONNECTION_CLOSED   := 1 shl 4,

  	(**
  	 * @brief Event notificaiton that a new incoming connection
  	 * has been accepted due to a listener started (\ref
  	 * vortex_listener_new or similar).
  	 *)
  	VORTEX_EVENT_CONNECTION_ACCEPTED  := 1 shl 5,

  	(**
  	 * @brief Event notification to signal that a channel start
  	 * request has been received.
  	 *
  	 * This event has the following espefic references defined:
  	 *
  	 * - \ref vortex_event_get_server_name (the serverName channel
  	 * start value).
  	 *
  	 * - \ref vortex_event_get_profile_content (the profile
  	 * content found in the channel start request).
  	 *
  	 * - \ref vortex_event_get_encoding (the profile encoding
  	 * found in the channel start request).
  	 *
  	 * With these values the channel start request must be replied
  	 * by accepting or denying the channel by using \ref
  	 * vortex_channel_notify_start.
  	 *)
  	VORTEX_EVENT_CHANNEL_START        := 1 shl 6

  );

(**
 * @brief A type definition that allows to define a set of events that
 * are ignored on a particular context. The event mask (\ref
 * VortexEventMask) can configured on several context (\ref VortexCtx)
 * where PULL API is enabled.
 *
 * A \ref VortexEventMask has a string identifier defined by the user
 * (it is not required) and has an activation state that allows to
 * block the mask during a desired period.
 *
 * A \ref VortexEventMask is created by using \ref
 * vortex_event_mask_new. The following functions are used to add,
 * remove and check events ignored by the mask:
 *
 *  - \ref vortex_event_mask_add
 *  - \ref vortex_event_mask_remove
 *  - \ref vortex_event_mask_is_set
 *
 * Once configured mask is associated to a context (\ref VortexCtx) to
 * block events by using: \ref vortex_pull_set_event_mask
 *
 * Even having the mask installed, you can disable it temporally by
 * using \ref vortex_event_mask_enable.
 *)
type
  PVortexEventMask = Pointer;

//axl_bool           vortex_pull_init                        (VortexCtx * ctx);
function             vortex_pull_init                        (ctx:PVortexCtx):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_pull_init';

//void               vortex_pull_cleanup                     (VortexCtx * ctx);
procedure            vortex_pull_cleanup                     (ctx:PVortexCtx);
                                                              cdecl;external External_library name 'vortex_pull_cleanup';

//axl_bool           vortex_pull_pending_events              (VortexCtx * ctx);
function             vortex_pull_pending_events              (ctx:PVortexCtx):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_pull_pending_events';

//int                vortex_pull_pending_events_num          (VortexCtx * ctx);
function             vortex_pull_pending_events_num          (ctx:PVortexCtx):longint;
                                                              cdecl;external External_library name 'vortex_pull_pending_events_num';

//VortexEvent      * vortex_pull_next_event                  (VortexCtx * ctx,
//                                                            int         milliseconds_to_wait);
function             vortex_pull_next_event                  (ctx:PVortexCtx;
                                                              milliseconds_to_wait:longint):PVortexEvent;
                                                              cdecl;external External_library name 'vortex_pull_next_event';

//axl_bool           vortex_event_ref                        (VortexEvent * event);
function             vortex_event_ref                        (event:PVortexEvent):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_event_ref';

//void               vortex_event_unref                      (VortexEvent * event);
procedure            vortex_event_unref                      (event:PVortexEvent);
                                                              cdecl;external External_library name 'vortex_event_unref';

//VortexEventType    vortex_event_get_type                   (VortexEvent * event);
function             vortex_event_get_type                   (event:PVortexEvent):TVortexEventType;
                                                              cdecl;external External_library name 'vortex_event_get_type';

//VortexCtx        * vortex_event_get_ctx                    (VortexEvent * event);
function             vortex_event_get_ctx                    (event:PVortexEvent):PVortexCtx;
                                                              cdecl;external External_library name 'vortex_event_get_ctx';

//VortexConnection * vortex_event_get_conn                   (VortexEvent * event);
function             vortex_event_get_conn                   (event:PVortexEvent):PVortexConnection;
                                                              cdecl;external External_library name 'vortex_event_get_conn';

//VortexChannel    * vortex_event_get_channel                (VortexEvent * event);
function             vortex_event_get_channel                (event:PVortexEvent):PVortexChannel;
                                                              cdecl;external External_library name 'vortex_event_get_channel';

//VortexFrame      * vortex_event_get_frame                  (VortexEvent * event);
function             vortex_event_get_frame                  (event:PVortexEvent):PVortexFrame;
                                                              cdecl;external External_library name 'vortex_event_get_frame';

//int                vortex_event_get_msgno                  (VortexEvent * event);
function             vortex_event_get_msgno                  (event:PVortexEvent):longint;
                                                              cdecl;external External_library name 'vortex_event_get_msgno';

//const char       * vortex_event_get_server_name            (VortexEvent * event);
function             vortex_event_get_server_name            (event:PVortexEvent):Pchar;
                                                              cdecl;external External_library name 'vortex_event_get_server_name';

//const char       * vortex_event_get_profile_content        (VortexEvent * event);
function             vortex_event_get_profile_content        (event:PVortexEvent):Pchar;
                                                              cdecl;external External_library name 'vortex_event_get_profile_content';

//VortexEncoding     vortex_event_get_encoding               (VortexEvent * event);
function             vortex_event_get_encoding               (event:PVortexEvent):TVortexEncoding;
                                                              cdecl;external External_library name 'vortex_event_get_encoding';

//VortexEventMask  * vortex_event_mask_new                   (const char  * identifier,
//                                                            int           initial_mask,
//                                                            axl_bool      initial_state);
function             vortex_event_mask_new                   (const identifier:Pchar;
                                                              initial_mask:longint;
                                                              initial_state:Taxl_bool):PVortexEventMask;
                                                              cdecl;external External_library name 'vortex_event_mask_new';

//void               vortex_event_mask_add                   (VortexEventMask * mask,
//                                                            int               events);
procedure            vortex_event_mask_add                   (mask:PVortexEventMask;
                                                              events:longint);
                                                              cdecl;external External_library name 'vortex_event_mask_add';

//void               vortex_event_mask_remove                (VortexEventMask * mask,
//                                                            int               events);
procedure            vortex_event_mask_remove                (mask:PVortexEventMask;
                                                              events:longint);
                                                              cdecl;external External_library name 'vortex_event_mask_remove';

//axl_bool           vortex_event_mask_is_set                (VortexEventMask * mask,
//                                                            VortexEventType   event);
function             vortex_event_mask_is_set                (mask:PVortexEventMask;
                                                              event:TVortexEventType):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_event_mask_is_set';

//void               vortex_event_mask_enable                (VortexEventMask * mask,
//                                                            axl_bool          enable);
procedure            vortex_event_mask_enable                (mask:PVortexEventMask;
                                                              enable:Taxl_bool);
                                                              cdecl;external External_library name 'vortex_event_mask_enable';

//axl_bool           vortex_pull_set_event_mask              (VortexCtx        * ctx,
//                                                            VortexEventMask  * mask,
//                                                            axlError        ** error);
function             vortex_pull_set_event_mask              (ctx:PVortexCtx;
                                                              mask:PVortexEventMask;
                                                              error:PPaxlError):Taxl_bool;
                                                              cdecl;external External_library name 'vortex_pull_set_event_mask';

//void               vortex_event_mask_free                  (VortexEventMask * mask);
procedure            vortex_event_mask_free                  (mask:PVortexEventMask);
                                                              cdecl;external External_library name 'vortex_event_mask_free';

(**
 * @}
 *)

implementation


end.

