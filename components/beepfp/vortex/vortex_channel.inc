
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
 * \addtogroup vortex_channel
 * @{
 *)

(**
 * @brief Allows to get context associated to the channel provided.
 * @param channel The channel to get the context from.
 * @return A reference to the context.
 *)
{$if 0}
#define CHANNEL_CTX(channel) (vortex_channel_get_ctx (channel))
//Use the real function, not a #define
{$endif}

//VortexChannel     * vortex_channel_new                         (VortexConnection      * connection,
//                                                                int                     channel_num,
//                                                                const char            * profile,
//                                                                VortexOnCloseChannel    close,
//                                                                axlPointer              close_user_data,
//                                                                VortexOnFrameReceived   received,
//                                                                axlPointer              received_user_data,
//                                                                VortexOnChannelCreated  on_channel_created,
//                                                                axlPointer user_data);
function              vortex_channel_new                         (connection         : PVortexConnection;
                                                                  channel_num        : longint;
                                                                  profile            : Pchar;
                                                                  close              : TVortexOnCloseChannel;
                                                                  close_user_data    : TaxlPointer;
                                                                  received           : TVortexOnFrameReceived;
                                                                  received_user_data : TaxlPointer;
                                                                  on_channel_created : TVortexOnChannelCreated;
                                                                  user_data          : TaxlPointer):PVortexChannel;
                                                                  cdecl;external External_library name 'vortex_channel_new';

//VortexChannel     * vortex_channel_new_full                    (VortexConnection      * connection,
//                                                                int                     channel_num,
//                                                                const char            * serverName,
//                                                                const char            * profile,
//                                                                VortexEncoding          encoding,
//                                                                const char            * profile_content,
//                                                                int                     profile_content_size,
//                                                                VortexOnCloseChannel    close,
//                                                                axlPointer              close_user_data,
//                                                                VortexOnFrameReceived   received,
//                                                                axlPointer              received_user_data,
//                                                                VortexOnChannelCreated  on_channel_created, axlPointer user_data);
function              vortex_channel_new_full                    (connection           : PVortexConnection;
                                                                  channel_num          : longint;
                                                                  serverName           : Pchar;
                                                                  profile              : Pchar;
                                                                  encoding             : TVortexEncoding;
                                                                  profile_content      : Pchar;
                                                                  profile_content_size : longint;
                                                                  close                : TVortexOnCloseChannel;
                                                                  close_user_data      : TaxlPointer;
                                                                  received             : TVortexOnFrameReceived;
                                                                  received_user_data   : TaxlPointer;
                                                                  on_channel_created   : TVortexOnChannelCreated;
                                                                  user_data            : TaxlPointer):PVortexChannel;
                                                                  cdecl;external External_library name 'vortex_channel_new_full';

//VortexChannel     * vortex_channel_new_fullv                   (VortexConnection      * connection,
//                                                                int                     channel_num,
//                                                                const char            * serverName,
//                                                                const char            * profile,
//                                                                VortexEncoding          encoding,
//                                                                VortexOnCloseChannel    close,
//                                                                axlPointer              close_user_data,
//                                                                VortexOnFrameReceived   received,
//                                                                axlPointer              received_user_data,
//                                                                VortexOnChannelCreated  on_channel_created, axlPointer user_data,
//                                                                const char            * profile_content_format,
//                                                                ...);
function              vortex_channel_new_fullv                   (connection             : PVortexConnection;
                                                                  channel_num            : longint;
                                                                  serverName             : Pchar;
                                                                  profile                : Pchar;
                                                                  encoding               : TVortexEncoding;
                                                                  close                  : TVortexOnCloseChannel;
                                                                  close_user_data        : TaxlPointer;
                                                                  received               : TVortexOnFrameReceived;
                                                                  received_user_data     : TaxlPointer;
                                                                  on_channel_created     : TVortexOnChannelCreated;
                                                                  user_data              : TaxlPointer;
                                                                  profile_content_format : Pchar;
                                                                  args                   : array of const):PVortexChannel;
                                                                  cdecl;external External_library name 'vortex_channel_new_fullv';

function              vortex_channel_new_fullv                   (connection             : PVortexConnection;
                                                                  channel_num            : longint;
                                                                  serverName             : Pchar;
                                                                  profile                : Pchar;
                                                                  encoding               : TVortexEncoding;
                                                                  close                  : TVortexOnCloseChannel;
                                                                  close_user_data        : TaxlPointer;
                                                                  received               : TVortexOnFrameReceived;
                                                                  received_user_data     : TaxlPointer;
                                                                  on_channel_created     : TVortexOnChannelCreated;
                                                                  user_data              : TaxlPointer;
                                                                  profile_content_format : Pchar):PVortexChannel;
                                                                  cdecl;external External_library name 'vortex_channel_new_fullv';

//axl_bool           vortex_channel_close_full                   (VortexChannel * channel,
//                                                                VortexOnClosedNotificationFull on_closed,
//                                                                axlPointer user_data);
function             vortex_channel_close_full                   (channel   : PVortexChannel;
                                                                  on_closed : TVortexOnClosedNotificationFull;
                                                                  user_data : TaxlPointer):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_close_full';

//axl_bool           vortex_channel_close                        (VortexChannel * channel,
//                                                                VortexOnClosedNotification on_closed);
function             vortex_channel_close                        (channel   : PVortexChannel;
                                                                  on_closed : TVortexOnClosedNotification):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_close';

//VortexChannel    * vortex_channel_empty_new                    (int                channel_num,
//                                                                const char       * profile,
//                                                                VortexConnection * connection);
function             vortex_channel_empty_new                    (channel_num:longint;
                                                                  profile:Pchar;
                                                                  connection:PVortexConnection):PVortexChannel;
                                                                  cdecl;external External_library name 'vortex_channel_empty_new';

//void               vortex_channel_set_close_handler            (VortexChannel * channel,
//                                                                VortexOnCloseChannel close,
//                                                                axlPointer user_data);
procedure            vortex_channel_set_close_handler            (channel   : PVortexChannel;
                                                                  close     : TVortexOnCloseChannel;
                                                                  user_data : TaxlPointer);
                                                                  cdecl;external External_library name 'vortex_channel_set_close_handler';

//void               vortex_channel_set_closed_handler           (VortexChannel          * channel,
//                                                                VortexOnClosedChannel    closed,
//                                                                axlPointer               user_data);
procedure            vortex_channel_set_closed_handler           (channel   : PVortexChannel;
                                                                  closed    : TVortexOnClosedChannel;
                                                                  user_data : TaxlPointer);
                                                                  cdecl;external External_library name 'vortex_channel_set_closed_handler';

//void               vortex_channel_invoke_closed                (VortexChannel          * channel);
procedure            vortex_channel_invoke_closed                (channel : PVortexChannel);
                                                                  cdecl;external External_library name 'vortex_channel_invoke_closed';

//void               vortex_channel_set_close_notify_handler     (VortexChannel              * channel,
//                                                                VortexOnNotifyCloseChannel   close_notify,
//                                                                axlPointer                   user_data);
procedure            vortex_channel_set_close_notify_handler     (channel      : PVortexChannel;
                                                                  close_notify : TVortexOnNotifyCloseChannel;
                                                                  user_data    : TaxlPointer);
                                                                  cdecl;external External_library name 'vortex_channel_set_close_notify_handler';

//void               vortex_channel_set_received_handler         (VortexChannel * channel,
//                                                                VortexOnFrameReceived received,
//                                                                axlPointer user_data);
procedure            vortex_channel_set_received_handler         (channel   : PVortexChannel;
                                                                  received  : TVortexOnFrameReceived;
                                                                  user_data : TaxlPointer);
                                                                  cdecl;external External_library name 'vortex_channel_set_received_handler';

//void               vortex_channel_set_complete_flag            (VortexChannel * channel,
//                                                                axl_bool        value);
procedure            vortex_channel_set_complete_flag            (channel : PVortexChannel;
                                                                  value   : Taxl_bool);
                                                                  cdecl;external External_library name 'vortex_channel_set_complete_flag';

//axl_bool           vortex_channel_have_previous_frame          (VortexChannel * channel);
function             vortex_channel_have_previous_frame          (channel:PVortexChannel):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_have_previous_frame';

//VortexFrame      * vortex_channel_get_previous_frame           (VortexChannel * channel);
function             vortex_channel_get_previous_frame           (channel:PVortexChannel):PVortexFrame;
                                                                  cdecl;external External_library name 'vortex_channel_get_previous_frame';

//void               vortex_channel_store_previous_frame         (VortexChannel * channel,
//                                                                VortexFrame   * new_frame);
procedure            vortex_channel_store_previous_frame         (channel   : PVortexChannel;
                                                                  new_frame : PVortexFrame);
                                                                  cdecl;external External_library name 'vortex_channel_store_previous_frame';

//VortexFrame      * vortex_channel_build_single_pending_frame   (VortexChannel * channel);
function             vortex_channel_build_single_pending_frame   (channel : PVortexChannel):PVortexFrame;
                                                                  cdecl;external External_library name 'vortex_channel_build_single_pending_frame';

//axl_bool           vortex_channel_have_complete_flag           (VortexChannel * channel);
function             vortex_channel_have_complete_flag           (channel : PVortexChannel):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_have_complete_flag';

//void               vortex_channel_update_status                (VortexChannel * channel,
//                                                                unsigned int    frame_size,
//                                                                int             msg_no,
//                                                                WhatUpdate update);
procedure            vortex_channel_update_status                (channel    : PVortexChannel;
                                                                  frame_size : longword;
                                                                  msg_no     : longint;
                                                                  update     : TWhatUpdate);
                                                                  cdecl;external External_library name 'vortex_channel_update_status';

//void               vortex_channel_update_status_received       (VortexChannel * channel,
//                                                                unsigned int    frame_size,
//                                                                int             msg_no,
//                                                                WhatUpdate      update);
procedure            vortex_channel_update_status_received       (channel    : PVortexChannel;
                                                                  frame_size : longword;
                                                                  msg_no     : longint;
                                                                  update     : TWhatUpdate);
                                                                  cdecl;external External_library name 'vortex_channel_update_status_received';

//int                vortex_channel_get_next_msg_no              (VortexChannel * channel);
function             vortex_channel_get_next_msg_no              (channel:PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_msg_no';

//void               vortex_channel_set_next_seq_no              (VortexChannel * channel,
//                                                                unsigned int    next_seq_no);
procedure            vortex_channel_set_next_seq_no              (channel     : PVortexChannel;
                                                                  next_seq_no : longword);
                                                                  cdecl;external External_library name 'vortex_channel_set_next_seq_no';

//int                vortex_channel_get_last_msg_no_received     (VortexChannel * channel);
function             vortex_channel_get_last_msg_no_received     (channel : PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_last_msg_no_received';

//int                vortex_channel_get_number                   (VortexChannel * channel);
function             vortex_channel_get_number                   (channel : PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_number';

//unsigned int       vortex_channel_get_next_seq_no              (VortexChannel * channel);
function             vortex_channel_get_next_seq_no              (channel : PVortexChannel):longword;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_seq_no';

//unsigned int       vortex_channel_get_next_expected_seq_no     (VortexChannel * channel);
function             vortex_channel_get_next_expected_seq_no     (channel : PVortexChannel):longword;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_expected_seq_no';

//unsigned int       vortex_channel_get_next_ans_no              (VortexChannel * channel);
function             vortex_channel_get_next_ans_no              (channel : PVortexChannel):longword;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_ans_no';

//unsigned int       vortex_channel_get_next_expected_ans_no     (VortexChannel * channel);
function             vortex_channel_get_next_expected_ans_no     (channel:PVortexChannel):longword;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_expected_ans_no';

//int                vortex_channel_get_next_reply_no            (VortexChannel * channel);
function             vortex_channel_get_next_reply_no            (channel:PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_reply_no';

//int                vortex_channel_get_next_expected_reply_no   (VortexChannel * channel);
function             vortex_channel_get_next_expected_reply_no   (channel : PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_next_expected_reply_no';

//int                vortex_channel_get_window_size              (VortexChannel * channel);
function             vortex_channel_get_window_size              (channel : PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_window_size';

//void               vortex_channel_set_window_size              (VortexChannel * channel,
//                                                                int             desired_size);
procedure            vortex_channel_set_window_size              (channel : PVortexChannel;
                                                                  desired_size:longint);
                                                                  cdecl;external External_library name 'vortex_channel_set_window_size';

//const char       * vortex_channel_get_mime_type                (VortexChannel * channel);
function             vortex_channel_get_mime_type                (channel : PVortexChannel):Pchar;
                                                                  cdecl;external External_library name 'vortex_channel_get_mime_type';

//const char       * vortex_channel_get_transfer_encoding        (VortexChannel * channel);
function             vortex_channel_get_transfer_encoding        (channel : PVortexChannel):Pchar;
                                                                  cdecl;external External_library name 'vortex_channel_get_transfer_encoding';

//void               vortex_channel_set_automatic_mime           (VortexChannel  * channel,
//                                                                int              value);
procedure            vortex_channel_set_automatic_mime           (channel : PVortexChannel;
                                                                  value   : longint);
                                                                  cdecl;external External_library name 'vortex_channel_set_automatic_mime';

//int                vortex_channel_get_automatic_mime           (VortexChannel * channel);
function             vortex_channel_get_automatic_mime           (channel : PVortexChannel):longint;
                                                                  cdecl;external External_library name 'vortex_channel_get_automatic_mime';

//unsigned int       vortex_channel_get_max_seq_no_remote_accepted (VortexChannel * channel);
function             vortex_channel_get_max_seq_no_remote_accepted (channel : PVortexChannel):longword;
                                                                    cdecl;external External_library name 'vortex_channel_get_max_seq_no_remote_accepted';

//int                vortex_channel_get_next_frame_size         (VortexChannel * channel,
//                                                               unsigned int    next_seq_no,
//                                                               int             message_size,
//                                                               unsigned int    max_seq_no);
function             vortex_channel_get_next_frame_size         (channel      : PVortexChannel;
                                                                 next_seq_no  : longword;
                                                                 message_size : longint;
                                                                 max_seq_no   : longword):longint;
                                                                 cdecl;external External_library name 'vortex_channel_get_next_frame_size';

//VortexChannelFrameSize  vortex_channel_set_next_frame_size_handler (VortexChannel          * channel,
//                                                                    VortexChannelFrameSize   next_frame_size,
//                                                                    axlPointer               user_data);
function                  vortex_channel_set_next_frame_size_handler (channel         : PVortexChannel;
                                                                      next_frame_size : TVortexChannelFrameSize;
                                                                      user_data       : TaxlPointer):TVortexChannelFrameSize;
                                                                      cdecl;external External_library name 'vortex_channel_set_next_frame_size_handler';

//void               vortex_channel_update_remote_incoming_buffer (VortexChannel * channel,
//                                                                 unsigned        ackno,
//                                                                 unsigned        window);
procedure            vortex_channel_update_remote_incoming_buffer (channel : PVortexChannel;
                                                                   ackno   : longword;
                                                                   window  : longword);
                                                                   cdecl;external External_library name 'vortex_channel_update_remote_incoming_buffer';

//axl_bool           vortex_channel_seq_no_exceeded_after_update  (VortexChannel * channel);
function             vortex_channel_seq_no_exceeded_after_update  (channel : PVortexChannel):Taxl_bool;
                                                                   cdecl;external External_library name 'vortex_channel_seq_no_exceeded_after_update';

//unsigned int       vortex_channel_get_max_seq_no_accepted       (VortexChannel * channel);
function             vortex_channel_get_max_seq_no_accepted       (channel : PVortexChannel):longword;
                                                                   cdecl;external External_library name 'vortex_channel_get_max_seq_no_accepted';

//void               vortex_channel_set_max_seq_no_accepted       (VortexChannel * channel,
//                                                                 unsigned int    seq_no);
procedure            vortex_channel_set_max_seq_no_accepted       (channel : PVortexChannel;
                                                                   seq_no  : longword);
                                                                   cdecl;external External_library name 'vortex_channel_set_max_seq_no_accepted';

//axl_bool           vortex_channel_are_equal                     (VortexChannel * channelA,
//                                                                 VortexChannel * channelB);
function             vortex_channel_are_equal                     (channelA : PVortexChannel;
                                                                   channelB : PVortexChannel):Taxl_bool;
                                                                   cdecl;external External_library name 'vortex_channel_are_equal';

//axl_bool           vortex_channel_update_incoming_buffer       (VortexChannel * channel,
//                                                                VortexFrame   * frame,
//                                                                unsigned int  * ackno,
//                                                                int           * window);
function             vortex_channel_update_incoming_buffer       (channel    : PVortexChannel;
                                                                  frame      : PVortexFrame;
                                                                  var ackno  : longword;
                                                                  var window : longint):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_update_incoming_buffer';

//void               vortex_channel_queue_pending_message        (VortexChannel * channel,
//                                                                axlPointer      message);
procedure            vortex_channel_queue_pending_message        (channel  : PVortexChannel;
                                                                  amessage : TaxlPointer);
                                                                  cdecl;external External_library name 'vortex_channel_queue_pending_message';

//axl_bool           vortex_channel_is_empty_pending_message     (VortexChannel * channel);
function             vortex_channel_is_empty_pending_message     (channel : PVortexChannel):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_is_empty_pending_message';

//axlPointer         vortex_channel_next_pending_message         (VortexChannel * channel);
function             vortex_channel_next_pending_message         (channel : PVortexChannel):TaxlPointer;
                                                                  cdecl;external External_library name 'vortex_channel_next_pending_message';

//void               vortex_channel_remove_pending_message       (VortexChannel * channel);
procedure            vortex_channel_remove_pending_message       (channel : PVortexChannel);
                                                                  cdecl;external External_library name 'vortex_channel_remove_pending_message';

//const char       * vortex_channel_get_profile                  (VortexChannel * channel);
function             vortex_channel_get_profile                  (channel : PVortexChannel):Pchar;
                                                                  cdecl;external External_library name 'vortex_channel_get_profile';

//axl_bool           vortex_channel_is_running_profile           (VortexChannel * channel,
//                                                                const char    * profile);
function             vortex_channel_is_running_profile           (channel : PVortexChannel;
                                                                  profile : Pchar):Taxl_bool;
                                                                  cdecl;external External_library name 'vortex_channel_is_running_profile';

//VortexConnection * vortex_channel_get_connection               (VortexChannel * channel);
function             vortex_channel_get_connection               (channel : PVortexChannel):PVortexConnection;
                                                                  cdecl;external External_library name 'vortex_channel_get_connection';

//axl_bool           vortex_channel_queue_frame                     (VortexChannel * channel,
//                                                                   VortexWriterData * data);
function             vortex_channel_queue_frame                     (channel : PVortexChannel;
                                                                     data    : PVortexWriterData):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_queue_frame';

//axl_bool           vortex_channel_queue_is_empty                  (VortexChannel * channel);
function             vortex_channel_queue_is_empty                  (channel : PVortexChannel):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_queue_is_empty';

//VortexWriterData * vortex_channel_queue_next_msg                  (VortexChannel * channel);
function             vortex_channel_queue_next_msg                  (channel : PVortexChannel):PVortexWriterData;
                                                                     cdecl;external External_library name 'vortex_channel_queue_next_msg';

//int                vortex_channel_queue_length                    (VortexChannel * channel);
function             vortex_channel_queue_length                    (channel : PVortexChannel):longint;
                                                                     cdecl;external External_library name 'vortex_channel_queue_length';

//void               vortex_channel_set_serialize                   (VortexChannel * channel,
//                                                                   axl_bool        serialize);
procedure            vortex_channel_set_serialize                   (channel   : PVortexChannel;
                                                                     serialize : Taxl_bool);
                                                                     cdecl;external External_library name 'vortex_channel_set_serialize';

//void               vortex_channel_set_data                        (VortexChannel * channel,
//                                                                   axlPointer key,
//                                                                   axlPointer value);
procedure            vortex_channel_set_data                        (channel : PVortexChannel;
                                                                     key     : TaxlPointer;
                                                                     value   : TaxlPointer);
                                                                     cdecl;external External_library name 'vortex_channel_set_data';

//void               vortex_channel_set_data_full                   (VortexChannel    * channel,
//                                                                   axlPointer         key,
//                                                                   axlPointer         value,
//                                                                   axlDestroyFunc     key_destroy,
//                                                                   axlDestroyFunc     value_destroy);
procedure            vortex_channel_set_data_full                   (channel       : PVortexChannel;
                                                                     key           : TaxlPointer;
                                                                     value         : TaxlPointer;
                                                                     key_destroy   : TaxlDestroyFunc;
                                                                     value_destroy : TaxlDestroyFunc);
                                                                     cdecl;external External_library name 'vortex_channel_set_data_full';

//void               vortex_channel_delete_data                     (VortexChannel    * channel,
//                                                                   axlPointer         key);
procedure            vortex_channel_delete_data                     (channel : PVortexChannel;
                                                                     key     : TaxlPointer);
                                                                     cdecl;external External_library name 'vortex_channel_delete_data';

//axlPointer         vortex_channel_get_data                        (VortexChannel * channel,
//                                                                   axlPointer key);
function             vortex_channel_get_data                        (channel : PVortexChannel;
                                                                     key     : TaxlPointer):TaxlPointer;
                                                                     cdecl;external External_library name 'vortex_channel_get_data';

//axl_bool           vortex_channel_ref                             (VortexChannel * channel);
function             vortex_channel_ref                             (channel : PVortexChannel):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_ref';

//void               vortex_channel_unref                           (VortexChannel * channel);
procedure            vortex_channel_unref                           (channel : PVortexChannel);
                                                                     cdecl;external External_library name 'vortex_channel_unref';


//int                vortex_channel_ref_count                       (VortexChannel * channel);
function             vortex_channel_ref_count                       (channel : PVortexChannel):longint;
                                                                     cdecl;external External_library name 'vortex_channel_ref_count';

//axl_bool           vortex_channel_send_msg                        (VortexChannel    * channel,
//                                                                   const void       * message,
//                                                                   size_t             message_size,
//                                                                   int              * msg_no);
function             vortex_channel_send_msg                        (channel      : PVortexChannel;
                                                                     amessage     : pointer;
                                                                     message_size : size_t;
                                                                     var msg_no   : longint):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msg';

//axl_bool           vortex_channel_send_msgv                       (VortexChannel * channel,
//                                                                   int           * msg_no,
//                                                                   const char    * format,
//                                                                   ...);
function             vortex_channel_send_msgv                       (channel    : PVortexChannel;
                                                                     var msg_no : longint;
                                                                     format     : Pchar;
                                                                     args       : array of const):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msgv';

function             vortex_channel_send_msgv                       (channel    : PVortexChannel;
                                                                     var msg_no : longint;
                                                                     format     : Pchar):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msgv';

//axl_bool           vortex_channel_send_msg_and_wait               (VortexChannel     * channel,
//                                                                   const void        * message,
//                                                                   size_t              message_size,
//                                                                   int               * msg_no,
//                                                                   WaitReplyData     * wait_reply);
function             vortex_channel_send_msg_and_wait               (channel      : PVortexChannel;
                                                                     amessage     : pointer;
                                                                     message_size : size_t;
                                                                     var msg_no   : longint;
                                                                     wait_reply   : PWaitReplyData):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msg_and_wait';

//axl_bool           vortex_channel_send_msg_and_waitv              (VortexChannel   * channel,
//                                                                   int             * msg_no,
//                                                                   WaitReplyData   * wait_reply,
//                                                                   const char      * format,
//                                                                   ...);
function             vortex_channel_send_msg_and_waitv              (channel    : PVortexChannel;
                                                                     var msg_no : longint;
                                                                     wait_reply : PWaitReplyData;
                                                                     format     : Pchar;
                                                                     args       : array of const):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msg_and_waitv';

function             vortex_channel_send_msg_and_waitv              (channel    :PVortexChannel;
                                                                     var msg_no :longint;
                                                                     wait_reply :PWaitReplyData;
                                                                     format     :Pchar):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msg_and_waitv';

//axl_bool           vortex_channel_send_msg_common                 (VortexChannel   * channel,
//                                                                   const void      * message,
//                                                                   size_t            message_size,
//                                                                   int               proposed_msg_no,
//                                                                   int             * msg_no,
//                                                                   WaitReplyData   * wait_reply);
function             vortex_channel_send_msg_common                 (channel         : PVortexChannel;
                                                                     amessage        : pointer;
                                                                     message_size    : size_t;
                                                                     proposed_msg_no : longint;
                                                                     var msg_no      : longint;
                                                                     wait_reply      : PWaitReplyData):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_msg_common';

//axl_bool           vortex_channel_send_rpy                        (VortexChannel    * channel,
//                                                                   const void       * message,
//                                                                   size_t             message_size,
//                                                                   int                msg_no_rpy);
function             vortex_channel_send_rpy                        (channel      : PVortexChannel;
                                                                     amessage     : pointer;
                                                                     message_size : size_t;
                                                                     msg_no_rpy   : longint):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_rpy';

//axl_bool           vortex_channel_send_rpyv                       (VortexChannel * channel,
//                                                                   int             msg_no_rpy,
//                                                                   const   char  * format,
//                                                                   ...);
function             vortex_channel_send_rpyv                       (channel    : PVortexChannel;
                                                                     msg_no_rpy : longint;
                                                                     format     : Pchar;
                                                                     args       : array of const):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_rpyv';

function             vortex_channel_send_rpyv                       (channel    : PVortexChannel;
                                                                     msg_no_rpy : longint;
                                                                     format     : Pchar):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_rpyv';

//axl_bool           vortex_channel_send_ans_rpy                    (VortexChannel    * channel,
//                                                                   const void       * message,
//                                                                   size_t             message_size,
//                                                                   int                msg_no_rpy);
function             vortex_channel_send_ans_rpy                    (channel      : PVortexChannel;
                                                                     amessage     : pointer;
                                                                     message_size : size_t;
                                                                     msg_no_rpy   : longint):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_ans_rpy';

//axl_bool           vortex_channel_send_ans_rpyv                   (VortexChannel * channel,
//                                                                   int             msg_no_rpy,
//                                                                   const char    * format,
//                                                                   ...);
function             vortex_channel_send_ans_rpyv                   (channel    : PVortexChannel;
                                                                     msg_no_rpy : longint;
                                                                     format     : Pchar;
                                                                     args       : array of const):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_ans_rpyv';

function             vortex_channel_send_ans_rpyv                   (channel    : PVortexChannel;
                                                                     msg_no_rpy : longint;
                                                                     format     : Pchar):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_send_ans_rpyv';

//axl_bool           vortex_channel_finalize_ans_rpy                (VortexChannel * channel,
//                                                                   int             msg_no_rpy);
function             vortex_channel_finalize_ans_rpy                (channel    : PVortexChannel;
                                                                     msg_no_rpy : longint):Taxl_bool;
                                                                     cdecl;external External_library name 'vortex_channel_finalize_ans_rpy';

//axl_bool           vortex_channel_send_err                       (VortexChannel    * channel,
//                                                                  const void       * message,
//                                                                  size_t             message_size,
//                                                                  int                msg_no_err);
function             vortex_channel_send_err                       (channel      : PVortexChannel;
                                                                    amessage     : pointer;
                                                                    message_size : size_t;
                                                                    msg_no_err   : longint):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_send_err';

//axl_bool           vortex_channel_send_errv                      (VortexChannel * channel,
//                                                                  int             msg_no_err,
//                                                                  const   char  * format,
//                                                                  ...);
function             vortex_channel_send_errv                      (channel    : PVortexChannel;
                                                                    msg_no_err : longint;
                                                                    format     : Pchar;
                                                                    args       : array of const):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_send_errv';

function             vortex_channel_send_errv                      (channel    : PVortexChannel;
                                                                    msg_no_err : longint;
                                                                    format     : Pchar):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_send_errv';

//axl_bool           vortex_channel_is_opened                      (VortexChannel * channel);
function             vortex_channel_is_opened                      (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_is_opened';

//axl_bool           vortex_channel_is_being_closed                (VortexChannel * channel);
function             vortex_channel_is_being_closed                (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_is_being_closed';

//void               vortex_channel_free                           (VortexChannel * channel);
procedure            vortex_channel_free                           (channel : PVortexChannel);
                                                                    cdecl;external External_library name 'vortex_channel_free';

//axl_bool           vortex_channel_is_defined_received_handler    (VortexChannel * channel);
function             vortex_channel_is_defined_received_handler    (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_is_defined_received_handler';

//axl_bool           vortex_channel_invoke_received_handler        (VortexConnection * connection,
//                                                                  VortexChannel    * channel,
//                                                                  VortexFrame      * frame);
function             vortex_channel_invoke_received_handler        (connection : PVortexConnection;
                                                                    channel    : PVortexChannel;
                                                                    frame      : PVortexFrame):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_invoke_received_handler';

//axl_bool           vortex_channel_is_defined_close_handler       (VortexChannel * channel);
function             vortex_channel_is_defined_close_handler       (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_is_defined_close_handler';

//axl_bool           vortex_channel_invoke_close_handler           (VortexChannel  * channel);
function             vortex_channel_invoke_close_handler           (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_invoke_close_handler';

//VortexFrame      * vortex_channel_wait_reply                     (VortexChannel * channel,
//                                                                  int  msg_no,
//                                                                  WaitReplyData * wait_reply);
function             vortex_channel_wait_reply                     (channel    : PVortexChannel;
                                                                    msg_no     : longint;
                                                                    wait_reply : PWaitReplyData):PVortexFrame;
                                                                    cdecl;external External_library name 'vortex_channel_wait_reply';

//WaitReplyData *    vortex_channel_create_wait_reply              ();
function             vortex_channel_create_wait_reply              :PWaitReplyData;
                                                                    cdecl;external External_library name 'vortex_channel_create_wait_reply';

//void               vortex_channel_wait_reply_ref                 (WaitReplyData * wait_reply);
procedure            vortex_channel_wait_reply_ref                 (wait_reply : PWaitReplyData);
                                                                    cdecl;external External_library name 'vortex_channel_wait_reply_ref';

//void               vortex_channel_free_wait_reply                (WaitReplyData * wait_reply);
procedure            vortex_channel_free_wait_reply                (wait_reply : PWaitReplyData);
                                                                    cdecl;external External_library name 'vortex_channel_free_wait_reply';

//axl_bool           vortex_channel_is_ready                       (VortexChannel * channel);
function             vortex_channel_is_ready                       (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_is_ready';

//void               vortex_channel_queue_reply                    (VortexChannel    * channel,
//                                                                  VortexConnection * connection,
//                                                                  VortexFrame      * frame,
//                                                                  axlPointer user_data);
procedure            vortex_channel_queue_reply                    (channel    : PVortexChannel;
                                                                    connection : PVortexConnection;
                                                                    frame      : PVortexFrame;
                                                                    user_data  : TaxlPointer);
                                                                    cdecl;external External_library name 'vortex_channel_queue_reply';

//VortexFrame      * vortex_channel_get_reply                      (VortexChannel    * channel,
//                                                                  VortexAsyncQueue * queue);
function             vortex_channel_get_reply                      (channel : PVortexChannel;
                                                                    queue   : PVortexAsyncQueue):PVortexFrame;
                                                                    cdecl;external External_library name 'vortex_channel_get_reply';

//VortexFrame      * vortex_channel_get_piggyback                  (VortexChannel    * channel);
function             vortex_channel_get_piggyback                  (channel : PVortexChannel):PVortexFrame;
                                                                    cdecl;external External_library name 'vortex_channel_get_piggyback';

//axl_bool           vortex_channel_have_piggyback                 (VortexChannel    * channel);
function             vortex_channel_have_piggyback                 (channel : PVortexChannel):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_have_piggyback';

//void               vortex_channel_set_piggyback                  (VortexChannel    * channel,
//                                                                  const char       * profile_content);
procedure            vortex_channel_set_piggyback                  (channel         : PVortexChannel;
                                                                    profile_content : Pchar);
                                                                    cdecl;external External_library name 'vortex_channel_set_piggyback';

//void               vortex_channel_defer_start                    (VortexChannel    * channel);
procedure            vortex_channel_defer_start                    (channel : PVortexChannel);
                                                                    cdecl;external External_library name 'vortex_channel_defer_start';

//axl_bool           vortex_channel_notify_start                   (VortexChannel    * new_channel,
//                                                                  const char       * profile_content_reply,
//                                                                  axl_bool           action);
function             vortex_channel_notify_start                   (new_channel           : PVortexChannel;
                                                                    profile_content_reply : Pchar;
                                                                    action                : Taxl_bool):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_notify_start';

//void               vortex_channel_notify_close                   (VortexChannel    * channel,
//                                                                  int                msg_no,
//                                                                  axl_bool           action);
procedure            vortex_channel_notify_close                   (channel : PVortexChannel;
                                                                    msg_no  : longint;
                                                                    action  : Taxl_bool);
                                                                    cdecl;external External_library name 'vortex_channel_notify_close';

(* message validation *)
//axl_bool           vortex_channel_validate_err                   (VortexFrame * frame,
//                                                                  char  ** code,
//                                                                  char  **msg);
function             vortex_channel_validate_err                   (frame : PVortexFrame;
                                                                    code  : PPchar;
                                                                    msg   : PPchar):Taxl_bool;
                                                                    cdecl;external External_library name 'vortex_channel_validate_err';

{ Internal vortex functions }
{ *Use this function instead of CHANNEL_CTX #define* }
//VortexCtx         * vortex_channel_get_ctx                        (VortexChannel * channel);
function              vortex_channel_get_ctx                        (channel : PVortexChannel):PVortexCtx;
                                                                     cdecl;external External_library name 'vortex_channel_get_ctx';

(* @} *)

