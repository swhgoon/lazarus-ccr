
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
 * \addtogroup vortex_frame
 * @{
 *)

//char  *        vortex_frame_build_up_from_params (VortexFrameType   type,
//                                                  int               channel,
//                                                  int               msgno,
//                                                  axl_bool          more,
//                                                  unsigned int      seqno,
//                                                  int               size,
//                                                  int               ansno,
//                                                  const void      * payload);
function         vortex_frame_build_up_from_params (_type   : TVortexFrameType;
                                                    channel : longint;
                                                    msgno   : longint;
                                                    more    : Taxl_bool;
                                                    seqno   : longword;
                                                    size    : longint;
                                                    ansno   : longint;
                                                    payload : pointer):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_build_up_from_params';

//char  *        vortex_frame_build_up_from_params_s (VortexFrameType   type,
//                                                    int               channel,
//                                                    int               msgno,
//                                                    axl_bool          more,
//                                                    unsigned int      seqno,
//                                                    int               size,
//                                                    int               ansno,
//                                                    const char   *    content_type,
//                                                    const char   *    transfer_encoding,
//                                                    const void   *    payload,
//                                                    int    *          frame_size);
function         vortex_frame_build_up_from_params_s (_type             : TVortexFrameType;
                                                      channel           : longint;
                                                      msgno             : longint;
                                                      more              : Taxl_bool;
                                                      seqno             : longword;
                                                      size              : longint;
                                                      ansno             : longint;
                                                      content_type      : Pchar;
                                                      transfer_encoding : Pchar;
                                                      payload           : pointer;
                                                      var frame_size    : longint):Pchar;
                                                      cdecl;external External_library name 'vortex_frame_build_up_from_params_s';

//char  * vortex_frame_build_up_from_params_s_buffer (VortexFrameType   type,
//                                                    int               channel,
//                                                    int               msgno,
//                                                    axl_bool          more,
//                                                    unsigned int      seqno,
//                                                    int               size,
//                                                    int               ansno,
//                                                    const char   *    content_type,
//                                                    const char   *    transfer_encoding,
//                                                    const void   *    payload,
//                                                    int    *          frame_size,
//                                                    char         *    buffer,
//                                                    int               buffer_size);
function  vortex_frame_build_up_from_params_s_buffer (_type             : TVortexFrameType;
                                                      channel           : longint;
                                                      msgno             : longint;
                                                      more              : Taxl_bool;
                                                      seqno             : longword;
                                                      size              : longint;
                                                      ansno             : longint;
                                                      content_type      : Pchar;
                                                      transfer_encoding : Pchar;
                                                      payload           : pointer;
                                                      var frame_size    : longint;
                                                      buffer            : Pchar;
                                                      buffer_size       : longint):Pchar;
                                                      cdecl;external External_library name 'vortex_frame_build_up_from_params_s_buffer';

//char  *        vortex_frame_seq_build_up_from_params (int           channel_num,
//                                                      unsigned int  ackno,
//                                                      int           window_size);
function         vortex_frame_seq_build_up_from_params (channel_num : longint;
                                                        ackno       : longword;
                                                        window_size : longint):Pchar;
                                                        cdecl;external External_library name 'vortex_frame_seq_build_up_from_params';

//char  * vortex_frame_seq_build_up_from_params_buffer (int    channel_num,
//                                                      int    ackno,
//                                                      int    window_size,
//                                                      char * buffer,
//                                                      int    buffer_size,
//                                                      int  * result_size);
function  vortex_frame_seq_build_up_from_params_buffer (channel_num     : longint;
                                                        ackno           : longint;
                                                        window_size     : longint;
                                                        buffer          : Pchar;
                                                        buffer_size     : longint;
                                                        var result_size : longint):Pchar;
                                                        cdecl;external External_library name 'vortex_frame_seq_build_up_from_params_buffer';

//VortexFrame * vortex_frame_create               (VortexCtx       * ctx,
//                                                 VortexFrameType   type,
//                                                 int               channel,
//                                                 int               msgno,
//                                                 axl_bool          more,
//                                                 unsigned int      seqno,
//                                                 int               size,
//                                                 int               ansno,
//                                                 const void      * payload);
function       vortex_frame_create                (ctx     : PVortexCtx;
                                                   _type   : TVortexFrameType;
                                                   channel : longint;
                                                   msgno   : longint;
                                                   more    : Taxl_bool;
                                                   seqno   : longword;
                                                   size    : longint;
                                                   ansno   : longint;
                                                   payload : pointer):PVortexFrame;
                                                   cdecl;external External_library name 'vortex_frame_create';

//VortexFrame * vortex_frame_create_full          (VortexCtx       * ctx,
//                                                 VortexFrameType   type,
//                                                 int               channel,
//                                                 int               msgno,
//                                                 axl_bool          more,
//                                                 unsigned int      seqno,
//                                                 int               size,
//                                                 int               ansno,
//                                                 const char      * content_type,
//                                                 const char      * transfer_encoding,
//                                                 const void      * payload);
function        vortex_frame_create_full          (ctx               : PVortexCtx;
                                                   _type             : TVortexFrameType;
                                                   channel           : longint;
                                                   msgno             : longint;
                                                   more              : Taxl_bool;
                                                   seqno             : longword;
                                                   size              : longint;
                                                   ansno             : longint;
                                                   content_type      : Pchar;
                                                   transfer_encoding : Pchar;
                                                   payload           : pointer):PVortexFrame;
                                                   cdecl;external External_library name 'vortex_frame_create_full';

//VortexFrame * vortex_frame_create_full_ref      (VortexCtx       * ctx,
//                                                 VortexFrameType   type,
//                                                 int               channel,
//                                                 int               msgno,
//                                                 axl_bool          more,
//                                                 unsigned int      seqno,
//                                                 int               size,
//                                                 int               ansno,
//                                                 const char      * content_type,
//                                                 const char      * transfer_encoding,
//                                                 void            * payload);
function        vortex_frame_create_full_ref      (ctx               : PVortexCtx;
                                                   _type             : TVortexFrameType;
                                                   channel           : longint;
                                                   msgno             : longint;
                                                   more              : Taxl_bool;
                                                   seqno             : longword;
                                                   size              : longint;
                                                   ansno             : longint;
                                                   content_type      : Pchar;
                                                   transfer_encoding : Pchar;
                                                   payload           : pointer):PVortexFrame;
                                                   cdecl;external External_library name 'vortex_frame_create_full_ref';

//VortexFrame * vortex_frame_copy                 (VortexFrame      * frame);
function        vortex_frame_copy                 (frame : PVortexFrame):PVortexFrame;
                                                   cdecl;external External_library name 'vortex_frame_copy';

//char  *       vortex_frame_get_raw_frame         (VortexFrame * frame);
function        vortex_frame_get_raw_frame         (frame : PVortexFrame):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_raw_frame';

//VortexFrame * vortex_frame_get_next              (VortexConnection * connection);
function        vortex_frame_get_next              (connection : PVortexConnection):PVortexFrame;
                                                    cdecl;external External_library name 'vortex_frame_get_next';

//axl_bool      vortex_frame_send_raw              (VortexConnection * connection,
//                                                  const char       * a_frame,
//                                                  int                frame_size);
function        vortex_frame_send_raw              (connection :PVortexConnection;
                                                    a_frame    :Pchar;
                                                    frame_size :longint):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_send_raw';

//int           vortex_frame_receive_raw           (VortexConnection * connection,
//                                                  char  * buffer, int  maxlen);
function        vortex_frame_receive_raw           (connection : PVortexConnection;
                                                    buffer     : Pchar;
                                                    maxlen     : longint):longint;
                                                    cdecl;external External_library name 'vortex_frame_receive_raw';

//int           vortex_frame_readline              (VortexConnection * connection,
//                                                  char             * buffer,
//                                                  int                maxlen);
function        vortex_frame_readline              (connection : PVortexConnection;
                                                    buffer     : Pchar;
                                                    maxlen     : longint):longint;
                                                    cdecl;external External_library name 'vortex_frame_readline';

//axl_bool      vortex_frame_ref                   (VortexFrame * frame);
function        vortex_frame_ref                   (frame : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_ref';

//void          vortex_frame_unref                 (VortexFrame * frame);
procedure       vortex_frame_unref                 (frame : PVortexFrame);
                                                    cdecl;external External_library name 'vortex_frame_unref';

//int           vortex_frame_ref_count             (VortexFrame * frame);
function        vortex_frame_ref_count             (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_ref_count';

//void          vortex_frame_free                  (VortexFrame * frame);
procedure       vortex_frame_free                  (frame : PVortexFrame);
                                                    cdecl;external External_library name 'vortex_frame_free';

//VortexFrame * vortex_frame_join                  (VortexFrame * a,
//                                                  VortexFrame * b);
function        vortex_frame_join                  (a : PVortexFrame;
                                                    b : PVortexFrame):PVortexFrame;
                                                    cdecl;external External_library name 'vortex_frame_join';

//VortexFrame * vortex_frame_join_extending       (VortexFrame * a,
//                                                 VortexFrame * b);
function        vortex_frame_join_extending       (a : PVortexFrame;
                                                   b : PVortexFrame):PVortexFrame;
                                                   cdecl;external External_library name 'vortex_frame_join_extending';

//axl_bool      vortex_frame_are_joinable          (VortexFrame * a,
//                                                  VortexFrame * b);
function        vortex_frame_are_joinable          (a : PVortexFrame;
                                                    b : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_are_joinable';

//axl_bool      vortex_frame_are_equal             (VortexFrame * a,
//                                                  VortexFrame * b);
function        vortex_frame_are_equal             (a : PVortexFrame;
                                                    b : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_are_equal';

//int           vortex_frame_get_id                (VortexFrame * frame);
function        vortex_frame_get_id                (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_id';

//VortexFrameType vortex_frame_get_type            (VortexFrame * frame);
function          vortex_frame_get_type            (frame:PVortexFrame):TVortexFrameType;
                                                    cdecl;external External_library name 'vortex_frame_get_type';

//const char  * vortex_frame_get_content_type      (VortexFrame * frame);
function        vortex_frame_get_content_type      (frame : PVortexFrame):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_content_type';

//const char  * vortex_frame_get_transfer_encoding (VortexFrame * frame);
function        vortex_frame_get_transfer_encoding (frame : PVortexFrame):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_transfer_encoding';

//int           vortex_frame_get_mime_header_size  (VortexFrame * frame);
function        vortex_frame_get_mime_header_size  (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_mime_header_size';

//int           vortex_frame_get_channel           (VortexFrame * frame);
function        vortex_frame_get_channel           (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_channel';

//VortexChannel * vortex_frame_get_channel_ref     (VortexFrame * frame);
function          vortex_frame_get_channel_ref     (frame : PVortexFrame):PVortexChannel;
                                                    cdecl;external External_library name 'vortex_frame_get_channel_ref';

//void          vortex_frame_set_channel_ref       (VortexFrame * frame,
//                                                  VortexChannel * channel);
procedure       vortex_frame_set_channel_ref       (frame   : PVortexFrame;
                                                    channel : PVortexChannel);
                                                    cdecl;external External_library name 'vortex_frame_set_channel_ref';

//int           vortex_frame_get_msgno             (VortexFrame * frame);
function        vortex_frame_get_msgno             (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_msgno';

//axl_bool      vortex_frame_get_more_flag         (VortexFrame * frame);
function        vortex_frame_get_more_flag         (frame : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_get_more_flag';

//unsigned int  vortex_frame_get_seqno             (VortexFrame * frame);
function        vortex_frame_get_seqno             (frame:PVortexFrame):longword;
                                                    cdecl;external External_library name 'vortex_frame_get_seqno';

//int           vortex_frame_get_ansno             (VortexFrame * frame);
function        vortex_frame_get_ansno             (frame:PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_ansno';

//const char * vortex_frame_get_ok_message         (void);
function       vortex_frame_get_ok_message          :Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_ok_message';

//int           vortex_frame_get_payload_size      (VortexFrame * frame);
function        vortex_frame_get_payload_size      (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_payload_size';

//const void *  vortex_frame_get_payload           (VortexFrame * frame);
function        vortex_frame_get_payload           (frame : PVortexFrame):pointer;
                                                    cdecl;external External_library name 'vortex_frame_get_payload';

//VortexCtx   * vortex_frame_get_ctx               (VortexFrame * frame);
function        vortex_frame_get_ctx               (frame : PVortexFrame):PVortexCtx;
                                                    cdecl;external External_library name 'vortex_frame_get_ctx';

//int           vortex_frame_get_content_size      (VortexFrame * frame);
function        vortex_frame_get_content_size      (frame : PVortexFrame):longint;
                                                    cdecl;external External_library name 'vortex_frame_get_content_size';

//const char *  vortex_frame_get_content           (VortexFrame * frame);
function        vortex_frame_get_content           (frame:PVortexFrame):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_content';

//char        * vortex_frame_get_error_message     (const char  * code,
//                                                  const char  * error_content,
//                                                  const char  * xml_lang);
function        vortex_frame_get_error_message     (code          : Pchar;
                                                    error_content : Pchar;
                                                    xml_lang      : Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_error_message';

//axl_bool      vortex_frame_is_error_message      (VortexFrame * frame,
//                                                  char  ** code,
//                                                  char  ** message);
function        vortex_frame_is_error_message      (frame    : PVortexFrame;
                                                    code     : PPchar;
                                                    amessage : PPchar):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_is_error_message';

//char        * vortex_frame_get_start_message     (int              channel_num,
//                                                  const char     * serverName,
//                                                  const char     * profile,
//                                                  VortexEncoding   encoding,
//                                                  const char     * content_profile,
//                                                  int              profile_content_size);
function        vortex_frame_get_start_message     (channel_num          : longint;
                                                    serverName           : Pchar;
                                                    profile              : Pchar;
                                                    encoding             : TVortexEncoding;
                                                    content_profile      : Pchar;
                                                    profile_content_size : longint):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_start_message';

//char        * vortex_frame_get_start_rpy_message (const char  * profile,
//                                                  const char  * profile_content);
function        vortex_frame_get_start_rpy_message (profile         : Pchar;
                                                    profile_content : Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_start_rpy_message';

//char        * vortex_frame_get_close_message     (int           number,
//                                                  const char  * code,
//                                                  const char  * xml_lang,
//                                                  const char  * close_content);
function        vortex_frame_get_close_message     (number        : longint;
                                                    code          : Pchar;
                                                    xml_lang      : Pchar;
                                                    close_content : Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_frame_get_close_message';

//axl_bool      vortex_frame_is_mime_message       (VortexFrame * frame);
function        vortex_frame_is_mime_message       (frame : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_is_mime_message';

//axl_bool      vortex_frame_mime_process          (VortexFrame * frame);
function        vortex_frame_mime_process          (frame : PVortexFrame):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_frame_mime_process';

//void          vortex_frame_set_mime_header       (VortexFrame * frame,
//                                                  const char  * mime_header,
//                                                  const char  * mime_header_content);
procedure       vortex_frame_set_mime_header       (frame               : PVortexFrame;
                                                    mime_header         : Pchar;
                                                    mime_header_content : Pchar);
                                                    cdecl;external External_library name 'vortex_frame_set_mime_header';

(**
 * @brief Allows to get the content of the first MIME header found,
 * located by the header_name provided.
 *
 * This parameter only provides access to the first MIME header. In
 * the case you are accessing a MIME header that could be defined
 * several times, use \ref vortex_frame_get_mime_header combined with
 * \ref vortex_frame_mime_header_next.
 * 
 * @param frame The frame where the content of the first MIME header
 * will be returned.
 *
 * @param header_name The MIME header name to lookup. See \ref
 * vortex_frame_get_mime_header for recognized values.
 * 
 * @return A reference to the content of the MIME header or NULL if
 * nothing is found.
 *)
{$if 0}
#define VORTEX_FRAME_GET_MIME_HEADER(frame, header_name) ((vortex_frame_get_mime_header (frame, header_name)) ? vortex_frame_mime_header_content (vortex_frame_get_mime_header (frame, header_name)) : NULL)
{Make this a function. Where to store? .inc files can't have implementation section}
{$endif}

//VortexMimeHeader *  vortex_frame_get_mime_header       (VortexFrame * frame,
//                                                        const char  * mime_header);
function              vortex_frame_get_mime_header       (frame       : PVortexFrame;
                                                          mime_header : Pchar):PVortexMimeHeader;
                                                          cdecl;external External_library name 'vortex_frame_get_mime_header';

//const char       * vortex_frame_mime_header_name       (VortexMimeHeader * header);
function             vortex_frame_mime_header_name       (header : PVortexMimeHeader):Pchar;
                                                          cdecl;external External_library name 'vortex_frame_mime_header_name';

//const char       * vortex_frame_mime_header_content    (VortexMimeHeader * header);
function             vortex_frame_mime_header_content    (header : PVortexMimeHeader):Pchar;
                                                          cdecl;external External_library name 'vortex_frame_mime_header_content';

//VortexMimeHeader * vortex_frame_mime_header_next       (VortexMimeHeader * header);
function             vortex_frame_mime_header_next       (header : PVortexMimeHeader):PVortexMimeHeader;
                                                          cdecl;external External_library name 'vortex_frame_mime_header_next';

//int                vortex_frame_mime_header_count      (VortexMimeHeader * header);
function             vortex_frame_mime_header_count      (header : PVortexMimeHeader):longint;
                                                          cdecl;external External_library name 'vortex_frame_mime_header_count';

//axl_bool           vortex_frame_mime_status_is_available  (VortexFrame * frame);
function             vortex_frame_mime_status_is_available  (frame : PVortexFrame):Taxl_bool;
                                                             cdecl;external External_library name 'vortex_frame_mime_status_is_available';

(* @} *)

