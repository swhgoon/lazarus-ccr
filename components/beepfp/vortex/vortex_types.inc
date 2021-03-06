
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
 * \defgroup vortex_types Vortex Types: Types definitions used across Vortex Library.
 *)
 
(**
 * \addtogroup vortex_types
 * @{
 *)

(*
 * @brief Debug levels to be used with \ref _vortex_log, which is used
 * through vortex_log macro.
 *
 * The set of functions allowing to activate the debug at run time and
 * its variants are:
 *
 * - \ref vortex_log_is_enabled
 * - \ref vortex_log2_is_enabled
 * - \ref vortex_log_enable
 * - \ref vortex_log2_enable
 *
 * Activate console color log (using ansi characters):
 *
 * - \ref vortex_color_log_is_enabled
 * - \ref vortex_color_log_enable
 *
 * To lock the log during its emision to avoid several threads doing
 * log at the same time:
 *
 * - \ref vortex_log_is_enabled_acquire_mutex
 * - \ref vortex_log_acquire_mutex
 *
 * Finally, to make the application level to configure a handler:
 *
 * - \ref vortex_log_set_handler
 * - \ref vortex_log_get_handler
 *
 * @param domain Domain that is registering a log.
 *
 * @param level Log level that is being registered.
 *
 * @param message Message that is being registered.
 *)
type
  TVortexDebugLevel = (
  	(**
  	 * @internal Log a message as a debug message.
  	 *)
  	VORTEX_LEVEL_DEBUG,
  	(**
  	 * @internal Log a warning message.
  	 *)
  	VORTEX_LEVEL_WARNING,
  	(**
  	 * @internal Log a critical message.
  	 *)
  	VORTEX_LEVEL_CRITICAL
  );


(**
 * @brief Macro declaration to protect code from misspelling
 * "MIME-Version" MIME header. This macro can be used by \ref
 * vortex_frame_set_mime_header and \ref vortex_frame_get_mime_header.
 *)
const MIME_VERSION = 'MIME-Version';
  
(**
 * @brief Macro declaration to protect code from misspelling
 * "Content-Type" MIME header. This macro can be used by \ref
 * vortex_frame_set_mime_header and \ref vortex_frame_get_mime_header.
 *)
const MIME_CONTENT_TYPE = 'Content-Type';

(**
 * @brief Macro declaration to protect code from misspelling
 * "Content-Transfer-Encoding" MIME header. This macro can be used by \ref
 * vortex_frame_set_mime_header and \ref vortex_frame_get_mime_header.
 *)
const MIME_CONTENT_TRANSFER_ENCODING = 'Content-Transfer-Encoding';

(**
 * @brief Macro declaration to protect code from misspelling
 * "Content-ID" MIME header. This macro can be used by \ref
 * vortex_frame_set_mime_header and \ref vortex_frame_get_mime_header.
 *)
const MIME_CONTENT_ID = 'Content-ID';

(**
 * @brief Macro declaration to protect code from misspelling
 * "Content-Description" MIME header. This macro can be used by \ref
 * vortex_frame_set_mime_header and \ref vortex_frame_get_mime_header.
 *)
const MIME_CONTENT_DESCRIPTION = 'Content-Description';


(**
 * @brief Describes the type allowed for a frame or the type a frame actually have.
 *
 * Every frame used inside Vortex Library have one of the following values.
 *)
type
  PVortexFrameType = ^TVortexFrameType;
  TVortexFrameType = (
  	(**
  	 * @brief The frame type is unknown, used to represent errors
  	 * across the Vortex Library.
  	 *
  	 * This only means an error have happen while receiving the
  	 * frame or while creating a new one. This frame have no valid
  	 * use inside the Vortex Library.
  	 *)
  	VORTEX_FRAME_TYPE_UNKNOWN,
  	(**
  	 * @brief Frame type is MSG. It is used to represent new
  	 * Vortex messages.
  	 *
  	 * This frame type represent a new message or a frame which
  	 * belongs to a set of frame conforming a message. This types
  	 * of frames are generated while using the following function:
  	 *
  	 *   - \ref vortex_channel_send_msg
  	 *   - \ref vortex_channel_send_msgv
  	 *   - \ref vortex_channel_send_msg_and_wait
  	 *   - \ref vortex_channel_send_msg_and_waitv
  	 *)
  	VORTEX_FRAME_TYPE_MSG,
  	(**
  	 * @brief Frame type is RPY. It is used to represent reply
  	 * frames to MSG frames called positive replies.
  	 *
  	 * This frame type represent replies produced to frame with
  	 * type MSG. The following function allows to generate frame
  	 * types for RPY:
  	 *
  	 *  - \ref vortex_channel_send_rpy
  	 *  - \ref vortex_channel_send_rpyv
  	 *)
  	VORTEX_FRAME_TYPE_RPY,
  	(**
  	 * @brief Frame type is ANS. It is used to represent reply
  	 * frames to MSG frames using the type ANS.
  	 *)
  	VORTEX_FRAME_TYPE_ANS,
  	(**
  	 * @brief Frame type is ERR. It is used to represent reply
  	 * frames to MSG frames called negative replies.
  	 *
  	 * This frame type is used to represent negative replies to
  	 * received frames with type MSG. The following functions
  	 * allows to generate ERR frames:
  	 *
  	 *   - \ref vortex_channel_send_err
  	 *   - \ref vortex_channel_send_errv
  	 *)

  	VORTEX_FRAME_TYPE_ERR,
  	(**
  	 * @brief Frame type is NUL. It is used to represent reply
  	 * frames to MSG ending a set of ANS frame replies.
  	 *)
  	VORTEX_FRAME_TYPE_NUL,
  	(**
  	 * @brief Frame type is SEQ. This is a especial frame type
  	 * used to allow application level to tweak how big is the
  	 * window size for a given channel.
  	 *)
  	VORTEX_FRAME_TYPE_SEQ
  );


(**
 * @brief Maximum buffer value to be read from the network in a
 * single operation.
 *)
const MAX_BUFFER_SIZE = 65536;

(**
 * @brief Maximum sequence number allowed to be used for a channel created.
 * 
 * The sequencer number is used inside Vortex Library to keep up to
 * date synchronization inside a channel while receiving and sending
 * data. This number represent the byte where the stream the channel
 * is transporting is supposed to start for further transmissions.
 * 
 *)
const MAX_SEQ_NO = int64(4294967295);


(**
 * @brief Module number used to rotate sequence number for a channel if it overcomes \ref MAX_SEQ_NO.
 *
 * While Vortex Library is operating with a channel, it may happen a
 * channel overcome \ref MAX_SEQ_NO so channel sequence number is
 * rotated using a modulo operation with \ref MAX_SEQ_MOD which is
 * just \ref MAX_SEQ_NO plus 1 unit.
 * 
 * 
 *)
const MAX_SEQ_MOD = int64(4294967296);


(**
 * @brief Maximum number of messages allowed to be sent for a specific channel instance.
 *
 * As defined by RFC 3080 every channel created cannot repeat message
 * number sent. Every message sent have an unique identifier used to
 * reply to it and to differentiate them from each other.
 *
 * A channel cannot sent more messages that the number defined for
 * MAX_MSG_NO. Once a channel reach that number it cannot be used for
 * any new message. This doesn't mean this channel must be closed. The
 * channel still have pending to receive all replies generated for
 * every message created.
 *
 *)
const MAX_MSG_NO = 2147483647;

(**
 * @brief Maximum number of channel allowed to be created inside a VortexConnection.
 * 
 * This number defines how many channels can be created inside a Vortex Connection.
 *)
const MAX_CHANNEL_NO = 2147483647;


(**
 * @brief The maximum number which can identify a message.
 * 
 *)
const MAX_MESSAGE_NO = 2147483647;


(**
 * @brief the maximum number of channel which can be created on an connection (or session).
 * 
 *)
const MAX_CHANNELS_NO = 2147483647;

(**
 * @brief The maximum number to be used on sequence frame payload 
 * 
 *)
const MAX_SEQUENCE_NO = 4294967295;

type
  PWhatUpdate = ^TWhatUpdate;
  TWhatUpdate = (
    UPDATE_SEQ_NO         := 1 shl 0,
    UPDATE_MSG_NO         := 1 shl 1,
    UPDATE_RPY_NO         := 1 shl 2,
    UPDATE_ANS_NO         := 1 shl 3,
    UPDATE_RPY_NO_WRITTEN := 1 shl 4,
    DECREASE_MSG_NO       := 1 shl 5,
    DECREASE_RPY_NO       := 1 shl 6
  );

(**
 * @brief Allows to specify which type of operation should be
 * implemented while calling to Vortex Library internal IO blocking
 * abstraction.
 *)
type
  PVortexIoWaitingFor = ^TVortexIoWaitingFor;
  TVortexIoWaitingFor = (
  	(**
  	 * @brief A read watching operation is requested. If this
  	 * value is received, the fd set containins a set of socket
  	 * descriptors which should be watched for incoming data to be
  	 * received.
  	 *)
  	READ_OPERATIONS  := 1 shl 0,
  	(**
  	 * @brief A write watching operation is requested. If this
  	 * value is received, the fd set contains a set of socket that
  	 * is being requested for its availability to perform a write
  	 * operation on them.
  	 *)
  	WRITE_OPERATIONS := 1 shl 1
);

(**
 * @brief Vortex library context. This object allows to store the
 * library status into a single reference, representing an independent
 * execution context.
 *
 * Its normal usage is to create a context variable using \ref
 * vortex_ctx_new and then start an execution context by calling to
 * \ref vortex_init_ctx. This makes the library to start its
 * operation.
 *
 * Once finished, a call to stop the context is required: \ref
 * vortex_exit_ctx followed by a call to \ref vortex_ctx_free.
 *
 *)
type
  PVortexCtx = Pointer;

(**
 * @brief A Vortex Connection object (BEEP session representation).
 *
 * This object represent a connection inside the Vortex Library. It
 * can be created using \ref vortex_connection_new and then checked
 * with \ref vortex_connection_is_ok.
 *
 * Internal \ref VortexConnection representation is not exposed to
 * user code space to ensure the minimal impact while improving or
 * changing Vortex Library internals. 
 * 
 * To operate with a \ref VortexConnection object \ref vortex_connection "check out its API documentation".
 * 
 *)
type
  PPVortexConnection = ^PVortexConnection;
  PVortexConnection = Pointer;

(**
 * @brief A Vortex Frame object.
 *
 * This object represent a frame received or to be sent to remote Vortex (or BEEP enabled) nodes.
 *
 * Internal \ref VortexFrame representation is not exposed to user
 * code space to ensure the minimal impact while improving or changing Vortex Library internals. 
 *
 * To operate with a \ref VortexFrame object \ref vortex_frame "check out its API documentation".
 * 
 *)
type
  PVortexFrame = Pointer;

(**
 * @brief A Vortex Channel object.
 *
 * This object represents a channel which enables to send and receive
 * data. A channel must be created inside a \ref VortexConnection by using \ref vortex_channel_new. 
 *
 * Internal \ref VortexChannel representation is not exposed to user
 * code space to ensure minimal impact while improving or changing
 * Vortex Library internals.
 *
 * To operate with a \ref VortexChannel object \ref vortex_channel "check out its API documentation".
 * 
 *)
type
  PPVortexChannel = ^PVortexChannel;
  PVortexChannel = Pointer;

(**
 * @brief Thread safe hash table based on Glib Hash table intensively used across Vortex Library.
 *
 * Internal \ref VortexHash representation is not exposed to user
 * code space to ensure minimal impact while improving or changing
 * Vortex Library internals.
 *
 * To operate with a \ref VortexHash object \ref vortex_hash "check out its API documentation".
 * 
 *)
type
  PVortexHash = Pointer;

(**
 * @brief Thread safe queue table based on Glib Queue used across Vortex Library.
 *
 * Internal \ref VortexQueue representation is not exposed to user
 * code space to ensure minimal impact while improving or changing
 * Vortex Library internals.
 *
 * To operate with a \ref VortexQueue object \ref vortex_queue "check out its API documentation".
 * 
 *)
type
  PVortexQueue = Pointer;

(**
 * @brief Vortex Channel Pool definition.
 *
 * The \ref VortexChannelPool is an abstraction which allows managing several \ref vortex_channel "channels"
 *  using the same profile under the same \ref vortex_connection "connection". 
 *
 * The \ref VortexChannelPool allows to get better performance reducing the
 * impact of channel creation and channel closing because enforces
 * reusing the channels created in a manner the channels used from the
 * pool every time are always ready. 
 *
 * Internal \ref VortexChannelPool representation is not exposed to user
 * code space to ensure minimal impact while improving or changing
 * Vortex Library internals.
 *
 * To operate with a \ref VortexChannelPool object \ref vortex_channel_pool "check out its API documentation".
 * 
 *)
type
  PVortexChannelPool = Pointer;

(**
 * @brief MIME header content representation, allowing to get the
 * content associated and the next value associated on the same header
 * (if were defined).
 *
 * This structure represents a single MIME header instance found on a
 * message. To get the MIME header name (left part of a MIME heeader
 * definition) use \ref vortex_frame_mime_header_name.
 *
 * To get the content associated to this MIME header instance use \ref
 * vortex_frame_mime_header_content. 
 *
 * To check if there are more instances defined for the same MIME
 * header name, use \ref vortex_frame_mime_header_next.
 *)
type
  PVortexMimeHeader = Pointer;

(**
 * @brief Vortex Operation Status.
 * 
 * This enum is used to represent different Vortex Library status,
 * especially while operating with \ref VortexConnection
 * references. Values described by this enumeration are returned by
 * \ref vortex_connection_get_status.
 *)
type
  PVortexStatus = ^TVortexStatus;
  TVortexStatus = (
  	(**
  	 * @brief Represents an Error while Vortex Library was operating.
  	 *
  	 * The operation asked to be done by Vortex Library could be
  	 * completed.
  	 *)
  	VortexError                  := 1,
  	(**
  	 * @brief Represents the operation have been successfully completed.
  	 *
  	 * The operation asked to be done by Vortex Library have been
  	 * completed.
  	 *)
  	VortexOk                     := 2,

  	(**
  	 * @brief The operation wasn't completed because an error to
  	 * tcp bind call. This usually means the listener can be
  	 * started because the port is already in use.
  	 *)
  	VortexBindError              := 3,

  	(**
  	 * @brief The operation can't be completed because a wrong
  	 * reference (memory address) was received. This also include
  	 * NULL references where this is not expected.
  	 *)
  	VortexWrongReference         := 4,

  	(**
  	 * @brief The operation can't be completed because a failure
  	 * resolving a name was found (usually a failure in the
  	 * gethostbyname function).
  	 *)
  	VortexNameResolvFailure      := 5,

  	(**
  	 * @brief A failure was found while creating a socket.
  	 *)
  	VortexSocketCreationError    := 6,

  	(**
  	 * @brief Found socket created to be using reserved system
  	 * socket descriptors. This will cause problems.
  	 *)
  	VortexSocketSanityError      := 7,

  	(**
  	 * @brief Connection error. Unable to connect to remote
  	 * host. Remote hosting is refusing the connection.
  	 *)
  	VortexConnectionError        := 8,

  	(**
  	 * @brief Connection error after timeout. Unable to connect to
  	 * remote host after after timeout expired.
  	 *)
  	VortexConnectionTimeoutError := 9,

  	(**
  	 * @brief Greetings error found. Initial BEEP senquence not
  	 * completed, failed to connect.
  	 *)
  	VortexGreetingsFailure       := 10,
  	(**
  	 * @brief Failed to complete operation due to an xml
  	 * validation error.
  	 *)
  	VortexXmlValidationError     := 11,
  	(**
  	 * @brief Connection is in transit to be closed. This is not
  	 * an error just an indication that the connection is being
  	 * closed at the time the call to \ref
  	 * vortex_connection_get_status was done.
  	 *)
  	VortexConnectionCloseCalled  := 12,
  	(**
  	 * @brief The connection was terminated due to a call to \ref
  	 * vortex_connection_shutdown or an internal implementation
  	 * tha closes the connection without taking place the BEEP
  	 * session close negociation.
  	 *)
  	VortexConnectionForcedClose  := 13,
  	(**
  	 * @brief Found a protocol error while operating.
  	 *)
  	VortexProtocolError          := 14,
  	(**
  	 * @brief  The connection was closed or not accepted due to a filter installed.
  	 *)
  	VortexConnectionFiltered     := 15
  );


(**
 * @brief In the context of the initial session creation, the BEEP
 * peer allows you to get which is the role of a given \ref VortexConnection object.
 *
 * This role could be used to implement some especial features
 * according to the peer type. Allowed values are: VortexRoleInitiator and
 * VortexRoleListener. 
 *
 * VortexListener value represent those connection that is accepting
 * inbound connections, while VortexInitiator value represent those
 * connections initiated to a BEEP peer acting as a server.
 *
 * You can get current role for a given connection using \ref vortex_connection_get_role.
 * 
 *)
type
  PVortexPeerRole = ^TVortexPeerRole;
  TVortexPeerRole = (
  	(**
  	 * @brief This value is used to represent an unknown role state.
  	 *)
  	VortexRoleUnknown,

  	(**
  	 * @brief The connection is acting as an Initiator one.
  	 *)
  	VortexRoleInitiator,

  	(**
  	 * @brief The connection is acting as a Listener one.
  	 *)
  	VortexRoleListener,

  	(**
  	 * @brief This especial value for the this enumeration allows
  	 * to know that the connection is a listener connection
  	 * accepting connections. It is not playing any role because
  	 * any BEEP session was initiated.
  	 *
  	 * Connections reference that were received with the following
  	 * functions are the only ones that can have this value:
  	 *
  	 * - \ref vortex_listener_new
  	 * - \ref vortex_listener_new2
  	 * - \ref vortex_listener_new_full
  	 *)
  	VortexRoleMasterListener
  );


(**
 * @brief Enum value used to report which encoding is being used for
 * the content profile send at the channel start phase. 
 * 
 * Check \ref vortex_channel_new_full documentation to know more about
 * how this enum is used.
 * 
 * By default, while sending profile content data, no encoding is used
 * and \ref EncodingNone must be used to report this fact. BEEP
 * definition allows to encode the profile content using Base64. On
 * this case, you have to use \ref EncodingBase64.
 *)
type
  PVortexEncoding = ^TVortexEncoding;
  TVortexEncoding = (
  	(**
  	 * @brief Default value used to report that the encoding information should be ignored.
  	 *)
  	EncodingUnknown,
  	(**
  	 * @brief No encoding was performed to the profile content data.
  	 *)
  	EncodingNone,
  	(**
  	 * @brief The profile content data was encoded with Base64.
  	 *)
  	EncodingBase64
  );

(**
 * @internal
 *
 * This type allows vortex_channel_send_msg and
 * vortex_channel_common_rpy to communicate the sequencer a new
 * message must be sent.  This a private structure for inter thread
 * communication and *MUST NOT* be used by vortex library consumer.
 *)
type
  PVortexSequencerData = Pointer;
{$if 0}
typedef struct _VortexSequencerData {
	(**
	 * @brief The channel where the message should be sequenced.
	 *)
	VortexChannel   * channel;

	/**
	 * @brief Reference to the connection.
	 */
	VortexConnection * conn;

	/**
	 * @brief The type of the frame to be sequenced.
	 */
	VortexFrameType   type;

	/**
	 * @brief The channel number to be sequenced (this is a
	 * redundancy data used for checking integrity).
	 */
	int              channel_num;

	/**
	 * @brief The message number value to be used for frames
	 * sequenced from this message.
	 */
	int              msg_no;

	/**
	 * Next sequence number to be used for the first byte on the
	 * message hold by this structure.
	 */
	unsigned int     first_seq_no;

	/**
	 * @brief The content to be sequenced into frames.
	 */
	char            * message;

	/**
	 * @brief The message size content.
	 */
        int              message_size;

	/**
	 * @brief This is a tricky value and it is used to allow
	 * vortex sequencer to keep track about byte stream to be used
	 * while sending remaining data.
	 *
	 * Because it could happened that a message to be sent doesn't
	 * hold into the buffer that the remote peer holds for the
	 * channel, the sequencer could find that the entire message
	 * could not be send because the channel is stale.
	 *
	 * On this context the vortex sequencer queue the message to
	 * be pending and flags on steps how many bytes remains to be
	 * sent *for the given message.
	 */
	unsigned int     step;

	/**
	 * @brief Signal value used by the vortex reader to instruct
	 * vortex sequencer that a new SEQ frame was received and the
	 * maximum number of bytes to be able to receive remote peer
	 * has been increased.
	 */
	axl_bool         resequence;

	/**
	 * @brief The ansno value to be used.
	 */
	int              ansno;

	/**
	 * @brief Discard flag used internally to drop packages that
	 * were queued to be sent.
	 */
	int              discard;

} VortexSequencerData;
{$endif}

(**
 * @internal
 *
 * This type is used to transport the message to be sent into the
 * vortex writer process. At the end of the process, the vortex writer
 * relies on vortex_connection_do_a_sending_round to actually send
 * message waiting to be sent in a round robin fashion.
 * 
 * Because some internal vortex process needs to waits (and ensure)
 * for a message to be sent, this type enable that function, the
 * ..do_a_sending_round, to signal a possible thread waiting for a
 * message to be sent.
 *)
{typedef struct _VortexWriterData
	VortexFrameType   type;
	int               msg_no;
	char            * the_frame;
	int               the_size;
	int               is_complete;
VortexWriterData;}

type
  PVortexWriterData = ^TVortexWriterData;
  TVortexWriterData = record
    _type      : TVortexFrameType;
	  msg_no     : longint;
	  the_frame  : PChar;
    the_size   : longint;
	  is_complete: longint;
  end;

(**
 * @brief Helper macro which allows to push data into a particular
 * queue, checking some conditions, which are logged at the particular
 * position if they fail.
 *
 * @param queue The queue to be used to push the new data. This reference can't be null.
 *
 * @param data The data to be pushed. This data can't be null.
 *)
{$WARNINGS OFF} //Don't display warnings about this C code
{$if 0}
{ NOTE: the two macros are translated but they must go somewhere else because
        the include file can't have an implementation section.
        Where should they go?
}
#define QUEUE_PUSH(queue, data)\
do {\
    if (queue == NULL) { \
       vortex_log (VORTEX_LEVEL_CRITICAL, "trying to push data in a null reference queue at: %s:%d", __AXL_FILE__, __AXL_LINE__); \
    } else if (data == NULL) {\
       vortex_log (VORTEX_LEVEL_CRITICAL, "trying to push null data in a queue at: %s:%d", __AXL_FILE__, __AXL_LINE__); \
    } else { \
       vortex_async_queue_push (queue,data);\
    }\
}while(0)

procedure QUEUE_PUSH(queue : PVortexAsyncQueue; data : PaxlPointer)
begin
  if (queue = NULL) then
     vortex_log (VORTEX_LEVEL_CRITICAL, 'trying to push data in a null reference queue at: %s:%d', __AXL_FILE__, __AXL_LINE__);
  else if (data = NULL)
     vortex_log (VORTEX_LEVEL_CRITICAL, 'trying to push null data in a queue at: %s:%d', __AXL_FILE__, __AXL_LINE__);
  else
     vortex_async_queue_push (queue,data);
end;


(**
 * @brief Helper macro which allows to PRIORITY push data into a
 * particular queue, checking some conditions, which are logged at the
 * particular position if they fail. 
 *
 * @param queue The queue to be used to push the new data. This reference can't be null.
 *
 * @param data The data to be pushed. This data can't be null.
 *)
#define QUEUE_PRIORITY_PUSH(queue, data)\
do {\
    if (queue == NULL) { \
       vortex_log (VORTEX_LEVEL_CRITICAL, "trying to push priority data in a null reference queue at: %s:%d", __AXL_FILE__, __AXL_LINE__); \
    } else if (data == NULL) {\
       vortex_log (VORTEX_LEVEL_CRITICAL, "trying to push priority null data in a queue at: %s:%d", __AXL_FILE__, __AXL_LINE__); \
    } else { \
       vortex_async_queue_priority_push (queue,data);\
    }\
}while(0)

procedure QUEUE_PRIORITY_PUSH(queue : PVortexAsyncQueue; data : PaxlPointer)
begin
  if (queue = NULL) then
     vortex_log (VORTEX_LEVEL_CRITICAL, 'trying to push priority data in a null reference queue at: %s:%d', __AXL_FILE__, __AXL_LINE__); \
  else if (data = NULL)
     vortex_log (VORTEX_LEVEL_CRITICAL, 'trying to push priority null data in a queue at: %s:%d', __AXL_FILE__, __AXL_LINE__);
  else
     vortex_async_queue_priority_push (queue,data);
end;

{$endif} //if 0
{$WARNINGS ON}


(**
 * @brief Thread definition, which encapsulates the os thread API,
 * allowing to provide a unified type for all threading
 * interface. 
  *)
type
  PVortexThread = Pointer;

(**
 * @brief Mutex definition that encapsulates the underlaying mutex
 * API.
  *)
type
  PVortexMutex = Pointer;

(**
 * @brief Conditional variable mutex, encapsulating the underlaying
 * operating system implementation for conditional variables inside
 * critical sections.
  *)
type
  PVortexCond = Pointer;

(**
 * @brief Message queue implementation that allows to communicate
 * several threads in a safe manner. 
 *)
type
  PVortexAsyncQueue = Pointer;

(**
 * @brief Handle definition for the family of function that is able to
 * accept the function \ref vortex_thread_create.
 *
 * The function receive a user defined pointer passed to the \ref
 * vortex_thread_create function, and returns an pointer reference
 * that must be used as integer value that could be retrieved if the
 * thread is joined.
 *
 * Keep in mind that there are differences between the windows and the
 * posix thread API, that are supported by this API, about the
 * returning value from the start function. 
 * 
 * While POSIX defines as returning value a pointer (which could be a
 * reference pointing to memory high above 32 bits under 64
 * architectures), the windows API defines an integer value, that
 * could be easily used to return pointers, but only safe on 32bits
 * machines.
 *
 * The moral of the story is that you must use another mechanism to
 * return data from this function to the thread that is expecting data
 * from this function. 
 * 
 * Obviously if you are going to return an status code, there is no
 * problem. This only applies to user defined data that is returned as
 * a reference to allocated data.
 *)
type
  TVortexThreadFunc = function (user_data:TaxlPointer):TaxlPointer;cdecl;
//typedef axlPointer ( * VortexThreadFunc) (axlPointer user_data);


(**
 * @brief Thread configuration its to modify default behaviour
 * provided by the thread creation API.
 *)
type
  PVortexThreadConf = ^TVortexThreadConf;
  TVortexThreadConf = (
  	(**
  	 * @brief Marker used to signal \ref vortex_thread_create that
  	 * the configuration list is finished.
  	 *
  	 * The following is an example on how to create a new thread
  	 * without providing any configuration, using defaults:
  	 *
  	 * \code
  	 * VortexThread thread;
  	 * if (! vortex_thread_created (&thread,
  	 *                              some_start_function, NULL,
  	 *                              VORTEX_THREAD_CONF_END)) {
  	 *      // failed to create the thread
  	 * }
  	 * // thread created
  	 * \endcode
  	 *)
  	VORTEX_THREAD_CONF_END := 0,
  	(**
  	 * @brief Allows to configure if the thread create can be
  	 * joined and waited by other.
  	 *
  	 * Default state for all thread created with \ref
  	 * vortex_thread_create is true, that is, the thread created
  	 * is joinable.
  	 *
  	 * If configured this value, you must provide as the following
  	 * value either axl_true or axl_false.
  	 *
  	 * \code
  	 * VortexThread thread;
  	 * if (! vortex_thread_create (&thread, some_start_function, NULL,
  	 *                             VORTEX_THREAD_CONF_JOINABLE, axl_false,
  	 *                             VORTEX_THREAD_CONF_END)) {
  	 *    // failed to create the thread
  	 * }
  	 *
  	 * // Nice! thread created
  	 * \endcode
  	 *)
  	VORTEX_THREAD_CONF_JOINABLE  := 1
  );

(**
 * @brief Wait Reply data used for \ref vortex_manual_wait_reply "Wait Reply Method".
 *
 * See \ref vortex_manual_wait_reply "this section to know more about Wait Reply Method.
 * 
 *)
type
  PWaitReplyData = Pointer;

(**
 * @brief Enumeration type that allows to use the waiting mechanism to
 * be used by the core library to perform wait on changes on sockets
 * handled.
 *)
type
  PVortexIoWaitingType = ^TVortexIoWaitingType;
  TVortexIoWaitingType = (
  	(**
  	 * @brief Allows to configure the select(2) system call based
  	 * mechanism. It is known to be available on any platform,
  	 * however it has some limitations while handling big set of
  	 * sockets, and it is limited to a maximum number of sockets,
  	 * which is configured at the compilation process.
  	 *
           * Its main disadvantage it that it can't handle
  	 * more connections than the number provided at the
  	 * compilation process. See <vortex.h> file, variable
  	 * FD_SETSIZE and VORTEX_FD_SETSIZE.
  	 *)
  	VORTEX_IO_WAIT_SELECT := 1,
  	(**
  	 * @brief Allows to configure the poll(2) system call based
  	 * mechanism.
  	 *
  	 * It is also a widely available mechanism on POSIX
  	 * envirionments, but not on Microsoft Windows. It doesn't
  	 * have some limitations found on select(2) call, but it is
  	 * known to not scale very well handling big socket sets as
  	 * happens with select(2) (\ref VORTEX_IO_WAIT_SELECT).
  	 *
  	 * This mechanism solves the runtime limitation that provides
  	 * select(2), making it possible to handle any number of
  	 * connections without providing any previous knowledge during
  	 * the compilation process.
  	 *
  	 * Several third party tests shows it performs badly while
  	 * handling many connections compared to (\ref VORTEX_IO_WAIT_EPOLL) epoll(2).
  	 *
  	 * However, reports showing that results, handles over 50.000
  	 * connections at the same time (up to 400.000!). In many
  	 * cases this is not going your production environment.
  	 *
  	 * At the same time, many reports (and our test results) shows
  	 * that select(2), poll(2) and epoll(2) performs the same
  	 * while handling up to 10.000 connections at the same time.
  	 *)
  	VORTEX_IO_WAIT_POLL   := 2,
  	(**
  	 * @brief Allows to configure the epoll(2) system call based
  	 * mechanism.
  	 *
  	 * It is a mechanism available on GNU/Linux starting from
  	 * kernel 2.6. It is supposed to be a better implementation
  	 * than poll(2) and select(2) due the way notifications are
  	 * done.
  	 *
  	 * It is currently selected by default if your kernel support
  	 * it. It has the advantage that performs very well with
  	 * little set of connections (0-10.000) like
  	 * (\ref VORTEX_IO_WAIT_POLL) poll(2) and (\ref VORTEX_IO_WAIT_SELECT)
  	 * select(2), but scaling much better when going to up heavy
  	 * set of connections (50.000-400.000).
  	 *
  	 * It has also the advantage to not require defining a maximum
  	 * socket number to be handled at the compilation process.
  	 *)
  	VORTEX_IO_WAIT_EPOLL  := 3
  );


(**
 * @brief Enum definition to configure stages during the connection
 * creation.
 *)
type
  PVortexConnectionStage = ^TVortexConnectionStage;
  TVortexConnectionStage = (
  	(**
  	 * @brief Actions to be executed after the connection
  	 * creation. Only connections properly created are notified.
  	 *)
  	CONNECTION_STAGE_POST_CREATED = 1
  );


(* @} *)
