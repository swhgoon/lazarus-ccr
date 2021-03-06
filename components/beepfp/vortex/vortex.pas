unit Vortex;

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
  BaseUnix, Sockets, axl;

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
 * \addtogroup vortex
 * @{
 *)

(* define default socket pool size for the VORTEX_IO_WAIT_SELECT
 * method. If you change this value, you must change the
 * following. This value must be synchronized with FD_SETSIZE. This
 * has been tested on windows to work properly, but under GNU/Linux,
 * the GNUC library just rejects providing this value, no matter where
 * you place them. The only solutions are:
 *
 * [1] Modify /usr/include/bits/typesizes.h the macro __FD_SETSIZE and
 *     update the following values: FD_SETSIZE and VORTEX_FD_SETSIZE.
 *
 * [2] Use better mechanism like poll or epoll which are also available 
 *     in the platform that is giving problems.
 * 
 * [3] The last soluction could be the one provided by you. Please report
 *     any solution you may find.
 **)
const
  VORTEX_FD_SETSIZE = 1024;
  FD_SETSIZE        = 1024;

(* Direct portable mapping definitions *)
type
  TVORTEX_SOCKET        = Tsocket;

const
  VORTEX_EINTR          = ESysEINTR;
  VORTEX_EWOULDBLOCK    = ESysEWOULDBLOCK;
  VORTEX_EINPROGRESS    = ESysEINPROGRESS;
  VORTEX_EAGAIN         = ESysEAGAIN;
  VORTEX_FILE_SEPARATOR = DirectorySeparator;

function vortex_close_socket (Sock:Longint):Longint; inline;
function vortex_getpid: SizeUInt; inline;
function vortex_is_disconnected: Taxl_bool; inline;
function vortex_sscanf(const s: String; const fmt: String; const Pointers: array of Pointer):integer;

(* Definitions for unix *)
//Do they differ from the windows ones?
const
  VORTEX_INVALID_SOCKET = -(1);
  VORTEX_SOCKET_ERROR   = -(1);

(* Definitions for windows *)
{$if defined(AXL_OS_WIN32)}
(* _WIN32_WINNT note: If the application including the header defines
 * the _WIN32_WINNT, it must include the bit defined by the value
 * 0x400. *)
#ifndef _WIN32_WINNT
#  define _WIN32_WINNT 0x400
#endif

const
  VORTEX_INVALID_SOCKET = INVALID_SOCKET;
  VORTEX_SOCKET_ERROR   = SOCKET_ERROR;

(* no link support windows *)
#define S_ISLNK(m) (0)

{$endif} (* end defined(AXL_OS_WINDOWS) *)

(* check for missing definition for S_ISDIR *)
//#ifndef S_ISDIR
//#  ifdef _S_ISDIR
//#    define S_ISDIR(x) _S_ISDIR(x)
//#  else
//#    ifdef S_IFDIR
//#      ifndef S_IFMT
//#        ifdef _S_IFMT
//#          define S_IFMT _S_IFMT
//#        endif
//#      endif
//#       ifdef S_IFMT
//#         define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
//#       endif
//#    endif
//#  endif
//#endif

(* check for missing definition for S_ISREG *)
//#if defined(_MSC_VER) && (_MSC_VER >= 1400)
//# define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
//#endif

(**
 * @brief Returns the minimum from two values.
 * @param a First value to compare.
 * @param b Second value to compare.
 *)
//#define VORTEX_MIN(a,b) ((a) > (b) ? b : a)
function VORTEX_MIN(a,b : longint) : longint; inline;


(* Internal includes and external includes for Vortex API consumers. *)
{$include vortex_types.inc}
{$include vortex_support.inc}
{$include vortex_handlers.inc}
{$include vortex_ctx.inc}
{$include vortex_thread.inc}
{$include vortex_thread_pool.inc}
{$include vortex_queue.inc}
{$include vortex_hash.inc}
{$include vortex_connection.inc}
{$include vortex_listener.inc}
{$include vortex_frame_factory.inc}
{$include vortex_greetings.inc}
{$include vortex_profiles.inc}
{$include vortex_channel.inc}
{$include vortex_channel_pool.inc}
{$include vortex_io.inc}
{$include vortex_reader.inc}
{$include vortex_dtds.inc}
{$include vortex_sequencer.inc}
{$include vortex_errno.inc}

{$if defined(AXL_OS_WIN32)}
{$include vortex_win32.inc}
{$endif}

(* console debug support:
 *
 * If enabled, the log reporting is activated as usual. If log is
 * stripped from vortex building all instructions are removed.
 *)
{$WARNINGS OFF}
{#if defined(ENABLE_VORTEX_LOG)
# define vortex_log(l, m, ...)   do{_vortex_log  (ctx, __AXL_FILE__, __AXL_LINE__, l, m, ##__VA_ARGS__);}while(0)
# define vortex_log2(l, m, ...)   do{_vortex_log2  (ctx, __AXL_FILE__, __AXL_LINE__, l, m, ##__VA_ARGS__);}while(0)
#else
# if defined(AXL_OS_WIN32) && !( defined(__GNUC__) || _MSC_VER >= 1400)
(* default case where '...' is not supported but log is still
 * disabled *)
#   define vortex_log _vortex_log
#   define vortex_log2 _vortex_log2
# else
#   define vortex_log(l, m, ...) (* nothing *)
#   define vortex_log2(l, m, message, ...) (* nothing *)
# endif
#endif
}
{TODO Still to be translated ^^^}
{$WARNINGS ON}

//axl_bool vortex_init_ctx(VortexCtx * ctx);
function vortex_init_ctx(ctx:PVortexCtx):Taxl_bool;cdecl;external External_library name 'vortex_init_ctx';
//function vortex_init_ctx(ctx:PVortexCtx):Taxl_bool;stdcall;external External_library name 'vortex_init_ctx';

//void      vortex_exit_ctx(VortexCtx * ctx   , axl_bool    free_ctx);
procedure vortex_exit_ctx(ctx:PVortexCtx; free_ctx:Taxl_bool);cdecl;external External_library name 'vortex_exit_ctx';

//axl_bool vortex_log_is_enabled(VortexCtx * ctx);
function vortex_log_is_enabled(ctx:PVortexCtx):Taxl_bool;cdecl;external External_library name 'vortex_log_is_enabled';

//axl_bool vortex_log2_is_enabled(VortexCtx * ctx);
function vortex_log2_is_enabled(ctx:PVortexCtx):Taxl_bool;cdecl;external External_library name 'vortex_log2_is_enabled';

//void     vortex_log_enable (VortexCtx * ctx, 	      axl_bool    status);
procedure vortex_log_enable(ctx:PVortexCtx; status:Taxl_bool);cdecl;external External_library name 'vortex_log_enable';

//void     vortex_log2_enable (VortexCtx * ctx,   axl_bool    status);
procedure vortex_log2_enable(ctx:PVortexCtx; status:Taxl_bool);cdecl;external External_library name 'vortex_log2_enable';

//axl_bool vortex_color_log_is_enabled (VortexCtx * ctx);
function vortex_color_log_is_enabled(ctx:PVortexCtx):Taxl_bool;cdecl;external External_library name 'vortex_color_log_is_enabled';

//void     vortex_color_log_enable    (VortexCtx * ctx,			      axl_bool    status);
procedure vortex_color_log_enable(ctx:PVortexCtx; status:Taxl_bool);cdecl;external External_library name 'vortex_color_log_enable';

//axl_bool vortex_log_is_enabled_acquire_mutex (VortexCtx * ctx);
function vortex_log_is_enabled_acquire_mutex(ctx:PVortexCtx):Taxl_bool;cdecl;external External_library name 'vortex_log_is_enabled_acquire_mutex';

//void     vortex_log_acquire_mutex  (VortexCtx * ctx, 					      axl_bool    status);
procedure vortex_log_acquire_mutex(ctx:PVortexCtx; status:Taxl_bool);cdecl;external External_library name 'vortex_log_acquire_mutex';

//void     vortex_log_set_handler  (VortexCtx         * ctx,				      VortexLogHandler    handler);
procedure vortex_log_set_handler(ctx:PVortexCtx; handler:TVortexLogHandler);cdecl;external External_library name 'vortex_log_set_handler';

//VortexLogHandler vortex_log_get_handler (VortexCtx      * ctx);
function vortex_log_get_handler(ctx:PVortexCtx):TVortexLogHandler;cdecl;external External_library name 'vortex_log_get_handler';

//void     vortex_writer_data_free (VortexWriterData * writer_data);
procedure vortex_writer_data_free(writer_data:PVortexWriterData);cdecl;external External_library name 'vortex_writer_data_free';

(**
 * @brief Allowed items to use for \ref vortex_conf_get.
 *)
type
  PVortexConfItem = ^TVortexConfItem;
  TVortexConfItem =
  (
  	(**
  	 * @brief Gets/sets current soft limit to be used by the library,
  	 * regarding the number of connections handled. Soft limit
  	 * means it is can be moved to hard limit.
  	 *
  	 * To configure this value, use the integer parameter at \ref vortex_conf_set. Example:
  	 * \code
  	 * vortex_conf_set (VORTEX_SOFT_SOCK_LIMIT, 4096, NULL);
  	 * \endcode
  	 *)
  	VORTEX_SOFT_SOCK_LIMIT = 1,
  	(**
  	 * @brief Gets/sets current hard limit to be used by the
  	 * library, regarding the number of connections handled. Hard
  	 * limit means it is not possible to exceed it.
  	 *
  	 * To configure this value, use the integer parameter at \ref vortex_conf_set. Example:
  	 * \code
  	 * vortex_conf_set (VORTEX_HARD_SOCK_LIMIT, 4096, NULL);
  	 * \endcode
  	 *)
  	VORTEX_HARD_SOCK_LIMIT = 2,
  	(**
  	 * @brief Gets/sets current backlog configuration for listener
  	 * connections.
  	 *
  	 * Once a listener is activated, the backlog is the number of
  	 * complete connections (with the finished tcp three-way
  	 * handshake), that are ready to be accepted by the
  	 * application. The default value is 5.
  	 *
  	 * Once a listener is activated, and its backlog is
  	 * configured, it can't be changed. In the case you configure
  	 * this value, you must set it (\ref vortex_conf_set) after
  	 * calling to the family of functions to create vortex
  	 * listeners (\ref vortex_listener_new).
  	 *
  	 * To configure this value, use the integer parameter at \ref vortex_conf_set. Example:
  	 * \code
  	 * vortex_conf_set (VORTEX_LISTENER_BACKLOG, 64, NULL);
  	 * \endcode
  	 *)
  	VORTEX_LISTENER_BACKLOG = 3,
  	(**
  	 * @brief By default, vortex will allow the application layer
  	 * to request a channel creation using a profile which wasn't
  	 * adviced by the remote peer. Though it could be not
  	 * required, some BEEP peers may want to hide some profiles
  	 * until some condition is meet.
  	 *
  	 * Because a new BEEP &lt;greeting&gt; can't be sent advising new
  	 * profiles supported once those conditions are meet, it is
  	 * required to allow creating channels under profiles that
  	 * aren't adviced by the remote peer at the first &lt;greetings&gt;
  	 * exchange.
  	 *
  	 * This is mainly used by Turbulence BEEP application server
  	 * for its profile path support, which allows to design a policy
  	 * to be follow while creating channels, selecting some
  	 * profiles under some conditions.
  	 *
  	 * By default, the value configured is axl_false, that is, allows
  	 * to create channels under profiles even not adviced.
  	 *)
  	VORTEX_ENFORCE_PROFILES_SUPPORTED = 4,
  	(**
  	 * @brief If configured, makes all messages send via
  	 * vortex_channel_send_* to automatically add MIME headers
  	 * configured.
  	 *
  	 * This means that all messages sent will be configured with a
  	 * CR+LF prefix assuming the application level is sending the
  	 * MIME body.
  	 *
  	 * See \ref vortex_manual_using_mime for a long
  	 * explanation. In sort, this function allows to configure if
  	 * MIME headers should be added or not automatically on each
  	 * message sent using the family of functions
  	 * vortex_channel_send_*.
  	 *
  	 * The set of functions that are affected by this configuration are:
  	 *
  	 *  - \ref vortex_channel_send_msg
  	 *  - \ref vortex_channel_send_msgv
  	 *  - \ref vortex_channel_send_msg_and_wait
  	 *  - \ref vortex_channel_send_rpy
  	 *  - \ref vortex_channel_send_rpyv
  	 *  - \ref vortex_channel_send_err
  	 *  - \ref vortex_channel_send_errv
  	 *  - \ref vortex_channel_send_ans_rpy
  	 *  - \ref vortex_channel_send_ans_rpyv
  	 *
  	 * Use the following values to configure this feature:
  	 *
  	 * - 1: Enable automatic MIME handling for messages send over
  	 * any channel that is not configured.
  	 *
  	 * - 2: Disable automatic MIME handling for channels that
  	 * aren't configured.
  	 *)
  	VORTEX_AUTOMATIC_MIME_HANDLING = 5
  );

//axl_bool  vortex_conf_get(VortexCtx      * ctx,
//				                  VortexConfItem   item,
//				                  int            * value);
function vortex_conf_get(ctx      : PVortexCtx;
                         item     : TVortexConfItem;
                         var value: longint):Taxl_bool;cdecl;external External_library name 'vortex_conf_get';

//axl_bool  vortex_conf_set(VortexCtx      * ctx,
//				                  VortexConfItem   item,
//				                  int              value,
//				                  const char     * str_value);
function vortex_conf_set(ctx      : PVortexCtx;
                         item     : TVortexConfItem;
                         value    : longint;
                         str_value: Pchar):Taxl_bool;cdecl;external External_library name 'vortex_conf_set';

//void     _vortex_log(VortexCtx        * ctx,
//                     const       char * file,
//				             int                line,
//				             VortexDebugLevel   level,
//				             const char       * message,
//	       			       ...);
procedure _vortex_log(ctx     : PVortexCtx;
                      afile   : Pchar;
                      line    : longint;
                      level   : TVortexDebugLevel;
                      amessage: Pchar;
                      args    : array of const);cdecl;external External_library name '_vortex_log';

procedure _vortex_log(ctx     : PVortexCtx;
                      afile   : Pchar;
                      line    : longint;
                      level   : TVortexDebugLevel;
                      amessage: Pchar);cdecl;external External_library name '_vortex_log';

//void     _vortex_log2                (VortexCtx        * ctx,
//				      const       char * file,
//				      int                line,
//				      VortexDebugLevel   level,
//				      const char       * message,
//				      ...);
procedure _vortex_log2(ctx     : PVortexCtx;
                       afile   : Pchar;
                       line    : longint;
                       level   : TVortexDebugLevel;
                       amessage: Pchar;
                       args    : array of const);cdecl;external External_library name '_vortex_log2';

procedure _vortex_log2(ctx     : PVortexCtx;
                       afile   : Pchar;
                       line    : longint;
                       level   : TVortexDebugLevel;
                       amessage: Pchar);cdecl;external External_library name '_vortex_log2';

(* @} *)

implementation

uses
  math, sysutils;

{$include vortex_thread_imp.inc}

{$inline on}
function VORTEX_MIN(a,b : longint) : longint; inline;
begin
  Result := Min(a, b);
end;

function vortex_close_socket (Sock:Longint):Longint; inline;
begin
  Result := CloseSocket(Sock);
end;

function vortex_getpid: SizeUInt; inline;
begin
  Result := GetProcessID;
end;

function vortex_is_disconnected: Taxl_bool; inline;
begin
  //This is translated as defined for unix systems.....
  if errno=ESysEPIPE then
    Result := axl_true
  else
    Result := axl_false;

  //.....on windows systems the c code is
  //#define vortex_is_disconnected ((errno == WSAESHUTDOWN) || (errno == WSAECONNABORTED) || (errno == WSAECONNRESET))
  //How should this be translated? Or would the linux version work?
end;

{$inline off}

function vortex_sscanf(const s: String; const fmt: String; const Pointers:
  array of Pointer):integer;
begin
  Result := SScanf(s, fmt, Pointers);
end;

initialization
  // Set pascal specific thread creators
  vortex_thread_set_create(@fpc_vortex_thread_create);
  vortex_thread_set_destroy(@fpc_vortex_thread_destroy);

end.
