program vortex_simple_listener;

(*
 * vortex_simple_listener: C to Free Pascal translation.
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

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils,
  axl, vortex;

{$IFDEF WINDOWS}{$R vortex_simple_listener.rc}{$ENDIF}

const
  PLAIN_PROFILE = 'http://fact.aspl.es/profiles/plain_profile';

var
  { listener context }
  ctx : PVortexCtx = nil;

procedure frame_received (channel: PVortexChannel; connection: PVortexConnection;
  frame: PVortexFrame; user_data: TaxlPointer); cdecl;
begin
  writeln(format('A frame received on channel: %d', [vortex_channel_get_number (channel)] ) );
	writeln(format('Data received: "%s"', [Pchar(vortex_frame_get_payload (frame))] ) );

	{ reply the peer client with the same content }
	vortex_channel_send_rpyv (channel,
				  vortex_frame_get_msgno (frame),
				  'Received Ok: %s',
				  [vortex_frame_get_payload (frame)]);

	writeln(format('VORTEX_LISTENER: end task (pid: %d)', [vortex_getpid ()] ) );
end;

function start_channel (channel_num: longint; connection: PVortexConnection;
  user_data: TaxlPointer):longint;cdecl;
begin
	{ implement profile requirement for allowing starting a new
	 * channel }

	{ to return axl_false denies channel creation to return axl_true
	 * allows create the channel }
  Result := axl_true;
end;

function close_channel(channel_num: longint; connection: PVortexConnection;
  user_data: TaxlPointer): longint; cdecl;
begin
	{ implement profile requirement for allowing to closeing a
	 * the channel to return axl_false denies channel closing to
	 * return axl_true allows to close the channel }
  Result := axl_true;
end;

function on_accepted(connection: PVortexConnection; data: TaxlPointer): longint;cdecl;
begin
	writeln(format('New connection accepted from: %s:%s',
		 [vortex_connection_get_host (connection),
		 vortex_connection_get_port (connection)] ) );

	{ return axl_true to accept the connection to be created }
  Result := axl_true;
end;

begin
	{ create the context }
	ctx := vortex_ctx_new ();

	{ init vortex library }
	if (axl_false = vortex_init_ctx (ctx)) then
  begin
		{ unable to init context }
		vortex_ctx_free (ctx);

		//return -1;
    RunError(1);
	end; { end if }

	{ register a profile }
	vortex_profiles_register (ctx, PLAIN_PROFILE,
				  @start_channel, nil,
				  @close_channel, nil,
				  @frame_received, nil);

	{ create a vortex server }
	vortex_listener_new (ctx, '0.0.0.0', '44000', nil, nil);

	{ configure connection notification }
	vortex_listener_set_on_connection_accepted (ctx, @on_accepted, nil);

	{ wait for listeners (until vortex_exit is called) }
	vortex_listener_wait (ctx);

	{ end vortex function }
	vortex_exit_ctx (ctx, axl_true);
end.

