program vortex_simple_client;

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

const
  PLAIN_PROFILE = 'http://fact.aspl.es/profiles/plain_profile';

var
  connection : PVortexConnection = nil;
	channel    : PVortexChannel = nil;
	frame      : PVortexFrame = nil;
	wait_reply : PWaitReplyData = nil;
	ctx        : PVortexCtx = nil;
	msg_no     : longint = 0;

{$IFDEF WINDOWS}{$R vortex_simple_client.rc}{$ENDIF}

{$goto on}

label
  _end;

begin
	{ init vortex library }
  ctx := vortex_ctx_new ();

	if (axl_false = vortex_init_ctx (ctx)) then
  begin
		{ unable to init vortex }
		vortex_ctx_free (ctx);

    RunError(1);
	end; { end if }

	{ creates a new connection against localhost:44000 }
	writeln ('connecting to localhost:44000...');
	connection := vortex_connection_new (ctx, 'localhost', '44000', nil, nil);
	if (axl_false = vortex_connection_is_ok (connection, axl_false)) then
  begin
		writeln('Unable to connect remote server, error was: ',
			 vortex_connection_get_message (connection));
		goto _end;
	end;
	writeln('ok');

	{* create a new channel (by chosing 0 as channel number the
	 * Vortex Library will automatically assign the new channel
	 * number free. *}
	channel := vortex_channel_new (connection, 0,
				      PLAIN_PROFILE,
				      { no close handling }
				      nil, nil,
				      {* no frame receive async
				       * handling *}
				      nil, nil,
				      {* no async channel creation *}
				      nil, nil);
	if (channel = nil) then
  begin
		writeln('Unable to create the channel..');
		goto _end;
	end;

	{ create a wait reply }
	wait_reply := vortex_channel_create_wait_reply ();

	{ now send the message using msg_and_wait/v }
	if (axl_false = vortex_channel_send_msg_and_wait (channel, PChar('my message'), strlen ('my message'), msg_no, wait_reply)) then
  begin
		writeln('Unable to send my message');
		vortex_channel_free_wait_reply (wait_reply);
		vortex_channel_close (channel, nil);
		goto _end;
	end;

	{* get blocked until the reply arrives, the wait_reply object
	 * must not be freed after this function because it already
	 * free it. *}
	frame := vortex_channel_wait_reply (channel, msg_no, wait_reply);
	if (frame = nil) then
  begin
		writeln('there was an error while receiving the reply or a timeout have occur');
		vortex_channel_close (channel, nil);
		goto _end;
	end;

	writeln('my reply have arrived: (size: ',vortex_frame_get_payload_size (frame),'):');
  writeln( Pchar( vortex_frame_get_payload (frame) ) );

 _end:
	vortex_connection_close (connection);

	{ terminate execution context }
	vortex_exit_ctx (ctx, axl_false);

	{ free ctx }
	vortex_ctx_free (ctx);
end.

