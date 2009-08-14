(*******************************************************************************
 *  BeepContext.pas: Vortex BEEP context
 *  Copyright (C) 2009,  Wimpie Nortje <wimpienortje@gmail.com>
 *
 *  This file is part of BeepFp.
 *
 *  BeepFp is free software: you can redistribute it and/or modify it under the
 *  terms of the GNU Lesser General Public License as published by the Free
 *  Software Foundation, either version 3 of the License, or (at your option)
 *  any later version.
 *
 *  BeepFp is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with BeepFp.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  BeepFp is further covered by a special exception as described in the file
 *  COPYING.modifiedLGPL.txt which should have been included in the
 *  distribution. If not, see
 *  <http://svn.freepascal.org/svn/lazarus/trunk/COPYING.modifiedLGPL.txt>
 *******************************************************************************
 *  TBeepContext implements the context handling capabilities of the Vortex
 *  library
 ******************************************************************************)
unit BeepContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl, Vortex, BeepUtils, BeepObject;

type
  { Exceptions }
  EBeepContext     = class(EBeepError);

  { TBeepContext }

  TBeepContext = class(TBEEPObject)
  private
    FCtx   : PVortexCtx;   //Vortex context
  public
    property VortexCtx: PVortexCtx read FCtx;

    constructor Create;
    destructor Destroy; override;

  end;

{ Vortex Context functions not implemented:

procedure   vortex_ctx_set_data

procedure   vortex_ctx_set_data_full

function    vortex_ctx_get_data

procedure   vortex_ctx_set_frame_received

procedure   vortex_ctx_set_close_notify_handler

procedure   vortex_ctx_set_channel_added_handler

procedure   vortex_ctx_set_channel_removed_handler

procedure   vortex_ctx_set_channel_start_handler

procedure   vortex_ctx_install_cleanup

DONE
function    vortex_ctx_new:PVortexCtx;
procedure   vortex_ctx_free            (ctx     : PVortexCtx);
}


implementation

{ TBeepContext }

constructor TBeepContext.Create;
var
  VResult : Taxl_bool;
begin
  //Create the context
	FCtx := vortex_ctx_new ();

	//Init vortex library
  VResult := vortex_init_ctx (FCtx);

  //Init error
  if axl_false = VResult then
    raise EBeepContext.Create('Could not initialise BEEP context. Network communication is not functional');
end;

destructor TBeepContext.Destroy;
begin
  if assigned(FCtx) then
	  vortex_exit_ctx(FCtx, axl_true);

  inherited Destroy;
end;

end.

