(*******************************************************************************
 *  BeepUtils.pas: Utilities for BEEP classes
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
 *  TBeepUtils implements some utility functions and definitions used across
 *  LazBEEP
 ******************************************************************************)
unit BeepUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  axl;

type
  { Exceptions }
  EBeepError                = class(Exception);
  EBeepInvalidContext       = class(EBeepError);
  EBeepInvalidConnection    = class(EBeepError);
  EBeepInvalidChannel       = class(EBeepError);
  EBeepInvalidPort          = class(EBeepError);
  EBeepInvalidProfile       = class(EBeepError);
  EBeepInvalidChannelNumber = class(EBeepError);
  EProfileNotSupported      = class(EBeepError);
  EProfileNotRegistered     = class(EBeepError);
  EBeepListener             = class(EBeepError);

const
  MAX_PORT = 65536;             //Highest port number allowed
  KEY_OWNER : PChar = 'Owner';  //Key used to store BEEP object pointers in the Vortex structures


function VortexBool(aValue: boolean): Taxl_bool; inline;
function VortexPortOK(aPort: integer): boolean;
function VortexPortOK(aPort: string): boolean;

implementation

{$inline on}
function VortexBool(aValue: boolean): Taxl_bool; inline;
begin
  if aValue then
    Result := axl_true
  else
    Result := axl_false;
end;

function VortexPortOK(aPort: string): boolean;
var
  Val: integer;
begin
  //Convert string
  Val := StrToInt(aPort);

  Result := VortexPortOK(Val);
end;

function VortexPortOK(aPort: integer): boolean;
begin
  Result := (aPort >= 0) and (MAX_PORT >= aPort)
end;
{$inline off}
end.

