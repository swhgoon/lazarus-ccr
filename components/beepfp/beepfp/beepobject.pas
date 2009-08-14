(*******************************************************************************
 *  BeepObject.pas: Utilities for BEEP classes
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
 *  TBeepObject implements the base class for all LazBEEP classes.
 ******************************************************************************)
unit BeepObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  { TBEEPOBject }
  TBEEPObject = class(TObject)
  private
    Semaphore: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AcquireLock;
    procedure ReleaseLock;
  end;

implementation

{ TBEEPObject }

procedure TBEEPObject.AcquireLock;
begin
  Semaphore.Acquire;
end;

constructor TBEEPObject.Create;
begin
  Semaphore := TCriticalSection.Create;
end;

destructor TBEEPObject.Destroy;
begin
  Semaphore.Free;

  inherited Destroy;
end;

procedure TBEEPObject.ReleaseLock;
begin
  Semaphore.Release;
end;

end.

