(* *****************************************************************************
 *  TestVortex.pas: Compilation test for Vortex.pas
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
 *  TestLibVortex is only used to check if the Vortex.h translation compiles
 *  correctly. The program does not do anything useful
 ******************************************************************************)
program TestVortex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  axl, Classes, SysUtils, CustApp, vortex, vortex_http, vortex_pull, vortex_tls,
  vortex_tunnel, vortex_xml_rpc;

type

  { TTestVortex }

  TTestVortex = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTestVortex }

procedure TTestVortex.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  writeln('Hello Vortex');

  // stop program loop
  Terminate;
end;

constructor TTestVortex.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestVortex.Destroy;
begin
  inherited Destroy;
end;

procedure TTestVortex.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TTestVortex;

{$IFDEF WINDOWS}{$R TestVortex.rc}{$ENDIF}

begin
  Application:=TTestVortex.Create(nil);
  Application.Title := 'project1';
  Application.Run;
  Application.Free;
end.

