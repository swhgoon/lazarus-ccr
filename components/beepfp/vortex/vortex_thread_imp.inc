
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

// Record to pass the thread info to the wrapper function
type
  PCdeclThreadFunc = ^TCdeclThreadFunc;
  TCdeclThreadFunc = record
    Func: TVortexThreadFunc;  //cdecl function
    Data: Pointer;            //orig data
  end;

{ C to pascal wrapper function.
  BeginThread expects a 'register' function. Vortex supplies a 'cdecl' function.}
function C2P_Translator(FuncData: pointer) : ptrint;
var
  ThreadData: TCdeclThreadFunc;
begin
  //Copy the supplied info
  ThreadData := PCdeclThreadFunc(FuncData)^;

  //Release memory allocated in fpc_vortex_thread_create()
  dispose(PCdeclThreadFunc(FuncData));

  //Finally we run the thread function
  Result := ptrint(ThreadData.Func(ThreadData.Data));
end;

{ FPC specific thread create function to replace Vortex's thread create.
 This is required because FPC doesn't work when C libraries create their own
 threads}
{$WARNINGS OFF} //For "cdecl'd functions have no high parameter"
function fpc_vortex_thread_create(thread_def : PVortexThread;
                                  func       : TVortexThreadFunc;
                                  user_data  : TaxlPointer;
                                  args       : array of const):Taxl_bool; cdecl;
var
  ThreadData: PCdeclThreadFunc;
begin
  //Pass thread information to C to Pascal wrapping function
  New(ThreadData);
  ThreadData^.Func := func;
  ThreadData^.Data := user_data;

  {$IFDEF WINDOWS}
  {$ERROR: Not implemented for windows yet.}
  //See vortex_thread_create()
  {$ENDIF}

  {$IFDEF UNIX}
    //Start thread
    BeginThread(@C2P_Translator, ThreadData, TThreadID(thread_def^) );

    if TThreadID(thread_def) > 0 then
      //Don't free memory here, it is done in the thread function
      Result := axl_true
    else
    begin
      //Free memory
      dispose(ThreadData);
      Result := axl_false;
    end;
  {$ENDIF}
end;
{$WARNINGS ON}

{ FPC specific thread destroy function to replace Vortex's thread destroy.
 This is required because FPC doesn't work when C libraries create their own
 threads}
function fpc_vortex_thread_destroy(thread_def : PVortexThread;
                                   free_data  : Taxl_bool):Taxl_bool; cdecl;
var
  Err: integer;
begin
  {$IFDEF WINDOWS}
  {$ERROR: Not implemented for windows yet.}
  //See vortex_thread_create()
  {$ENDIF}

  {$IFDEF UNIX}
    //Wait for thread
    Err := WaitForThreadTerminate(TThreadID(thread_def^), 0);

    //Free resources
    if axl_true = free_data then
      axl_free(thread_def);
  {$ENDIF}

  if 0 = Err then
    Result := axl_true
  else
    Result := axl_false;
end;

