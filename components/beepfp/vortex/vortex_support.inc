
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

  {*
   * \addtogroup vortex_support
   * @
    }

//void    vortex_support_free                      (int  params, ...);
procedure vortex_support_free                      (params:longint;
                                                    args:array of const);
                                                    cdecl;external External_library name 'vortex_support_free';
procedure vortex_support_free                      (params:longint);
                                                    cdecl;external External_library name 'vortex_support_free';

//void    vortex_support_add_search_path           (VortexCtx  * ctx,
//						                                      const char * path);
procedure vortex_support_add_search_path           (ctx : PVortexCtx;
                                                    path: Pchar);
                                                    cdecl;external External_library name 'vortex_support_add_search_path';

//void    vortex_support_add_search_path_ref       (VortexCtx * ctx,
//                                                  char      * path);
procedure vortex_support_add_search_path_ref       (ctx : PVortexCtx;
                                                    path: Pchar);
                                                    cdecl;external External_library name 'vortex_support_add_search_path_ref';

//void    vortex_support_add_domain_search_path    (VortexCtx  * ctx,
//                                                  const char * domain,
//                                                  const char * path);
procedure vortex_support_add_domain_search_path    (ctx   : PVortexCtx;
                                                    domain: Pchar;
                                                    path  : Pchar);
                                                    cdecl;external External_library name 'vortex_support_add_domain_search_path';

//void    vortex_support_add_domain_search_path_ref(VortexCtx  * ctx,
//                                                  char       * domain,
//                                                  char       * path);
procedure vortex_support_add_domain_search_path_ref(ctx   : PVortexCtx;
                                                    domain: Pchar;
                                                    path  : Pchar);
                                                    cdecl;external External_library name 'vortex_support_add_domain_search_path_ref';

//char  * vortex_support_find_data_file            (VortexCtx  * ctx,
//                                                  const char * name);
function  vortex_support_find_data_file            (ctx : PVortexCtx;
                                                    name: Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_support_find_data_file';

//char   * vortex_support_domain_find_data_file    (VortexCtx  * ctx,
//                                  						    const char * domain,
//                                  						    const char * name);
function vortex_support_domain_find_data_file      (ctx   : PVortexCtx;
                                                    domain: Pchar;
                                                    name  : Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_support_domain_find_data_file';

//void    vortex_support_init                      (VortexCtx * ctx);
procedure vortex_support_init                      (ctx : PVortexCtx);
                                                    cdecl;external External_library name 'vortex_support_init';

//void    vortex_support_cleanup                   (VortexCtx * ctx);
procedure vortex_support_cleanup                   (ctx : PVortexCtx);
                                                    cdecl;external External_library name 'vortex_support_cleanup';

//int     vortex_support_getenv_int                (const char * env_name);
function  vortex_support_getenv_int                (env_name : Pchar):longint;
                                                    cdecl;external External_library name 'vortex_support_getenv_int';

//axl_bool vortex_support_setenv                   (const char * env_name,
//                                                  const char * env_value);
function  vortex_support_setenv                    (env_name : Pchar;
                                                    env_value: Pchar):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_support_setenv';

//axl_bool vortex_support_unsetenv                 (const char * env_name);
function  vortex_support_unsetenv                  (env_name : Pchar):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_support_unsetenv';

(**
 * @brief Available tests to be performed while using \ref
 * vortex_support_file_test
 *)
type
  PVortexFileTest = ^TVortexFileTest;
  TVortexFileTest = (
  	(**
  	 * @brief Check if the path exist.
  	 *)
  	FILE_EXISTS     := 1 shl 0,
  	(**
  	 * @brief Check if the path provided is a symlink.
  	 *)
  	FILE_IS_LINK    := 1 shl 1,
  	(**
  	 * @brief Check if the path provided is a directory.
  	 *)
  	FILE_IS_DIR     := 1 shl 2,
  	(**
  	 * @brief Check if the path provided is a regular file.
  	 *)
  	FILE_IS_REGULAR := 1 shl 3
  );

//axl_bool vortex_support_file_test                (const char * path,
//                                                  VortexFileTest test);
function  vortex_support_file_test                 (path : Pchar;
                                                    test : TVortexFileTest):Taxl_bool;
                                                    cdecl;external External_library name 'vortex_support_file_test';

//char  * vortex_support_build_filename            (const char  * name, ...);
function  vortex_support_build_filename            (name : Pchar;
                                                    args : array of const):Pchar;
                                                    cdecl;external External_library name 'vortex_support_build_filename';
function  vortex_support_build_filename            (name : Pchar):Pchar;
                                                    cdecl;external External_library name 'vortex_support_build_filename';

//double  vortex_support_strtod                    (const char  * param,
//                                                  char       ** string_aux);
function  vortex_support_strtod                    (param     : Pchar;
                                                    string_aux: PPchar):double;
                                                    cdecl;external External_library name 'vortex_support_strtod';

//int     vortex_timeval_substract                 (struct timeval * a,
//                                                  struct timeval * b,
//                                                  struct timeval * result);
function  vortex_timeval_substract                 (a     : Ptimeval;
                                                    b     : Ptimeval;
                                                    result: Ptimeval):longint;
                                                    cdecl;external External_library name 'vortex_timeval_substract';

//char  * vortex_support_inet_ntoa                 (VortexCtx          * ctx,
//                                                  struct sockaddr_in * sin);
function  vortex_support_inet_ntoa                 (ctx:PVortexCtx;
                                                    sin:Psockaddr_in):Pchar;
                                                    cdecl;external External_library name 'vortex_support_inet_ntoa';

{ @  }

