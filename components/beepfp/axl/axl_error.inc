
(*
 * LibAxl: C Header file to Free Pascal translation.
 * Copyright (C) 2009, Wimpie Nortje <wimpienortje@gmail.com>
 *)

(*
 *  LibAxl:  Another XML library
 *  Copyright (C) 2006 Advanced Software Production Line, S.L.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of 
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *  
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to
 *  develop proprietary applications using this library without any
 *  royalty or fee but returning back any change, improvement or
 *  addition in the form of source code, project image, documentation
 *  patches, etc. 
 *
 *  For commercial support on build XML enabled solutions contact us:
 *          
 *      Postal address:
 *         Advanced Software Production Line, S.L.
 *         Edificio Alius A, Oficina 102,
 *         C/ Antonio Suarez Nº 10,
 *         Alcalá de Henares 28802 Madrid
 *         Spain
 *
 *      Email address:
 *         info@aspl.es - http://www.aspl.es/xml
 *)

//void       axl_error_new      (int code, char * error, axlStream * stream, axlError ** _error);
procedure    axl_error_new      (code: longint; aerror: PChar; stream: PaxlStream; _error: PPaxlError);
                                 cdecl;external External_library name 'axl_error_new';

//void       axl_error_report   (axlError ** _error, int code, char * format, ...);
procedure    axl_error_report   (_error:PPaxlError; code:longint; format:Pchar; args:array of const);
                                 cdecl;external External_library name 'axl_error_report';

procedure    axl_error_report   (_error:PPaxlError; code:longint; format:Pchar);
                                 cdecl;external External_library name 'axl_error_report';

//axl_bool   axl_error_was_ok   (axlError * _error);
function     axl_error_was_ok   (_error:PaxlError):Taxl_bool;
                                 cdecl;external External_library name 'axl_error_was_ok';

//int        axl_error_get_code (axlError * _error);
function     axl_error_get_code (_error:PaxlError):longint;
                                 cdecl;external External_library name 'axl_error_get_code';

//char     * axl_error_get      (axlError * _error);
function     axl_error_get      (_error:PaxlError):Pchar;
                                 cdecl;external External_library name 'axl_error_get';

//void       axl_error_free     (axlError * _error);
procedure    axl_error_free     (_error:PaxlError);
                                 cdecl;external External_library name 'axl_error_free';
