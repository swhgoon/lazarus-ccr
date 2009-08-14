unit axl;

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

interface

uses
  unixtype;

const
  External_library='libaxl'; {Setup as you need}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$include axl_decl.inc}
{$include axl_list.inc}
{$include axl_stack.inc}
{$include axl_hash.inc}
{$include axl_stream.inc}
{$include axl_doc.inc}
{$include axl_node.inc}
{$include axl_dtd.inc}
{$include axl_error.inc}
{$include axl_log.inc}
{$include axl_factory.inc}

(**
 * \addtogroup axl_module
 * @{
 *)

//axl_bool  axl_init ();
function    axl_init  :Taxl_bool;cdecl;external External_library name 'axl_init';

//void      axl_end ();
procedure   axl_end;cdecl;external External_library name 'axl_end';

implementation

{$include axl_decl_imp.inc}
//{$include axl_list_imp.inc}
//{$include axl_stack_imp.inc}
//{$include axl_hash_imp.inc}
{$include axl_stream_imp.inc}
{$include axl_doc_imp.inc}
{$include axl_node_imp.inc}
//{$include axl_dtd_imp.inc}
//{$include axl_error_imp.inc}
//{$include axl_log_imp.inc}
//{$include axl_factory_imp.inc}

end.

(* @} *)

