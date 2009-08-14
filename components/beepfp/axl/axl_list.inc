
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

//axlList  * axl_list_new    (axlEqualFunc are_equal, axlDestroyFunc destroy_data);
function     axl_list_new    (are_equal:TaxlEqualFunc; destroy_data:TaxlDestroyFunc):PaxlList;
                              cdecl;external External_library name 'axl_list_new';

//void     axl_list_set_destroy_func (axlList * list, axlDestroyFunc destroy_func);
procedure  axl_list_set_destroy_func (list:PaxlList; destroy_func:TaxlDestroyFunc);
                                      cdecl;external External_library name 'axl_list_set_destroy_func';

//int     axl_list_equal_string (axlPointer a, axlPointer b);
function  axl_list_equal_string (a:TaxlPointer; b:TaxlPointer):longint;
                                 cdecl;external External_library name 'axl_list_equal_string';

//int     axl_list_equal_int    (axlPointer a, axlPointer b);
function  axl_list_equal_int    (a:TaxlPointer; b:TaxlPointer):longint;
                                 cdecl;external External_library name 'axl_list_equal_int';

//int     axl_list_always_return_1 (axlPointer a, axlPointer b);
function  axl_list_always_return_1 (a:TaxlPointer; b:TaxlPointer):longint;
                                    cdecl;external External_library name 'axl_list_always_return_1';

//axlList  * axl_list_copy   (axlList * list, axlDuplicateFunc func);
function     axl_list_copy   (list:PaxlList; func:TaxlDuplicateFunc):PaxlList;
                              cdecl;external External_library name 'axl_list_copy';

//void      axl_list_add    (axlList * list, axlPointer pointer);
procedure   axl_list_add    (list:PaxlList; pointer:TaxlPointer);
                             cdecl;external External_library name 'axl_list_add';

//void     axl_list_add_at (axlList * list, axlPointer pointer, int position);
procedure  axl_list_add_at (list:PaxlList; pointer:TaxlPointer; position:longint);
                            cdecl;external External_library name 'axl_list_add_at';

//void      axl_list_prepend (axlList * list, axlPointer pointer);
procedure   axl_list_prepend (list:PaxlList; pointer:TaxlPointer);
                              cdecl;external External_library name 'axl_list_prepend';

//void      axl_list_append  (axlList * list, axlPointer pointer);
procedure   axl_list_append  (list:PaxlList; pointer:TaxlPointer);
                              cdecl;external External_library name 'axl_list_append';

//void     axl_list_remove     (axlList * list, axlPointer pointer);
procedure  axl_list_remove     (list:PaxlList; pointer:TaxlPointer);
                                cdecl;external External_library name 'axl_list_remove';

//void     axl_list_remove_ptr (axlList * list, axlPointer pointer);
procedure  axl_list_remove_ptr (list:PaxlList; pointer:TaxlPointer);
                                cdecl;external External_library name 'axl_list_remove_ptr';

//void     axl_list_remove_at  (axlList * list, int position);
procedure  axl_list_remove_at  (list:PaxlList; position:longint);
                                cdecl;external External_library name 'axl_list_remove_at';

//void     axl_list_unlink (axlList * list, axlPointer pointer);
procedure  axl_list_unlink (list:PaxlList; pointer:TaxlPointer);
                            cdecl;external External_library name 'axl_list_unlink';

//void     axl_list_unlink_ptr (axlList * list, axlPointer pointer);
procedure  axl_list_unlink_ptr (list:PaxlList; pointer:TaxlPointer);
                                cdecl;external External_library name 'axl_list_unlink_ptr';

//void     axl_list_unlink_at  (axlList * list, int position);
procedure  axl_list_unlink_at  (list:PaxlList; position:longint);
                                cdecl;external External_library name 'axl_list_unlink_at';

//void     axl_list_remove_first (axlList * list);
procedure  axl_list_remove_first (list:PaxlList);
                                  cdecl;external External_library name 'axl_list_remove_first';

//void     axl_list_unlink_first (axlList * list);
procedure  axl_list_unlink_first (list:PaxlList);
                                  cdecl;external External_library name 'axl_list_unlink_first';

//void     axl_list_remove_last (axlList * list);
procedure  axl_list_remove_last (list:PaxlList);
                                  cdecl;external External_library name 'axl_list_remove_last';

//void     axl_list_unlink_last (axlList * list);
procedure  axl_list_unlink_last (list:PaxlList);
                                 cdecl;external External_library name 'axl_list_unlink_last';

//axl_bool axl_list_exists (axlList * list, axlPointer pointer);
function   axl_list_exists (list:PaxlList; pointer:TaxlPointer):Taxl_bool;
                            cdecl;external External_library name 'axl_list_exists';

//axl_bool axl_list_exists_at (axlList * list, axlPointer pointer, int position);
function   axl_list_exists_at (list:PaxlList; pointer:TaxlPointer; position:longint):Taxl_bool;
                                cdecl;external External_library name 'axl_list_exists_at';

//axl_bool  axl_list_is_empty  (axlList * list);
function    axl_list_is_empty  (list:PaxlList):Taxl_bool;
                                cdecl;external External_library name 'axl_list_is_empty';

//axlPointer axl_list_get_first (axlList * list);
function     axl_list_get_first (list:PaxlList):TaxlPointer;
                                  cdecl;external External_library name 'axl_list_get_first';

//axlPointer axl_list_get_last  (axlList * list);
function     axl_list_get_last  (list:PaxlList):TaxlPointer;
                                  cdecl;external External_library name 'axl_list_get_last';

//axlPointer axl_list_get_nth   (axlList * list, int position);
function     axl_list_get_nth   (list:PaxlList; position:longint):TaxlPointer;
                                  cdecl;external External_library name 'axl_list_get_nth';

//axlPointer axl_list_lookup    (axlList * list, axlLookupFunc func, axlPointer data);
function     axl_list_lookup    (list:PaxlList; func:TaxlLookupFunc; data:TaxlPointer):TaxlPointer;
                                  cdecl;external External_library name 'axl_list_lookup';

//axl_bool   axl_list_find_string (axlPointer element, axlPointer data);
function     axl_list_find_string (element:TaxlPointer; data:TaxlPointer):Taxl_bool;
                                    cdecl;external External_library name 'axl_list_find_string';

//int        axl_list_length (axlList * list);
function     axl_list_length (list:PaxlList):longint;
                              cdecl;external External_library name 'axl_list_length';

//void       axl_list_free (axlList * list);
procedure    axl_list_free (list:PaxlList);
                            cdecl;external External_library name 'axl_list_free';

(* cursor interface *)
//axlListCursor * axl_list_cursor_new          (axlList * list);
function          axl_list_cursor_new          (list:PaxlList):PaxlListCursor;
                                                cdecl;external External_library name 'axl_list_cursor_new';

//void            axl_list_cursor_first        (axlListCursor * cursor);
procedure         axl_list_cursor_first        (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_first';

//void            axl_list_cursor_last         (axlListCursor * cursor);
procedure         axl_list_cursor_last         (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_last';

//void            axl_list_cursor_next         (axlListCursor * cursor);
procedure         axl_list_cursor_next         (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_next';

//void            axl_list_cursor_previous     (axlListCursor * cursor);
procedure         axl_list_cursor_previous     (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_previous';

//axl_bool        axl_list_cursor_has_next     (axlListCursor * cursor);
function          axl_list_cursor_has_next     (cursor:PaxlListCursor):Taxl_bool;
                                                cdecl;external External_library name 'axl_list_cursor_has_next';

//axl_bool        axl_list_cursor_has_previous (axlListCursor * cursor);
function          axl_list_cursor_has_previous (cursor:PaxlListCursor):Taxl_bool;
                                                cdecl;external External_library name 'axl_list_cursor_has_previous';

//axl_bool        axl_list_cursor_has_item     (axlListCursor * cursor);
function          axl_list_cursor_has_item     (cursor:PaxlListCursor):Taxl_bool;
                                                cdecl;external External_library name 'axl_list_cursor_has_item';

//void            axl_list_cursor_unlink       (axlListCursor * cursor);
procedure         axl_list_cursor_unlink       (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_unlink';

//void            axl_list_cursor_remove       (axlListCursor * cursor);
procedure         axl_list_cursor_remove       (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_remove';

//axlPointer      axl_list_cursor_get          (axlListCursor * cursor);
function          axl_list_cursor_get          (cursor:PaxlListCursor):TaxlPointer;
                                                cdecl;external External_library name 'axl_list_cursor_get';

//axlList       * axl_list_cursor_list         (axlListCursor * cursor);
function          axl_list_cursor_list         (cursor:PaxlListCursor):PaxlList;
                                                cdecl;external External_library name 'axl_list_cursor_list';

//void            axl_list_cursor_free         (axlListCursor * cursor);
procedure         axl_list_cursor_free         (cursor:PaxlListCursor);
                                                cdecl;external External_library name 'axl_list_cursor_free';

