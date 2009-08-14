
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

//axlHash       * axl_hash_new          (axlHashFunc    hash,
//                                       axlEqualFunc   equal);
function          axl_hash_new          (hash  : TaxlHashFunc;
                                         equal : TaxlEqualFunc):PaxlHash;
                                         cdecl;external External_library name 'axl_hash_new';

//axlHash       * axl_hash_new_full     (axlHashFunc    hash,
//                                       axlEqualFunc   equal,
//                                       int            step);
function          axl_hash_new_full     (hash  : TaxlHashFunc;
                                         equal : TaxlEqualFunc;
                                         step  : longint):PaxlHash;
                                         cdecl;external External_library name 'axl_hash_new_full';

//unsigned int    axl_hash_string       (axlPointer key);
function          axl_hash_string       (key:TaxlPointer):longword;
                                         cdecl;external External_library name 'axl_hash_string';

//int             axl_hash_equal_string (axlPointer keya,
//                                       axlPointer keyb);
function          axl_hash_equal_string (keya:TaxlPointer;
                                         keyb:TaxlPointer):longint;
                                         cdecl;external External_library name 'axl_hash_equal_string';

//unsigned int    axl_hash_int          (axlPointer key);
function          axl_hash_int          (key:TaxlPointer):longword;
                                         cdecl;external External_library name 'axl_hash_int';

//int             axl_hash_equal_int    (axlPointer keya,
//                                       axlPointer keyb);
function          axl_hash_equal_int    (keya:TaxlPointer;
                                         keyb:TaxlPointer):longint;
                                         cdecl;external External_library name 'axl_hash_equal_int';

//void            axl_hash_insert       (axlHash    * hash,
//                                       axlPointer   key,
//                                       axlPointer   data);
procedure         axl_hash_insert       (hash:PaxlHash;
                                         key:TaxlPointer;
                                         data:TaxlPointer);
                                         cdecl;external External_library name 'axl_hash_insert';

//void            axl_hash_insert_full  (axlHash        * hash,
//                                       axlPointer       key,
//                                       axlDestroyFunc   destroy_key,
//                                       axlPointer       data,
//                                       axlDestroyFunc   destroy_data);
procedure         axl_hash_insert_full  (hash:PaxlHash;
                                         key:TaxlPointer;
                                         destroy_key:TaxlDestroyFunc;
                                         data:TaxlPointer;
                                         destroy_data:TaxlDestroyFunc);
                                         cdecl;external External_library name 'axl_hash_insert_full';

//axl_bool        axl_hash_remove       (axlHash    * hash,
//                                       axlPointer   key);
function          axl_hash_remove       (hash:PaxlHash;
                                         key:TaxlPointer):Taxl_bool;
                                         cdecl;external External_library name 'axl_hash_remove';

//axl_bool        axl_hash_delete       (axlHash    * hash,
//                                       axlPointer   key);
function          axl_hash_delete       (hash:PaxlHash;
                                         key:TaxlPointer):Taxl_bool;
                                         cdecl;external External_library name 'axl_hash_delete';

//axl_bool        axl_hash_exists       (axlHash   * hash,
//                                       axlPointer  key);
function          axl_hash_exists       (hash:PaxlHash; key:TaxlPointer):Taxl_bool;
                                         cdecl;external External_library name 'axl_hash_exists';

//axlPointer      axl_hash_get          (axlHash * hash,
//                                       axlPointer key);
function          axl_hash_get          (hash:PaxlHash;
                                         key:TaxlPointer):TaxlPointer;
                                         cdecl;external External_library name 'axl_hash_get';

//void            axl_hash_foreach      (axlHash            * hash,
//                                       axlHashForeachFunc   func,
//                                       axlPointer           user_data);
procedure         axl_hash_foreach      (hash:PaxlHash;
                                         func:TaxlHashForeachFunc;
                                         user_data:TaxlPointer);
                                         cdecl;external External_library name 'axl_hash_foreach';

//void            axl_hash_foreach2     (axlHash            * hash,
//                                       axlHashForeachFunc2  func,
//                                       axlPointer           user_data,
//                                       axlPointer           user_data2);
procedure         axl_hash_foreach2     (hash:PaxlHash;
                                         func:TaxlHashForeachFunc2;
                                         user_data:TaxlPointer;
                                         user_data2:TaxlPointer);
                                         cdecl;external External_library name 'axl_hash_foreach2';

//void            axl_hash_foreach3     (axlHash            * hash,
//                                       axlHashForeachFunc3  func,
//                                       axlPointer           user_data,
//                                       axlPointer           user_data2,
//                                       axlPointer           user_data3);
procedure         axl_hash_foreach3     (hash:PaxlHash;
                                         func:TaxlHashForeachFunc3;
                                         user_data:TaxlPointer;
                                         user_data2:TaxlPointer;
                                         user_data3:TaxlPointer);
                                         cdecl;external External_library name 'axl_hash_foreach3';

//void            axl_hash_foreach4     (axlHash            * hash,
//                                       axlHashForeachFunc4  func,
//                                       axlPointer           user_data,
//                                       axlPointer           user_data2,
//                                       axlPointer           user_data3,
//                                       axlPointer           user_data4);
procedure         axl_hash_foreach4     (hash:PaxlHash;
                                         func:TaxlHashForeachFunc4;
                                         user_data:TaxlPointer;
                                         user_data2:TaxlPointer;
                                         user_data3:TaxlPointer;
                                         user_data4:TaxlPointer);
                                         cdecl;external External_library name 'axl_hash_foreach4';

//int             axl_hash_items        (axlHash * hash);
function          axl_hash_items        (hash:PaxlHash):longint;
                                         cdecl;external External_library name 'axl_hash_items';

//axlHash       * axl_hash_copy         (axlHash             * hash,
//                                       axlHashItemCopy       key_copy,
//                                       axlHashItemCopy       value_copy);
function          axl_hash_copy         (hash:PaxlHash;
                                         key_copy:TaxlHashItemCopy;
                                         value_copy:TaxlHashItemCopy):PaxlHash;
                                         cdecl;external External_library name 'axl_hash_copy';

//void            axl_hash_show_status  (axlHash * hash);
procedure         axl_hash_show_status  (hash:PaxlHash);
                                          cdecl;external External_library name 'axl_hash_show_status';

//typedef void ( *axlHashPrintKeyData) (axlPointer key, axlPointer data);
type
  TaxlHashPrintKeyData = procedure (key:TaxlPointer; data:TaxlPointer);cdecl;

//void            axl_hash_show_status_full (axlHash * hash,
//                                           axlHashPrintKeyData show_item);
procedure         axl_hash_show_status_full (hash:PaxlHash;
                                             show_item:TaxlHashPrintKeyData);
                                             cdecl;external External_library name 'axl_hash_show_status_full';

//void            axl_hash_free         (axlHash * hash);
procedure         axl_hash_free         (hash:PaxlHash);
                                         cdecl;external External_library name 'axl_hash_free';

(* cursor interface *)
//axlHashCursor * axl_hash_cursor_new          (axlHash * hash);
function          axl_hash_cursor_new          (hash:PaxlHash):PaxlHashCursor;
                                                cdecl;external External_library name 'axl_hash_cursor_new';

//void            axl_hash_cursor_first        (axlHashCursor * cursor);
procedure         axl_hash_cursor_first        (cursor:PaxlHashCursor);
                                                cdecl;external External_library name 'axl_hash_cursor_first';

//void            axl_hash_cursor_last         (axlHashCursor * cursor);
procedure         axl_hash_cursor_last         (cursor:PaxlHashCursor);
                                                cdecl;external External_library name 'axl_hash_cursor_last';

//void            axl_hash_cursor_next         (axlHashCursor * cursor);
procedure         axl_hash_cursor_next         (cursor:PaxlHashCursor);
                                                cdecl;external External_library name 'axl_hash_cursor_next';

//axl_bool        axl_hash_cursor_has_next     (axlHashCursor * cursor);
function          axl_hash_cursor_has_next     (cursor:PaxlHashCursor):Taxl_bool;
                                                cdecl;external External_library name 'axl_hash_cursor_has_next';

//axl_bool        axl_hash_cursor_has_item     (axlHashCursor * cursor);
function          axl_hash_cursor_has_item     (cursor:PaxlHashCursor):Taxl_bool;
                                                cdecl;external External_library name 'axl_hash_cursor_has_item';

//void            axl_hash_cursor_remove       (axlHashCursor * cursor);
procedure         axl_hash_cursor_remove       (cursor:PaxlHashCursor);
                                                cdecl;external External_library name 'axl_hash_cursor_remove';

//axlPointer      axl_hash_cursor_get_key      (axlHashCursor * cursor);
function          axl_hash_cursor_get_key      (cursor:PaxlHashCursor):TaxlPointer;
                                                cdecl;external External_library name 'axl_hash_cursor_get_key';

//axlPointer      axl_hash_cursor_get_value    (axlHashCursor * cursor);
function          axl_hash_cursor_get_value    (cursor:PaxlHashCursor):TaxlPointer;
                                                cdecl;external External_library name 'axl_hash_cursor_get_value';

//axlHash       * axl_hash_cursor_hash         (axlHashCursor * cursor);
function          axl_hash_cursor_hash         (cursor:PaxlHashCursor):PaxlHash;
                                                cdecl;external External_library name 'axl_hash_cursor_hash';

//void            axl_hash_cursor_free         (axlHashCursor * cursor);
procedure         axl_hash_cursor_free         (cursor:PaxlHashCursor);
                                                cdecl;external External_library name 'axl_hash_cursor_free';

