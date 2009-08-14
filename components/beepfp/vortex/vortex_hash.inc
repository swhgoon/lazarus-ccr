
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

//VortexHash * vortex_hash_new_full(axlHashFunc    hash_func,
//                                  axlEqualFunc   key_equal_func,
//                                  axlDestroyFunc key_destroy_func,
//                                  axlDestroyFunc value_destroy_func);
function       vortex_hash_new_full(hash_func          : TaxlHashFunc;
                                    key_equal_func     : TaxlEqualFunc;
                                    key_destroy_func   : TaxlDestroyFunc;
                                    value_destroy_func : TaxlDestroyFunc):PVortexHash;
                                    cdecl;external External_library name 'vortex_hash_new_full';

//VortexHash * vortex_hash_new     (axlHashFunc    hash_func,
//                                  axlEqualFunc   key_equal_func);
function       vortex_hash_new     (hash_func      : TaxlHashFunc;
                                    key_equal_func : TaxlEqualFunc):PVortexHash;
                                    cdecl;external External_library name 'vortex_hash_new';

//void         vortex_hash_ref     (VortexHash   * hash_table);
procedure      vortex_hash_ref     (hash_table : PVortexHash);
                                    cdecl;external External_library name 'vortex_hash_ref';

//void         vortex_hash_unref   (VortexHash   * hash_table);
procedure      vortex_hash_unref   (hash_table : PVortexHash);
                                    cdecl;external External_library name 'vortex_hash_unref';

//void         vortex_hash_insert  (VortexHash *hash_table,
//                                  axlPointer  key,
//                                  axlPointer  value);
procedure      vortex_hash_insert  (hash_table : PVortexHash;
                                    key        : TaxlPointer;
                                    value      : TaxlPointer);
                                    cdecl;external External_library name 'vortex_hash_insert';

//void         vortex_hash_replace (VortexHash *hash_table,
//                                  axlPointer  key,
//                                  axlPointer  value);
procedure      vortex_hash_replace (hash_table : PVortexHash;
                                    key        : TaxlPointer;
                                    value:TaxlPointer);
                                    cdecl;external External_library name 'vortex_hash_replace';

//void         vortex_hash_replace_full(VortexHash     * hash_table,
//                                      axlPointer       key,
//                                      axlDestroyFunc   key_destroy,
//                                      axlPointer       value,
//                                      axlDestroyFunc   value_destroy);
procedure      vortex_hash_replace_full(hash_table    : PVortexHash;
                                        key           : TaxlPointer;
                                        key_destroy   : TaxlDestroyFunc;
                                        value         : TaxlPointer;
                                        value_destroy : TaxlDestroyFunc);
                                        cdecl;external External_library name 'vortex_hash_replace_full';

//int          vortex_hash_size    (VortexHash   *hash_table);
function       vortex_hash_size    (hash_table : PVortexHash):longint;
                                    cdecl;external External_library name 'vortex_hash_size';

//axlPointer   vortex_hash_lookup  (VortexHash   *hash_table,
//                                  axlPointer    key);
function       vortex_hash_lookup  (hash_table : PVortexHash;
                                    key        : TaxlPointer):TaxlPointer;
                                    cdecl;external External_library name 'vortex_hash_lookup';

//axlPointer   vortex_hash_lookup_and_clear  (VortexHash   *hash_table,
//                                            axlPointer    key);
function       vortex_hash_lookup_and_clear  (hash_table : PVortexHash;
                                              key        : TaxlPointer):TaxlPointer;
                                              cdecl;external External_library name 'vortex_hash_lookup_and_clear';

//int          vortex_hash_lock_until_changed (VortexHash   *hash_table,
//                                             long int      wait_microseconds);
function       vortex_hash_lock_until_changed (hash_table        : PVortexHash;
                                               wait_microseconds : int64):longint;
                                               cdecl;external External_library name 'vortex_hash_lock_until_changed';

//axl_bool     vortex_hash_remove  (VortexHash   *hash_table,
//                                  axlPointer    key);
function       vortex_hash_remove  (hash_table : PVortexHash;
                                    key        : TaxlPointer):Taxl_bool;
                                    cdecl;external External_library name 'vortex_hash_remove';

//void         vortex_hash_destroy (VortexHash *hash_table);
procedure      vortex_hash_destroy (hash_table : PVortexHash);
                                    cdecl;external External_library name 'vortex_hash_destroy';

//axl_bool     vortex_hash_delete  (VortexHash   *hash_table,
//                                  axlPointer    key);
function       vortex_hash_delete  (hash_table : PVortexHash;
                                    key        : TaxlPointer):Taxl_bool;
                                    cdecl;external External_library name 'vortex_hash_delete';

//void         vortex_hash_foreach (VortexHash         * hash_table,
//                                  axlHashForeachFunc   func,
//                                  axlPointer           user_data);
procedure      vortex_hash_foreach (hash_table : PVortexHash;
                                    func       : TaxlHashForeachFunc;
                                    user_data  : TaxlPointer);
                                    cdecl;external External_library name 'vortex_hash_foreach';

//void         vortex_hash_foreach2(VortexHash         * hash_table,
//                                  axlHashForeachFunc2  func,
//                                  axlPointer           user_data,
//                                  axlPointer           user_data2);
procedure      vortex_hash_foreach2(hash_table : PVortexHash;
                                    func       : TaxlHashForeachFunc2;
                                    user_data  : TaxlPointer;
                                    user_data2 : TaxlPointer);
                                    cdecl;external External_library name 'vortex_hash_foreach2';

//void         vortex_hash_foreach3(VortexHash         * hash_table,
//                                  axlHashForeachFunc3  func,
//                                  axlPointer           user_data,
//                                  axlPointer           user_data2,
//                                  axlPointer           user_data3);
procedure      vortex_hash_foreach3(hash_table : PVortexHash;
                                    func       : TaxlHashForeachFunc3;
                                    user_data  : TaxlPointer;
                                    user_data2 : TaxlPointer;
                                    user_data3 : TaxlPointer);
                                    cdecl;external External_library name 'vortex_hash_foreach3';

//void         vortex_hash_clear   (VortexHash *hash_table);
procedure      vortex_hash_clear   (hash_table : PVortexHash);
                                    cdecl;external External_library name 'vortex_hash_clear';

