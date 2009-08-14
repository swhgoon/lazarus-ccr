
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

(**
 * \addtogroup axl_doc_module
 * @{
 *)

//axlDoc  * axl_doc_create                   (const char     * version,
//                                            const char     * encoding,
//                                            axl_bool   standalone);
function    axl_doc_create                   (const version:Pchar;
                                              const encoding:Pchar;
                                              standalone:Taxl_bool):PaxlDoc;
                                              cdecl;external External_library name 'axl_doc_create';

//axlDoc  * axl_doc_parse                    (const char * entity,
//                                            int entity_size,
//                                            axlError ** error);
function    axl_doc_parse                    (const entity:Pchar;
                                              entity_size:longint;
                                              error:PPaxlError):PaxlDoc;
                                              cdecl;external External_library name 'axl_doc_parse';

//axlDoc  * axl_doc_parse_strings            (axlError ** error,
//                                            ...);
function    axl_doc_parse_strings            (error:PPaxlError;
                                              args:array of const):PaxlDoc;
                                              cdecl;external External_library name 'axl_doc_parse_strings';

function    axl_doc_parse_strings            (error:PPaxlError):PaxlDoc;
                                              cdecl;external External_library name 'axl_doc_parse_strings';

//axlDoc  * axl_doc_parse_from_file          (const char * file_path,
//                                            axlError  ** error);
function    axl_doc_parse_from_file          (const file_path:Pchar;
                                              error:PPaxlError):PaxlDoc;
                                              cdecl;external External_library name 'axl_doc_parse_from_file';

//axl_bool  axl_doc_dump                     (axlDoc  * doc,
//                                            char   ** content,
//                                            int     * size);
function    axl_doc_dump                     (doc:PaxlDoc;
                                              content:PPchar;
                                              var size:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_dump';

//axl_bool  axl_doc_dump_pretty              (axlDoc  * doc,
//                                            char   ** content,
//                                            int     * size,
//                                            int       tabular);
function    axl_doc_dump_pretty              (doc:PaxlDoc;
                                              content:PPchar;
                                              var size:longint;
                                              tabular:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_dump_pretty';

//axl_bool  axl_doc_dump_to_file             (axlDoc     * doc,
//                                            const char * file_path);
function    axl_doc_dump_to_file             (doc:PaxlDoc;
                                              const file_path:Pchar):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_dump_to_file';

//axl_bool  axl_doc_dump_pretty_to_file      (axlDoc     * doc,
//                                            const char * file_path,
//                                            int          tabular);
function    axl_doc_dump_pretty_to_file      (doc:PaxlDoc;
                                              const file_path:Pchar;
                                              tabular:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_dump_pretty_to_file';

//int       axl_doc_get_flat_size            (axlDoc * doc);
function    axl_doc_get_flat_size            (doc:PaxlDoc):longint;
                                              cdecl;external External_library name 'axl_doc_get_flat_size';

//axl_bool  axl_doc_are_equal                (axlDoc * doc,
//                                            axlDoc * doc2);
function    axl_doc_are_equal                (doc:PaxlDoc;
                                              doc2:PaxlDoc):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_are_equal';

//axl_bool  axl_doc_are_equal_trimmed        (axlDoc * doc,
//                                            axlDoc * doc2);
function    axl_doc_are_equal_trimmed        (doc:PaxlDoc;
                                              doc2:PaxlDoc):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_are_equal_trimmed';

//axl_bool  axl_doc_are_equal_full           (axlDoc    * doc,
//                                            axlDoc    * doc2,
//                                            axl_bool    trimmed,
//                                            axlError ** error);
function    axl_doc_are_equal_full           (doc:PaxlDoc;
                                              doc2:PaxlDoc;
                                              trimmed:Taxl_bool;
                                              error:PPaxlError):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_are_equal_full';

//axlNode * axl_doc_get_root                 (axlDoc * doc);
function    axl_doc_get_root                 (doc:PaxlDoc):PaxlNode;
                                              cdecl;external External_library name 'axl_doc_get_root';

//void      axl_doc_set_root                 (axlDoc * doc,
//                                            axlNode * root);
procedure   axl_doc_set_root                 (doc:PaxlDoc;
                                              root:PaxlNode);
                                              cdecl;external External_library name 'axl_doc_set_root';

//axlList * axl_doc_get_list                 (axlDoc     * doc,
//                                            const char * path_to);
function    axl_doc_get_list                 (doc:PaxlDoc;
                                              const path_to:Pchar):PaxlList;
                                              cdecl;external External_library name 'axl_doc_get_list';

//axlNode * axl_doc_get                      (axlDoc     * doc,
//                                            const char * path_to);
function    axl_doc_get                      (doc:PaxlDoc;
                                              const path_to:Pchar):PaxlNode;
                                              cdecl;external External_library name 'axl_doc_get';

(**
 * @brief Convenience macro that allows to call \ref
 * axl_node_find_called, providing a document, making the parent
 * reference to use the root document node.
 *
 * See \ref axl_node_find_called for more information.
 * 
 * @param doc The document where the node lookup will be produced.
 * @param name The child node name that is looked up.
 * 
 * @return A reference to the node found or NULL if it fails to find
 * the node requested.
 *)
//#define axl_doc_find_called(doc,name) (axl_node_find_called(axl_doc_get_root(doc),name))
function axl_doc_find_called(doc:PaxlDoc; name:Pchar):PaxlNode;

//const char * axl_doc_get_content_at        (axlDoc     * doc,
//                                            const char * path_to,
//                                            int        * content_size);
function       axl_doc_get_content_at        (doc:PaxlDoc;
                                              const path_to:Pchar;
                                              var content_size:longint):Pchar;
                                              cdecl;external External_library name 'axl_doc_get_content_at';

//const char * axl_doc_get_encoding          (axlDoc * doc);
function       axl_doc_get_encoding          (doc:PaxlDoc):Pchar;
                                              cdecl;external External_library name 'axl_doc_get_encoding';

//axl_bool  axl_doc_get_standalone           (axlDoc * doc);
function    axl_doc_get_standalone           (doc:PaxlDoc):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_get_standalone';

//void      axl_doc_free                     (axlDoc * doc);
procedure   axl_doc_free                     (doc:PaxlDoc);
                                              cdecl;external External_library name 'axl_doc_free';

//void      axl_doc_set_child_current_parent (axlDoc * doc,
//                                            axlNode * node);
procedure   axl_doc_set_child_current_parent (doc:PaxlDoc;
                                              node:PaxlNode);
                                              cdecl;external External_library name 'axl_doc_set_child_current_parent';

//void      axl_doc_pop_current_parent       (axlDoc * doc);
procedure   axl_doc_pop_current_parent       (doc:PaxlDoc);
                                              cdecl;external External_library name 'axl_doc_pop_current_parent';

//void      axl_doc_add_pi_target            (axlDoc * doc,
//                                            char * target,
//                                            char * content);
procedure   axl_doc_add_pi_target            (doc:PaxlDoc;
                                              target:Pchar;
                                              content:Pchar);
                                              cdecl;external External_library name 'axl_doc_add_pi_target';

//axl_bool  axl_doc_has_pi_target            (axlDoc * doc,
//                                            char * pi_target);
function    axl_doc_has_pi_target            (doc:PaxlDoc;
                                              pi_target:Pchar):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_has_pi_target';

//char    * axl_doc_get_pi_target_content    (axlDoc * doc,
//                                            char * pi_target);
function    axl_doc_get_pi_target_content    (doc:PaxlDoc;
                                              pi_target:Pchar):Pchar;
                                              cdecl;external External_library name 'axl_doc_get_pi_target_content';

//axlList * axl_doc_get_pi_target_list       (axlDoc * doc);
function    axl_doc_get_pi_target_list       (doc:PaxlDoc):PaxlList;
                                              cdecl;external External_library name 'axl_doc_get_pi_target_list';

//axlPI   * axl_pi_create                    (char * name,
//                                            char * content);
function    axl_pi_create                    (name:Pchar;
                                              content:Pchar):PaxlPI;
                                              cdecl;external External_library name 'axl_pi_create';

//axlPI   * axl_pi_copy                      (axlPI  * pi);
function    axl_pi_copy                      (pi:PaxlPI):PaxlPI;
                                              cdecl;external External_library name 'axl_pi_copy';

//axl_bool  axl_pi_are_equal                 (axlPI  * pi,
//                                            axlPI * pi2);
function    axl_pi_are_equal                 (pi:PaxlPI;
                                              pi2:PaxlPI):Taxl_bool;
                                              cdecl;external External_library name 'axl_pi_are_equal';

//char    * axl_pi_get_name                  (axlPI  * pi);
function    axl_pi_get_name                  (pi:PaxlPI):Pchar;
                                              cdecl;external External_library name 'axl_pi_get_name';

//char    * axl_pi_get_content               (axlPI  * pi);
function    axl_pi_get_content               (pi:PaxlPI):Pchar;
                                              cdecl;external External_library name 'axl_pi_get_content';

//void      axl_pi_free                      (axlPI  * pi);
procedure   axl_pi_free                      (pi:PaxlPI);
                                              cdecl;external External_library name 'axl_pi_free';

//int       axl_pi_get_size                  (axlPI  * pi);
function    axl_pi_get_size                  (pi:PaxlPI):longint;
                                              cdecl;external External_library name 'axl_pi_get_size';

//axl_bool  axl_doc_iterate                  (axlDoc           * doc,
//                                            AxlIterationMode   mode,
//                                            axlIterationFunc   func,
//                                            axlPointer         ptr);
function    axl_doc_iterate                  (doc:PaxlDoc;
                                              mode:TaxlIterationMode;
                                              func:TaxlIterationFunc;
                                              ptr:TaxlPointer):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_iterate';

//axl_bool  axl_doc_iterate_full             (axlDoc           * doc,
//                                            AxlIterationMode   mode,
//                                            axlIterationFunc2  func,
//                                            axlPointer         ptr,
//                                            axlPointer         ptr2);
function    axl_doc_iterate_full             (doc:PaxlDoc;
                                              mode:TaxlIterationMode;
                                              func:TaxlIterationFunc2;
                                              ptr:TaxlPointer;
                                              ptr2:TaxlPointer):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_iterate_full';

//axl_bool  axl_doc_iterate_full_from        (axlDoc           * doc,
//                                            axlNode          * starting_from,
//                                            AxlIterationMode   mode,
//                                            axlIterationFunc2  func,
//                                            axlPointer         ptr,
//                                            axlPointer         ptr2);
function    axl_doc_iterate_full_from        (doc:PaxlDoc;
                                              starting_from:PaxlNode;
                                              mode:TAxlIterationMode;
                                              func:TaxlIterationFunc2;
                                              ptr:TaxlPointer;
                                              ptr2:TaxlPointer):Taxl_bool;
                                              cdecl;external External_library name 'axl_doc_iterate_full_from';

(* @} *)

