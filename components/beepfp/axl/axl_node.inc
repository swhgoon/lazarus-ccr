
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
 * \addtogroup axl_node_module
 * @{
 *)

//axlNode * axl_node_create                   (const char * name);
function    axl_node_create                   (const name:Pchar):PaxlNode;
                                               cdecl;external External_library name 'axl_node_create';

//axlNode * axl_node_create_ref               (char * name);
function    axl_node_create_ref               (name:Pchar):PaxlNode;
                                               cdecl;external External_library name 'axl_node_create_ref';

//axlNode * axl_node_parse_strings            (axlError ** error, ...);
function    axl_node_parse_strings            (error:PPaxlError; args:array of const):PaxlNode;
                                                cdecl;external External_library name 'axl_node_parse_strings';

function    axl_node_parse_strings            (error:PPaxlError):PaxlNode;
                                                cdecl;external External_library name 'axl_node_parse_strings';

//axlNode * axl_node_parse                    (axlError ** error, const char * content, ...);
function    axl_node_parse                    (error:PPaxlError; const content:Pchar; args:array of const):PaxlNode;
                                              cdecl;external External_library name 'axl_node_parse';

function    axl_node_parse                    (error:PPaxlError; content:Pchar):PaxlNode;
                                              cdecl;external External_library name 'axl_node_parse';

//void      axl_node_set_name                 (axlNode * node,
//                                             const char * name);
procedure   axl_node_set_name                 (node:PaxlNode;
                                               const name:Pchar);
                                               cdecl;external External_library name 'axl_node_set_name';

//void      axl_node_set_name_ref             (axlNode * node,
//                                             char * name);
procedure   axl_node_set_name_ref             (node:PaxlNode;
                                               name:Pchar);
                                               cdecl;external External_library name 'axl_node_set_name_ref';

//void      axl_node_set_name_from_factory    (axlNode * node,
//                                             char * name);
procedure   axl_node_set_name_from_factory    (node:PaxlNode;
                                               name:Pchar);
                                               cdecl;external External_library name 'axl_node_set_name_from_factory';

//axlNode * axl_node_copy                     (axlNode * node,
//                                             axl_bool  copy_attributes,
//                                             axl_bool  copy_childs);
function    axl_node_copy                    (node:PaxlNode;
                                              copy_attributes:Taxl_bool;
                                              copy_childs:Taxl_bool):PaxlNode;
                                              cdecl;external External_library name 'axl_node_copy';

//axlDoc  * axl_node_get_doc                  (axlNode * node);
function    axl_node_get_doc                  (node:PaxlNode):PaxlDoc;
                                               cdecl;external External_library name 'axl_node_get_doc';

//void      axl_node_set_doc                  (axlNode * node,
//                                             axlDoc  * doc);
procedure   axl_node_set_doc                  (node:PaxlNode;
                                               doc:PaxlDoc);
                                               cdecl;external External_library name 'axl_node_set_doc';

//void      axl_node_set_attribute            (axlNode    * node,
//                                              const char * attribute,
//                                              const char * value);
procedure   axl_node_set_attribute            (node:PaxlNode;
                                                const attribute:Pchar;
                                                const value:Pchar);
                                                cdecl;external External_library name 'axl_node_set_attribute';

//void      axl_node_set_attribute_ref        (axlNode * node,
//                                            char    * attribute,
//                                            char    * value);
procedure   axl_node_set_attribute_ref        (node:PaxlNode;
                                              attribute:Pchar;
                                              value:Pchar);
                                              cdecl;external External_library name 'axl_node_set_attribute_ref';

//void      axl_node_set_attribute_from_factory  (axlFactory * factory,
//                                                axlNode    * node,
//                                                char       * attribute,
//                                                char       * value);
procedure   axl_node_set_attribute_from_factory  (factory:PaxlFactory;
                                                  node:PaxlNode;
                                                  attribute:Pchar;
                                                  value:Pchar);
                                                  cdecl;external External_library name 'axl_node_set_attribute_from_factory';

(**
 * @brief Macro definition which works as an alias for \ref
 * axl_node_get_attribute_value.
 * 
 * @param node The node being checked to have an attribute value.
 * @param attr The attribute that is being check to be present in the node provided.
 * 
 * @return \ref axl_true if the node has the provided attribute, otherwise
 * \ref axl_false is returned.
 *)
//#define HAS_ATTR(node,attr) axl_node_has_attribute(node,attr)
function HAS_ATTR (node:PaxlNode; const attribute:Pchar):Taxl_bool;

//axl_bool  axl_node_has_attribute            (axlNode    * node,
//                                             const char * attribute);
function    axl_node_has_attribute            (node:PaxlNode;
                                               const attribute:Pchar):Taxl_bool;
                                               cdecl;external External_library name 'axl_node_has_attribute';

//void      axl_node_remove_attribute         (axlNode    * node,
//                                             const char * attribute);
procedure   axl_node_remove_attribute         (node:PaxlNode;
                                               const attribute:Pchar);
                                               cdecl;external External_library name 'axl_node_remove_attribute';

//int       axl_node_num_attributes           (axlNode    * node);
function    axl_node_num_attributes           (node:PaxlNode):longint;
                                               cdecl;external External_library name 'axl_node_num_attributes';

{$inline on}
(**
 * @brief Macro definition, which works as an alias for \ref
 * axl_node_get_attribute_value.
 * 
 * @param node The node that is being required to return the content
 * of a particular node.
 *
 * @param attr The attribute that is requested.
 * 
 * @return The value assocaited to the attribute value or null if it
 * fails. See \ref axl_node_get_attribute_value.
 *)
//#define ATTR_VALUE(node,attr) axl_node_get_attribute_value(node, attr)
function ATTR_VALUE(node:PaxlNode; const attribute:Pchar):Pchar;
{$inline off}

//const char    * axl_node_get_attribute_value      (axlNode    * node,
//                                                   const char * attribute);
function          axl_node_get_attribute_value      (node:PaxlNode;
                                                     const attribute:Pchar):Pchar;
                                                     cdecl;external External_library name 'axl_node_get_attribute_value';

//char    * axl_node_get_attribute_value_copy (axlNode    * node,
//                                             const char * attribute);
function    axl_node_get_attribute_value_copy (node:PaxlNode;
                                               const attribute:Pchar):Pchar;
                                               cdecl;external External_library name 'axl_node_get_attribute_value_copy';

{$inline on}
(**
 * @brief Simple alias definition to \ref axl_node_get_attribute_value_trans function.
 * 
 * @param node The node which contains the attribute requested.
 *
 * @param attr The attribute key that is being requested.
 * 
 * @return A newly allocated string containing the attribute value,
 * with known entity references translated.
 *)
//#define ATTR_VALUE_TRANS(node,attr) axl_node_get_attribute_value_trans(node,attr)
function ATTR_VALUE_TRANS(node:PaxlNode; const attribute:Pchar):Pchar;
{$inline off}

//char    * axl_node_get_attribute_value_trans (axlNode * node,
//                                              const char * attribute);
function    axl_node_get_attribute_value_trans (node:PaxlNode;
                                                const attribute:Pchar):Pchar;
                                                cdecl;external External_library name 'axl_node_get_attribute_value_trans';

{$inline on}
(**
 * @brief Allows to get the value associated to the attributed
 * provided, inside the node selected, removing trailing and ending
 * white spaces (in the W3C sence: \\n, \\t, \\r, ' ').
 *
 * This is a macro alias to \ref axl_node_get_attribute_value_trimmed.
 * 
 * @param node The node that is requested to return the associated 
 * value to the attributed.
 *
 * @param attr The attribute that is being requested.
 * 
 * @return A reference to the attribute value or NULL if it fails.
 *)
//#define ATTR_VALUE_TRIMMED(node,attr) axl_node_get_attribute_value_trimmed(node,attr)
function ATTR_VALUE_TRIMMED(node:PaxlNode; const attribute:Pchar):Pchar;
{$inline off}

//const char    * axl_node_get_attribute_value_trimmed (axlNode    * node,
//                                                      const char * attribute);
function          axl_node_get_attribute_value_trimmed (node:PaxlNode;
                                                        const attribute:Pchar):Pchar;
                                                        cdecl;external External_library name 'axl_node_get_attribute_value_trimmed';

{$inline on}
(**
 * @brief Convenience macro wrapping \ref
 * axl_node_has_attribute_value.
 *)
//#define HAS_ATTR_VALUE(node, attr,value) (axl_node_has_attribute_value (node, attr, value))
function HAS_ATTR_VALUE(node:PaxlNode; const attribute:Pchar; const value:Pchar):Taxl_bool;
{$inline off}

//axl_bool      axl_node_has_attribute_value       (axlNode    * node,
//                                                  const char * attribute,
//                                                  const char * value);
function        axl_node_has_attribute_value       (node:PaxlNode;
                                                    const attribute:Pchar;
                                                    const value:Pchar):Taxl_bool;
                                                    cdecl;external External_library name 'axl_node_has_attribute_value';

//axl_bool      axl_node_has_attributes (axlNode * node);
function        axl_node_has_attributes (node:PaxlNode):Taxl_bool;
                                         cdecl;external External_library name 'axl_node_has_attributes';

(**
 * @}
 *)

(**
 * \addtogroup axl_node_attribute_cursor
 * @{
 *)

(* XML node attribute iteration API provided to traverse all
 * attributes without knowing them. *)
//axlAttrCursor * axl_node_attr_cursor_new       (axlNode * node);
function          axl_node_attr_cursor_new       (node:PaxlNode):PaxlAttrCursor;
                                                  cdecl;external External_library name 'axl_node_attr_cursor_new';

//void            axl_node_attr_cursor_first     (axlAttrCursor * cursor);
procedure         axl_node_attr_cursor_first     (cursor:PaxlAttrCursor);
                                                  cdecl;external External_library name 'axl_node_attr_cursor_first';

//void            axl_node_attr_cursor_next      (axlAttrCursor * cursor);
procedure         axl_node_attr_cursor_next      (cursor:PaxlAttrCursor);
                                                  cdecl;external External_library name 'axl_node_attr_cursor_next';

//axl_bool        axl_node_attr_cursor_has_next  (axlAttrCursor * cursor);
function          axl_node_attr_cursor_has_next  (cursor:PaxlAttrCursor):Taxl_bool;
                                                  cdecl;external External_library name 'axl_node_attr_cursor_has_next';

//axl_bool        axl_node_attr_cursor_has_item  (axlAttrCursor * cursor);
function          axl_node_attr_cursor_has_item  (cursor:PaxlAttrCursor):Taxl_bool;
                                                  cdecl;external External_library name 'axl_node_attr_cursor_has_item';

//const char *    axl_node_attr_cursor_get_key   (axlAttrCursor * cursor);
function          axl_node_attr_cursor_get_key   (cursor:PaxlAttrCursor):Pchar;
                                                  cdecl;external External_library name 'axl_node_attr_cursor_get_key';

//const char *    axl_node_attr_cursor_get_value (axlAttrCursor * cursor);
function          axl_node_attr_cursor_get_value (cursor:PaxlAttrCursor):Pchar;
                                                  cdecl;external External_library name 'axl_node_attr_cursor_get_value';

//void            axl_node_attr_cursor_free      (axlAttrCursor * cursor);
procedure         axl_node_attr_cursor_free      (cursor:PaxlAttrCursor);
                                                  cdecl;external External_library name 'axl_node_attr_cursor_free';

//void            axl_node_attr_foreach          (axlNode       * node,
//                                                axlNodeAttrForeachFunc func,
//                                                axlPointer      data,
//                                                axlPointer      data2);
procedure         axl_node_attr_foreach          (node:PaxlNode;
                                                  func:TaxlNodeAttrForeachFunc;
                                                  data:TaxlPointer;
                                                  data2:TaxlPointer);
                                                  cdecl;external External_library name 'axl_node_attr_foreach';

(**
 * @}
 *)

(**
 * \addtogroup axl_node_annotate
 * @{
 *)

//void      axl_node_annotate_data                 (axlNode     * node,
//                                                  const char  * key,
//                                                  axlPointer    data);
procedure   axl_node_annotate_data                 (node:PaxlNode;
                                                    const key:Pchar;
                                                    data:TaxlPointer);
                                                    cdecl;external External_library name 'axl_node_annotate_data';

//void      axl_node_annotate_data_full            (axlNode       * node,
//                                                  const char    * key,
//                                                  axlDestroyFunc  key_destroy,
//                                                  axlPointer      data,
//                                                  axlDestroyFunc  data_destroy);
procedure   axl_node_annotate_data_full            (node:PaxlNode;
                                                    const key:Pchar;
                                                    key_destroy:TaxlDestroyFunc;
                                                    data:TaxlPointer;
                                                    data_destroy:TaxlDestroyFunc);
                                                    cdecl;external External_library name 'axl_node_annotate_data_full';

//axlPointer axl_node_annotate_get                 (axlNode       * node,
//                                                  const char    * key,
//                                                  axl_bool        lookup_in_parent);
function     axl_node_annotate_get                 (node:PaxlNode;
                                                    const key:Pchar;
                                                    lookup_in_parent:Taxl_bool):TaxlPointer;
                                                    cdecl;external External_library name 'axl_node_annotate_get';

//void       axl_node_annotate_int                 (axlNode * node,
//                                                  const char    * key,
//                                                  int             int_value);
procedure    axl_node_annotate_int                 (node:PaxlNode;
                                                    const key:Pchar;
                                                    int_value:longint);
                                                    cdecl;external External_library name 'axl_node_annotate_int';

//void       axl_node_annotate_string              (axlNode * node,
//                                                  const char    * key,
//                                                  const char    * string_value);
procedure    axl_node_annotate_string              (node:PaxlNode;
                                                    const key:Pchar;
                                                    const string_value:Pchar);
                                                    cdecl;external External_library name 'axl_node_annotate_string';

//void       axl_node_annotate_double              (axlNode    * node,
//                                                  const char * key,
//                                                  double       double_value);
procedure    axl_node_annotate_double              (node:PaxlNode;
                                                    const key:Pchar;
                                                    double_value:double);
                                                    cdecl;external External_library name 'axl_node_annotate_double';

//int        axl_node_annotate_get_int             (axlNode     * node,
//                                                  const char   * key,
//                                                  axl_bool       lookup_in_parent);
function     axl_node_annotate_get_int             (node:PaxlNode;
                                                    const key:Pchar;
                                                    lookup_in_parent:Taxl_bool):longint;
                                                    cdecl;external External_library name 'axl_node_annotate_get_int';

//char *     axl_node_annotate_get_string          (axlNode       * node,
//                                                  const char     * key,
//                                                  axl_bool         lookup_in_parent);
function     axl_node_annotate_get_string          (node:PaxlNode;
                                                    const key:Pchar;
                                                    lookup_in_parent:Taxl_bool):Pchar;
                                                    cdecl;external External_library name 'axl_node_annotate_get_string';

//double     axl_node_annotate_get_double          (axlNode      * node,
//                                                  const char    * key,
//                                                  axl_bool        lookup_in_parent);
function     axl_node_annotate_get_double          (node:PaxlNode;
                                                    const key:Pchar;
                                                    lookup_in_parent:Taxl_bool):double;
                                                    cdecl;external External_library name 'axl_node_annotate_get_double';

(**
 * @}
 *)


(**
 * \addtogroup axl_node_module
 * @{
 *)

(**
 * @brief Allows to easily check that the given xml node (\ref
 * axlNode) have the provided name.
 *
 * This macro is provided as a convenience to check that a particular
 * node have a particular name. Check the documentation that this
 * macro is wrapping to get examples: \ref axl_node_get_name.
 *
 * <i><b>NOTE:</b> This function isn't XML Namespace aware. You must use \ref axl_ns_node_cmp instead. See \ref axl_ns_doc_validate. </i>
 * 
 * @param node The \ref axlNode where the name will be compared with
 * the provided value.
 *
 * @param name The name to compare.
 * 
 * @return \ref axl_true if names are equal or \ref axl_false if not.
 *)
//#define NODE_CMP_NAME(node,name) (axl_cmp ((node != NULL) ? axl_node_get_name(node) : "><", (name != NULL) ? name : "<>"))
{TODO: complete function^^^}

//const char * axl_node_get_name           (axlNode * node);
function       axl_node_get_name           (node:PaxlNode):Pchar;
                                            cdecl;external External_library name 'axl_node_get_name';

//axlNode * axl_node_get_parent         (axlNode * node);
function    axl_node_get_parent         (node:PaxlNode):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_parent';

//axlNode * axl_node_get_next           (axlNode * node);
function    axl_node_get_next           (node:PaxlNode):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_next';

//axlNode * axl_node_get_next_called    (axlNode * node,
//                                       char    * name);
function    axl_node_get_next_called    (node:PaxlNode;
                                         name:Pchar):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_next_called';

//axlNode * axl_node_get_previous         (axlNode * node);
function    axl_node_get_previous         (node:PaxlNode):PaxlNode;
                                           cdecl;external External_library name 'axl_node_get_previous';

//axlNode * axl_node_get_previous_called  (axlNode * node,
//                                         char    * name);
function    axl_node_get_previous_called  (node:PaxlNode;
                                           name:Pchar):PaxlNode;
                                           cdecl;external External_library name 'axl_node_get_previous_called';

//axlNode * axl_node_get_first_child    (axlNode * node);
function    axl_node_get_first_child    (node:PaxlNode):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_first_child';

//axlNode * axl_node_get_last_child     (axlNode * node);
function    axl_node_get_last_child     (node:PaxlNode):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_last_child';

//void      axl_node_set_child          (axlNode * parent,
//                                       axlNode * child);
procedure   axl_node_set_child          (parent:PaxlNode;
                                         child:PaxlNode);
                                         cdecl;external External_library name 'axl_node_set_child';

//void      axl_node_set_child_after    (axlNode * reference,
//                                       axlNode * child);
procedure   axl_node_set_child_after    (reference:PaxlNode;
                                         child:PaxlNode);
                                         cdecl;external External_library name 'axl_node_set_child_after';

//void      axl_node_replace            (axlNode * node,
//                                       axlNode * new_node,
//                                       axl_bool  dealloc);
procedure   axl_node_replace            (node:PaxlNode;
                                         new_node:PaxlNode;
                                         dealloc:Taxl_bool);
                                         cdecl;external External_library name 'axl_node_replace';

//void      axl_node_remove             (axlNode * node,
//                                       axl_bool  dealloc);
procedure   axl_node_remove             (node:PaxlNode;
                                         dealloc:Taxl_bool);
                                         cdecl;external External_library name 'axl_node_remove';

//void      axl_node_deattach           (axlNode * node);
procedure   axl_node_deattach           (node:PaxlNode);
                                         cdecl;external External_library name 'axl_node_deattach';

//void      axl_node_set_is_empty       (axlNode  * node,
//                                       axl_bool   empty);
procedure   axl_node_set_is_empty       (node:PaxlNode;
                                         empty:Taxl_bool);
                                         cdecl;external External_library name 'axl_node_set_is_empty';

//axl_bool      axl_node_is_empty           (axlNode * node);
function        axl_node_is_empty           (node:PaxlNode):Taxl_bool;
                                             cdecl;external External_library name 'axl_node_is_empty';

//const char    * axl_node_get_content  (axlNode * node,
//                                       int  * content_size);
function          axl_node_get_content  (node:PaxlNode;
                                         var content_size:longint):Pchar;
                                         cdecl;external External_library name 'axl_node_get_content';

//char    * axl_node_get_content_copy   (axlNode * node,
//                                       int  * content_size);
function    axl_node_get_content_copy   (node:PaxlNode;
                                         var content_size:longint):Pchar;
                                         cdecl;external External_library name 'axl_node_get_content_copy';

//char    * axl_node_get_content_trans  (axlNode * node,
//                                       int * content_size);
function    axl_node_get_content_trans  (node:PaxlNode;
                                         var content_size:longint):Pchar;
                                         cdecl;external External_library name 'axl_node_get_content_trans';

//char    * axl_node_get_content_trim   (axlNode * node,
//                                       int * content_size);
function    axl_node_get_content_trim   (node:PaxlNode;
                                         var content_size:longint):Pchar;
                                         cdecl;external External_library name 'axl_node_get_content_trim';

//void      axl_node_set_content        (axlNode    * node,
//                                       const char * content,
//                                       int content_size);
procedure   axl_node_set_content        (node:PaxlNode;
                                        const content:Pchar;
                                        content_size:longint);
                                        cdecl;external External_library name 'axl_node_set_content';

//void      axl_node_set_content_ref    (axlNode * node,
//                                      char * content,
//                                      int content_size);
procedure   axl_node_set_content_ref    (node:PaxlNode;
                                         content:Pchar;
                                         content_size:longint);
                                         cdecl;external External_library name 'axl_node_set_content_ref';

//void      axl_node_set_content_from_factory (axlFactory * factory,
//                                            axlNode    * node,
//                                            char       * content,
//                                            int          content_size);
procedure   axl_node_set_content_from_factory (factory:PaxlFactory;
                                                node:PaxlNode;
                                                content:Pchar;
                                                content_size:longint);
                                                cdecl;external External_library name 'axl_node_set_content_from_factory';

//void      axl_node_set_cdata_content  (axlNode    * node,
//                                      const char * content,
//                                      int          content_size);
procedure   axl_node_set_cdata_content  (node:PaxlNode;
                                         const content:Pchar;
                                         content_size:longint);
                                         cdecl;external External_library name 'axl_node_set_cdata_content';

//void      axl_node_set_cdata_content_from_factory (axlFactory  * factory,
//                                                  axlNode     * node,
//                                                  char        * content,
//                                                  int           content_size);
procedure   axl_node_set_cdata_content_from_factory (factory:PaxlFactory;
                                                     node:PaxlNode;
                                                     content:Pchar;
                                                     content_size:longint);
                                                     cdecl;external External_library name 'axl_node_set_cdata_content_from_factory';

//void      axl_node_set_comment        (axlNode * node,
//                                       char * comment,
//                                       int    comment_size);
procedure   axl_node_set_comment        (node:PaxlNode;
                                         comment:Pchar;
                                         comment_size:longint);
                                         cdecl;external External_library name 'axl_node_set_comment';

//void      axl_node_set_have_childs    (axlNode * node, axl_bool childs);
procedure   axl_node_set_have_childs    (node:PaxlNode; childs:Taxl_bool);
                                         cdecl;external External_library name 'axl_node_set_have_childs';

//axl_bool  axl_node_have_childs        (axlNode * node);
function    axl_node_have_childs        (node:PaxlNode):Taxl_bool;
                                         cdecl;external External_library name 'axl_node_have_childs';

//axlNode * axl_node_get_child_called   (axlNode * parent, char * name);
function    axl_node_get_child_called   (parent:PaxlNode;
                                         name:Pchar):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_child_called';

//axlNode * axl_node_find_called        (axlNode * parent, char * name);
function    axl_node_find_called        (parent:PaxlNode; name:Pchar):PaxlNode;
                                         cdecl;external External_library name 'axl_node_find_called';

//axlNode * axl_node_get_child_nth      (axlNode * parent, int position);
function    axl_node_get_child_nth      (parent:PaxlNode;
                                         position:longint):PaxlNode;
                                         cdecl;external External_library name 'axl_node_get_child_nth';

//int       axl_node_get_child_num      (axlNode * parent);
function    axl_node_get_child_num      (parent:PaxlNode):longint;
                                         cdecl;external External_library name 'axl_node_get_child_num';

//axlList * axl_node_get_childs         (axlNode * node);
function    axl_node_get_childs         (node:PaxlNode):PaxlList;
                                         cdecl;external External_library name 'axl_node_get_childs';

//axl_bool  axl_node_are_equal          (axlNode * node, axlNode * node2);
function    axl_node_are_equal          (node:PaxlNode; node2:PaxlNode):Taxl_bool;
                                         cdecl;external External_library name 'axl_node_are_equal';

//axl_bool  axl_node_are_equal_full     (axlNode * node, axlNode * node2, axlError ** error);
function    axl_node_are_equal_full     (node:PaxlNode; node2:PaxlNode; error:PPaxlError):Taxl_bool;
                                         cdecl;external External_library name 'axl_node_are_equal_full';

//void      axl_node_add_pi_target            (axlNode * node,
//                                             char * target,
//                                             char * content);
procedure   axl_node_add_pi_target            (node:PaxlNode;
                                               target:Pchar;
                                               content:Pchar);
                                               cdecl;external External_library name 'axl_node_add_pi_target';

//axl_bool  axl_node_has_pi_target            (axlNode * node,
//                                             char * pi_target);
function    axl_node_has_pi_target            (node:PaxlNode;
                                               pi_target:Pchar):Taxl_bool;
                                               cdecl;external External_library name 'axl_node_has_pi_target';

//char    * axl_node_get_pi_target_content    (axlNode * node,
//                                             char * pi_target);
function    axl_node_get_pi_target_content    (node:PaxlNode;
                                               pi_target:Pchar):Pchar;
                                               cdecl;external External_library name 'axl_node_get_pi_target_content';

//axlList * axl_node_get_pi_target_list       (axlNode * node);
function    axl_node_get_pi_target_list       (node:PaxlNode):PaxlList;
                                               cdecl;external External_library name 'axl_node_get_pi_target_list';

//void      axl_node_transfer_childs          (axlNode * old_parent,
//                                             axlNode * new_parent);
procedure   axl_node_transfer_childs          (old_parent:PaxlNode;
                                               new_parent:PaxlNode);
                                               cdecl;external External_library name 'axl_node_transfer_childs';

//axl_bool  axl_node_dump                    (axlNode  * node,
//                                            char    ** content,
//                                            int      * size);
function    axl_node_dump                    (node:PaxlNode;
                                              content:PPchar;
                                              var size:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_node_dump';

//axl_bool  axl_node_dump_pretty             (axlNode  * node,
//                                            char    ** content,
//                                            int      * size,
//                                            int        tabular);
function    axl_node_dump_pretty             (node:PaxlNode;
                                              content:PPchar;
                                              var size:longint;
                                              tabular:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_node_dump_pretty';

//axl_bool  axl_node_dump_to_file            (axlNode  * node,
//                                            char     * file_path);
function    axl_node_dump_to_file            (node:PaxlNode;
                                              file_path:Pchar):Taxl_bool;
                                              cdecl;external External_library name 'axl_node_dump_to_file';

//axl_bool  axl_node_dump_pretty_to_file     (axlNode  * node,
//                                            char     * file_path,
//                                            int        tabular);
function    axl_node_dump_pretty_to_file     (node:PaxlNode;
                                              file_path:Pchar;
                                              tabular:longint):Taxl_bool;
                                              cdecl;external External_library name 'axl_node_dump_pretty_to_file';

//int       axl_node_get_flat_size            (axlNode * node,
//                                            axl_bool  pretty_print,
//                                            int       level,
//                                            int       tabular);
function    axl_node_get_flat_size            (node:PaxlNode;
                                               pretty_print:Taxl_bool;
                                               level:longint;
                                               tabular:longint):longint;
                                               cdecl;external External_library name 'axl_node_get_flat_size';

//int       axl_node_dump_at                  (axlNode * node,
//                                            char    * content,
//                                            int       desp,
//                                            axl_bool  pretty_print,
//                                            int       level,
//                                            int       tabular);
function    axl_node_dump_at                 (node:PaxlNode;
                                              content:Pchar;
                                              desp:longint;
                                              pretty_print:Taxl_bool;
                                              level:longint;
                                              tabular:longint):longint;
                                              cdecl;external External_library name 'axl_node_dump_at';

//axl_bool    axl_node_has_invalid_chars        (const char * content,
//                                               int    content_size,
//                                               int  * added_size);
function      axl_node_has_invalid_chars        (const content:Pchar;
                                                 content_size:longint;
                                                 var added_size:longint):Taxl_bool;
                                                 cdecl;external External_library name 'axl_node_has_invalid_chars';

//axl_bool    axl_node_has_invalid_chars_cdata    (const char * content,
//                                                 int          content_size,
//                                                 int        * added_size);
function      axl_node_has_invalid_chars_cdata    (const content:Pchar;
                                                   content_size:longint;
                                                   var added_size:longint):Taxl_bool;
                                                   cdecl;external External_library name 'axl_node_has_invalid_chars_cdata';

//char      * axl_node_content_translate_defaults (char * content,
//                                                 int  * content_size);
function      axl_node_content_translate_defaults (content:Pchar;
                                                   var content_size:longint):Pchar;
                                                   cdecl;external External_library name 'axl_node_content_translate_defaults';

//char      * axl_node_content_copy_and_escape    (const char * content,
//                                                 int          content_size,
//                                                 int          additional_size);
function      axl_node_content_copy_and_escape    (const content:Pchar;
                                                   content_size:longint;
                                                   additional_size:longint):Pchar;
                                                   cdecl;external External_library name 'axl_node_content_copy_and_escape';

//char * axl_node_content_copy_and_escape_cdata (const char * content,
//                                               int          content_size,
//                                               int          additional_size);
function axl_node_content_copy_and_escape_cdata (const content:Pchar;
                                                 content_size:longint;
                                                 additional_size:longint):Pchar;
                                                 cdecl;external External_library name 'axl_node_content_copy_and_escape_cdata';

//void      axl_node_free            (axlNode * node);
procedure   axl_node_free            (node:PaxlNode);
                                      cdecl;external External_library name 'axl_node_free';

//void      axl_node_free_full       (axlNode * node, axl_bool also_childs);
procedure   axl_node_free_full       (node:PaxlNode; also_childs:Taxl_bool);
                                      cdecl;external External_library name 'axl_node_free_full';

(* @} *)

(* item interface *)

//axlItem     * axl_item_new             (AxlItemType type,
//                                        axlPointer  data);
function        axl_item_new             (atype:TAxlItemType;
                                          data:TaxlPointer):PaxlItem;
                                          cdecl;external External_library name 'axl_item_new';

//axlItem     * axl_item_new_ref         (AxlItemType type,
//                                        axlPointer  data);
function        axl_item_new_ref         (atype:TAxlItemType;
                                          data:TaxlPointer):PaxlItem;
                                          cdecl;external External_library name 'axl_item_new_ref';

//axlDoc      * axl_item_get_doc         (axlItem * item);
function        axl_item_get_doc         (item:PaxlItem):PaxlDoc;
                                          cdecl;external External_library name 'axl_item_get_doc';

//void          axl_item_set_doc         (axlItem * item, axlDoc * doc);
procedure       axl_item_set_doc         (item:PaxlItem; doc:PaxlDoc);
                                          cdecl;external External_library name 'axl_item_set_doc';

//axlNode     * axl_item_get_parent      (axlItem * item);
function        axl_item_get_parent      (item:PaxlItem):PaxlNode;
                                          cdecl;external External_library name 'axl_item_get_parent';

//axlItem     * axl_item_get_next        (axlItem * item);
function        axl_item_get_next        (item:PaxlItem):PaxlItem;
                                          cdecl;external External_library name 'axl_item_get_next';

//axlItem     * axl_item_node_next       (axlNode * node);
function        axl_item_node_next       (node:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_node_next';

//axlItem     * axl_item_get_previous    (axlItem * item);
function        axl_item_get_previous    (item:PaxlItem):PaxlItem;
                                          cdecl;external External_library name 'axl_item_get_previous';

//axlItem     * axl_item_node_previous   (axlNode * node);
function        axl_item_node_previous   (node:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_node_previous';

//axlItem     * axl_item_node_holder     (axlNode * node);
function        axl_item_node_holder     (node:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_node_holder';

//axlItem     * axl_item_get_first_child (axlNode * node);
function        axl_item_get_first_child (node:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_get_first_child';

//axlItem     * axl_item_get_last_child  (axlNode * node);
function        axl_item_get_last_child  (node:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_get_last_child';

//AxlItemType   axl_item_get_type        (axlItem * item);
function        axl_item_get_type        (item:PaxlItem):TAxlItemType;
                                          cdecl;external External_library name 'axl_item_get_type';

//axlPointer    axl_item_get_data        (axlItem * item);
function        axl_item_get_data        (item:PaxlItem):TaxlPointer;
                                          cdecl;external External_library name 'axl_item_get_data';

//char        * axl_item_get_content     (axlItem * item,
//                                        int * size);
function        axl_item_get_content     (item:PaxlItem;
                                          var size:longint):Pchar;
                                          cdecl;external External_library name 'axl_item_get_content';

//void          axl_item_set_child       (axlNode * parent,
//                                        AxlItemType type,
//                                        axlPointer data);
procedure       axl_item_set_child       (parent:PaxlNode;
                                          atype:TAxlItemType;
                                          data:TaxlPointer);
                                          cdecl;external External_library name 'axl_item_set_child';

//void          axl_item_set_child_ref   (axlNode * parent,
//                                        axlItem * item);
procedure       axl_item_set_child_ref   (parent:PaxlNode;
                                          item:PaxlItem);
                                          cdecl;external External_library name 'axl_item_set_child_ref';

//void          axl_item_set_after       (axlItem * item,
//                                        AxlItemType type,
//                                        axlPointer data);
procedure       axl_item_set_after       (item:PaxlItem;
                                          atype:TAxlItemType;
                                          data:TaxlPointer);
                                          cdecl;external External_library name 'axl_item_set_after';

//axlItem     * axl_item_copy            (axlItem * item,
//                                        axlNode * set_parent);
function        axl_item_copy            (item:PaxlItem;
                                          set_parent:PaxlNode):PaxlItem;
                                          cdecl;external External_library name 'axl_item_copy';

//void          axl_item_remove          (axlItem * item,
//                                        axl_bool  dealloc);
procedure       axl_item_remove          (item:PaxlItem;
                                          dealloc:Taxl_bool);
                                          cdecl;external External_library name 'axl_item_remove';

//void          axl_item_replace         (axlItem * item,
//                                        axlItem * new_item,
//                                        axl_bool  dealloc);
procedure       axl_item_replace         (item:PaxlItem;
                                          new_item:PaxlItem;
                                          dealloc:Taxl_bool);
                                          cdecl;external External_library name 'axl_item_replace';

//void          axl_item_transfer_childs_after (axlNode * old_parent,
//                                              axlItem * item_ref);
procedure       axl_item_transfer_childs_after (old_parent:PaxlNode;
                                                item_ref:PaxlItem);
                                                cdecl;external External_library name 'axl_item_transfer_childs_after';

//axl_bool      axl_item_are_equal      (axlItem * item,
//                                       axlItem * item2,
//                                       axl_bool      trimmed);
function        axl_item_are_equal      (item:PaxlItem;
                                         item2:PaxlItem;
                                         trimmed:Taxl_bool):Taxl_bool;
                                         cdecl;external External_library name 'axl_item_are_equal';

//axl_bool      axl_item_are_equal_full (axlItem    * item,
//                                       axlItem    * item2,
//                                       axl_bool     trimmed,
//                                       axlError  ** error);
function        axl_item_are_equal_full (item:PaxlItem;
                                         item2:PaxlItem;
                                         trimmed:Taxl_bool;
                                         error:PPaxlError):Taxl_bool;
                                         cdecl;external External_library name 'axl_item_are_equal_full';

//void          axl_item_free           (axlItem * item,
//                                       axl_bool      dealloc);
procedure       axl_item_free           (item:PaxlItem;
                                         dealloc:Taxl_bool);
                                         cdecl;external External_library name 'axl_item_free';

//axlFactory     * axl_item_factory_create ();
function           axl_item_factory_create :PaxlFactory;
                                            cdecl;external External_library name 'axl_item_factory_create';

//axlItem        * axl_item_factory_get (axlFactory * factory);
function           axl_item_factory_get (factory:PaxlFactory):PaxlItem;
                                         cdecl;external External_library name 'axl_item_factory_get';

//axlFactory     * axl_node_factory_create ();
function           axl_node_factory_create  :PaxlFactory;
                                             cdecl;external External_library name 'axl_node_factory_create';

//axlNode        * axl_node_factory_get (axlFactory * factory);
function           axl_node_factory_get (factory:PaxlFactory):PaxlNode;
                                         cdecl;external External_library name 'axl_node_factory_get';

//axlFactory     * axl_item_content_factory_create ();
function           axl_item_content_factory_create  :PaxlFactory;
                                                     cdecl;external External_library name 'axl_item_content_factory_create';

//axlFactory     * axl_item_attr_factory_create ();
function           axl_item_attr_factory_create :PaxlFactory;
                                                  cdecl;external External_library name 'axl_item_attr_factory_create';

