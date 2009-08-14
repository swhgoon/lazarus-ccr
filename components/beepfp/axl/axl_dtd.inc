
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
 * \addtogroup axl_dtd_module
 * @{
 *)

//axlDtd             * axl_dtd_parse            (const char * entity,
//                                               int          entity_size,
//                                               axlError ** error);
function               axl_dtd_parse            (const entity:Pchar;
                                                 entity_size: longint;
                                                 error:PPaxlError):PaxlDtd;
                                                 cdecl;external External_library name 'axl_dtd_parse';

//axlDtd             * axl_dtd_parse_from_file  (const char * file_path,
//                                               axlError ** error);
function               axl_dtd_parse_from_file  (const file_path:Pchar;
                                                 error:PPaxlError):PaxlDtd;
                                                 cdecl;external External_library name 'axl_dtd_parse_from_file';

//axl_bool             axl_dtd_validate         (axlDoc * doc,
//                                               axlDtd * dtd,
//                                               axlError ** error);
function               axl_dtd_validate         (doc:PaxlDoc;
                                                 dtd:PaxlDtd;
                                                 error:PPaxlError):Taxl_bool;
                                                 cdecl;external External_library name 'axl_dtd_validate';

//axl_bool             axl_dtd_are_equal        (axlDtd * dtd,
//                                               axlDtd * dtd2);
function               axl_dtd_are_equal        (dtd:PaxlDtd;
                                                 dtd2:PaxlDtd):Taxl_bool;
                                                 cdecl;external External_library name 'axl_dtd_are_equal';

//axlDtdElement      * axl_dtd_get_root         (axlDtd * dtd);
function               axl_dtd_get_root         (dtd:PaxlDtd):PaxlDtdElement;
                                                 cdecl;external External_library name 'axl_dtd_get_root';

//axlDtdElement      * axl_dtd_get_element      (axlDtd * dtd,
//                                               const char * name);
function               axl_dtd_get_element      (dtd:PaxlDtd;
                                                 const name:Pchar):PaxlDtdElement;
                                                 cdecl;external External_library name 'axl_dtd_get_element';

//char               * axl_dtd_get_element_name (axlDtdElement * element);
function               axl_dtd_get_element_name (element:PaxlDtdElement):Pchar;
                                                 cdecl;external External_library name 'axl_dtd_get_element_name';

//AxlDtdElementType    axl_dtd_get_element_type (axlDtdElement * element);
function               axl_dtd_get_element_type (element:PaxlDtdElement):TAxlDtdElementType;
                                                 cdecl;external External_library name 'axl_dtd_get_element_type';

//axlDtdElementList  * axl_dtd_get_item_list    (axlDtdElement * element);
function               axl_dtd_get_item_list    (element:PaxlDtdElement):PaxlDtdElementList;
                                                 cdecl;external External_library name 'axl_dtd_get_item_list';

//axl_bool             axl_dtd_element_is_toplevel (axlDtd * dtd, axlDtdElement * element);
function               axl_dtd_element_is_toplevel (dtd:PaxlDtd; element:PaxlDtdElement):Taxl_bool;
                                                    cdecl;external External_library name 'axl_dtd_element_is_toplevel';

//int                  axl_dtd_item_list_count  (axlDtdElementList * itemList);
function               axl_dtd_item_list_count  (itemList:PaxlDtdElementList):longint;
                                                 cdecl;external External_library name 'axl_dtd_item_list_count';

//AxlDtdNestedType     axl_dtd_item_list_type   (axlDtdElementList * itemList);
function               axl_dtd_item_list_type   (itemList:PaxlDtdElementList):TAxlDtdNestedType;
                                                 cdecl;external External_library name 'axl_dtd_item_list_type';

//AxlDtdTimes          axl_dtd_item_list_repeat (axlDtdElementList * itemList);
function               axl_dtd_item_list_repeat (itemList:PaxlDtdElementList):TAxlDtdTimes;
                                                 cdecl;external External_library name 'axl_dtd_item_list_repeat';

//axlDtdElementListNode * axl_dtd_item_list_get_node (axlDtdElementList * itemList,
//                                                    int position);
function                  axl_dtd_item_list_get_node (itemList:PaxlDtdElementList;
                                                      position:longint):PaxlDtdElementListNode;
                                                      cdecl;external External_library name 'axl_dtd_item_list_get_node';

//NodeType             axl_dtd_item_node_get_type (axlDtdElementListNode * node);
function               axl_dtd_item_node_get_type (node:PaxlDtdElementListNode):TNodeType;
                                                   cdecl;external External_library name 'axl_dtd_item_node_get_type';

//axlDtdElementList  * axl_dtd_item_node_get_list (axlDtdElementListNode * node);
function               axl_dtd_item_node_get_list (node:PaxlDtdElementListNode):PaxlDtdElementList;
                                                   cdecl;external External_library name 'axl_dtd_item_node_get_list';

//char               * axl_dtd_item_node_get_value (axlDtdElementListNode * node);
function               axl_dtd_item_node_get_value (node:PaxlDtdElementListNode):Pchar;
                                                    cdecl;external External_library name 'axl_dtd_item_node_get_value';

//AxlDtdTimes          axl_dtd_item_node_get_repeat (axlDtdElementListNode * node);
function               axl_dtd_item_node_get_repeat (node:PaxlDtdElementListNode):TAxlDtdTimes;
                                                     cdecl;external External_library name 'axl_dtd_item_node_get_repeat';

//axl_bool             axl_dtd_entity_exists    (axlDtd            * dtd,
//                                               const char        * name,
//                                               axlDtdEntityType    type);
function               axl_dtd_entity_exists    (dtd:PaxlDtd;
                                                 const name:Pchar;
                                                 atype:TaxlDtdEntityType):Taxl_bool;
                                                 cdecl;external External_library name 'axl_dtd_entity_exists';

//char               * axl_dtd_entity_value     (axlDtd            * dtd,
//                                                const char        * name,
//                                                axlDtdEntityType    type);
function               axl_dtd_entity_value     (dtd:PaxlDtd;
                                                 const name:Pchar;
                                                 atype:TaxlDtdEntityType):Pchar;
                                                 cdecl;external External_library name 'axl_dtd_entity_value';

//axlDtdAttribute    * axl_dtd_get_attr         (axlDtd * dtd,
//                                               const char * nodeName);
function               axl_dtd_get_attr         (dtd:PaxlDtd;
                                                 const nodeName:Pchar):PaxlDtdAttribute;
                                                 cdecl;external External_library name 'axl_dtd_get_attr';

//int                  axl_dtd_get_attr_contraints (axlDtd * dtd,
//                                                  const char * nodeName);
function               axl_dtd_get_attr_contraints (dtd:PaxlDtd;
                                                    const nodeName:Pchar):longint;
                                                    cdecl;external External_library name 'axl_dtd_get_attr_contraints';

//void                 axl_dtd_free             (axlDtd * dtd);
procedure              axl_dtd_free             (dtd:PaxlDtd);
                                                 cdecl;external External_library name 'axl_dtd_free';

//void                 axl_dtd_element_free     (axlDtdElement * element);
procedure              axl_dtd_element_free     (element:PaxlDtdElement);
                                                 cdecl;external External_library name 'axl_dtd_element_free';

//void                 axl_dtd_item_list_free   (axlDtdElementList * list);
procedure              axl_dtd_item_list_free   (list:PaxlDtdElementList);
                                                 cdecl;external External_library name 'axl_dtd_item_list_free';

//axl_bool             axl_dtd_check_entity_ref_and_expand  (axlDtdEntityResolver   resolver,
//                                                           axlPointer             data,
//                                                           axlStream            * stream,
//                                                           const           char * prefix,
//                                                           axlError            ** error);
function               axl_dtd_check_entity_ref_and_expand  (resolver:TaxlDtdEntityResolver;
                                                             data:TaxlPointer;
                                                             stream:PaxlStream;
                                                             const prefix:Pchar;
                                                             error:PPaxlError):Taxl_bool;
                                                             cdecl;external External_library name 'axl_dtd_check_entity_ref_and_expand';

(* @} *)

