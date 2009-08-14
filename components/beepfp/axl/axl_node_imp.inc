
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

{$inline on}

//#define HAS_ATTR(node,attr) axl_node_has_attribute(node,attr)
function HAS_ATTR(node:PaxlNode; const attribute:Pchar):Taxl_bool; inline;
begin
  Result := axl_node_has_attribute(node,attribute);
end;

//#define ATTR_VALUE(node,attr) axl_node_get_attribute_value(node, attr)
function ATTR_VALUE(node:PaxlNode; const attribute:Pchar):Pchar; inline;
begin
  Result := axl_node_get_attribute_value(node, attribute);
end;

//#define ATTR_VALUE_TRANS(node,attr) axl_node_get_attribute_value_trans(node,attr)
function ATTR_VALUE_TRANS(node:PaxlNode; const attribute:Pchar):Pchar; inline;
begin
  Result := axl_node_get_attribute_value_trans(node,attribute);
end;

//#define ATTR_VALUE_TRIMMED(node,attr) axl_node_get_attribute_value_trimmed(node,attr)
function ATTR_VALUE_TRIMMED(node:PaxlNode; const attribute:Pchar):Pchar; inline;
begin
  Result := axl_node_get_attribute_value_trimmed(node,attribute);
end;

//#define HAS_ATTR_VALUE(node, attr,value) (axl_node_has_attribute_value (node, attr, value))
function HAS_ATTR_VALUE(node:PaxlNode; const attribute:Pchar; const value:Pchar):Taxl_bool; inline;
begin
  Result := axl_node_has_attribute_value (node, attribute, value);
end;

//#define NODE_CMP_NAME(node,name) (axl_cmp ((node != NULL) ? axl_node_get_name(node) : "><", (name != NULL) ? name : "<>"))
//function       axl_node_get_name           (node:PaxlNode):Pchar;

{$inline off}

