
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
 * \addtogroup axl_stack_module
 * @{
 *)

//axlStack * axl_stack_new (axlDestroyFunc destroy_data);
function     axl_stack_new (destroy_data:TaxlDestroyFunc):PaxlStack;
                            cdecl;external External_library name 'axl_stack_new';

//void       axl_stack_push (axlStack * stack, axlPointer data);
procedure    axl_stack_push (stack:PaxlStack; data:TaxlPointer);
                             cdecl;external External_library name 'axl_stack_push';

//axlPointer axl_stack_pop  (axlStack * stack);
function     axl_stack_pop  (stack:PaxlStack):TaxlPointer;
                             cdecl;external External_library name 'axl_stack_pop';

//axlPointer axl_stack_peek (axlStack * stack);
function     axl_stack_peek (stack:PaxlStack):TaxlPointer;
                             cdecl;external External_library name 'axl_stack_peek';

//axl_bool   axl_stack_foreach (axlStack         * stack,
//                              axlStackForeach2   func,
//                              axlPointer         user_data,
//                              axlPointer         user_data2);
function     axl_stack_foreach (stack:PaxlStack;
                                func:TaxlStackForeach2;
                                user_data:TaxlPointer;
                                user_data2:TaxlPointer):Taxl_bool;
                                cdecl;external External_library name 'axl_stack_foreach';

//axl_bool   axl_stack_foreach3 (axlStack         * stack,
//                               axlStackForeach3   func,
//                               axlPointer         user_data,
//                               axlPointer         user_data2,
//                               axlPointer         user_data3);
function     axl_stack_foreach3 (stack:PaxlStack;
                                 func:TaxlStackForeach3;
                                 user_data:TaxlPointer;
                                 user_data2:TaxlPointer;
                                 user_data3:TaxlPointer):Taxl_bool;
                                 cdecl;external External_library name 'axl_stack_foreach3';

//int        axl_stack_size (axlStack * stack);
function     axl_stack_size (stack:PaxlStack):longint;
                             cdecl;external External_library name 'axl_stack_size';

//axl_bool   axl_stack_is_empty (axlStack * stack);
function     axl_stack_is_empty (stack:PaxlStack):Taxl_bool;
                                 cdecl;external External_library name 'axl_stack_is_empty';

//void       axl_stack_free (axlStack * stack);
procedure    axl_stack_free (stack:PaxlStack);
                             cdecl;external External_library name 'axl_stack_free';

//axlBinaryStack * axl_binary_stack_new ();
function           axl_binary_stack_new :PaxlBinaryStack;
                                         cdecl;external External_library name 'axl_binary_stack_new';

//void             axl_binary_stack_push          (axlBinaryStack * bstack, axl_bool status);
procedure          axl_binary_stack_push          (bstack:PaxlBinaryStack; status:Taxl_bool);
                                                   cdecl;external External_library name 'axl_binary_stack_push';

//void             axl_binary_stack_push_the_same (axlBinaryStack * bstack);
procedure          axl_binary_stack_push_the_same (bstack:PaxlBinaryStack);
                                                   cdecl;external External_library name 'axl_binary_stack_push_the_same';

//axl_bool         axl_binary_stack_pop  (axlBinaryStack * bstack);
function           axl_binary_stack_pop  (bstack:PaxlBinaryStack):Taxl_bool;
                                          cdecl;external External_library name 'axl_binary_stack_pop';

//axl_bool         axl_binary_stack_peek (axlBinaryStack * bstack);
function           axl_binary_stack_peek (bstack:PaxlBinaryStack):Taxl_bool;
                                          cdecl;external External_library name 'axl_binary_stack_peek';

//axl_bool         axl_binary_stack_is_empty (axlBinaryStack * bstack);
function           axl_binary_stack_is_empty (bstack:PaxlBinaryStack):Taxl_bool;
                                              cdecl;external External_library name 'axl_binary_stack_is_empty';

//int              axl_binary_stack_size     (axlBinaryStack * bstack);
function           axl_binary_stack_size     (bstack:PaxlBinaryStack):longint;
                                              cdecl;external External_library name 'axl_binary_stack_size';

//void             axl_binary_stack_free (axlBinaryStack * bstack);
procedure          axl_binary_stack_free (bstack:PaxlBinaryStack);
                                          cdecl;external External_library name 'axl_binary_stack_free';

(* @} *)

