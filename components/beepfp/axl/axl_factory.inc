
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

//axlFactory * axl_factory_create (int size_of_type);
function       axl_factory_create (size_of_type:longint):PaxlFactory;
                                  cdecl;external External_library name 'axl_factory_create';

//axlPointer   axl_factory_get (axlFactory * factory);
function       axl_factory_get (factory:PaxlFactory):TaxlPointer;
                                cdecl;external External_library name 'axl_factory_get';

//void         axl_factory_free (axlFactory * factory);
procedure      axl_factory_free (factory:PaxlFactory);
                                cdecl;external External_library name 'axl_factory_free';

//axlStrFactory * axl_string_factory_create ();
function          axl_string_factory_create :PaxlStrFactory;
                                              cdecl;external External_library name 'axl_string_factory_create';

//char          * axl_string_factory_alloc  (axlStrFactory * factory, int size);
function          axl_string_factory_alloc  (factory:PaxlStrFactory; size:longint):Pchar;
                                            cdecl;external External_library name 'axl_string_factory_alloc';

//void            axl_string_factory_free   (axlStrFactory * factory);
procedure         axl_string_factory_free   (factory:PaxlStrFactory);
                                              cdecl;external External_library name 'axl_string_factory_free';

