{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}
program ws_helper;

{$APPTYPE CONSOLE}

uses
  delphi_init_com,
  Classes,
  SysUtils,
  wst_resources_utils,
  generator,
  wsdl_generator,
  parserutils,
  source_utils,
  command_line_parser,
  metadata_generator,
  binary_streamer,
  XMLDoc,
  XMLIntf,
  wst_delphi_xml,
  pastree,
  pparser,
  pascal_parser_intf,
  logger_intf,
  xsd_parser,
  ws_parser_imp,
  wsdl_parser,
  xsd_generator,
  locators;

{$INCLUDE ws_helper_prog.inc}


