{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program ws_helper;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  wst_resources_utils,
  generator,
  parserutils,
  source_utils,
  command_line_parser,
  metadata_generator,
  binary_streamer,
  DOM,
  XMLWrite,
  XMLRead,
  wst_fpc_xml,
  pastree,
  pparser,
  pascal_parser_intf,
  logger_intf,
  xsd_parser,
  ws_parser_imp,
  wsdl_parser,
  xsd_generator, wsdl_generator,
  locators;

  
{$INCLUDE ws_helper_prog.inc}
