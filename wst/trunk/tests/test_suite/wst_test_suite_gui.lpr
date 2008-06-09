program wst_test_suite_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, GuiTestRunner,
  TestFormatter_unit, testmetadata_unit,
  server_service_soap, soap_formatter, base_binary_formatter,
  base_service_intf, base_soap_formatter, binary_formatter, binary_streamer,
  server_binary_formatter, metadata_repository,
  metadata_generator, parserdefs, server_service_intf, metadata_wsdl,
  test_parserdef, base_xmlrpc_formatter, wst_fpc_xml, test_utilities,
  server_service_xmlrpc, test_parsers, wsdl_generator, xsd_generator,
  xsd_consts, base_json_formatter, wsdl_parser, test_support, basex_encode,
  test_basex_encode, json_formatter, server_service_json, test_json,
  test_suite_utils, test_generators, fpcunittestrunner;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

