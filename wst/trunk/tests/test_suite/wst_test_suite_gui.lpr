program wst_test_suite_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring,
  {$ENDIF}
  Interfaces, Forms, GuiTestRunner, TestFormatter_unit, testmetadata_unit,
  server_service_soap, soap_formatter, base_binary_formatter, base_service_intf,
  base_soap_formatter, binary_formatter, binary_streamer,
  server_binary_formatter, metadata_repository, metadata_generator,
  server_service_intf, metadata_wsdl, base_xmlrpc_formatter, wst_fpc_xml,
  test_utilities, server_service_xmlrpc, test_parsers, wsdl_generator,
  xsd_generator, xsd_consts, base_json_formatter, wsdl_parser, test_support,
  basex_encode, test_basex_encode, json_formatter, server_service_json,
  test_json, test_suite_utils, test_generators, fpcunittestrunner,
  test_std_cursors, test_rtti_filter, rtti_filters, wst_cursors,
  test_wst_cursors, test_registry, test_soap_specific, test_generators_runtime,
  test_date_utils, config_objects, test_support_client, test_filter, wst_consts;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

