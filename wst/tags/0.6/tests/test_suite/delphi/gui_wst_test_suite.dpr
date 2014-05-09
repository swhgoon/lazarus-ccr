program gui_wst_test_suite;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  delphi_init_com,
  testmetadata_unit in '..\testmetadata_unit.pas',
  test_parsers in '..\test_parsers.pas',
  test_support in '..\test_support.pas',
  test_utilities in '..\test_utilities.pas',
  testformatter_unit in '..\testformatter_unit.pas',
  base_service_intf in '..\..\..\base_service_intf.pas',
  basex_encode in '..\..\..\basex_encode.pas',
  test_basex_encode in '..\test_basex_encode.pas',
  xsd_parser in '..\..\..\ws_helper\xsd_parser.pas',
  pascal_parser_intf in '..\..\..\ws_helper\pascal_parser_intf.pas',
  ws_parser_imp in '..\..\..\ws_helper\ws_parser_imp.pas',
  xsd_consts in '..\..\..\ws_helper\xsd_consts.pas',
  xsd_generator in '..\..\..\ws_helper\xsd_generator.pas',
  test_generators in '..\test_generators.pas',
  test_suite_utils in '..\test_suite_utils.pas',
  test_std_cursors in '..\test_std_cursors.pas',
  test_rtti_filter in '..\test_rtti_filter.pas',
  test_wst_cursors in '..\test_wst_cursors.pas',
  test_registry in '..\test_registry.pas',
  test_soap_specific in '..\test_soap_specific.pas',
  test_generators_runtime in '..\test_generators_runtime.pas',
  date_utils in '..\..\..\date_utils.pas',
  test_date_utils in '..\test_date_utils.pas',
  config_objects in '..\..\..\config_objects.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
